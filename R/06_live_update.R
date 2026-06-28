library(brms)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

# ── Flujo de uso ───────────────────────────────────────────────────────────────
# Corre source("R/06_live_update.R") después de cada jornada.
# Los resultados reales se descargan automáticamente de martj42/international_results
# (misma fuente que el modelo histórico, actualizada en tiempo casi real).
# ──────────────────────────────────────────────────────────────────────────────

# ── 0. Auto-sincronizar resultados reales desde martj42 ──────────────────────

fetch_wc_results <- function(path = "data/live/resultados_reales.csv") {
  url <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"

  history_live <- tryCatch(
    read_csv(url, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("  ⚠ Sin conexión — usando CSV existente sin actualizar")
      return(NULL)
    }
  )
  if (is.null(history_live)) return(invisible(FALSE))

  wc2026 <- history_live |>
    filter(
      str_detect(tournament, regex("FIFA World Cup", ignore_case = TRUE)),
      !str_detect(tournament, regex("qualif|eliminat", ignore_case = TRUE)),
      year(date) == 2026,
      !is.na(home_score), !is.na(away_score)
    )

  if (nrow(wc2026) == 0) {
    message("  Sin resultados del Mundial 2026 en la fuente todavía.")
    return(invisible(FALSE))
  }

  resultados <- read_csv(path, show_col_types = FALSE)
  n_new <- 0L

  for (i in seq_len(nrow(wc2026))) {
    home <- wc2026$home_team[i]
    away <- wc2026$away_team[i]
    gh   <- as.integer(wc2026$home_score[i])
    ga   <- as.integer(wc2026$away_score[i])

    idx <- which(
      (resultados$equipo1 == home & resultados$equipo2 == away) |
      (resultados$equipo1 == away & resultados$equipo2 == home)
    )
    if (length(idx) == 0) next
    idx <- idx[1]

    # Solo actualizar si aún está en NA (no sobreescribir correcciones manuales)
    if (!is.na(resultados$g1[idx])) next

    if (resultados$equipo1[idx] == home) {
      resultados$g1[idx] <- gh
      resultados$g2[idx] <- ga
    } else {
      resultados$g1[idx] <- ga
      resultados$g2[idx] <- gh
    }
    n_new <- n_new + 1L
  }

  write_csv(resultados, path)

  n_total <- sum(!is.na(resultados$g1))
  message(sprintf("  ✓ %d partidos nuevos añadidos  (%d/72 completados)", n_new, n_total))
  invisible(TRUE)
}

message("── 0. Sincronizando resultados reales... ──")
fetch_wc_results("data/live/resultados_reales.csv")

# ── 1. Cargar resultados reales ───────────────────────────────────────────────

live_path <- "data/live/resultados_reales.csv"
if (!file.exists(live_path)) {
  stop("No existe ", live_path, ". Corre primero source('R/04_simulation.R') para generarlo.")
}

resultados <- read_csv(live_path, show_col_types = FALSE)

jugados    <- resultados |> filter(!is.na(g1), !is.na(g2))
pendientes <- resultados |> filter(is.na(g1) | is.na(g2))

message(sprintf("Partidos jugados:   %d / 72", nrow(jugados)))
message(sprintf("Partidos pendientes: %d / 72", nrow(pendientes)))

if (nrow(jugados) == 0) {
  message("\nAún no hay resultados reales. Corre 04_simulation.R en su lugar.")
  stop("Sin resultados reales.", call. = FALSE)
}

if (nrow(pendientes) == 0) {
  message("\nTodos los partidos están jugados — fase de grupos completa.")
  message("Las clasificaciones son deterministas; no se necesita simulación.")
}

# ── 2. Cargar modelo y draws del posterior ────────────────────────────────────

message("\nCargando modelo y draws...")
elo_ratings <- read_csv("data/processed/elo_ratings.csv", show_col_types = FALSE)
elo_lookup  <- as.list(setNames(elo_ratings$elo, elo_ratings$team))

dc_path <- "output/posteriors/draws_DC_wc_vs_wc.rds"
use_dc  <- file.exists(dc_path)

if (use_dc) {
  message("Modelo: Dixon-Coles (Poisson + corrección ρ)")
  draws_df <- readRDS(dc_path)
} else {
  message("Modelo: NegBinomial brms (Modelo B)")
  fit_B    <- readRDS("output/posteriors/fit_B_wc_vs_wc.rds")
  draws_df <- as_draws_df(fit_B)
}

extract_re <- function(draws, prefix) {
  pattern <- paste0(prefix, "[")
  cols    <- names(draws)[grepl(pattern, names(draws), fixed = TRUE)]
  if (length(cols) == 0) return(data.frame())
  result    <- as.data.frame(draws[, cols, drop = FALSE])
  raw_names <- sub(paste0(".*", prefix, "\\[([^,]+),.*"), "\\1", cols)
  names(result) <- gsub("\\.", " ", raw_names)
  result
}

ataque     <- extract_re(draws_df, "r_equipo")
defensa    <- extract_re(draws_df, "r_rival")
intercepto <- draws_df$b_Intercept
b_elo      <- draws_df$b_elo_diff
rho_s      <- if (use_dc && "rho" %in% names(draws_df)) as.numeric(draws_df$rho) else NULL
n_draws    <- length(intercepto)

get_re <- function(re_df, team, n) {
  col <- re_df[[team]]
  if (is.null(col)) rep(0.0, n) else as.numeric(col)
}

# ── 3. Simulación híbrida ─────────────────────────────────────────────────────
# Partidos jugados   → resultado fijo, igual en todos los escenarios
# Partidos pendientes → simulados con el modelo bayesiano

n_sim <- 10000
set.seed(2026)
idx <- sample(seq_len(n_draws), n_sim, replace = TRUE)

int_s   <- intercepto[idx]
b_elo_s <- b_elo[idx]
atk     <- as.data.frame(ataque[idx, , drop = FALSE])
def     <- as.data.frame(defensa[idx, , drop = FALSE])
rho_sim <- if (!is.null(rho_s)) rho_s[idx] else NULL

# Construir sims de partidos jugados (resultado idéntico en todos los escenarios)
if (nrow(jugados) > 0) {
  sims_jugados <- jugados |>
    crossing(sim_id = seq_len(n_sim)) |>
    mutate(
      pts1 = if_else(g1 > g2, 3L, if_else(g1 == g2, 1L, 0L)),
      pts2 = if_else(g2 > g1, 3L, if_else(g1 == g2, 1L, 0L))
    )
} else {
  sims_jugados <- tibble()
}

# Simular partidos pendientes
if (nrow(pendientes) > 0) {
  pb <- txtProgressBar(min = 0, max = nrow(pendientes), style = 3, width = 50)
  message(sprintf("\nSimulando %d partidos pendientes × %d escenarios...\n",
                  nrow(pendientes), n_sim))

  sims_pend_list <- vector("list", nrow(pendientes))

  for (i in seq_len(nrow(pendientes))) {
    t1 <- pendientes$equipo1[i]
    t2 <- pendientes$equipo2[i]

    elo1 <- if (!is.null(elo_lookup[[t1]])) elo_lookup[[t1]] else 1500
    elo2 <- if (!is.null(elo_lookup[[t2]])) elo_lookup[[t2]] else 1500
    elo_diff_t1 <- (elo1 - elo2) / 400

    lambda1 <- exp(int_s + get_re(atk, t1, n_sim) - get_re(def, t2, n_sim) + b_elo_s *  elo_diff_t1)
    lambda2 <- exp(int_s + get_re(atk, t2, n_sim) - get_re(def, t1, n_sim) + b_elo_s * -elo_diff_t1)

    g1 <- rpois(n_sim, lambda1)
    g2 <- rpois(n_sim, lambda2)

    if (!is.null(rho_sim)) {
      tau        <- rep(1.0, n_sim)
      m00 <- g1 == 0L & g2 == 0L
      m10 <- g1 == 1L & g2 == 0L
      m01 <- g1 == 0L & g2 == 1L
      m11 <- g1 == 1L & g2 == 1L
      if (any(m00)) tau[m00] <- pmax(1 - lambda1[m00] * lambda2[m00] * rho_sim[m00], 1e-10)
      if (any(m10)) tau[m10] <- pmax(1 + lambda2[m10] * rho_sim[m10],               1e-10)
      if (any(m01)) tau[m01] <- pmax(1 + lambda1[m01] * rho_sim[m01],               1e-10)
      if (any(m11)) tau[m11] <- pmax(1 - rho_sim[m11],                              1e-10)
      dc_idx <- sample.int(n_sim, n_sim, replace = TRUE, prob = tau / sum(tau))
      g1 <- g1[dc_idx]; g2 <- g2[dc_idx]
    }

    sims_pend_list[[i]] <- tibble(
      sim_id  = seq_len(n_sim),
      group   = pendientes$group[i],
      equipo1 = t1, equipo2 = t2,
      g1 = g1, g2 = g2,
      pts1 = if_else(g1 > g2, 3L, if_else(g1 == g2, 1L, 0L)),
      pts2 = if_else(g2 > g1, 3L, if_else(g1 == g2, 1L, 0L))
    )
    setTxtProgressBar(pb, i)
  }
  close(pb)
  sims_pendientes <- bind_rows(sims_pend_list)
} else {
  sims_pendientes <- tibble()
}

sims <- bind_rows(sims_jugados, sims_pendientes)

# ── 4. Standings (mismo código que 04_simulation.R) ──────────────────────────

message("\nCalculando standings...")

standings_long <- bind_rows(
  sims |> transmute(sim_id, group, equipo = equipo1, pts = pts1,
                    gf = g1, gc = g2, gd = g1 - g2),
  sims |> transmute(sim_id, group, equipo = equipo2, pts = pts2,
                    gf = g2, gc = g1, gd = g2 - g1)
) |>
  group_by(sim_id, group, equipo) |>
  summarise(pts = sum(pts, na.rm = TRUE), gf = sum(gf, na.rm = TRUE),
            gc = sum(gc, na.rm = TRUE), gd = sum(gd, na.rm = TRUE),
            .groups = "drop") |>
  group_by(sim_id, group) |>
  arrange(desc(pts), desc(gd), desc(gf), .by_group = TRUE) |>
  mutate(posicion = row_number()) |>
  ungroup()

best_thirds <- standings_long |>
  filter(posicion == 3) |>
  group_by(sim_id) |>
  arrange(desc(pts), desc(gd), desc(gf), .by_group = TRUE) |>
  mutate(rank_3rd = row_number()) |>
  ungroup() |>
  mutate(clasifica_como_3ro = rank_3rd <= 8)

standings_long <- standings_long |>
  left_join(best_thirds |> select(sim_id, equipo, clasifica_como_3ro),
            by = c("sim_id", "equipo")) |>
  mutate(
    clasifica_como_3ro = coalesce(clasifica_como_3ro, FALSE),
    clasifica = posicion <= 2 | clasifica_como_3ro
  )

# ── 5. Probabilidades actualizadas ───────────────────────────────────────────

groups_raw <- resultados |>
  select(group, history_name = equipo1) |>
  bind_rows(resultados |> select(group, history_name = equipo2)) |>
  distinct()

probs <- standings_long |>
  group_by(equipo) |>
  summarise(
    prob_1ro  = mean(posicion == 1),
    prob_2do  = mean(posicion == 2),
    prob_3ro  = mean(posicion == 3),
    prob_4to  = mean(posicion == 4),
    prob_clas = mean(clasifica),
    pts_md    = median(pts),
    gd_md     = median(gd),
    .groups   = "drop"
  ) |>
  left_join(groups_raw, by = c("equipo" = "history_name")) |>
  arrange(group, desc(prob_clas))

write_csv(probs,          "output/tables/qualification_probs.csv")
write_csv(standings_long, "output/tables/standings_long.csv")
message("✓ output/tables/qualification_probs.csv  actualizado")
message("✓ output/tables/standings_long.csv       actualizado")

# ── 6. Resumen de partidos pendientes ─────────────────────────────────────────

if (nrow(pendientes) > 0) {
  top_scores <- function(g1, g2, n = 3) {
    sc  <- paste(g1, g2, sep = "-")
    tab <- sort(table(sc), decreasing = TRUE)
    pct <- round(as.numeric(tab) / length(sc) * 100)
    paste(sprintf("%s(%d%%)", names(tab)[seq_len(min(n, length(tab)))],
                  pct[seq_len(min(n, length(tab)))]), collapse = "  ")
  }

  match_summary <- sims_pendientes |>
    group_by(group, equipo1, equipo2) |>
    summarise(
      goles1_esp  = round(mean(g1), 2),
      goles2_esp  = round(mean(g2), 2),
      top3_scores = top_scores(g1, g2),
      prob_gana1  = round(mean(g1 > g2), 3),
      prob_empate = round(mean(g1 == g2), 3),
      prob_gana2  = round(mean(g1 < g2), 3),
      .groups     = "drop"
    ) |>
    arrange(group, equipo1)

  write_csv(match_summary, "output/tables/match_summary_pendientes.csv")

  message("\n── Partidos pendientes ──")
  for (grp in sort(unique(match_summary$group))) {
    df <- match_summary |> filter(group == grp)
    if (nrow(df) == 0) next
    cat(sprintf("\n─── Grupo %s ───\n", grp))
    for (i in seq_len(nrow(df))) {
      cat(sprintf("  %-22s vs %-22s  |  esp: %.2f–%.2f  |  %s  |  P(1/X/2): %.0f%%/%.0f%%/%.0f%%\n",
                  df$equipo1[i], df$equipo2[i],
                  df$goles1_esp[i], df$goles2_esp[i],
                  df$top3_scores[i],
                  df$prob_gana1[i] * 100, df$prob_empate[i] * 100, df$prob_gana2[i] * 100))
    }
  }
}

# ── 7. Resultados ya jugados ──────────────────────────────────────────────────

if (nrow(jugados) > 0) {
  message("\n── Resultados registrados ──")
  for (grp in sort(unique(jugados$group))) {
    df <- jugados |> filter(group == grp)
    cat(sprintf("\n─── Grupo %s ───\n", grp))
    for (i in seq_len(nrow(df))) {
      cat(sprintf("  %-22s %d – %d  %-22s  [FINAL]\n",
                  df$equipo1[i], df$g1[i], df$g2[i], df$equipo2[i]))
    }
  }
}

# ── 8. Top 15 clasificación actualizada ──────────────────────────────────────

message("\n── Top 15 por probabilidad de clasificación (actualizado) ──")
print(probs |>
        select(group, equipo, prob_clas, prob_1ro, prob_2do, pts_md) |>
        slice_max(prob_clas, n = 15))

# ── 9. Gráfico actualizado ────────────────────────────────────────────────────

plot_group_probs <- function(grupo, probs_df) {
  g <- probs_df |> filter(group == grupo) |> arrange(desc(prob_clas))
  equipos <- g$equipo
  mat <- rbind(g$prob_1ro,
               g$prob_2do,
               g$prob_3ro,
               g$prob_4to)
  barplot(mat,
          names.arg   = equipos,
          col         = c("#1a6fba","#5ba4e3","#f5a623","#e05c4b"),
          ylim        = c(0, 1),
          ylab        = "Probabilidad",
          main        = paste("Grupo", grupo, "— distribución de posiciones"),
          legend.text = c("1°","2°","3°","4°"),
          args.legend = list(x = "topright", bty = "n"),
          border      = NA, las = 2)
  abline(h = seq(0.2, 0.8, 0.2), col = "grey80", lty = 2)
}

pdf("output/figures/grupos_probabilidades.pdf", width = 10, height = 6)
for (g in sort(unique(probs$group))) plot_group_probs(g, probs)
dev.off()
message("✓ output/figures/grupos_probabilidades.pdf  actualizado")

jugados_pct <- round(nrow(jugados) / 72 * 100)
message(sprintf("\n── Avance del torneo: %d/72 partidos jugados (%d%%) ──", nrow(jugados), jugados_pct))

# ── 10. Mapa de calor de marcadores (distribución predictiva + resultado real) ─
# Para partidos jugados: re-simula con el modelo para obtener la distribución
# predictiva y luego marca el resultado real con un círculo dorado.
# Para partidos pendientes: solo muestra la distribución.

if (nrow(jugados) > 0) {
  message("\nGenerando distribuciones predictivas para heatmaps...")
  viz_list <- vector("list", nrow(jugados))
  for (i in seq_len(nrow(jugados))) {
    t1  <- jugados$equipo1[i]; t2 <- jugados$equipo2[i]
    elo1 <- if (!is.null(elo_lookup[[t1]])) elo_lookup[[t1]] else 1500
    elo2 <- if (!is.null(elo_lookup[[t2]])) elo_lookup[[t2]] else 1500
    ed   <- (elo1 - elo2) / 400
    lam1 <- exp(int_s + get_re(atk, t1, n_sim) - get_re(def, t2, n_sim) + b_elo_s *  ed)
    lam2 <- exp(int_s + get_re(atk, t2, n_sim) - get_re(def, t1, n_sim) + b_elo_s * -ed)
    viz_list[[i]] <- tibble(
      sim_id  = seq_len(n_sim),
      group   = jugados$group[i],
      equipo1 = t1, equipo2 = t2,
      g1      = rpois(n_sim, lam1),
      g2      = rpois(n_sim, lam2)
    )
  }
  sims_jugados_viz <- bind_rows(viz_list)
} else {
  sims_jugados_viz <- tibble()
}

sims_viz <- bind_rows(
  sims_jugados_viz,
  if (nrow(sims_pendientes) > 0)
    sims_pendientes |> select(sim_id, group, equipo1, equipo2, g1, g2)
  else
    tibble()
)

plot_score_heatmap_live <- function(t1, t2, grp, sub_sims,
                                    real_g1 = NA, real_g2 = NA, max_g = 7) {
  g1c   <- pmin(sub_sims$g1, max_g)
  g2c   <- pmin(sub_sims$g2, max_g)
  tab   <- table(factor(g1c, levels = 0:max_g), factor(g2c, levels = 0:max_g))
  probs <- tab / sum(tab)

  ax_labels <- c(as.character(0:(max_g - 1L)), paste0(max_g, "+"))
  pal        <- colorRampPalette(c("#f7fbff", "#1a6fba"))(100)

  jugado <- !is.na(real_g1) && !is.na(real_g2)
  titulo <- sprintf("Grupo %s  |  %s vs %s", grp, t1, t2)
  if (jugado) titulo <- paste0(titulo, sprintf("  [FINAL: %d–%d]", real_g1, real_g2))

  image(0:max_g, 0:max_g, probs,
        col  = pal, zlim = c(0, max(probs)),
        xlab = t1, ylab = t2,
        main = titulo, axes = FALSE, cex.main = 0.78)

  axis(1, at = 0:max_g, labels = ax_labels, cex.axis = 0.72)
  axis(2, at = 0:max_g, labels = ax_labels, cex.axis = 0.72, las = 2)

  for (i in 0:max_g) {
    for (j in 0:max_g) {
      pct <- round(probs[i + 1L, j + 1L] * 100)
      if (pct >= 1L) {
        col_txt <- if (probs[i + 1L, j + 1L] > max(probs) * 0.55) "white" else "grey20"
        text(i, j, paste0(pct, "%"), cex = 0.6, col = col_txt, font = 2)
      }
    }
  }

  abline(0, 1, col = "grey50", lty = 2, lwd = 0.9)

  if (jugado) {
    rg1c <- min(as.integer(real_g1), max_g)
    rg2c <- min(as.integer(real_g2), max_g)
    points(rg1c, rg2c, pch = 21, bg = "#FFD700", col = "black", cex = 3.0, lwd = 1.5)
    text(rg1c, rg2c,
         sprintf("%d–%d", as.integer(real_g1), as.integer(real_g2)),
         cex = 0.65, font = 2, col = "black")
  }

  box()
}

all_matches <- bind_rows(
  jugados   |> mutate(jugado = TRUE),
  pendientes |> mutate(jugado = FALSE)
) |> arrange(group, equipo1)

pdf("output/figures/heatmap_marcadores_live.pdf", width = 14, height = 10)

for (grp in sort(unique(all_matches$group))) {
  df_grp    <- all_matches |> filter(group == grp)
  n_sim_grp <- sims_viz |> filter(group == grp) |> pull(sim_id) |> n_distinct()

  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2.5, 0))

  for (i in seq_len(nrow(df_grp))) {
    t1       <- df_grp$equipo1[i]
    t2       <- df_grp$equipo2[i]
    sub_sims <- sims_viz |> filter(equipo1 == t1, equipo2 == t2)
    if (nrow(sub_sims) == 0) next

    real_g1 <- if (df_grp$jugado[i]) df_grp$g1[i] else NA
    real_g2 <- if (df_grp$jugado[i]) df_grp$g2[i] else NA

    plot_score_heatmap_live(t1, t2, grp, sub_sims,
                            real_g1 = real_g1, real_g2 = real_g2, max_g = 7)
  }

  n_jug <- sum(df_grp$jugado)
  mtext(sprintf("Grupo %s  —  %d/6 partidos jugados  (n = %s sims predictivas)",
                grp, n_jug, format(n_sim_grp, big.mark = ",")),
        outer = TRUE, cex = 1.1, font = 2)
}

dev.off()
message("✓ Guardado: output/figures/heatmap_marcadores_live.pdf")
