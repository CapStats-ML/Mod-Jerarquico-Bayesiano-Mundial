library(brms)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

# ── Flujo de uso ───────────────────────────────────────────────────────────────
#
# 1. Después de cada partido, abre data/live/resultados_reales.csv y escribe
#    los goles reales en las columnas g1 y g2 de esa fila.
#    Deja g1/g2 como NA en los partidos aún no jugados.
#
# 2. Corre: source("R/06_live_update.R")
#
# 3. Se regeneran output/tables/ y output/figures/ con probabilidades actualizadas.
#    Los partidos jugados tienen resultado fijo en todos los escenarios;
#    solo los pendientes se simulan → la incertidumbre colapsa a medida que
#    avanza el torneo.
# ──────────────────────────────────────────────────────────────────────────────

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
fit_B       <- readRDS("output/posteriors/fit_B_wc_vs_wc.rds")
elo_ratings <- read_csv("data/processed/elo_ratings.csv", show_col_types = FALSE)
elo_lookup  <- as.list(setNames(elo_ratings$elo, elo_ratings$team))

draws_df <- as_draws_df(fit_B)

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
