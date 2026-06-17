library(brms)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

# ── 1. Cargar modelo y mapa de nombres ────────────────────────────────────────
# Si existe draws_DC_wc_vs_wc.rds (modelo Dixon-Coles) se usa automáticamente.
# Si no, cae al Modelo B (NegBinomial brms). El flag use_dc controla la
# corrección Dixon-Coles en la simulación.

teams_map   <- read_csv("data/processed/teams_name_map.csv",  show_col_types = FALSE)
elo_ratings <- read_csv("data/processed/elo_ratings.csv",     show_col_types = FALSE)

dc_path <- "output/posteriors/draws_DC_wc_vs_wc.rds"
use_dc  <- file.exists(dc_path)

if (use_dc) {
  message("Modelo: Dixon-Coles (Poisson jerárquico con corrección ρ)")
  draws_df <- readRDS(dc_path)
} else {
  message("Modelo: NegBinomial brms (Modelo B — ejecuta 03b_model_dc.R para activar DC)")
  fit_B    <- readRDS("output/posteriors/fit_B_wc_vs_wc.rds")
  draws_df <- as_draws_df(fit_B)
}

# Lista nombrada: team → elo (lookup con [[]] que devuelve NULL si falta la clave)
elo_lookup  <- as.list(setNames(elo_ratings$elo, elo_ratings$team))

# ── 2. Grupos del Mundial 2026 ────────────────────────────────────────────────
# Fuente confirmada: ESPN standings https://www.espn.com/soccer/standings/_/league/FIFA.WORLD

groups_raw <- tribble(
  ~group, ~history_name,
  "A", "Mexico",
  "A", "South Korea",
  "A", "Czech Republic",
  "A", "South Africa",
  "B", "Canada",
  "B", "Bosnia and Herzegovina",
  "B", "Qatar",
  "B", "Switzerland",
  "C", "Brazil",
  "C", "Morocco",
  "C", "Haiti",
  "C", "Scotland",
  "D", "United States",
  "D", "Paraguay",
  "D", "Australia",
  "D", "Turkey",
  "E", "Germany",
  "E", "Curaçao",
  "E", "Ivory Coast",
  "E", "Ecuador",
  "F", "Netherlands",
  "F", "Japan",
  "F", "Sweden",
  "F", "Tunisia",
  "G", "Belgium",
  "G", "Egypt",
  "G", "Iran",
  "G", "New Zealand",
  "H", "Spain",
  "H", "Cape Verde",
  "H", "Saudi Arabia",
  "H", "Uruguay",
  "I", "France",
  "I", "Senegal",
  "I", "Iraq",
  "I", "Norway",
  "J", "Argentina",
  "J", "Algeria",
  "J", "Austria",
  "J", "Jordan",
  "K", "Portugal",
  "K", "DR Congo",
  "K", "Uzbekistan",
  "K", "Colombia",
  "L", "England",
  "L", "Croatia",
  "L", "Ghana",
  "L", "Panama"
)

# ── 3. Generar los 72 partidos de fase de grupos ──────────────────────────────
# C(4,2) = 6 partidos por grupo × 12 grupos = 72 partidos totales

group_fixtures <- groups_raw |>
  group_by(group) |>
  group_modify(function(g, ...) {
    equipos <- g$history_name
    combn(equipos, 2, simplify = FALSE) |>
      map_dfr(~ tibble(equipo1 = .x[1], equipo2 = .x[2]))
  }) |>
  ungroup()

message(sprintf("Partidos generados: %d (esperado: 72)", nrow(group_fixtures)))

# Inicializar CSV de seguimiento en vivo (solo la primera vez; no sobreescribir)
live_path <- "data/live/resultados_reales.csv"
if (!file.exists(live_path)) {
  dir.create("data/live", showWarnings = FALSE, recursive = TRUE)
  group_fixtures |>
    mutate(g1 = NA_integer_, g2 = NA_integer_) |>
    write_csv(live_path)
  message(sprintf("✓ Creado: %s  — rellena g1/g2 conforme se jueguen los partidos", live_path))
}

# ── 4. Extraer draws del posterior ────────────────────────────────────────────

message(sprintf("Extrayendo draws del posterior (%s)...",
                if (use_dc) "Dixon-Coles DC" else "Modelo B NegBin"))

wc_teams <- teams_map$history_name

# Renombrar columnas para acceso limpio: "r_equipo[England,Intercept]" → "England"
extract_re <- function(draws, prefix) {
  pattern <- paste0(prefix, "[", sep = "")
  cols    <- names(draws)[grepl(pattern, names(draws), fixed = TRUE)]
  if (length(cols) == 0) return(data.frame())
  result       <- as.data.frame(draws[, cols, drop = FALSE])
  # sub() más robusto que lookbehind para nombres con espacios
  # brms reemplaza espacios con puntos en nombres de niveles: revertir
  raw_names     <- sub(paste0(".*", prefix, "\\[([^,]+),.*"), "\\1", cols)
  names(result) <- gsub("\\.", " ", raw_names)
  result
}

ataque     <- extract_re(draws_df, "r_equipo")
defensa    <- extract_re(draws_df, "r_rival")
intercepto <- draws_df$b_Intercept
b_elo      <- draws_df$b_elo_diff
rho_s      <- if (use_dc && "rho" %in% names(draws_df)) as.numeric(draws_df$rho) else NULL

n_draws <- length(intercepto)
message(sprintf("Draws disponibles: %d", n_draws))
message(sprintf("Equipos con parámetro de ataque:  %d", ncol(ataque)))
message(sprintf("Equipos con parámetro de defensa: %d", ncol(defensa)))

# Equipos sin parámetro estimado (no aparecieron suficiente en training data)
missing_teams <- setdiff(wc_teams, names(ataque))
if (length(missing_teams) > 0) {
  message(sprintf("\n⚠  %d equipos sin parámetro propio → ataque/defensa = prior mean (0):", length(missing_teams)))
  message(paste0("   ", missing_teams, collapse = "\n"))
  message("   Su fortaleza relativa se captura vía elo_diff.\n")
}

# Verificar que todos los equipos WC tienen Elo
missing_elo <- setdiff(wc_teams, names(elo_lookup))
if (length(missing_elo) > 0) {
  message(sprintf("\n⚠  %d equipos sin Elo → usando 1500 (media global):", length(missing_elo)))
  message(paste0("   ", missing_elo, collapse = "\n"))
} else {
  elos_wc <- unlist(elo_lookup[wc_teams])
  message(sprintf("✓ Elo disponible para los %d equipos WC (rango: %.0f – %.0f)",
                  length(wc_teams), min(elos_wc), max(elos_wc)))
}

# Función auxiliar: devuelve el vector de draws para un equipo,
# o ceros (prior mean) si el equipo no tiene parámetro estimado.
get_re <- function(re_df, team, n) {
  col <- re_df[[team]]
  if (is.null(col)) rep(0.0, n) else as.numeric(col)
}

# ── 5. Simulación vectorizada ─────────────────────────────────────────────────
# Para cada partido, samplea n_sim draws del posterior y simula goles

simulate_group_stage <- function(fixtures, n_sim = 1000, seed = 2026) {
  set.seed(seed)
  idx <- sample(seq_len(n_draws), n_sim, replace = n_sim > n_draws)

  int_s   <- intercepto[idx]
  b_elo_s <- b_elo[idx]
  atk     <- as.data.frame(ataque[idx, , drop = FALSE])
  def     <- as.data.frame(defensa[idx, , drop = FALSE])
  rho_sim <- if (!is.null(rho_s)) rho_s[idx] else NULL

  message(sprintf("\nSimulando %d escenarios para %d partidos%s...\n",
                  n_sim, nrow(fixtures),
                  if (!is.null(rho_sim)) "  [Dixon-Coles activo]" else ""))
  pb <- txtProgressBar(min = 0, max = nrow(fixtures), style = 3, width = 50)

  match_results <- vector("list", nrow(fixtures))

  for (i in seq_len(nrow(fixtures))) {
    t1 <- fixtures$equipo1[i]
    t2 <- fixtures$equipo2[i]

    elo1 <- if (!is.null(elo_lookup[[t1]])) elo_lookup[[t1]] else 1500
    elo2 <- if (!is.null(elo_lookup[[t2]])) elo_lookup[[t2]] else 1500
    elo_diff_t1 <- (elo1 - elo2) / 400

    lambda1 <- exp(int_s + get_re(atk, t1, n_sim) - get_re(def, t2, n_sim) + b_elo_s *  elo_diff_t1)
    lambda2 <- exp(int_s + get_re(atk, t2, n_sim) - get_re(def, t1, n_sim) + b_elo_s * -elo_diff_t1)

    g1 <- rpois(n_sim, lambda1)
    g2 <- rpois(n_sim, lambda2)

    # Corrección Dixon-Coles: remuestrea (g1, g2) con pesos τ
    # τ ≠ 1 solo para los 4 marcadores de baja puntuación; ajusta la correlación
    # entre goles sin cambiar las λ individuales.
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

    match_results[[i]] <- tibble(
      sim_id  = seq_len(n_sim),
      group   = fixtures$group[i],
      equipo1 = t1, equipo2 = t2,
      g1 = g1, g2 = g2,
      pts1 = if_else(g1 > g2, 3L, if_else(g1 == g2, 1L, 0L)),
      pts2 = if_else(g2 > g1, 3L, if_else(g1 == g2, 1L, 0L))
    )
    setTxtProgressBar(pb, i)
  }
  close(pb)
  bind_rows(match_results)
}

sims_full <- simulate_group_stage(group_fixtures, n_sim = 100000)

# Thinning: conservar 1 de cada 10 → 10k escenarios para cálculos downstream
sims <- sims_full |> filter(sim_id %% 10 == 0)
rm(sims_full)   # liberar memoria
message(sprintf("Escenarios tras thinning (1 de cada 10): %d", n_distinct(sims$sim_id)))

# ── 6. Standings por simulación ───────────────────────────────────────────────

message("\nCalculando standings por simulación...")

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

# ── 7. Mejores terceros (clasifican al Round of 32) ───────────────────────────
# En el Mundial 2026, los 8 mejores terceros clasifican

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

# ── 8. Probabilidades de clasificación ───────────────────────────────────────

probs <- standings_long |>
  group_by(equipo) |>
  summarise(
    prob_1ro  = mean(posicion == 1),
    prob_2do  = mean(posicion == 2),
    prob_3ro  = mean(posicion == 3),
    prob_4to  = mean(posicion == 4),
    prob_top2 = mean(posicion <= 2),
    prob_clas = mean(clasifica),
    pts_md    = median(pts),
    gd_md     = median(gd),
    .groups   = "drop"
  ) |>
  left_join(groups_raw, by = c("equipo" = "history_name")) |>
  arrange(group, desc(prob_clas))

write_csv(probs, "output/tables/qualification_probs.csv")
message("\n✓ Guardado: output/tables/qualification_probs.csv")

write_csv(standings_long, "output/tables/standings_long.csv")
message("✓ Guardado: output/tables/standings_long.csv")

# ── 9. Visualización (R base) ─────────────────────────────────────────────────

plot_group_probs <- function(grupo, probs_df) {
  g <- probs_df |>
    filter(group == grupo) |>
    arrange(desc(prob_clas))

  equipos <- g$equipo
  mat <- rbind(g$prob_1ro, g$prob_2do, g$prob_3ro, g$prob_4to)

  barplot(
    mat,
    names.arg  = equipos,
    col        = c("#1a6fba", "#5ba4e3", "#f5a623", "#e05c4b"),
    ylim       = c(0, 1),
    ylab       = "Probabilidad",
    main       = paste("Grupo", grupo, "— distribución de posiciones"),
    legend.text = c("1°", "2°", "3°", "4°"),
    args.legend = list(x = "topright", bty = "n"),
    border      = NA,
    las         = 2
  )
  abline(h = seq(0.2, 0.8, 0.2), col = "grey80", lty = 2)
}

# Guardar un plot por grupo
pdf("output/figures/grupos_probabilidades.pdf", width = 10, height = 6)
for (g in LETTERS[1:12]) {
  plot_group_probs(g, probs)
}
dev.off()

message("✓ Guardado: output/figures/grupos_probabilidades.pdf")
message("\n── Top 15 equipos por probabilidad de clasificación ──")
print(probs |> select(group, equipo, prob_clas, prob_1ro, prob_2do, pts_md) |>
        slice_max(prob_clas, n = 15))

# ── 10. Resumen de goles y resultados por partido ────────────────────────────
# Para cada uno de los 72 partidos calcula:
#   - Goles esperados (media posterior predictiva)
#   - Marcador más probable (moda de las 1000 simulaciones)
#   - Probabilidad de victoria equipo1, empate, victoria equipo2

message("\n── Goles esperados y probabilidades de resultado por partido ──")

# Top N marcadores con su probabilidad empírica
top_scores <- function(g1, g2, n = 3) {
  sc  <- paste(g1, g2, sep = "-")
  tab <- sort(table(sc), decreasing = TRUE)
  pct <- round(as.numeric(tab) / length(sc) * 100)
  paste(sprintf("%s(%d%%)", names(tab)[seq_len(min(n, length(tab)))],
                pct[seq_len(min(n, length(tab)))]),
        collapse = "  ")
}

match_summary <- sims |>
  group_by(group, equipo1, equipo2) |>
  summarise(
    goles1_esp  = round(mean(g1), 2),
    goles2_esp  = round(mean(g2), 2),
    top3_scores = top_scores(g1, g2, 3),
    prob_gana1  = round(mean(g1 > g2), 3),
    prob_empate = round(mean(g1 == g2), 3),
    prob_gana2  = round(mean(g1 < g2), 3),
    .groups     = "drop"
  ) |>
  arrange(group, equipo1)

write_csv(match_summary, "output/tables/match_summary.csv")
message("✓ Guardado: output/tables/match_summary.csv")

# Imprimir partido a partido por grupo
for (grp in sort(unique(match_summary$group))) {
  cat(sprintf("\n─── Grupo %s ───\n", grp))
  df <- match_summary |> filter(group == grp)
  for (i in seq_len(nrow(df))) {
    cat(sprintf("  %-22s vs %-22s  |  esp: %.2f–%.2f  |  %s  |  P(1/X/2): %.0f%%/%.0f%%/%.0f%%\n",
                df$equipo1[i], df$equipo2[i],
                df$goles1_esp[i], df$goles2_esp[i],
                df$top3_scores[i],
                df$prob_gana1[i]  * 100,
                df$prob_empate[i] * 100,
                df$prob_gana2[i]  * 100))
  }
}

# ── 11. Gráficos de distribución de goles por partido (R base) ───────────────
# Un PDF con histogramas de g1 y g2 por partido (72 páginas, 1 por partido)

pdf("output/figures/goles_por_partido.pdf", width = 9, height = 5)

for (i in seq_len(nrow(match_summary))) {
  grp <- match_summary$group[i]
  t1  <- match_summary$equipo1[i]
  t2  <- match_summary$equipo2[i]

  sub_sims <- sims |> filter(equipo1 == t1, equipo2 == t2)

  max_g <- max(sub_sims$g1, sub_sims$g2, 5)
  breaks <- seq(-0.5, max_g + 0.5, 1)

  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

  hist(sub_sims$g1,
       breaks  = breaks,
       col     = "#1a6fba",
       border  = "white",
       main    = sprintf("Grupo %s: %s", grp, t1),
       xlab    = "Goles",
       ylab    = "Simulaciones",
       xlim    = c(-0.5, max_g + 0.5))
  abline(v = mean(sub_sims$g1), col = "red", lwd = 2, lty = 2)
  legend("topright", legend = sprintf("media = %.2f", mean(sub_sims$g1)),
         col = "red", lty = 2, lwd = 2, bty = "n", cex = 0.85)

  hist(sub_sims$g2,
       breaks  = breaks,
       col     = "#e05c4b",
       border  = "white",
       main    = sprintf("Grupo %s: %s", grp, t2),
       xlab    = "Goles",
       ylab    = "Simulaciones",
       xlim    = c(-0.5, max_g + 0.5))
  abline(v = mean(sub_sims$g2), col = "darkred", lwd = 2, lty = 2)
  legend("topright", legend = sprintf("media = %.2f", mean(sub_sims$g2)),
         col = "darkred", lty = 2, lwd = 2, bty = "n", cex = 0.85)
}

dev.off()
message("✓ Guardado: output/figures/goles_por_partido.pdf  (72 partidos)")

# ── 12. Mapa de calor de marcadores por partido ──────────────────────────────
# Para cada grupo: 6 heatmaps (layout 2×3). Cada celda (i,j) muestra
# P(equipo1 anota i goles, equipo2 anota j goles) a partir de las simulaciones.
# Goles se capan en max_g+ para concentrar la masa en la región visible.

plot_score_heatmap <- function(t1, t2, grp, sub_sims, max_g = 10) {
  g1c <- pmin(sub_sims$g1, max_g)
  g2c <- pmin(sub_sims$g2, max_g)

  tab   <- table(factor(g1c, levels = 0:max_g),
                 factor(g2c, levels = 0:max_g))
  probs <- tab / sum(tab)  # matriz (max_g+1) × (max_g+1)

  ax_labels <- c(as.character(0:(max_g - 1)), paste0(max_g, "+"))
  pal        <- colorRampPalette(c("#f7fbff", "#1a6fba"))(100)

  # image(): probs[i,j] se dibuja centrado en (i-1, j-1)
  # eje x = goles equipo1, eje y = goles equipo2
  image(0:max_g, 0:max_g, probs,
        col  = pal,
        zlim = c(0, max(probs)),
        xlab = t1,
        ylab = t2,
        main = sprintf("Grupo %s  |  %s vs %s", grp, t1, t2),
        axes = FALSE,
        cex.main = 0.85)

  axis(1, at = 0:max_g, labels = ax_labels, cex.axis = 0.8)
  axis(2, at = 0:max_g, labels = ax_labels, cex.axis = 0.8, las = 2)

  # Porcentaje en cada celda (solo si ≥ 1%)
  for (i in 0:max_g) {
    for (j in 0:max_g) {
      pct <- round(probs[i + 1L, j + 1L] * 100)
      if (pct >= 1L) {
        col_txt <- if (probs[i + 1L, j + 1L] > max(probs) * 0.55) "white" else "grey20"
        text(i, j, paste0(pct, "%"), cex = 0.72, col = col_txt, font = 2)
      }
    }
  }

  # Diagonal punteada = empate (g1 == g2)
  abline(0, 1, col = "grey50", lty = 2, lwd = 0.9)
  box()
}

pdf("output/figures/heatmap_marcadores.pdf", width = 14, height = 10)

for (grp in sort(unique(match_summary$group))) {
  df_grp <- match_summary |> filter(group == grp)
  n_sim_grp <- sims |> filter(group == grp) |> pull(sim_id) |> n_distinct()

  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2.5, 0))

  for (i in seq_len(nrow(df_grp))) {
    t1       <- df_grp$equipo1[i]
    t2       <- df_grp$equipo2[i]
    sub_sims <- sims |> filter(equipo1 == t1, equipo2 == t2)
    plot_score_heatmap(t1, t2, grp, sub_sims, max_g = 10)
  }

  mtext(sprintf("Grupo %s  —  Distribución de marcadores  (n = %s simulaciones)",
                grp, format(n_sim_grp, big.mark = ",")),
        outer = TRUE, cex = 1.1, font = 2)
}

dev.off()
message("✓ Guardado: output/figures/heatmap_marcadores.pdf  (12 grupos, 6 partidos c/u)")
