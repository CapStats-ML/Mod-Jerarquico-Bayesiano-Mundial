library(brms)
library(dplyr)
library(readr)

long_df <- read_csv("data/processed/matches_model.csv", show_col_types = FALSE)

# ── Priors ────────────────────────────────────────────────────────────────────
# Intercepto en log-escala: exp(0.3) ≈ 1.35 goles/partido, razonable
# sd de efectos aleatorios: 0.3 en log-escala → diferencias de ataque/defensa
#   modestas entre selecciones (rango típico ±0.6 log-goles ≈ ×1.8)
# Ventaja de local: prior centrado en 0.2 (≈ 22% más goles en casa)

priors <- c(
  prior(normal(0.3, 0.5), class = "Intercept"),
  prior(normal(0.2, 0.3), class = "b",  coef = "es_local"),
  # elo_diff = (elo_equipo - elo_rival) / 400; escala ~(-2, 2) en los datos
  # β > 0: equipos más fuertes (Elo) producen más goles → esperado ~0.3
  prior(normal(0,   0.3), class = "b",  coef = "elo_diff"),
  prior(normal(0,   0.3), class = "sd")
)

# ── Modelo A: todos los equipos (48 WC + 165 rivales históricos) ─────────────
# Más datos por equipo WC pero incluye rivales que no clasificaron.
# Estima parámetros de 213 equipos; solo usamos los 48 en simulación.

message("\n[A] Ajustando modelo con todos los equipos...")

fit_A <- brm(
  bf(goles | weights(peso) ~ 1 + es_local + elo_diff + (1 | equipo) + (1 | rival)),
  data    = long_df,
  family  = negbinomial(link = "log"),
  prior   = priors,

  chains  = 4,
  iter    = 5000,
  warmup  = 1000,
  cores   = 4,
  seed    = 42,
  refresh = 100,   # imprime progreso cada 100 iteraciones por chain
  silent  = 0,     # muestra output de Stan (cadenas, divergencias, etc.)
  file    = "output/posteriors/fit_A_todos"
)

message("\n[A] Diagnósticos:")
print(summary(fit_A)$fixed)
rhat_A <- max(rhat(fit_A), na.rm = TRUE)
message(sprintf("  R-hat máximo: %.4f  (< 1.01 es bueno)", rhat_A))

# ── Modelo B: solo partidos WC vs WC ─────────────────────────────────────────
# Filtra a partidos donde ambos equipos clasificaron al Mundial 2026.
# Menos datos pero la señal viene íntegramente de rivales del mismo nivel.

long_wc <- long_df |>
  filter(!is.na(equipo_id), !is.na(rival_id))

message(sprintf("\n[B] Dataset WC vs WC: %d filas (%d partidos únicos)",
                nrow(long_wc), n_distinct(long_wc$partido_id)))
message("[B] Ajustando modelo solo con partidos entre clasificados...")

fit_B <- brm(
  bf(goles | weights(peso) ~ 1 + es_local + elo_diff + (1 | equipo) + (1 | rival)),
  data    = long_wc,
  family  = negbinomial(link = "log"),
  prior   = priors,

  chains  = 4,
  iter    = 5000,
  warmup  = 1000,
  cores   = 4,
  seed    = 42,
  refresh = 100,
  silent  = 0,
  file    = "output/posteriors/fit_B_wc_vs_wc"
)

message("\n[B] Diagnósticos:")
print(summary(fit_B)$fixed)
rhat_B <- max(rhat(fit_B), na.rm = TRUE)
message(sprintf("  R-hat máximo: %.4f  (< 1.01 es bueno)", rhat_B))

# ── Comparación rápida de efectos aleatorios por equipo WC ───────────────────

compare_team_effects <- function(fit, label) {
  re <- ranef(fit)
  ataque  <- as.data.frame(re$equipo[, , "Intercept"]) |>
    tibble::rownames_to_column("equipo") |>
    rename(ataque_est = Estimate, ataque_sd = Est.Error)
  defensa <- as.data.frame(re$rival[, , "Intercept"]) |>
    tibble::rownames_to_column("equipo") |>
    rename(defensa_est = Estimate, defensa_sd = Est.Error)

  ataque |>
    inner_join(defensa, by = "equipo") |>
    mutate(
      fuerza_neta = ataque_est - defensa_est,
      modelo      = label
    ) |>
    arrange(desc(fuerza_neta))
}

teams_map <- read_csv("data/processed/teams_name_map.csv", show_col_types = FALSE)

ranking_A <- compare_team_effects(fit_A, "A_todos") |>
  semi_join(teams_map, by = c("equipo" = "history_name"))

ranking_B <- compare_team_effects(fit_B, "B_wc_vs_wc") |>
  semi_join(teams_map, by = c("equipo" = "history_name"))

message("\n── Top 10 por fuerza neta (ataque - defensa) ──")
message("\nModelo A (todos):")
print(head(ranking_A |> select(equipo, ataque_est, defensa_est, fuerza_neta), 10))
message("\nModelo B (WC vs WC):")
print(head(ranking_B |> select(equipo, ataque_est, defensa_est, fuerza_neta), 10))

# Guardar rankings para usarlos en simulación
write_csv(ranking_A, "output/tables/ranking_A.csv")
write_csv(ranking_B, "output/tables/ranking_B.csv")

message("\n✓ Modelos ajustados y guardados en output/posteriors/")
