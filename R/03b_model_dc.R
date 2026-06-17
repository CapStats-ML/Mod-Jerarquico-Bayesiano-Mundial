library(rstan)
library(dplyr)
library(readr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ── 1. Cargar datos y reconstruir formato ancho ────────────────────────────────
# matches_model.csv está en formato largo (2 filas por partido).
# Para el modelo DC necesitamos (g1, g2) del mismo partido juntos.
# La primera fila de cada partido_id = perspectiva local/equipo1 (bind_rows order).

message("Cargando datos...")
long_df <- read_csv("data/processed/matches_model.csv", show_col_types = FALSE)

# Filtrar a WC vs WC (equivalente al Modelo B)
long_wc <- long_df |> filter(!is.na(equipo_id), !is.na(rival_id))

wide_df <- long_wc |>
  group_by(partido_id) |>
  mutate(pos = row_number()) |>
  ungroup()

team1_df <- wide_df |>
  filter(pos == 1) |>
  select(partido_id, fecha, peso, team1 = equipo, g1 = goles,
         elo_diff, es_local1 = es_local)

team2_df <- wide_df |>
  filter(pos == 2) |>
  select(partido_id, team2 = equipo, g2 = goles)

matches <- inner_join(team1_df, team2_df, by = "partido_id") |>
  arrange(fecha, partido_id)

message(sprintf("Partidos únicos WC vs WC: %d", nrow(matches)))
message(sprintf("Rango de goles — g1: [%d, %d]  g2: [%d, %d]",
                min(matches$g1), max(matches$g1),
                min(matches$g2), max(matches$g2)))

# ── 2. Índice de equipos ───────────────────────────────────────────────────────
all_teams <- sort(unique(c(matches$team1, matches$team2)))
T         <- length(all_teams)
team_idx  <- setNames(seq_along(all_teams), all_teams)

message(sprintf("Equipos únicos: %d", T))

# ── 3. Lista de datos para Stan ────────────────────────────────────────────────
stan_data <- list(
  N         = nrow(matches),
  T         = T,
  g1        = matches$g1,
  g2        = matches$g2,
  team1_idx = unname(team_idx[matches$team1]),
  team2_idx = unname(team_idx[matches$team2]),
  elo_diff  = matches$elo_diff,
  es_local1 = as.numeric(matches$es_local1),
  peso      = matches$peso
)

# ── 4. Compilar y ajustar ──────────────────────────────────────────────────────
message("\nCompilando modelo Stan (Dixon-Coles + Poisson jerárquico)...")
stan_model_dc <- stan_model("stan/model_dc.stan")

message("Ajustando modelo — 4 cadenas × 3000 iter (warmup 1000)...")
message("Tiempo estimado: 15–40 min según el equipo.\n")

fit_dc <- sampling(
  stan_model_dc,
  data    = stan_data,
  chains  = 4,
  iter    = 3000,
  warmup  = 1000,
  cores   = 4,
  seed    = 42,
  refresh = 200,
  control = list(adapt_delta = 0.99, max_treedepth = 12)
)

# ── 5. Diagnósticos ────────────────────────────────────────────────────────────
message("\n── Diagnósticos ──")
params_core <- c("alpha", "b_elo", "b_local", "rho", "sigma_atk", "sigma_def")
print(summary(fit_dc, pars = params_core)$summary[, c("mean","sd","2.5%","97.5%","Rhat","n_eff")])

rhat_max <- max(summary(fit_dc)$summary[, "Rhat"], na.rm = TRUE)
message(sprintf("\nR-hat máximo: %.4f  (< 1.01 ideal)", rhat_max))
check_hmc_diagnostics(fit_dc)

if (rhat_max > 1.02) warning("R-hat > 1.02 en algunos parámetros — considera más iteraciones.")

# ── 6. Guardar fit raw (para diagnósticos adicionales en cualquier momento) ────
dir.create("output/posteriors", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(fit = fit_dc, team_names = all_teams, n_matches = nrow(matches)),
  "output/posteriors/fit_DC_raw.rds"
)
message("✓ output/posteriors/fit_DC_raw.rds  (fit completo para diagnósticos)")

# ── 7. Extraer draws y renombrar al formato del pipeline ──────────────────────
# 04_simulation.R y 06_live_update.R esperan columnas con nombres brms:
#   b_Intercept, b_elo_diff, r_equipo[Equipo,Intercept], r_rival[Equipo,Intercept]
# Añadimos columna `rho` para la corrección DC en la simulación.

message("\nProcesando draws para el pipeline...")
draws_raw <- as.data.frame(fit_dc)

draws_pipe <- draws_raw |>
  rename(
    b_Intercept = alpha,
    b_elo_diff  = b_elo,
    b_es_local  = b_local
  )

# atk[i] / def[i] → r_equipo[Nombre,Intercept] / r_rival[Nombre,Intercept]
# rstan nombra transformed parameters como "atk[1]", "atk[2]", etc.
for (i in seq_along(all_teams)) {
  nm      <- all_teams[i]
  old_atk <- paste0("atk[", i, "]")
  new_atk <- paste0("r_equipo[", nm, ",Intercept]")
  old_def <- paste0("def[", i, "]")
  new_def <- paste0("r_rival[", nm, ",Intercept]")

  idx_atk <- match(old_atk, names(draws_pipe))
  idx_def <- match(old_def, names(draws_pipe))
  if (!is.na(idx_atk)) names(draws_pipe)[idx_atk] <- new_atk
  if (!is.na(idx_def)) names(draws_pipe)[idx_def] <- new_def
}

# Guardar draws procesados (cargados directamente por 04 y 06)
saveRDS(draws_pipe, "output/posteriors/draws_DC_wc_vs_wc.rds")

message("✓ output/posteriors/draws_DC_wc_vs_wc.rds")
message(sprintf("  %d draws, %d equipos con efectos estimados",
                nrow(draws_pipe), T))

rho_post <- draws_pipe$rho
message(sprintf("\n── Corrección Dixon-Coles estimada ──"))
message(sprintf("  ρ  media = %+.4f  sd = %.4f  95%% CI [%+.4f, %+.4f]",
                mean(rho_post), sd(rho_post),
                quantile(rho_post, 0.025), quantile(rho_post, 0.975)))
message("\n  ρ < 0 → aumenta prob. de empates (0-0 y 1-1) y reduce 1-0 / 0-1.")
message("  El pipeline (04 / 06) detectará este archivo automáticamente.")
