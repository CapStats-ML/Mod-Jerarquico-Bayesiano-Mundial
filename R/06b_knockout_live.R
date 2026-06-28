# ── 06b_knockout_live.R ────────────────────────────────────────────────────────
# Live update para la fase eliminatoria del Mundial 2026.
# Corre después de que la fase de grupos esté completa (los 72 partidos).
#
# Flujo:
#   1. Descarga todos los resultados del Mundial 2026 desde martj42
#   2. Calcula standings reales de fase de grupos
#   3. Construye el bracket real de R32 (equipo 1ro, 2do, mejores 3ros)
#   4. Carga el modelo Dixon-Coles (DC) activo
#   5. Para cada partido: usa resultado real si ya se jugó, modelo si no
#   6. Guarda bracket_ml.rds  (→ 07_bracket_viz.R)
#         y knockout_probs.csv (probabilidades Monte Carlo con 10k sims)
#
# Uso: source("R/06b_knockout_live.R") después de cada jornada eliminatoria.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)

# ── 0. Descargar resultados del Mundial 2026 ──────────────────────────────────

message("── 0. Descargando resultados del Mundial 2026 desde martj42...")
url <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"

all_wc <- tryCatch(
  read_csv(url, show_col_types = FALSE, progress = FALSE) |>
    filter(
      str_detect(tournament, regex("FIFA World Cup", ignore_case = TRUE)),
      !str_detect(tournament, regex("qualif|eliminat", ignore_case = TRUE)),
      year(date) == 2026,
      !is.na(home_score), !is.na(away_score)
    ),
  error = function(e) {
    message("  Sin conexion — operando sin datos de martj42")
    tibble(date = as.Date(character()), home_team = character(), away_team = character(),
           home_score = integer(), away_score = integer())
  }
)
message(sprintf("  %d partidos del Mundial 2026 en martj42", nrow(all_wc)))

# Separar fase de grupos (ya en resultados_reales.csv) de eliminatorias
gs <- read_csv("data/live/resultados_reales.csv", show_col_types = FALSE) |>
  filter(!is.na(g1), !is.na(g2))

if (nrow(gs) < 72)
  stop(sprintf("Solo hay %d/72 partidos de grupos completos. Corre 06_live_update.R primero.", nrow(gs)))

gs_pairs <- gs |>
  mutate(pair = paste(pmin(equipo1, equipo2), pmax(equipo1, equipo2), sep = "_")) |>
  pull(pair)

ko_data <- all_wc |>
  mutate(pair = paste(pmin(home_team, away_team), pmax(home_team, away_team), sep = "_")) |>
  filter(!pair %in% gs_pairs) |>
  select(-pair)

message(sprintf("  %d partidos de eliminatorias con resultado registrado", nrow(ko_data)))

# ── 1. Standings reales de fase de grupos ─────────────────────────────────────

message("\n── 1. Calculando standings reales...")

standings_real <- bind_rows(
  gs |> transmute(group, equipo = equipo1,
                  pts = if_else(g1 > g2, 3L, if_else(g1 == g2, 1L, 0L)),
                  gf = as.integer(g1), gc = as.integer(g2), gd = as.integer(g1 - g2)),
  gs |> transmute(group, equipo = equipo2,
                  pts = if_else(g2 > g1, 3L, if_else(g1 == g2, 1L, 0L)),
                  gf = as.integer(g2), gc = as.integer(g1), gd = as.integer(g2 - g1))
) |>
  group_by(group, equipo) |>
  summarise(pts = sum(pts), gf = sum(gf), gc = sum(gc), gd = sum(gd), .groups = "drop") |>
  group_by(group) |>
  arrange(desc(pts), desc(gd), desc(gf), equipo, .by_group = TRUE) |>
  mutate(posicion = row_number()) |>
  ungroup()

# Los 8 mejores terceros (por pts, luego GD, luego GF)
thirds_ranked <- standings_real |>
  filter(posicion == 3) |>
  arrange(desc(pts), desc(gd), desc(gf))

standings_real <- standings_real |>
  mutate(
    clasifica_como_3ro = posicion == 3 & equipo %in% thirds_ranked$equipo[1:8],
    clasifica          = posicion <= 2 | clasifica_como_3ro
  )

# Imprimir clasificados
message("  Grupos:")
for (g in sort(unique(standings_real$group))) {
  cl  <- standings_real |> filter(group == g) |> arrange(posicion)
  t3  <- cl |> filter(clasifica_como_3ro)
  msg <- sprintf("  %s: 1° %-22s 2° %-22s", g, cl$equipo[1], cl$equipo[2])
  if (nrow(t3) > 0) msg <- paste0(msg, sprintf(" 3°* %s", t3$equipo))
  cat(msg, "\n")
}

# ── 2. Cargar modelo Dixon-Coles ──────────────────────────────────────────────

message("\n── 2. Cargando modelo Dixon-Coles...")
elo_ratings <- read_csv("data/processed/elo_ratings.csv", show_col_types = FALSE)
elo_lookup  <- as.list(setNames(elo_ratings$elo, elo_ratings$team))
get_elo     <- function(t) { v <- elo_lookup[[t]]; if (is.null(v)) 1500.0 else as.numeric(v) }

draws_df   <- readRDS("output/posteriors/draws_DC_wc_vs_wc.rds")
intercepto <- as.numeric(draws_df$b_Intercept)
b_elo_v    <- as.numeric(draws_df$b_elo_diff)
rho_v      <- if ("rho" %in% names(draws_df)) as.numeric(draws_df$rho) else NULL
n_draws    <- length(intercepto)
message(sprintf("  %d draws disponibles del posterior DC", n_draws))

extract_re <- function(draws, prefix) {
  pattern <- paste0(prefix, "[")
  cols    <- names(draws)[grepl(pattern, names(draws), fixed = TRUE)]
  if (length(cols) == 0) return(data.frame())
  res       <- as.data.frame(draws[, cols, drop = FALSE])
  raw_names <- sub(paste0(".*", prefix, "\\[([^,]+),.*"), "\\1", cols)
  names(res) <- gsub("\\.", " ", raw_names)
  res
}

atq_df <- extract_re(draws_df, "r_equipo")
def_df <- extract_re(draws_df, "r_rival")

get_atq <- function(t, idx) { col <- atq_df[[t]]; if (is.null(col)) rep(0.0, length(idx)) else as.numeric(col)[idx] }
get_def <- function(t, idx) { col <- def_df[[t]]; if (is.null(col)) rep(0.0, length(idx)) else as.numeric(col)[idx] }

# ── 3. Vectores para simulación (N_SIM draws) ─────────────────────────────────

N_SIM <- 10000L
set.seed(2026)
sim_idx   <- sample(seq_len(n_draws), N_SIM, replace = TRUE)
int_sim   <- intercepto[sim_idx]
belo_sim  <- b_elo_v[sim_idx]
atq_sim   <- as.data.frame(atq_df[sim_idx, , drop = FALSE])
def_sim   <- as.data.frame(def_df[sim_idx, , drop = FALSE])
rho_sim   <- if (!is.null(rho_v)) rho_v[sim_idx] else NULL

get_atq_s <- function(t) { v <- atq_sim[[t]]; if (is.null(v)) rep(0.0, N_SIM) else as.numeric(v) }
get_def_s <- function(t) { v <- def_sim[[t]]; if (is.null(v)) rep(0.0, N_SIM) else as.numeric(v) }

# Simulación vectorizada: devuelve estadísticas del modelo + ganador ML
sim_ko_match <- function(t1, t2, real_g1 = NA_real_, real_g2 = NA_real_) {
  elo_d <- (get_elo(t1) - get_elo(t2)) / 400
  l1 <- exp(int_sim + get_atq_s(t1) - get_def_s(t2) + belo_sim *  elo_d)
  l2 <- exp(int_sim + get_atq_s(t2) - get_def_s(t1) + belo_sim * -elo_d)

  g1 <- rpois(N_SIM, l1)
  g2 <- rpois(N_SIM, l2)

  if (!is.null(rho_sim)) {
    tau <- rep(1.0, N_SIM)
    m00 <- g1 == 0L & g2 == 0L; if (any(m00)) tau[m00] <- pmax(1 - l1[m00]*l2[m00]*rho_sim[m00], 1e-10)
    m10 <- g1 == 1L & g2 == 0L; if (any(m10)) tau[m10] <- pmax(1 + l2[m10]*rho_sim[m10], 1e-10)
    m01 <- g1 == 0L & g2 == 1L; if (any(m01)) tau[m01] <- pmax(1 + l1[m01]*rho_sim[m01], 1e-10)
    m11 <- g1 == 1L & g2 == 1L; if (any(m11)) tau[m11] <- pmax(1 - rho_sim[m11], 1e-10)
    dc_i <- sample.int(N_SIM, N_SIM, replace = TRUE, prob = tau / sum(tau))
    g1 <- g1[dc_i]; g2 <- g2[dc_i]
  }

  p1    <- mean(g1 > g2)
  px    <- mean(g1 == g2)
  p2    <- mean(g1 < g2)
  padv1 <- p1 + px * 0.5   # incluye 50-50 en penales

  played <- !is.na(real_g1) && !is.na(real_g2)

  if (played) {
    if (real_g1 > real_g2) {
      winner <- t1
    } else if (real_g1 < real_g2) {
      winner <- t2
    } else {
      # Empate en 90' → ET/penales. Tomamos equipo con mayor prob del modelo.
      winner <- if (padv1 >= 0.5) t1 else t2
      message(sprintf("    ! %s vs %s: empate %d-%d en 90' — ganador real por penales no disponible en datos, usando modelo", t1, t2, as.integer(real_g1), as.integer(real_g2)))
    }
    # Para el bracket viz: mostrar goles reales
    show_g1 <- as.numeric(real_g1)
    show_g2 <- as.numeric(real_g2)
  } else {
    winner  <- if (padv1 >= 0.5) t1 else t2
    show_g1 <- round(mean(l1), 2)
    show_g2 <- round(mean(l2), 2)
  }

  top3_sc <- function(n = 3) {
    sc  <- paste(g1, g2, sep = "-")
    tab <- sort(table(sc), decreasing = TRUE)
    pct <- round(as.numeric(tab) / N_SIM * 100)
    paste(sprintf("%s(%d%%)", names(tab)[seq_len(min(n, length(tab)))],
                  pct[seq_len(min(n, length(tab)))]), collapse = "  ")
  }

  list(
    t1       = t1,
    t2       = t2,
    esp_g1   = show_g1,
    esp_g2   = show_g2,
    top3     = top3_sc(),
    p_gana1  = round(p1,    3),
    p_empate = round(px,    3),
    p_gana2  = round(p2,    3),
    p_adv_t1 = round(padv1, 3),
    winner   = winner,
    played   = played
  )
}

# Helper: busca resultado real en ko_data
get_real <- function(t1, t2) {
  if (nrow(ko_data) == 0) return(list(g1 = NA_real_, g2 = NA_real_))
  row <- ko_data |>
    filter((home_team == t1 & away_team == t2) |
           (home_team == t2 & away_team == t1)) |>
    arrange(desc(date)) |>
    slice(1)
  if (nrow(row) == 0) return(list(g1 = NA_real_, g2 = NA_real_))
  if (row$home_team[1] == t1)
    list(g1 = as.numeric(row$home_score[1]), g2 = as.numeric(row$away_score[1]))
  else
    list(g1 = as.numeric(row$away_score[1]), g2 = as.numeric(row$home_score[1]))
}

# Simula y reporta un round completo
run_round <- function(pairs, label, match_ids) {
  message(sprintf("\n── %s ──", label))
  lapply(seq_along(pairs), function(i) {
    p    <- pairs[[i]]
    real <- get_real(p[1], p[2])
    res  <- sim_ko_match(p[1], p[2], real_g1 = real$g1, real_g2 = real$g2)
    status <- if (res$played)
      sprintf("[FINAL: %.0f-%.0f]", res$esp_g1, res$esp_g2)
    else
      sprintf("[pendiente — P(avanza): %.0f%%]", res$p_adv_t1 * 100)
    cat(sprintf("  [%s] %-22s vs %-22s  %s\n",
                match_ids[i], p[1], p[2], status))
    res
  })
}

# ── 4. Construir bracket real R32 ─────────────────────────────────────────────

message("\n── 4. Construyendo bracket de R32 desde standings reales...")

third_slots <- list(
  M74 = c("A","B","C","D","F"),
  M77 = c("C","D","F","G","H"),
  M79 = c("C","E","F","H","I"),
  M80 = c("E","H","I","J","K"),
  M81 = c("B","E","F","I","J"),
  M82 = c("A","E","H","I","J"),
  M85 = c("E","F","G","I","J"),
  M87 = c("D","E","I","J","L")
)

assign_thirds <- function(thirds_df) {
  slot_names    <- names(third_slots)
  elig_per_team <- lapply(thirds_df$group, function(g)
    slot_names[vapply(third_slots, function(gs) g %in% gs, logical(1))])
  ord      <- order(lengths(elig_per_team))
  assigned <- setNames(rep(NA_character_, length(slot_names)), slot_names)
  bt <- function(k, asgn) {
    if (k > length(ord)) return(asgn)
    idx   <- ord[k]; team <- thirds_df$equipo[idx]
    avail <- elig_per_team[[idx]][is.na(asgn[elig_per_team[[idx]]])]
    for (slot in avail) {
      asgn[slot] <- team
      result     <- bt(k + 1, asgn)
      if (!is.null(result)) return(result)
      asgn[slot] <- NA_character_
    }
    NULL
  }
  result <- bt(1, assigned)
  if (is.null(result)) {
    warning("assign_thirds: sin asignacion valida; usando fallback sin restricciones")
    result     <- assigned
    unassigned <- thirds_df$equipo[!thirds_df$equipo %in% result]
    empty      <- slot_names[is.na(result)]
    for (i in seq_along(unassigned)) result[empty[i]] <- unassigned[i]
  }
  result
}

gp <- function(grp, pos)
  standings_real$equipo[standings_real$group == grp & standings_real$posicion == pos]

thirds_df <- standings_real |>
  filter(clasifica_como_3ro) |>
  arrange(desc(pts), desc(gd), desc(gf)) |>
  select(equipo, group)

slots <- assign_thirds(thirds_df)

r32_pairs <- list(
  c(gp("A",2), gp("B",2)),           # M73
  c(gp("E",1), slots[["M74"]]),       # M74
  c(gp("F",1), gp("C",2)),           # M75
  c(gp("C",1), gp("F",2)),           # M76
  c(gp("I",1), slots[["M77"]]),       # M77
  c(gp("E",2), gp("I",2)),           # M78
  c(gp("A",1), slots[["M79"]]),       # M79
  c(gp("L",1), slots[["M80"]]),       # M80
  c(gp("D",1), slots[["M81"]]),       # M81
  c(gp("G",1), slots[["M82"]]),       # M82
  c(gp("K",2), gp("L",2)),           # M83
  c(gp("H",1), gp("J",2)),           # M84
  c(gp("B",1), slots[["M85"]]),       # M85
  c(gp("J",1), gp("H",2)),           # M86
  c(gp("K",1), slots[["M87"]]),       # M87
  c(gp("D",2), gp("G",2))            # M88
)

r16_pares <- list(c(2,5), c(1,3), c(4,6), c(7,8), c(11,12), c(9,10), c(14,16), c(13,15))
qf_pares  <- list(c(1,2), c(5,6), c(3,4), c(7,8))
sf_pares  <- list(c(1,2), c(3,4))

# Mostrar el bracket real
message("  Partidos de Ronda de 32:")
for (i in seq_along(r32_pairs))
  cat(sprintf("    M%d: %-22s vs %s\n", 72 + i, r32_pairs[[i]][1], r32_pairs[[i]][2]))

# ── 5. Simular rounds con resultados reales ───────────────────────────────────

r32_res <- run_round(r32_pairs, "Ronda de 32", paste0("M", 73:88))
r32_win <- sapply(r32_res, `[[`, "winner")

r16_pairs <- lapply(r16_pares, function(p) c(r32_win[p[1]], r32_win[p[2]]))
r16_res   <- run_round(r16_pairs, "Octavos de Final (R16)", paste0("M", 89:96))
r16_win   <- sapply(r16_res, `[[`, "winner")

qf_pairs <- lapply(qf_pares, function(p) c(r16_win[p[1]], r16_win[p[2]]))
qf_res   <- run_round(qf_pairs, "Cuartos de Final", paste0("M", 97:100))
qf_win   <- sapply(qf_res, `[[`, "winner")

sf_pairs <- lapply(sf_pares, function(p) c(qf_win[p[1]], qf_win[p[2]]))
sf_res   <- run_round(sf_pairs, "Semifinales", c("SF1","SF2"))
sf_win   <- sapply(sf_res, `[[`, "winner")

final_res <- run_round(list(c(sf_win[1], sf_win[2])), "Final", "FINAL")[[1]]

sf_l1     <- if (sf_res[[1]]$winner == sf_res[[1]]$t1) sf_res[[1]]$t2 else sf_res[[1]]$t1
sf_l2     <- if (sf_res[[2]]$winner == sf_res[[2]]$t1) sf_res[[2]]$t2 else sf_res[[2]]$t1
third_res <- sim_ko_match(sf_l1, sf_l2)
cat(sprintf("  [3ro] %-22s vs %s\n", sf_l1, sf_l2))

cat(sprintf("\n  *** Campeón del escenario más probable: %s ***\n", final_res$winner))

# ── 6. Guardar bracket_ml.rds ─────────────────────────────────────────────────

saveRDS(
  list(r32 = r32_res, r16 = r16_res, qf = qf_res, sf = sf_res,
       final = final_res, third = third_res),
  "output/tables/bracket_ml.rds"
)
message("\n✓ output/tables/bracket_ml.rds  actualizado")

# ── 7. Probabilidades por ronda (Monte Carlo) ─────────────────────────────────
# Fija los resultados reales ya jugados; simula el resto con el modelo DC.

message("\n── 7. Calculando probabilidades de avance (Monte Carlo)...")

# Resultados ya conocidos
r32_known <- sapply(r32_res, function(m) if (m$played) m$winner else NA_character_)
r16_known <- sapply(r16_res, function(m) if (m$played) m$winner else NA_character_)
qf_known  <- sapply(qf_res,  function(m) if (m$played) m$winner else NA_character_)
sf_known  <- sapply(sf_res,  function(m) if (m$played) m$winner else NA_character_)
fin_known <- if (final_res$played) final_res$winner else NA_character_

# Simulación escalar por sorteo (fija resultados conocidos, simula el resto)
sim_ko_scalar <- function(t1, t2, k) {
  elo_d <- (get_elo(t1) - get_elo(t2)) / 400
  atq_k <- function(t) { v <- atq_df[[t]]; if (is.null(v)) 0.0 else as.numeric(v)[k] }
  def_k <- function(t) { v <- def_df[[t]]; if (is.null(v)) 0.0 else as.numeric(v)[k] }
  l1 <- exp(intercepto[k] + atq_k(t1) - def_k(t2) + b_elo_v[k] *  elo_d)
  l2 <- exp(intercepto[k] + atq_k(t2) - def_k(t1) + b_elo_v[k] * -elo_d)
  g1 <- rpois(1, l1); g2 <- rpois(1, l2)
  if (g1 > g2) t1 else if (g1 < g2) t2 else if (runif(1) < 0.5) t1 else t2
}

N_PROB <- 10000L
set.seed(2027)
prob_draw_idx <- sample(seq_len(n_draws), N_PROB, replace = TRUE)

pb <- txtProgressBar(min = 0, max = N_PROB, style = 3, width = 50)
ko_sims <- vector("list", N_PROB)

for (s in seq_len(N_PROB)) {
  k <- prob_draw_idx[s]

  w32 <- vapply(seq_len(16L), function(i) {
    if (!is.na(r32_known[i])) return(r32_known[i])
    sim_ko_scalar(r32_pairs[[i]][1], r32_pairs[[i]][2], k)
  }, character(1))

  r16_p_s <- lapply(r16_pares, function(p) c(w32[p[1]], w32[p[2]]))
  w16 <- vapply(seq_len(8L), function(i) {
    if (!is.na(r16_known[i])) return(r16_known[i])
    sim_ko_scalar(r16_p_s[[i]][1], r16_p_s[[i]][2], k)
  }, character(1))

  qf_p_s <- lapply(qf_pares, function(p) c(w16[p[1]], w16[p[2]]))
  wqf <- vapply(seq_len(4L), function(i) {
    if (!is.na(qf_known[i])) return(qf_known[i])
    sim_ko_scalar(qf_p_s[[i]][1], qf_p_s[[i]][2], k)
  }, character(1))

  sf_p_s <- lapply(sf_pares, function(p) c(wqf[p[1]], wqf[p[2]]))
  wsf <- vapply(seq_len(2L), function(i) {
    if (!is.na(sf_known[i])) return(sf_known[i])
    sim_ko_scalar(sf_p_s[[i]][1], sf_p_s[[i]][2], k)
  }, character(1))

  wfin <- if (!is.na(fin_known)) fin_known else sim_ko_scalar(wsf[1], wsf[2], k)
  wrun <- if (wsf[1] == wfin) wsf[2] else wsf[1]

  ko_sims[[s]] <- list(r32 = w32, r16 = w16, qf = wqf, sf = wsf,
                       champion = wfin, runner_up = wrun)
  setTxtProgressBar(pb, s)
}
close(pb)

# Agregar probabilidades
all_teams <- sort(unique(standings_real$equipo[standings_real$clasifica]))
nt        <- length(all_teams)
cnt <- function(v) tabulate(match(v, all_teams), nbins = nt) / N_PROB

r32_w  <- unlist(lapply(ko_sims, `[[`, "r32"))
r16_w  <- unlist(lapply(ko_sims, `[[`, "r16"))
qf_w   <- unlist(lapply(ko_sims, `[[`, "qf"))
sf_w   <- unlist(lapply(ko_sims, `[[`, "sf"))
ch_v   <- sapply(ko_sims, `[[`, "champion")
ru_v   <- sapply(ko_sims, `[[`, "runner_up")

prob_table <- tibble(equipo = all_teams) |>
  mutate(
    prob_r32   = cnt(r32_w),
    prob_r16   = cnt(r16_w),
    prob_qf    = cnt(qf_w),
    prob_sf    = cnt(sf_w),
    prob_final = cnt(ch_v) + cnt(ru_v),
    prob_camp  = cnt(ch_v)
  ) |>
  arrange(desc(prob_camp))

write_csv(prob_table, "output/tables/knockout_probs.csv")
message("✓ output/tables/knockout_probs.csv  actualizado")

# ── 8. Resumen en consola ─────────────────────────────────────────────────────

message("\n════════════════════════════════════════════════════════════════")
message("  PROBABILIDADES POR RONDA — FIFA World Cup 2026 (fase KO)")
message("════════════════════════════════════════════════════════════════")
message(sprintf("%-24s %6s %6s %6s %6s %7s %7s",
                "Equipo", "R32", "R16", "QF", "SF", "Final", "Camp."))
message(strrep("─", 64))
for (i in seq_len(min(nrow(prob_table), 20L))) {
  r <- prob_table[i, ]
  message(sprintf("%-24s %5.1f%% %5.1f%% %5.1f%% %5.1f%% %6.1f%% %6.1f%%",
                  r$equipo,
                  r$prob_r32   * 100, r$prob_r16  * 100,
                  r$prob_qf    * 100, r$prob_sf   * 100,
                  r$prob_final * 100, r$prob_camp * 100))
}

# Resumen partidos por ronda
n_jugados_r32 <- sum(sapply(r32_res, `[[`, "played"))
n_jugados_r16 <- sum(sapply(r16_res, `[[`, "played"))
n_jugados_qf  <- sum(sapply(qf_res,  `[[`, "played"))
n_jugados_sf  <- sum(sapply(sf_res,  `[[`, "played"))
n_jugados_fin <- as.integer(final_res$played)
total_jugados <- n_jugados_r32 + n_jugados_r16 + n_jugados_qf + n_jugados_sf + n_jugados_fin
total_ko      <- 16L + 8L + 4L + 2L + 1L  # 31 partidos KO (sin 3ro)

message(sprintf("\n── Avance: %d/31 partidos eliminatorios jugados ──", total_jugados))
message(sprintf("   R32: %d/16  |  R16: %d/8  |  QF: %d/4  |  SF: %d/2  |  Final: %d/1",
                n_jugados_r32, n_jugados_r16, n_jugados_qf, n_jugados_sf, n_jugados_fin))

# ── 9. Regenerar bracket PNG ──────────────────────────────────────────────────

message("\n── 9. Regenerando bracket PNG...")
source("R/07_bracket_viz.R")
message("\n✓ Todo listo. Corre de nuevo source('R/06b_knockout_live.R') después de cada jornada.")
