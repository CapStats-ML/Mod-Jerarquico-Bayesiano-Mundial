library(brms)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ── 1. Cargar datos ────────────────────────────────────────────────────────────
# Requiere: output/tables/standings_long.csv  (generado por 04_simulation.R)
#           output/posteriors/fit_B_wc_vs_wc.rds

message("Cargando datos...")
fit_B       <- readRDS("output/posteriors/fit_B_wc_vs_wc.rds")
elo_ratings <- read_csv("data/processed/elo_ratings.csv",   show_col_types = FALSE)
standings   <- read_csv("output/tables/standings_long.csv", show_col_types = FALSE)

elo_lookup <- as.list(setNames(elo_ratings$elo, elo_ratings$team))

# ── 2. Draws del posterior ────────────────────────────────────────────────────

message("Extrayendo draws del posterior...")
draws_df <- as_draws_df(fit_B)

extract_re_list <- function(draws, prefix) {
  pattern   <- paste0(prefix, "[")
  cols      <- names(draws)[grepl(pattern, names(draws), fixed = TRUE)]
  if (length(cols) == 0) return(list())
  mat       <- as.matrix(draws[, cols, drop = FALSE])
  raw_names <- sub(paste0(".*", prefix, "\\[([^,]+),.*"), "\\1", cols)
  raw_names <- gsub("\\.", " ", raw_names)
  colnames(mat) <- raw_names
  setNames(lapply(seq_len(ncol(mat)), function(j) mat[, j]), raw_names)
}

atq_list  <- extract_re_list(draws_df, "r_equipo")
def_list  <- extract_re_list(draws_df, "r_rival")
int_vec   <- as.numeric(draws_df$b_Intercept)
b_elo_vec <- as.numeric(draws_df$b_elo_diff)
n_draws   <- length(int_vec)
message(sprintf("Draws disponibles: %d", n_draws))

# ── 3. Funciones de partido ────────────────────────────────────────────────────

get_atq <- function(t, i) { v <- atq_list[[t]]; if (is.null(v)) 0.0 else v[[i]] }
get_def <- function(t, i) { v <- def_list[[t]]; if (is.null(v)) 0.0 else v[[i]] }
get_elo <- function(t)    { v <- elo_lookup[[t]]; if (is.null(v)) 1500.0 else v }

# Simula un partido eliminatorio; empates en 90' → penales 50-50
sim_ko <- function(t1, t2, di) {
  elo_d <- (get_elo(t1) - get_elo(t2)) / 400
  l1 <- exp(int_vec[[di]] + get_atq(t1,di) - get_def(t2,di) + b_elo_vec[[di]] *  elo_d)
  l2 <- exp(int_vec[[di]] + get_atq(t2,di) - get_def(t1,di) + b_elo_vec[[di]] * -elo_d)
  g1 <- rpois(1, l1);  g2 <- rpois(1, l2)
  if (g1 > g2) return(t1)
  if (g1 < g2) return(t2)
  if (runif(1) < 0.5) t1 else t2   # penales
}

# ── 4. Bracket oficial FIFA 2026 ──────────────────────────────────────────────
#
# Fuente: Reglamento FIFA + Wikipedia (2026 FIFA World Cup knockout stage)
#
# RONDA DE 32 (M73–M88):
#   M73: 2A vs 2B          M74: 1E vs 3rd(A/B/C/D/F)
#   M75: 1F vs 2C          M76: 1C vs 2F
#   M77: 1I vs 3rd(C/D/F/G/H)   M78: 2E vs 2I
#   M79: 1A vs 3rd(C/E/F/H/I)   M80: 1L vs 3rd(E/H/I/J/K)
#   M81: 1D vs 3rd(B/E/F/I/J)   M82: 1G vs 3rd(A/E/H/I/J)
#   M83: 2K vs 2L          M84: 1H vs 2J
#   M85: 1B vs 3rd(E/F/G/I/J)   M86: 1J vs 2H
#   M87: 1K vs 3rd(D/E/I/J/L)   M88: 2D vs 2G
#
# OCTAVOS (M89–M96):
#   M89: W74 vs W77   M90: W73 vs W75
#   M91: W76 vs W78   M92: W79 vs W80
#   M93: W83 vs W84   M94: W81 vs W82
#   M95: W86 vs W88   M96: W85 vs W87
#
# CUARTOS (M97–M100):
#   M97: W89 vs W90   M98: W93 vs W94
#   M99: W91 vs W92   M100: W95 vs W96
#
# SEMIS: M97 vs M98,  M99 vs M100
# FINAL: ganadores de semis

# Grupos elegibles para cada slot de terceros
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

# Asigna los 8 mejores terceros a los slots mediante backtracking.
# El greedy simple falla en algunos escenarios (ej: grupos D y L compiten por M87).
# Backtracking garantiza encontrar una asignación válida siempre que exista
# (FIFA diseñó las 495 combinaciones para que siempre exista una).
assign_thirds <- function(thirds_df) {
  slot_names <- names(third_slots)

  # Para cada equipo: cuáles slots son elegibles según su grupo
  elig_per_team <- lapply(thirds_df$group, function(g) {
    slot_names[vapply(third_slots, function(gs) g %in% gs, logical(1))]
  })

  # Ordenar por más restringido primero (poda el árbol más rápido)
  ord <- order(lengths(elig_per_team))

  assigned <- setNames(rep(NA_character_, length(slot_names)), slot_names)

  # Backtracking recursivo
  bt <- function(k, asgn) {
    if (k > length(ord)) return(asgn)          # solución completa encontrada
    idx   <- ord[k]
    team  <- thirds_df$equipo[idx]
    avail <- elig_per_team[[idx]][ is.na(asgn[elig_per_team[[idx]]]) ]
    for (slot in avail) {
      asgn[slot] <- team
      result <- bt(k + 1, asgn)
      if (!is.null(result)) return(result)      # propagar solución
      asgn[slot] <- NA_character_               # deshacer y probar siguiente
    }
    NULL  # callejón sin salida → backtrack
  }

  result <- bt(1, assigned)

  # Fallback defensivo (no debería ocurrir con datos FIFA válidos)
  if (is.null(result)) {
    warning("assign_thirds: no se encontró asignación válida; usando fallback sin restricciones")
    result <- assigned
    unassigned <- thirds_df$equipo[ !thirds_df$equipo %in% result ]
    empty      <- slot_names[ is.na(result) ]
    for (i in seq_along(unassigned)) result[ empty[i] ] <- unassigned[i]
  }
  result
}

# Construye el vector de 16 partidos del R32 para una simulación
build_r32 <- function(st) {
  gp <- function(grp, pos) st$equipo[st$group == grp & st$posicion == pos]

  t3_df <- st |>
    filter(posicion == 3, clasifica_como_3ro) |>
    arrange(desc(pts), desc(gd), desc(gf)) |>
    select(equipo, group)

  slots <- assign_thirds(t3_df)

  # Orden: M73, M74, ..., M88  (índices 1-16 usados en bracket de R16/QF)
  list(
    c(gp("A",2), gp("B",2)),          # M73
    c(gp("E",1), slots[["M74"]]),      # M74
    c(gp("F",1), gp("C",2)),          # M75
    c(gp("C",1), gp("F",2)),          # M76
    c(gp("I",1), slots[["M77"]]),      # M77
    c(gp("E",2), gp("I",2)),          # M78
    c(gp("A",1), slots[["M79"]]),      # M79
    c(gp("L",1), slots[["M80"]]),      # M80
    c(gp("D",1), slots[["M81"]]),      # M81
    c(gp("G",1), slots[["M82"]]),      # M82
    c(gp("K",2), gp("L",2)),          # M83
    c(gp("H",1), gp("J",2)),          # M84
    c(gp("B",1), slots[["M85"]]),      # M85
    c(gp("J",1), gp("H",2)),          # M86
    c(gp("K",1), slots[["M87"]]),      # M87
    c(gp("D",2), gp("G",2))           # M88
  )
}

# R16, QF, SF como pares de índices del vector de ganadores anterior
# Índices referenciados al vector r32_winners[1..16] (M73=1 … M88=16)
r16_pares <- list(c(2,5), c(1,3), c(4,6), c(7,8), c(11,12), c(9,10), c(14,16), c(13,15))
# M89..M96: W74vsW77, W73vsW75, W76vsW78, W79vsW80, W83vsW84, W81vsW82, W86vsW88, W85vsW87

qf_pares  <- list(c(1,2), c(5,6), c(3,4), c(7,8))
# M97: W89vsW90  M98: W93vsW94  M99: W91vsW92  M100: W95vsW96

sf_pares  <- list(c(1,2), c(3,4))
# SF1: W97vsW98   SF2: W99vsW100

# Corre el bracket completo para una simulación
run_knockout <- function(st, di) {
  pairs <- build_r32(st)

  r32 <- vapply(pairs, function(p) sim_ko(p[1], p[2], di), character(1))
  r16 <- vapply(r16_pares, function(p) sim_ko(r32[p[1]], r32[p[2]], di), character(1))
  qf  <- vapply(qf_pares,  function(p) sim_ko(r16[p[1]], r16[p[2]], di), character(1))
  sf  <- vapply(sf_pares,  function(p) sim_ko(qf[p[1]],  qf[p[2]],  di), character(1))
  ch  <- sim_ko(sf[1], sf[2], di)

  list(r32=r32, r16=r16, qf=qf, sf=sf, champion=ch)
}

# ── 5. Simulación ─────────────────────────────────────────────────────────────

sim_ids <- sort(unique(standings$sim_id))
n_sim   <- length(sim_ids)
message(sprintf("\nSimulando eliminatorias para %d escenarios...\n", n_sim))

standings_split <- split(standings, standings$sim_id)

set.seed(2026)
draw_idx <- sample(seq_len(n_draws), n_sim, replace = TRUE)

pb <- txtProgressBar(min = 0, max = n_sim, style = 3, width = 50)
ko_results <- vector("list", n_sim)
for (k in seq_len(n_sim)) {
  ko_results[[k]] <- run_knockout(standings_split[[ as.character(sim_ids[k]) ]], draw_idx[k])
  setTxtProgressBar(pb, k)
}
close(pb)

# ── 6. Agregar resultados ─────────────────────────────────────────────────────

message("\nAgregando resultados...")
teams_all <- sort(unique(standings$equipo[standings$clasifica]))
nt        <- length(teams_all)
tab <- function(v) tabulate(match(v, teams_all), nbins = nt) / n_sim

r32_w <- unlist(lapply(ko_results, `[[`, "r32"))
r16_w <- unlist(lapply(ko_results, `[[`, "r16"))
qf_w  <- unlist(lapply(ko_results, `[[`, "qf"))
sf_w  <- unlist(lapply(ko_results, `[[`, "sf"))
ch_v  <- sapply(ko_results, `[[`, "champion")
ru_v  <- sapply(ko_results, function(r) {
  sf <- r$sf; ch <- r$champion
  if (sf[1] == ch) sf[2] else sf[1]
})

prob_table <- tibble(equipo = teams_all) |>
  mutate(
    prob_r32   = tab(r32_w),
    prob_r16   = tab(r16_w),
    prob_qf    = tab(qf_w),
    prob_sf    = tab(sf_w),
    prob_final = tab(ch_v) + tab(ru_v),
    prob_camp  = tab(ch_v)
  ) |>
  arrange(desc(prob_camp))

write_csv(prob_table, "output/tables/knockout_probs.csv")
message("✓ Guardado: output/tables/knockout_probs.csv")

# ── 7. Consola ────────────────────────────────────────────────────────────────

message("\n════════════════════════════════════════════════════════════")
message("  PROBABILIDADES POR RONDA — FIFA World Cup 2026")
message("════════════════════════════════════════════════════════════")
message(sprintf("%-24s %6s %6s %6s %6s %7s %7s",
                "Equipo", "R32", "R16", "QF", "SF", "Final", "Camp."))
message(strrep("─", 64))
for (i in seq_len(nrow(prob_table))) {
  r <- prob_table[i, ]
  message(sprintf("%-24s %5.1f%% %5.1f%% %5.1f%% %5.1f%% %6.1f%% %6.1f%%",
                  r$equipo,
                  r$prob_r32   * 100, r$prob_r16  * 100,
                  r$prob_qf    * 100, r$prob_sf   * 100,
                  r$prob_final * 100, r$prob_camp * 100))
}

# ── 8. Gráficos (R base) ──────────────────────────────────────────────────────

top16 <- prob_table |> slice_head(n = 16)

pdf("output/figures/knockout_champion_probs.pdf", width = 11, height = 6)
par(mar = c(7, 4, 3, 1))
bp <- barplot(
  top16$prob_camp * 100,
  names.arg  = top16$equipo,
  col        = colorRampPalette(c("#1a6fba", "#5ba4e3"))(16),
  border     = NA,
  ylim       = c(0, max(top16$prob_camp * 100) * 1.25),
  ylab       = "Probabilidad de ser campeón (%)",
  main       = "FIFA World Cup 2026 — Favoritos al título",
  las        = 2,
  cex.names  = 0.85
)
text(bp, top16$prob_camp * 100 + 0.3,
     labels = sprintf("%.1f%%", top16$prob_camp * 100),
     cex = 0.75, adj = c(0.5, 0))
abline(h = seq(2, 20, 2), col = "grey85", lty = 2)
dev.off()

top12 <- prob_table |> slice_head(n = 12)
rondas <- c("R32","R16","QF","SF","Final","Campeón")
mat    <- as.matrix(top12[, c("prob_r32","prob_r16","prob_qf",
                               "prob_sf","prob_final","prob_camp")]) * 100

pdf("output/figures/knockout_rounds_top12.pdf", width = 12, height = 6)
par(mar = c(6, 4, 3, 1))
barplot(
  t(mat),
  beside      = TRUE,
  names.arg   = top12$equipo,
  col         = c("#cce5ff","#99caff","#5ba4e3","#1a6fba","#f5a623","#e05c4b"),
  border      = NA,
  ylim        = c(0, 75),
  ylab        = "Probabilidad (%)",
  main        = "Probabilidad de avanzar por ronda — Top 12",
  legend.text = rondas,
  args.legend = list(x = "topright", bty = "n", cex = 0.8),
  las         = 2,
  cex.names   = 0.8
)
dev.off()

message("✓ Guardado: output/figures/knockout_champion_probs.pdf")
message("✓ Guardado: output/figures/knockout_rounds_top12.pdf")
message("\n─── Simulación de eliminatorias completa ───")

# ── 9. Escenario más probable: goles por partido ──────────────────────────────
# Construye el bracket del "escenario central" (equipo más probable por posición
# en cada grupo) y simula cada partido de forma vectorizada para obtener:
# goles esperados, top-3 marcadores y probabilidad de avanzar.

message("\n\n═══════════════════════════════════════════════════════════════════")
message("  ESCENARIO MÁS PROBABLE — Goles por partido en eliminatorias")
message("═══════════════════════════════════════════════════════════════════\n")

qual_probs <- read_csv("output/tables/qualification_probs.csv", show_col_types = FALSE)

# Draws para esta simulación vectorizada (separados de los del loop principal)
n_ko_sim <- 5000
set.seed(2027)
ko_idx   <- sample(seq_len(n_draws), n_ko_sim, replace = TRUE)
int_ko   <- int_vec[ko_idx]
b_elo_ko <- b_elo_vec[ko_idx]
atq_ko   <- lapply(atq_list, function(v) v[ko_idx])
def_ko   <- lapply(def_list, function(v) v[ko_idx])

get_atq_ko  <- function(t) { v <- atq_ko[[t]]; if (is.null(v)) rep(0.0, n_ko_sim) else v }
get_def_ko  <- function(t) { v <- def_ko[[t]]; if (is.null(v)) rep(0.0, n_ko_sim) else v }

top3_ko <- function(g1, g2, n = 3) {
  sc  <- paste(g1, g2, sep = "-")
  tab <- sort(table(sc), decreasing = TRUE)
  pct <- round(as.numeric(tab) / length(sc) * 100)
  paste(sprintf("%s(%d%%)", names(tab)[seq_len(min(n, length(tab)))],
                pct[seq_len(min(n, length(tab)))]), collapse = "  ")
}

sim_match_ko <- function(t1, t2) {
  elo_d <- (get_elo(t1) - get_elo(t2)) / 400
  l1    <- exp(int_ko + get_atq_ko(t1) - get_def_ko(t2) + b_elo_ko *  elo_d)
  l2    <- exp(int_ko + get_atq_ko(t2) - get_def_ko(t1) + b_elo_ko * -elo_d)
  g1    <- rpois(n_ko_sim, l1)
  g2    <- rpois(n_ko_sim, l2)

  p1  <- mean(g1 > g2)   # gana t1 en 90'
  px  <- mean(g1 == g2)  # empate en 90' → penales 50-50
  p2  <- mean(g1 < g2)   # gana t2 en 90'
  padv1 <- p1 + px * 0.5

  list(
    t1       = t1,
    t2       = t2,
    esp_g1   = round(mean(l1), 2),
    esp_g2   = round(mean(l2), 2),
    top3     = top3_ko(g1, g2),
    p_gana1  = round(p1, 3),
    p_empate = round(px, 3),
    p_gana2  = round(p2, 3),
    p_adv_t1 = round(padv1, 3),
    winner   = if (padv1 >= 0.5) t1 else t2
  )
}

# Equipo más probable por posición en cada grupo
get_best_ml <- function(grp, col, exclude = character(0)) {
  d <- qual_probs[qual_probs$group == grp & !qual_probs$equipo %in% exclude, ]
  d <- d[order(-d[[col]]), ]
  d$equipo[1]
}

grupos_ml <- sort(unique(qual_probs$group))  # A..L

ml_1ro <- setNames(sapply(grupos_ml, function(g) get_best_ml(g, "prob_1ro")),             grupos_ml)
ml_2do <- setNames(sapply(grupos_ml, function(g) get_best_ml(g, "prob_2do", ml_1ro[g])), grupos_ml)
ml_3ro <- setNames(sapply(grupos_ml, function(g)
  get_best_ml(g, "prob_3ro", c(ml_1ro[g], ml_2do[g]))), grupos_ml)

# 8 mejores terceros: ordenar grupos por prob_3ro de su equipo más probable 3ro
p3_vals <- sapply(grupos_ml, function(g) {
  d <- qual_probs[qual_probs$equipo == ml_3ro[g] & qual_probs$group == g, ]
  if (nrow(d) == 0) 0 else d$prob_3ro[1]
})
best_8_grps  <- grupos_ml[order(p3_vals, decreasing = TRUE)[1:8]]
thirds_df_ml <- data.frame(equipo = ml_3ro[best_8_grps], group = best_8_grps,
                            stringsAsFactors = FALSE)
slots_ml <- assign_thirds(thirds_df_ml)

r32_ml <- list(
  c(ml_2do["A"], ml_2do["B"]),            # M73
  c(ml_1ro["E"], slots_ml["M74"]),         # M74
  c(ml_1ro["F"], ml_2do["C"]),            # M75
  c(ml_1ro["C"], ml_2do["F"]),            # M76
  c(ml_1ro["I"], slots_ml["M77"]),         # M77
  c(ml_2do["E"], ml_2do["I"]),            # M78
  c(ml_1ro["A"], slots_ml["M79"]),         # M79
  c(ml_1ro["L"], slots_ml["M80"]),         # M80
  c(ml_1ro["D"], slots_ml["M81"]),         # M81
  c(ml_1ro["G"], slots_ml["M82"]),         # M82
  c(ml_2do["K"], ml_2do["L"]),            # M83
  c(ml_1ro["H"], ml_2do["J"]),            # M84
  c(ml_1ro["B"], slots_ml["M85"]),         # M85
  c(ml_1ro["J"], ml_2do["H"]),            # M86
  c(ml_1ro["K"], slots_ml["M87"]),         # M87
  c(ml_2do["D"], ml_2do["G"])             # M88
)

print_round <- function(results, ids) {
  for (i in seq_along(results)) {
    m <- results[[i]]
    cat(sprintf(
      "  [%s] %-24s vs %-24s | esp: %.2f–%.2f | %-38s | P(avanza): %.0f%% – %.0f%%\n",
      ids[i], m$t1, m$t2,
      m$esp_g1, m$esp_g2,
      m$top3,
      m$p_adv_t1 * 100, (1 - m$p_adv_t1) * 100
    ))
  }
}

# ── R32 ──
cat("── Ronda de 32 ──\n\n")
r32_res <- lapply(r32_ml, function(p) sim_match_ko(p[1], p[2]))
r32_win <- sapply(r32_res, `[[`, "winner")
print_round(r32_res, paste0("M", 73:88))

# ── R16 ──
cat("\n── Octavos de Final ──\n\n")
r16_res <- lapply(r16_pares, function(p) sim_match_ko(r32_win[p[1]], r32_win[p[2]]))
r16_win <- sapply(r16_res, `[[`, "winner")
print_round(r16_res, paste0("M", 89:96))

# ── QF ──
cat("\n── Cuartos de Final ──\n\n")
qf_res <- lapply(qf_pares, function(p) sim_match_ko(r16_win[p[1]], r16_win[p[2]]))
qf_win <- sapply(qf_res, `[[`, "winner")
print_round(qf_res, paste0("M", 97:100))

# ── SF ──
cat("\n── Semifinales ──\n\n")
sf_res <- lapply(sf_pares, function(p) sim_match_ko(qf_win[p[1]], qf_win[p[2]]))
sf_win <- sapply(sf_res, `[[`, "winner")
print_round(sf_res, c("SF1", "SF2"))

# ── Final ──
cat("\n── Final ──\n\n")
final_res <- sim_match_ko(sf_win[1], sf_win[2])
print_round(list(final_res), "FINAL")

cat(sprintf("\n  *** Campeón del escenario más probable: %s ***\n\n", final_res$winner))
