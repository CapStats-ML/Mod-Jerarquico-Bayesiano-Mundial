# ── 08_dag.R — DAG del modelo bayesiano jerárquico ───────────────────────────
# Genera output/figures/dag_modelo.png

library(grDevices)

# ── Canvas ────────────────────────────────────────────────────────────────────
W <- 800L; H <- 500L

COL_DARK  <- "#1a3a5c"
COL_OBS   <- "#d4e3f0"   # nodo observado (sombreado)
COL_PRIOR <- "#7a9ab5"   # texto de priors
COL_PLATE <- "#1a3a5c"   # borde de plate

R_NODE <- 22L            # radio de nodo

# ── Coordenadas de nodos ──────────────────────────────────────────────────────
nd <- list(
  mu      = c(100, 395),
  eta     = c(205, 395),
  beta    = c(310, 395),
  s_atq   = c(460, 395),
  s_def   = c(570, 395),
  rho     = c(710, 395),
  alpha_k = c(460, 270),
  delta_k = c(570, 270),
  x_loc   = c(148, 158),
  elo     = c(285, 158),
  lambda  = c(490, 158),
  y_i     = c(490,  62)
)

# ── Helpers ───────────────────────────────────────────────────────────────────

# Círculo llenado con polygon (confiable con asp=1)
draw_node <- function(x, y, label, observed = FALSE, r = R_NODE) {
  theta    <- seq(0, 2 * pi, length.out = 120)
  col_fill <- if (observed) COL_OBS else "white"
  polygon(x + r * cos(theta), y + r * sin(theta),
          col = col_fill, border = COL_DARK, lwd = 1.6)
  text(x, y, label, cex = 0.72, col = COL_DARK, font = 2L)
}

# Plate (rectángulo punteado)
draw_plate <- function(x0, y0, x1, y1, label) {
  rect(x0, y0, x1, y1, col = NA, border = COL_PLATE, lty = 2, lwd = 0.9)
  text(x1 - 5, y0 + 7, label, adj = c(1, 0), cex = 0.46, col = COL_PLATE, font = 3L)
}

# Flecha entre dos nodos (sale del borde del nodo origen, entra al destino)
arr <- function(from, to) {
  dx <- nd[[to]][1]   - nd[[from]][1]
  dy <- nd[[to]][2]   - nd[[from]][2]
  d  <- sqrt(dx^2 + dy^2)
  x0 <- nd[[from]][1] + dx / d * (R_NODE + 1)
  y0 <- nd[[from]][2] + dy / d * (R_NODE + 1)
  x1 <- nd[[to]][1]   - dx / d * (R_NODE + 3)
  y1 <- nd[[to]][2]   - dy / d * (R_NODE + 3)
  arrows(x0, y0, x1, y1, length = 0.07, angle = 22,
         col = COL_DARK, lwd = 1.1)
}

# ── Abrir PNG ─────────────────────────────────────────────────────────────────
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
png("output/figures/dag_modelo.png",
    width = W * 2L, height = H * 2L, res = 240L, bg = "white")
par(mar = c(0, 0, 0, 0), xpd = TRUE)
plot(NA, xlim = c(0, W), ylim = c(0, H), asp = 1,
     axes = FALSE, xlab = "", ylab = "")

# ── Título ────────────────────────────────────────────────────────────────────
text(W / 2, H - 14,
     "Modelo Bayesiano Jerárquico — Poisson + Dixon-Coles",
     adj = c(0.5, 0.5), cex = 0.82, col = COL_DARK, font = 2L)
segments(30, H - 26, W - 30, H - 26, col = "#cccccc", lwd = 0.7)

# ── Plates ────────────────────────────────────────────────────────────────────
draw_plate(415, 230, 625, 312, "k = 1…48  equipos")
draw_plate( 82,  28, 752, 205, "i = 1…N  partidos")

# ── Flechas (debajo de los nodos) ─────────────────────────────────────────────
# Priors → parámetros globales (no se dibujan flechas; los priors son anotaciones)

# σ → efectos aleatorios
arr("s_atq", "alpha_k")
arr("s_def", "delta_k")

# Parámetros globales → λ
arr("mu",   "lambda")
arr("eta",  "lambda")
arr("beta", "lambda")

# Observados → λ
arr("x_loc", "lambda")
arr("elo",   "lambda")

# Efectos de equipo → λ
arr("alpha_k", "lambda")
arr("delta_k", "lambda")

# λ, ρ → y_i
arr("lambda", "y_i")
arr("rho",    "y_i")

# ── Nodos ─────────────────────────────────────────────────────────────────────
draw_node(nd$mu[1],      nd$mu[2],      "μ")
draw_node(nd$eta[1],     nd$eta[2],     "η")
draw_node(nd$beta[1],    nd$beta[2],    "β")
draw_node(nd$s_atq[1],   nd$s_atq[2],  "σ_α")
draw_node(nd$s_def[1],   nd$s_def[2],  "σ_δ")
draw_node(nd$rho[1],     nd$rho[2],     "ρ")
draw_node(nd$alpha_k[1], nd$alpha_k[2],"α[k]")
draw_node(nd$delta_k[1], nd$delta_k[2],"δ[k]")
draw_node(nd$x_loc[1],   nd$x_loc[2],  "x_loc",  observed = TRUE)
draw_node(nd$elo[1],     nd$elo[2],     "elo_d",  observed = TRUE)
draw_node(nd$lambda[1],  nd$lambda[2],  "λ_i")
draw_node(nd$y_i[1],     nd$y_i[2],     "y_i",    observed = TRUE)

# ── Anotaciones de priors (sobre cada parámetro global) ───────────────────────
priors <- list(
  mu    = "N(0.3, 0.5)",
  eta   = "N(0.2, 0.3)",
  beta  = "N(0, 0.3)",
  s_atq = "Exp(5)",
  s_def = "Exp(5)",
  rho   = "N(0, 0.1)"
)
for (nm in names(priors)) {
  text(nd[[nm]][1], nd[[nm]][2] + R_NODE + 13,
       priors[[nm]], cex = 0.46, col = COL_PRIOR, adj = c(0.5, 0))
}

# ── Ecuación del predictor lineal ─────────────────────────────────────────────
text(130, 235,
     expression(log(lambda[i]) == mu + alpha[k] - delta[j] + eta %.% x[i]^loc + beta %.% elo[i]),
     adj = c(0, 0.5), cex = 0.62, col = COL_DARK)
text(130, 213,
     expression(group("(", list(g[1], g[2]), ")") ~
                "~  DC-Poisson(" * lambda[1] * ", " * lambda[2] * ", " * rho * ")"),
     adj = c(0, 0.5), cex = 0.58, col = COL_PRIOR)

# ── Leyenda ───────────────────────────────────────────────────────────────────
lx <- 15; ly <- 185
theta <- seq(0, 2 * pi, length.out = 80)
polygon(lx + 7 * cos(theta), ly      + 7 * sin(theta), col = "white",   border = COL_DARK, lwd = 1.3)
polygon(lx + 7 * cos(theta), ly - 20 + 7 * sin(theta), col = COL_OBS,  border = COL_DARK, lwd = 1.3)
text(lx + 14, ly,      "Latente",   adj = c(0, 0.5), cex = 0.46, col = COL_DARK)
text(lx + 14, ly - 20, "Observado", adj = c(0, 0.5), cex = 0.46, col = COL_DARK)

dev.off()
message("✓ output/figures/dag_modelo.png")
