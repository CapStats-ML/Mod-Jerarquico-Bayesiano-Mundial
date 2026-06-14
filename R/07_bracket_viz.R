# ── 07_bracket_viz.R — Parte 1: Estructura base ──────────────────────────────
# Canvas PNG 1200×700, layout del bracket R32 → Final.
# Datos reales se agregarán en la siguiente parte.

library(grDevices)
library(png)

# ── Dimensiones y colores ─────────────────────────────────────────────────────
W  <- 1200L
H  <- 628L   # ratio 1.91:1 — óptimo LinkedIn
# Resolución de salida: 2× los píxeles lógicos → imagen de alta definición
OUT_W <- W * 2L   # 2400px
OUT_H <- H * 2L   # 1400px
OUT_RES <- 240L   # DPI (doble del original → el texto y las líneas quedan nítidos)

COL_BG    <- "white"
COL_DARK  <- "#1a3a5c"
COL_LINES <- "#cccccc"
COL_BOX   <- "#f0f4f8"
COL_BOX_B <- "#dde5ee"   # borde de celdas

# ── Layout ────────────────────────────────────────────────────────────────────
TOP_M    <- 72L          # margen superior (título + subtítulo + etiquetas de rondas)
BOT_M    <- 25L
USABLE_H <- H - TOP_M - BOT_M   # 615px

N_ROUNDS <- 4L           # columnas por lado: R32, R16, QF, SF
BOX_W    <- 112L         # ancho de cada caja de partido
BOX_H    <- 44L          # alto (2 filas de 22px)
ROW_H    <- BOX_H / 2L   # 22px por equipo

CEN_X    <- W / 2L       # 600 — eje central

# Ancho de columna: divide el espacio disponible en 4 columnas iguales por lado
# Se deja espacio para la Final (BOX_W) en el centro.
SIDE_W  <- CEN_X - BOX_W / 2L         # ~544px por lado
COL_W   <- SIDE_W / N_ROUNDS          # ~136px por columna

# Centros de columna (izquierda, de fuera hacia el centro: R32 → SF)
col_x_L <- COL_W * (seq_len(N_ROUNDS) - 0.5)
# Derecha: espejo
col_x_R <- W - col_x_L

# ── Posiciones Y de los partidos ──────────────────────────────────────────────
# 8 partidos R32 por lado, distribuidos uniformemente
r32_y <- TOP_M + (seq_len(8L) - 0.5) * USABLE_H / 8L

# Cada ronda subsiguiente se centra entre los dos partidos que alimenta
r16_y  <- (r32_y[seq(1, 7, 2)] + r32_y[seq(2, 8, 2)]) / 2   # 4 matches
qf_y   <- (r16_y[seq(1, 3, 2)] + r16_y[seq(2, 4, 2)]) / 2   # 2 matches
sf_y   <- (qf_y[1] + qf_y[2]) / 2                             # 1 match

final_y <- sf_y   # alinea la Final con las SFs

# ── Helpers de dibujo ─────────────────────────────────────────────────────────
FLAG_W <- 16L   # ancho lógico de cada bandera
FLAG_H <- 11L   # alto lógico (≈ 3:2)

draw_box <- function(cx, cy, label = "", t1 = "—", t2 = "—",
                     win = 0L, score1 = "", score2 = "",
                     flag1 = NULL, flag2 = NULL) {
  x0 <- cx - BOX_W / 2;  x1 <- cx + BOX_W / 2
  y0 <- cy - BOX_H / 2;  y1 <- cy + BOX_H / 2
  ym <- cy

  rect(x0, y0, x1, y1, col = COL_BOX,  border = COL_BOX_B, lwd = 0.7)
  segments(x0, ym, x1, ym, col = COL_BOX_B, lwd = 0.5)

  # ── Fila 1 ──
  col1 <- if (win == 1L) COL_DARK else "grey55"
  fw1  <- if (win == 1L) 2L       else 1L
  fy1  <- cy - ROW_H / 2
  if (!is.null(flag1))
    rasterImage(flag1, xleft = x0 + 3, xright = x0 + 3 + FLAG_W,
                ybottom = fy1 - FLAG_H/2, ytop = fy1 + FLAG_H/2,
                interpolate = TRUE)
  tx1 <- x0 + if (!is.null(flag1)) FLAG_W + 6L else 5L
  text(tx1, fy1, t1, adj = c(0, 0.5), cex = 0.50, col = col1, font = fw1)
  if (nchar(score1) > 0)
    text(x1 - 4, fy1, score1, adj = c(1, 0.5), cex = 0.47, col = col1, font = fw1)

  # ── Fila 2 ──
  col2 <- if (win == 2L) COL_DARK else "grey55"
  fw2  <- if (win == 2L) 2L       else 1L
  fy2  <- cy + ROW_H / 2
  if (!is.null(flag2))
    rasterImage(flag2, xleft = x0 + 3, xright = x0 + 3 + FLAG_W,
                ybottom = fy2 - FLAG_H/2, ytop = fy2 + FLAG_H/2,
                interpolate = TRUE)
  tx2 <- x0 + if (!is.null(flag2)) FLAG_W + 6L else 5L
  text(tx2, fy2, t2, adj = c(0, 0.5), cex = 0.50, col = col2, font = fw2)
  if (nchar(score2) > 0)
    text(x1 - 4, fy2, score2, adj = c(1, 0.5), cex = 0.47, col = col2, font = fw2)

  if (nchar(label) > 0)
    text(cx, y0 - 9, label, adj = c(0.5, 1), cex = 0.38, col = "grey65")
}

# Conector lado izquierdo: dos boxes en from_cx → un box en to_cx
connect_L <- function(from_y1, from_y2, to_y, from_cx, to_cx) {
  xe <- from_cx + BOX_W / 2   # borde derecho de la columna origen
  xs <- to_cx   - BOX_W / 2   # borde izquierdo de la columna destino
  mx <- (xe + xs) / 2

  segments(xe, from_y1, mx, from_y1, col = COL_LINES, lwd = 0.8)
  segments(xe, from_y2, mx, from_y2, col = COL_LINES, lwd = 0.8)
  segments(mx, from_y1, mx, from_y2, col = COL_LINES, lwd = 0.8)
  segments(mx, to_y,    xs, to_y,    col = COL_LINES, lwd = 0.8)
}

# Conector lado derecho: espejo del anterior
connect_R <- function(from_y1, from_y2, to_y, from_cx, to_cx) {
  xe <- from_cx - BOX_W / 2
  xs <- to_cx   + BOX_W / 2
  mx <- (xe + xs) / 2

  segments(xe, from_y1, mx, from_y1, col = COL_LINES, lwd = 0.8)
  segments(xe, from_y2, mx, from_y2, col = COL_LINES, lwd = 0.8)
  segments(mx, from_y1, mx, from_y2, col = COL_LINES, lwd = 0.8)
  segments(mx, to_y,    xs, to_y,    col = COL_LINES, lwd = 0.8)
}

# ── Cargar resultados del escenario más probable ─────────────────────────────
# Generado por 05_knockout.R sección 9. Corre ese script primero.
bk_path <- "output/tables/bracket_ml.rds"
if (!file.exists(bk_path))
  stop("No se encontró ", bk_path, ". Corre primero source('R/05_knockout.R').")

bk <- readRDS(bk_path)   # lista: $r32 (16), $r16 (8), $qf (4), $sf (2), $final

# ── Banderas ──────────────────────────────────────────────────────────────────
flag_codes <- c(
  "Spain"="es","Argentina"="ar","France"="fr","England"="gb-eng",
  "Portugal"="pt","Colombia"="co","Brazil"="br","Germany"="de",
  "Netherlands"="nl","Mexico"="mx","Ecuador"="ec","Turkey"="tr",
  "Norway"="no","Croatia"="hr","Morocco"="ma","Japan"="jp",
  "Switzerland"="ch","Uruguay"="uy","Belgium"="be","South Korea"="kr",
  "Canada"="ca","Senegal"="sn","Australia"="au","Iran"="ir",
  "United States"="us","Austria"="at","Algeria"="dz","Scotland"="gb-sct",
  "Panama"="pa","Paraguay"="py","Egypt"="eg","Ivory Coast"="ci",
  "Czech Republic"="cz","Uzbekistan"="uz","Sweden"="se","DR Congo"="cd",
  "Jordan"="jo","Iraq"="iq","Tunisia"="tn","New Zealand"="nz",
  "Haiti"="ht","Saudi Arabia"="sa","Cape Verde"="cv","Curaçao"="cw",
  "Bosnia and Herzegovina"="ba","Qatar"="qa","South Africa"="za","Ghana"="gh"
)

load_flag <- function(iso, cache_dir = "output/flags") {
  path <- file.path(cache_dir, paste0(iso, ".png"))
  if (!file.exists(path)) {
    url <- paste0("https://flagcdn.com/20x15/", iso, ".png")
    tryCatch(download.file(url, path, quiet = TRUE, mode = "wb"),
             error = function(e) NULL)
  }
  if (file.exists(path)) tryCatch(png::readPNG(path), error = function(e) NULL)
  else NULL
}

dir.create("output/flags", showWarnings = FALSE)
bracket_teams <- unique(c(sapply(bk$r32, `[[`, "t1"), sapply(bk$r32, `[[`, "t2")))
message("Descargando banderas...")
flag_imgs <- setNames(lapply(bracket_teams, function(nm) {
  iso <- flag_codes[nm]
  if (is.na(iso)) { message("  Sin codigo: ", nm); return(NULL) }
  load_flag(iso)
}), bracket_teams)
n_ok <- sum(!sapply(flag_imgs, is.null))
message(sprintf("  %d/%d banderas cargadas", n_ok, length(flag_imgs)))

# Orden visual → índice en la lista de resultados
# (el bracket FIFA 2026 no es secuencial: ver r16_pares / qf_pares en 05_knockout.R)
#
# Izquierda (SF1 branch):  M97/M98 → M89,M90,M93,M94 → M74,M77,M73,M75,M83,M84,M81,M82
# Derecha   (SF2 branch):  M99/M100 → M91,M92,M95,M96 → M76,M78,M79,M80,M86,M88,M85,M87

r32_idx_L <- c(2L,5L, 1L,3L, 11L,12L, 9L,10L)
r16_idx_L <- c(1L, 2L, 5L, 6L)
qf_idx_L  <- c(1L, 2L)

r32_idx_R <- c(4L,6L, 7L,8L, 14L,16L, 13L,15L)
r16_idx_R <- c(3L, 4L, 7L, 8L)
qf_idx_R  <- c(3L, 4L)

r32_lbl_L <- c("M74","M77","M73","M75","M83","M84","M81","M82")
r16_lbl_L <- c("M89","M90","M93","M94")
r32_lbl_R <- c("M76","M78","M79","M80","M86","M88","M85","M87")
r16_lbl_R <- c("M91","M92","M95","M96")

# Abreviaciones para nombres largos
short_nm <- function(nm, max_len = 14L) {
  nm <- gsub("Trinidad and Tobago", "T&T",           nm, fixed = TRUE)
  nm <- gsub("Korea Republic",      "Korea Rep.",    nm, fixed = TRUE)
  nm <- gsub("United States",       "USA",           nm, fixed = TRUE)
  nm <- gsub("New Zealand",         "New Zealand",   nm, fixed = TRUE)
  nm <- gsub("Saudi Arabia",        "Saudi Arabia",  nm, fixed = TRUE)
  if (nchar(nm) > max_len) nm <- paste0(substr(nm, 1L, max_len - 1L), ".")
  nm
}

# Dibuja un box a partir de un elemento de resultado
box_res <- function(res_list, idx, cx, cy, label = "") {
  m <- res_list[[idx]]
  w <- if (m$winner == m$t1) 1L else 2L
  draw_box(cx, cy,
           label  = label,
           t1     = short_nm(m$t1),
           t2     = short_nm(m$t2),
           win    = w,
           score1 = sprintf("%.1f", m$esp_g1),
           score2 = sprintf("%.1f", m$esp_g2),
           flag1  = flag_imgs[[m$t1]],
           flag2  = flag_imgs[[m$t2]])
}

# ── Abrir PNG ─────────────────────────────────────────────────────────────────
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
png("output/figures/bracket_wc2026.png",
    width = OUT_W, height = OUT_H, res = OUT_RES, bg = COL_BG)
par(mar = c(0, 0, 0, 0), xpd = TRUE)
plot(NA, xlim = c(0, W), ylim = c(H, 0),    # y invertido: 0=arriba
     asp = 1, axes = FALSE, xlab = "", ylab = "")

# ── Título ────────────────────────────────────────────────────────────────────
text(CEN_X, 10, "FIFA World Cup 2026 — Knock-Out Stage Prediction",
     adj = c(0.5, 0.5), cex = 1.05, col = COL_DARK, font = 2L)
text(CEN_X, 32, "Modelo Bayesiano Jerárquico · Escenario más probable",
     adj = c(0.5, 0.5), cex = 0.60, col = "grey25")

# ── Etiquetas de rondas (van ANTES de la línea para fijar su posición) ────────
# adj=(0.5,0): el baseline del texto queda en y, texto sube hacia valores menores
round_lbl <- c("Round of 32", "Round of 16", "Quarterfinals", "Semifinals")
for (i in seq_len(N_ROUNDS)) {
  text(col_x_L[i], TOP_M - 10, round_lbl[i],
       adj = c(0.5, 0), cex = 0.42, col = "grey25", font = 3L)
  text(col_x_R[i], TOP_M - 10, round_lbl[i],
       adj = c(0.5, 0), cex = 0.42, col = "grey25", font = 3L)
}
text(CEN_X, TOP_M - 10, "FINAL",
     adj = c(0.5, 0), cex = 0.42, col = COL_DARK, font = 2L)

# Línea divisoria: justo encima del área del bracket, debajo de las etiquetas
segments(20, TOP_M - 2, W - 20, TOP_M - 2, col = COL_LINES, lwd = 0.6)

# ── LADO IZQUIERDO ────────────────────────────────────────────────────────────

# R32
for (i in seq_len(8L))
  box_res(bk$r32, r32_idx_L[i], col_x_L[1], r32_y[i], label = r32_lbl_L[i])

# Conectores R32→R16
for (i in seq_len(4L))
  connect_L(r32_y[2*i-1], r32_y[2*i], r16_y[i], col_x_L[1], col_x_L[2])

# R16
for (i in seq_len(4L))
  box_res(bk$r16, r16_idx_L[i], col_x_L[2], r16_y[i], label = r16_lbl_L[i])

# Conectores R16→QF
for (i in seq_len(2L))
  connect_L(r16_y[2*i-1], r16_y[2*i], qf_y[i], col_x_L[2], col_x_L[3])

# QF
for (i in seq_len(2L))
  box_res(bk$qf, qf_idx_L[i], col_x_L[3], qf_y[i], label = c("M97","M98")[i])

# Conectores QF→SF
connect_L(qf_y[1], qf_y[2], sf_y, col_x_L[3], col_x_L[4])

# SF izquierda
box_res(bk$sf, 1L, col_x_L[4], sf_y, label = "SF1")

# ── LADO DERECHO ──────────────────────────────────────────────────────────────

# R32
for (i in seq_len(8L))
  box_res(bk$r32, r32_idx_R[i], col_x_R[1], r32_y[i], label = r32_lbl_R[i])

# Conectores R32→R16 (derecha)
for (i in seq_len(4L))
  connect_R(r32_y[2*i-1], r32_y[2*i], r16_y[i], col_x_R[1], col_x_R[2])

# R16
for (i in seq_len(4L))
  box_res(bk$r16, r16_idx_R[i], col_x_R[2], r16_y[i], label = r16_lbl_R[i])

# Conectores R16→QF
for (i in seq_len(2L))
  connect_R(r16_y[2*i-1], r16_y[2*i], qf_y[i], col_x_R[2], col_x_R[3])

# QF
for (i in seq_len(2L))
  box_res(bk$qf, qf_idx_R[i], col_x_R[3], qf_y[i], label = c("M99","M100")[i])

# Conectores QF→SF
connect_R(qf_y[1], qf_y[2], sf_y, col_x_R[3], col_x_R[4])

# SF derecha
box_res(bk$sf, 2L, col_x_R[4], sf_y, label = "SF2")

# ── FINAL (centro) ────────────────────────────────────────────────────────────

# Conectores SF → Final
segments(col_x_L[4] + BOX_W/2, sf_y, CEN_X - BOX_W/2, sf_y,
         col = COL_LINES, lwd = 0.8)
segments(col_x_R[4] - BOX_W/2, sf_y, CEN_X + BOX_W/2, sf_y,
         col = COL_LINES, lwd = 0.8)

# Final
box_res(list(bk$final), 1L, CEN_X, final_y, label = "FINAL")

# ── Partido por el 3er lugar ──────────────────────────────────────────────────
THIRD_Y <- round(final_y + BOX_H + 30L)
text(CEN_X, THIRD_Y - BOX_H/2 - 8,
     "3rd Place", adj = c(0.5, 1), cex = 0.42, col = "grey55", font = 3L)
box_res(list(bk$third), 1L, CEN_X, THIRD_Y)

# ── Cerrar dispositivo ────────────────────────────────────────────────────────
dev.off()
message("✓ output/figures/bracket_wc2026.png  (2400×1256 — LinkedIn HD)")
