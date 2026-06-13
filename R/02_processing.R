library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# ── 1. Cargar datos ───────────────────────────────────────────────────────────

history  <- read_csv("data/raw/international_matches.csv",  show_col_types = FALSE)
teams    <- read_csv("data/processed/teams.csv",            show_col_types = FALSE)
fixtures <- read_csv("data/processed/wc2026_fixtures.csv",  show_col_types = FALSE)

# ── 2. Normalización de nombres ───────────────────────────────────────────────
# El dataset history usa nombres en inglés como "United States", "Turkey".
# ESPN usa slugs como "united-states", "turkiye".
# Necesitamos un mapa slug → nombre_history para cruzar las dos fuentes.

# Paso A: slug → display name (mecánico)
slug_to_display <- function(slug) {
  slug |>
    str_replace_all("-", " ") |>
    str_to_title()
}

teams <- teams |>
  mutate(display_name = slug_to_display(name))

# Paso B: detectar qué display names NO están en history
history_teams <- sort(unique(c(history$home_team, history$away_team)))

teams_with_match <- teams |>
  mutate(in_history = display_name %in% history_teams)

# Reporte de mismatches
mismatches <- teams_with_match |> filter(!in_history)

if (nrow(mismatches) > 0) {
  message("\n⚠  Equipos sin match en history (requieren mapeo manual):")
  message(paste0("   - ", mismatches$display_name, "  [slug: ", mismatches$name, "]",
                 collapse = "\n"))
} else {
  message("\n✓ Todos los equipos tienen match directo en history")
}

# Paso C: tabla de correcciones manuales
# Completar según los mismatches reportados arriba
name_corrections <- tribble(
  ~display_name,        ~history_name,
  "Czechia",            "Czech Republic",
  "Bosnia Herzegovina", "Bosnia and Herzegovina",
  "Turkiye",            "Turkey",
  "Curacao",            "Curaçao",
  "Congo Dr",           "DR Congo",
  "Ivory Coast",        "Ivory Coast",          # probablemente ya coincide
  "South Korea",        "South Korea",          # probablemente ya coincide
  "Usa",                "United States"         # por si acaso
)

# Paso D: aplicar correcciones y construir mapa final slug → history_name
teams <- teams |>
  left_join(name_corrections, by = "display_name") |>
  mutate(
    history_name = coalesce(history_name, display_name)
  )

# Verificar que ahora todo está en history
still_missing <- teams |>
  filter(!history_name %in% history_teams)

if (nrow(still_missing) > 0) {
  message("\n⚠  Aún sin resolver (ajustar name_corrections):")
  message(paste0("   - display: '", still_missing$display_name,
                 "'  →  history_name candidato: '", still_missing$history_name, "'",
                 collapse = "\n"))

  # Sugerir candidatos por similitud de texto
  message("\n   Candidatos en history (similares):")
  for (nm in still_missing$history_name) {
    candidates <- history_teams[str_detect(history_teams,
                                           regex(word(nm, 1), ignore_case = TRUE))]
    message(sprintf("   '%s'  →  %s", nm,
                    paste(head(candidates, 4), collapse = ", ")))
  }
} else {
  message("\n✓ Normalización completa — todos los equipos resueltos")
}

# Guardar mapa de nombres
write_csv(teams |> select(id, name, display_name, history_name),
          "data/processed/teams_name_map.csv")

# ── 3. Ratings Elo históricos ─────────────────────────────────────────────────
# Se calcula sobre TODOS los partidos desde 1872 para que el rating de cada
# equipo en 2010 ya refleje su historia completa. Guardamos el Elo ANTES de
# cada partido (sin data leakage): es una covariable exógena para el modelo.
#
# K-factor (escala eloratings.net ajustada):
#   60 · gd_mult  — fases finales del Mundial
#   50 · gd_mult  — campeonatos continentales
#   40 · gd_mult  — eliminatorias del Mundial
#   20 · gd_mult  — amistosos
#   35 · gd_mult  — otros competitivos
#
# Multiplicador por diferencia de goles: 1.0 | 1.5 | (11+GD)/8
# Ventaja de local: +100 en el cálculo de probabilidad esperada (sedes neutras: 0)

k_for_tournament <- function(tournament) {
  if (str_detect(tournament, regex("FIFA World Cup", ignore_case = TRUE))) {
    if (str_detect(tournament, regex("qualif|eliminat", ignore_case = TRUE))) return(40)
    return(60)
  }
  if (str_detect(tournament, regex(
      "Copa Americ|UEFA Euro[^p]|African Cup|Africa Cup|Asian Cup|Gold Cup|Nations League|Confederations Cup",
      ignore_case = TRUE))) return(50)
  if (str_detect(tournament, regex("qualif|eliminat", ignore_case = TRUE))) return(40)
  if (str_detect(tournament, regex("friendly", ignore_case = TRUE))) return(20)
  35
}

gd_multiplier <- function(gd) {
  if (gd <= 1) return(1.0)
  if (gd == 2) return(1.5)
  (11 + gd) / 8
}

compute_elo <- function(matches) {
  # matches debe estar ordenado por fecha (ascendente)
  all_teams <- unique(c(matches$home_team, matches$away_team))
  elo_vec   <- setNames(rep(1500.0, length(all_teams)), all_teams)
  team_idx  <- setNames(seq_along(all_teams), all_teams)

  n            <- nrow(matches)
  elo_home_pre <- numeric(n)
  elo_away_pre <- numeric(n)

  # Extraer vectores para evitar overhead de acceso a data frame en el loop
  home_v    <- matches$home_team
  away_v    <- matches$away_team
  gh_v      <- matches$home_score
  ga_v      <- matches$away_score
  neutral_v <- matches$neutral
  tourney_v <- matches$tournament

  for (i in seq_len(n)) {
    hi <- team_idx[[ home_v[[i]] ]]
    ai <- team_idx[[ away_v[[i]] ]]

    ra <- elo_vec[[hi]]
    rb <- elo_vec[[ai]]

    elo_home_pre[[i]] <- ra
    elo_away_pre[[i]] <- rb

    gh <- gh_v[[i]]
    ga <- ga_v[[i]]
    if (is.na(gh) || is.na(ga)) next

    # Ventaja de campo: +100 Elo para local en sede no neutral
    ha <- if (isTRUE(neutral_v[[i]])) 0 else 100
    ea <- 1 / (1 + 10^((rb - ra - ha) / 400))

    sa <- if (gh > ga) 1.0 else if (gh < ga) 0.0 else 0.5
    k  <- k_for_tournament(tourney_v[[i]]) * gd_multiplier(abs(gh - ga))

    elo_vec[[hi]] <- ra + k * (sa - ea)
    elo_vec[[ai]] <- rb + k * (ea - sa)   # = rb + k*(sb - eb)
  }

  list(
    matches_elo = dplyr::mutate(matches, elo_home = elo_home_pre, elo_away = elo_away_pre),
    final_elo   = elo_vec
  )
}

message("Calculando Elo histórico (todos los partidos)... ", appendLF = FALSE)
elo_result       <- compute_elo(arrange(history, date))
history_with_elo <- elo_result$matches_elo
final_elo        <- elo_result$final_elo
message(sprintf("OK  (rango Elo: %.0f – %.0f)",
                min(c(history_with_elo$elo_home, history_with_elo$elo_away)),
                max(c(history_with_elo$elo_home, history_with_elo$elo_away))))

# Guardar ratings actuales para usarlos en la simulación
tibble(team = names(final_elo), elo = unname(final_elo)) |>
  arrange(desc(elo)) |>
  write_csv("data/processed/elo_ratings.csv")
message("✓ Guardado: data/processed/elo_ratings.csv")

# ── 4. Ponderación temporal (Dixon-Coles) ────────────────────────────────────
# Partidos recientes pesan exponencialmente más
# half_life = 365 días → un partido de hace 1 año pesa 0.5 vs uno de hoy

decay_weight <- function(date, ref_date = Sys.Date(), half_life_days = 365) {
  delta <- as.numeric(ref_date - as.Date(date))
  exp(-log(2) * delta / half_life_days)
}

# ── 5. Construir dataset en formato largo para el modelo ─────────────────────
#
# Criterios de filtrado:
#   - Partidos desde 2010 (cubre 4 Mundiales como historia de entrenamiento)
#   - Al menos uno de los dos equipos debe ser del Mundial 2026
#   - Se excluyen partidos con NA en goles (partidos sin resultado)
#
# Pesos compuestos:
#   - Decaimiento temporal (half-life 365 días)
#   - Multiplicador por tipo de torneo: WC > eliminatorias > amistosos
#
# Formato largo: una fila por equipo-partido
#   partido_id | fecha | equipo | rival | goles | es_local | neutral | peso | elo_diff

wc_names <- teams$history_name  # nombres de los 48 equipos en formato history

torneo_multiplier <- function(torneo) {
  case_when(
    str_detect(torneo, regex("FIFA World Cup", ignore_case = TRUE)) ~ 2.0,
    str_detect(torneo, regex("qualifier|qualification|eliminat", ignore_case = TRUE)) ~ 1.5,
    str_detect(torneo, regex("friendly", ignore_case = TRUE)) ~ 0.5,
    TRUE ~ 1.0
  )
}

history_filtered <- history_with_elo |>
  filter(
    year(date) >= 2010,
    home_team %in% wc_names | away_team %in% wc_names,
    !is.na(home_score), !is.na(away_score)
  ) |>
  mutate(
    partido_id   = row_number(),
    peso_tiempo  = decay_weight(date),
    peso_torneo  = torneo_multiplier(tournament),
    peso         = peso_tiempo * peso_torneo
  )

message(sprintf("\nPartidos filtrados (2010+, al menos 1 equipo WC): %d", nrow(history_filtered)))

# Perspectiva del equipo local
long_home <- history_filtered |>
  transmute(
    partido_id,
    fecha      = date,
    torneo     = tournament,
    equipo     = home_team,
    rival      = away_team,
    goles      = home_score,
    es_local   = if_else(neutral, 0L, 1L),
    neutral,
    peso,
    elo_equipo = elo_home,
    elo_rival  = elo_away
  )

# Perspectiva del equipo visitante
long_away <- history_filtered |>
  transmute(
    partido_id,
    fecha      = date,
    torneo     = tournament,
    equipo     = away_team,
    rival      = home_team,
    goles      = away_score,
    es_local   = 0L,
    neutral,
    peso,
    elo_equipo = elo_away,
    elo_rival  = elo_home
  )

long_df <- bind_rows(long_home, long_away) |>
  # elo_diff > 0: el equipo es más fuerte que su rival
  # Se escala por 400 para que el coeficiente sea interpretable (~Normal(0,1))
  mutate(elo_diff = (elo_equipo - elo_rival) / 400) |>
  # Añadir ESPN ID donde el equipo es del Mundial 2026
  left_join(teams |> select(history_name, equipo_id = id), by = c("equipo" = "history_name")) |>
  left_join(teams |> select(history_name, rival_id  = id), by = c("rival"  = "history_name")) |>
  arrange(fecha, partido_id)

message(sprintf("Filas en formato largo:              %d", nrow(long_df)))
message(sprintf("Equipos únicos (equipo):             %d", n_distinct(long_df$equipo)))
message(sprintf("Equipos WC con ID asignado:          %d", n_distinct(long_df$equipo_id[!is.na(long_df$equipo_id)])))
message(sprintf("Rango de pesos:    [%.4f, %.4f]", min(long_df$peso), max(long_df$peso)))
message(sprintf("Rango elo_diff:    [%.2f,  %.2f]", min(long_df$elo_diff), max(long_df$elo_diff)))

write_csv(long_df, "data/processed/matches_model.csv")
message("\n✓ Guardado: data/processed/matches_model.csv")
