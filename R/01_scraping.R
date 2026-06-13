library(rvest)
library(httr2)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

# ── 1. Historial completo de partidos internacionales ─────────────────────────
# Fuente: github.com/martj42/international_results (actualizado continuamente)
# ~47,000 partidos desde 1872, columnas: date, home_team, away_team,
# home_score, away_score, tournament, city, country, neutral

get_international_history <- function(dest = "data/raw/international_matches.csv") {
  url <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"

  message("[ 1/3 ] Descargando historial de partidos internacionales... ", appendLF = FALSE)
  req     <- request(url) |> req_perform()
  raw     <- resp_body_string(req)
  matches <- read_csv(I(raw), show_col_types = FALSE)

  write_csv(matches, dest)
  message(sprintf("OK  (%d partidos, %s – %s)", nrow(matches), min(matches$date), max(matches$date)))
  invisible(matches)
}

# ── 2. Equipos del Mundial 2026 (data mining ESPN) ───────────────────────────

get_world_cup_teams <- function() {
  message("[ 2/3 ] Obteniendo equipos del Mundial 2026... ", appendLF = FALSE)
  url  <- "https://www.espn.com/soccer/standings/_/league/FIFA.WORLD"
  page <- read_html(url)

  hrefs <- page |>
    html_elements("a[href*='/soccer/team/_/id/']") |>
    html_attr("href") |>
    unique()

  tibble(href = hrefs) |>
    mutate(
      id   = str_extract(href, "(?<=/id/)\\d+"),
      name = str_extract(href, "(?<=/id/\\d{1,6}/).*")
    ) |>
    select(id, name) |>
    filter(!is.na(id)) |>
    (\(df) { message(sprintf("OK  (%d equipos)", nrow(df))); df })()
}

# ── 3. Resultados recientes por equipo (ESPN .co) ─────────────────────────────

parse_team_results <- function(page) {
  # ESPN estructura los resultados en filas <tr> dentro de tablas con clase Table
  rows <- page |> html_elements("tr.Table__TR")
  if (length(rows) == 0) return(NULL)

  map_dfr(rows, function(row) {
    cells <- row |> html_elements("td")
    if (length(cells) < 3) return(NULL)

    date_text  <- cells[[1]] |> html_text2() |> str_squish()
    match_text <- cells[[2]] |> html_text2() |> str_squish()
    result     <- cells[[3]] |> html_text2() |> str_squish()
    comp       <- if (length(cells) >= 4) cells[[4]] |> html_text2() |> str_squish() else NA_character_

    tibble(fecha = date_text, partido = match_text, resultado = result, competencia = comp)
  })
}

get_team_results <- function(id, name, delay_ms = 500) {
  url  <- paste0("https://www.espn.com.co/futbol/equipo/resultados/_/id/", id, "/", name)
  page <- tryCatch(read_html(url), error = function(e) {
    message("Error en equipo ", name, ": ", conditionMessage(e))
    NULL
  })

  if (is.null(page)) return(NULL)
  Sys.sleep(delay_ms / 1000)  # cortesía con el servidor

  df <- parse_team_results(page)
  if (!is.null(df)) df$team_id <- id
  df
}

get_all_recent_form <- function(teams, dest = "data/processed/recent_form.csv") {
  n <- nrow(teams)
  message(sprintf("\n[ 3/3 ] Forma reciente: %d equipos (~%d seg estimados)\n", n, n / 2))

  pb      <- txtProgressBar(min = 0, max = n, style = 3, width = 50)
  results <- vector("list", n)

  for (i in seq_len(n)) {
    results[[i]] <- get_team_results(teams$id[i], teams$name[i])
    setTxtProgressBar(pb, i)
  }
  close(pb)

  combined <- bind_rows(results)
  write_csv(combined, dest)
  message(sprintf("\nGuardado: %s (%d filas)", dest, nrow(combined)))
  invisible(combined)
}

# ── 4. Fixtures del Mundial 2026 (ESPN) ───────────────────────────────────────
# Los match links tienen texto " v " o "FT" — no los nombres de equipos.
# Estrategia: extraer el slug del href (ej: "paraguay-united-states") y
# partirlo en dos slugs conocidos usando la tabla de equipos.
# Convención ESPN en URL: away-home (se etiquetan equipo1/equipo2).

get_wc_fixtures <- function(teams_df, dest = "data/processed/wc2026_fixtures.csv") {
  url  <- "https://www.espn.com/soccer/schedule/_/league/FIFA.WORLD"
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) { message("Error obteniendo fixtures"); return(NULL) }

  # Slugs ordenados de mayor a menor longitud para evitar conflictos de prefijo
  # Ej: "south-korea" debe intentarse antes que "south"
  slugs_ord  <- teams_df$name[order(nchar(teams_df$name), decreasing = TRUE)]
  slug_to_id <- setNames(teams_df$id, teams_df$name)

  # Parte "slugA-slugB" en dos slugs conocidos
  split_teams <- function(combined) {
    for (s in slugs_ord) {
      if (startsWith(combined, paste0(s, "-"))) {
        rest <- substring(combined, nchar(s) + 2)
        if (rest %in% teams_df$name) return(c(s, rest))
      }
    }
    c(NA_character_, NA_character_)
  }

  hrefs <- page |>
    html_elements("a[href*='/soccer/match/_/gameId/']") |>
    html_attr("href") |>
    unique()

  rows <- tibble(href = hrefs) |>
    mutate(
      game_id = str_extract(href, "(?<=gameId/)\\d+"),
      slug    = sub(".*gameId/\\d+/", "", href)
    ) |>
    distinct(game_id, .keep_all = TRUE) |>
    filter(!is.na(game_id))

  splits <- t(vapply(rows$slug, split_teams, character(2)))
  rows$equipo1_slug <- splits[, 1]
  rows$equipo2_slug <- splits[, 2]

  fixtures <- rows |>
    mutate(
      equipo1_id = slug_to_id[equipo1_slug],
      equipo2_id = slug_to_id[equipo2_slug]
    ) |>
    filter(!is.na(equipo1_id), !is.na(equipo2_id)) |>
    select(game_id, equipo1_slug, equipo1_id, equipo2_slug, equipo2_id)

  write_csv(fixtures, dest)
  message(sprintf("OK  (%d partidos)", nrow(fixtures)))
  invisible(fixtures)
}

# ── Ejecución ─────────────────────────────────────────────────────────────────
# Corre al hacer source("R/01_scraping.R") o Rscript R/01_scraping.R

message("\n=== Pipeline de datos: Mundial 2026 ===\n")

history <- get_international_history()

teams <- get_world_cup_teams()
write_csv(teams, "data/processed/teams.csv")

recent_form <- get_all_recent_form(teams)

message("\n[ + ] Fixtures del Mundial 2026... ", appendLF = FALSE)
fixtures <- get_wc_fixtures(teams)
if (!is.null(fixtures)) {
  cat("\n"); print(fixtures)
}

message("\n=== Completado ===\n")
