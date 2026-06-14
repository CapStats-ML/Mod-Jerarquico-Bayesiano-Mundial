# Predicción FIFA World Cup 2026 — Modelo Bayesiano Jerárquico

[![R](https://img.shields.io/badge/R-4.3%2B-276DC3?logo=r)](https://www.r-project.org/)
[![brms](https://img.shields.io/badge/brms-Negative%20Binomial-blue)](https://paul-buerkner.github.io/brms/)
[![Stan](https://img.shields.io/badge/Stan-MCMC-red)](https://mc-stan.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Modelo estadístico completo para predecir el torneo FIFA World Cup 2026. Implementa un modelo bayesiano jerárquico estilo Dixon-Coles con distribución **Binomial Negativa** para goles, inferencia vía MCMC (Stan/brms) y simulación completa del torneo — fase de grupos hasta la final.

![Bracket predicción FIFA 2026](output/figures/bracket_wc2026.png)

---

## Resultados destacados

| Equipo | Campeón | Final | Semifinal |
|--------|--------:|------:|----------:|
| Spain | 13.5% | 21.5% | 21.5% |
| Argentina | 9.2% | 15.6% | 15.6% |
| France | 8.1% | 14.3% | 14.3% |
| England | 5.6% | 10.3% | 10.3% |
| Portugal | 5.1% | 9.3% | 9.3% |

> Probabilidades basadas en 10 000 simulaciones del torneo completo. Tabla completa en [`output/tables/knockout_probs.csv`](output/tables/knockout_probs.csv).

---

## Modelo

### Especificación

Los goles de cada equipo en cada partido siguen una distribución **Binomial Negativa** para capturar la sobredispersión observada en datos reales:

```
Goles_i ~ NegBin(λ_i, φ)

log(λ_i) = μ + α[equipo_i] − δ[rival_i] + η · es_local + β · elo_diff_i

α[k] ~ Normal(0, σ_α)   # fuerza de ataque (efecto aleatorio)
δ[k] ~ Normal(0, σ_δ)   # solidez defensiva (efecto aleatorio)
φ    ~ Half-Cauchy(0, 1) # parámetro de forma (sobredispersión)
```

### Priors

| Parámetro | Prior | Justificación |
|-----------|-------|---------------|
| μ (intercepto) | Normal(0.3, 0.5) | exp(0.3) ≈ 1.35 goles/partido |
| η (ventaja local) | Normal(0.2, 0.3) | ≈ 22% más goles en casa |
| β (efecto Elo) | Normal(0, 0.3) | escala elo_diff ∈ (−2, 2) |
| σ (sd efectos) | Normal(0, 0.3) | diferencias moderadas entre selecciones |

### MCMC

- 4 cadenas · 5 000 iteraciones · 1 000 warmup
- Inferencia via `brms` + Stan (HMC-NUTS)
- Dos modelos: **A** (todos los equipos históricos) y **B** (solo WC vs WC)

---

## Pipeline

```
01_scraping.R       →  Datos crudos (ESPN, historial internacional)
02_processing.R     →  Limpieza, features, ponderación temporal, Elo
03_model.R          →  Ajuste modelos A y B (brms/Stan)
04_simulation.R     →  Simulación fase de grupos (10 000 escenarios)
05_knockout.R       →  Simulación eliminatorias + escenario más probable
06_live_update.R    →  Actualización en tiempo real con resultados reales
07_bracket_viz.R    →  Bracket visual PNG (R base graphics, 2400×1256px)
```

Ejecutar en orden:

```r
source("R/01_scraping.R")
source("R/02_processing.R")
source("R/03_model.R")       # ~20 min (ajuste MCMC)
source("R/04_simulation.R")  # ~5 min  (10 000 sims)
source("R/05_knockout.R")    # ~5 min  (10 000 sims eliminatorias)
source("R/07_bracket_viz.R") # segundos (genera PNG)
```

---

## Estructura del repositorio

```
├── R/
│   ├── 01_scraping.R
│   ├── 02_processing.R
│   ├── 03_model.R
│   ├── 04_simulation.R
│   ├── 05_knockout.R
│   ├── 06_live_update.R
│   └── 07_bracket_viz.R
├── data/
│   ├── raw/                    # historial completo de partidos internacionales
│   ├── processed/              # datos limpios (Elo, fixtures, equipos)
│   └── live/                   # resultados reales (actualización manual)
├── output/
│   ├── figures/                # gráficos y bracket PNG
│   ├── tables/                 # probabilidades por ronda (CSV)
│   └── posteriors/             # muestras MCMC (.rds, no versionadas)
└── docs/
    └── modelo_mundial_2026.tex # documento metodológico completo
```

---

## Fuentes de datos

| Fuente | Método | Contenido |
|--------|--------|-----------|
| International Football Results | CSV (Kaggle) | 49 000+ partidos desde 1872 |
| ClubElo / eloratings.net | scraping | Ratings Elo históricos por selección |
| ESPN | scraping (`rvest`) | Fixtures y grupos FIFA 2026 |

---

## Paquetes principales

```r
brms, tidybayes   # modelo bayesiano y extracción del posterior
dplyr, readr      # manipulación de datos
rvest, httr2      # scraping
png               # banderas en el bracket visual
```

---

## Referencias

- Dixon, M. & Coles, S. (1997). *Modelling Association Football Scores and Inefficiencies in the Football Betting Market.* Journal of the Royal Statistical Society, Series C.
- Bürkner, P.C. (2017). *brms: An R Package for Bayesian Multilevel Models Using Stan.* Journal of Statistical Software.
- Gelman, A. & Hill, J. (2007). *Data Analysis Using Regression and Multilevel/Hierarchical Models.* Cambridge University Press.

---

*Predicciones generadas con datos hasta mayo 2026. Los modelos no son perfectos y pueden equivocarse — pero la evidencia estadística respalda estas proyecciones.*
