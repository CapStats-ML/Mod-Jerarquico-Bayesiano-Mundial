# Modelo Predictivo Mundial 2026

Modelo bayesiano jerárquico de Poisson para predecir resultados de partidos de la Copa del Mundo FIFA 2026. Basado en el enfoque Dixon-Coles (1997) con inferencia bayesiana completa vía `brms`/Stan.

## Estructura

```
Modelo-Mundial/
├── data/
│   ├── raw/           # datos crudos sin modificar (no versionados)
│   ├── processed/     # datos limpios listos para modelar
│   └── external/      # rankings FIFA, datasets externos (no versionados)
├── R/
│   ├── 01_scraping.R        # extracción de datos ESPN y APIs
│   ├── 02_processing.R      # limpieza, features, ponderación temporal
│   ├── 03_model.R           # especificación y ajuste del modelo bayesiano
│   └── 04_simulation.R      # simulación del torneo y extracción de resultados
├── stan/
│   └── poisson_model.stan   # modelo Stan (si se usa rstan directamente)
├── output/
│   ├── figures/       # gráficos (R base)
│   ├── tables/        # tablas de resultados
│   └── posteriors/    # muestras del posterior (.rds, no versionadas)
└── docs/              # notas metodológicas y referencias
```

## Modelo

Poisson jerárquico con parámetros de ataque y defensa por selección:

```
Goles_local  ~ Poisson(exp(μ + ataque_A - defensa_B + η))
Goles_visita ~ Poisson(exp(μ + ataque_B - defensa_A))

ataque_k, defensa_k ~ Normal(0, σ)   # efectos aleatorios jerárquicos
```

## Fuentes de datos

| Fuente | Método | Contenido |
|--------|--------|-----------|
| ESPN | scraping (`rvest`) | Fixtures, forma reciente, grupos |
| football-data.org | API REST | Historial completo partidos A |
| FIFA Rankings | CSV oficial | Rankings mensuales históricos |

## Paquetes principales

```r
rvest, httr2    # scraping y APIs
tidyverse       # manipulación de datos
brms            # modelo bayesiano jerárquico
tidybayes       # extracción de posteriores
```
