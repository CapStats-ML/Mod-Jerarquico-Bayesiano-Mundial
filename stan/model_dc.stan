// Modelo Bayesiano Jerárquico con corrección Dixon-Coles
//
// Probabilidad conjunta de un partido (g1, g2):
//   P(g1, g2) = Poisson(g1; λ1) × Poisson(g2; λ2) × τ(g1, g2, λ1, λ2, ρ)
//
// τ solo difiere de 1 para marcadores con g1+g2 ≤ 1:
//   τ(0,0) = 1 − λ1·λ2·ρ
//   τ(1,0) = 1 + λ2·ρ
//   τ(0,1) = 1 + λ1·ρ
//   τ(1,1) = 1 − ρ
//
// ρ < 0 aumenta la frecuencia de empates (0-0, 1-1) y reduce la de
// victorias ajustadas (1-0, 0-1), corrigiendo el sesgo de independencia.
//
// Parametrización no centrada para efectos aleatorios (mejor mixing MCMC).

data {
  int<lower=1> N;                            // número de partidos
  int<lower=1> T;                            // número de equipos
  array[N] int<lower=0>          g1;         // goles equipo 1
  array[N] int<lower=0>          g2;         // goles equipo 2
  array[N] int<lower=1, upper=T> team1_idx;  // índice equipo 1
  array[N] int<lower=1, upper=T> team2_idx;  // índice equipo 2
  vector[N]                      elo_diff;   // (elo1 − elo2) / 400
  vector[N]                      es_local1;  // 1 si equipo 1 es local
  vector<lower=0>[N]             peso;       // peso temporal × tipo torneo
}

parameters {
  real alpha;                               // intercepto (log-escala)
  real b_elo;                               // coef. diferencia de Elo
  real b_local;                             // ventaja de local
  real<lower=-0.5, upper=0.3> rho;          // corrección Dixon-Coles

  vector[T] atk_raw;                        // efectos de ataque (std. normal)
  vector[T] def_raw;                        // efectos de defensa (std. normal)
  real<lower=0> sigma_atk;
  real<lower=0> sigma_def;
}

transformed parameters {
  vector[T] atk = sigma_atk * atk_raw;
  vector[T] def = sigma_def * def_raw;
}

model {
  // Priors — misma escala que el Modelo B para comparabilidad
  alpha     ~ normal(0.3, 0.5);
  b_elo     ~ normal(0.0, 0.3);
  b_local   ~ normal(0.2, 0.3);
  rho       ~ normal(0.0, 0.1);   // típicamente −0.13 a 0 en fútbol internacional
  sigma_atk ~ exponential(5);
  sigma_def ~ exponential(5);
  atk_raw   ~ std_normal();
  def_raw   ~ std_normal();

  // Verosimilitud con corrección Dixon-Coles
  for (n in 1:N) {
    real lam1 = exp(alpha
                    + atk[team1_idx[n]] - def[team2_idx[n]]
                    + b_elo   * elo_diff[n]
                    + b_local * es_local1[n]);
    real lam2 = exp(alpha
                    + atk[team2_idx[n]] - def[team1_idx[n]]
                    + b_elo   * (-elo_diff[n]));

    real log_tau;
    if      (g1[n] == 0 && g2[n] == 0) log_tau = log(1.0 - lam1 * lam2 * rho);
    else if (g1[n] == 1 && g2[n] == 0) log_tau = log(1.0 + lam2 * rho);
    else if (g1[n] == 0 && g2[n] == 1) log_tau = log(1.0 + lam1 * rho);
    else if (g1[n] == 1 && g2[n] == 1) log_tau = log1m(rho);
    else                                log_tau = 0.0;

    target += peso[n] * (poisson_lpmf(g1[n] | lam1)
                       + poisson_lpmf(g2[n] | lam2)
                       + log_tau);
  }
}
