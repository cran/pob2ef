# pob2ef

`pob2ef` implements a partially optimized Bayesian single-arm phase II design
for a binary efficacy endpoint with efficacy and futility stopping. The design
searches candidate response-count boundaries and, subject to prespecified error
constraints, selects the design that minimizes the grey-zone at the final look.
The package creates decision tables and evaluates operating characteristics by
exact trial-path enumeration or Monte Carlo simulation.

## Installation

After CRAN publication, install the released package with:

```r
install.packages("pob2ef")
```

To install the development version from GitHub:

```r
remotes::install_github("clinrfnl/pob2ef")
```

## Statistical framework

Let $\theta$ denote the true response rate, with unacceptable benchmark $p_0$
and clinically desirable target $p_1 > p_0$. At an analysis after $n$
evaluable participants, the design declares:

- futility when $\Pr(\theta > p_0 \mid D_n) < C_F(n)$;
- superiority when $\Pr(\theta > p_1 \mid D_n) > C_E(n)$; or
- continuation at an interim analysis, or a final grey-zone result at the
  maximum sample size, when neither rule is met.

The boundary family is

$$
C_F(n) = \lambda_1(n/N)^\gamma, \qquad
C_E(n) = 1 - \lambda_2(n/N)^\eta.
$$

The tuning parameters satisfy

$$
0 < \lambda_1 \leq 0.5, \qquad
0 < \lambda_2 < 0.5, \qquad
\gamma, \eta \geq 0.
$$

The design is calibrated to constrain superiority probability at $p_0$ by
$\alpha$ and futility probability at $p_1$ by $\beta$. Among feasible boundary
settings, it minimizes the final grey-zone probability at $p_1$, then expected
sample size at $p_0$.

## Related methods

`pob2ef` is a distinct method. Related published methods include BOP2 and
BOP2-FE:

> Zhou, H., Lee, J. J., and Yuan, Y. (2017). BOP2: Bayesian optimal design for
> phase II clinical trials with simple and complex endpoints. *Statistics in
> Medicine*, 36(21), 3302-3314. <https://doi.org/10.1002/sim.7338>

> Xu, X., Hashimoto, A., Yimer, B. B., and Takeda, K. (2025). BOP2-FE:
> Bayesian optimal phase II design with futility and efficacy-stopping
> boundaries. *Journal of Biopharmaceutical Statistics*.
> <https://doi.org/10.1080/10543406.2025.2558142>

## Example

```r
library(pob2ef)

design <- select_design(
  n_max = 20,
  looks = c(10, 20),
  p0 = 0.20,
  p1 = 0.40,
  alpha = 0.10,
  beta = 0.20,
  lambda1_grid = c(0.10, 0.30, 0.50),
  lambda2_grid = c(0.05, 0.10, 0.20),
  gamma_grid = 0:2,
  eta_grid = 0:2
)

design$selected
decision_table(design)
plot_decision_boundaries(design)

simulation <- simulate_design(
  design,
  true_rates = c(0.20, 0.30, 0.40),
  n_sim = 1000,
  seed = 2026
)

simulation$results
plot_operating_characteristics(
  simulation,
  metric = "superiority_probability"
)
```
