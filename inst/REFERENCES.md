# Published Methodological References

- Zhou, H., Lee, J. J., and Yuan, Y. (2017). BOP2: Bayesian optimal design for
  phase II clinical trials with simple and complex endpoints. *Statistics in
  Medicine*, 36(21), 3302-3314. <https://doi.org/10.1002/sim.7338>
- Xu, X., Hashimoto, A., Yimer, B. B., and Takeda, K. (2025). BOP2-FE:
  Bayesian optimal phase II design with futility and efficacy-stopping
  boundaries. *Journal of Biopharmaceutical Statistics*.
  <https://doi.org/10.1080/10543406.2025.2558142>

# External Reference Implementations

The following repositories are useful for understanding published methods and
user-interface expectations. `pob2ef` must implement its own code and must not
copy source code unless its license explicitly permits reuse and the required
attribution is added.

- BOP2FE: <https://github.com/belayb/BOP2FE>. License was not confirmed during
  scaffold creation; treat as a read-only methodological reference.
- BOP2-FE-RShiny: <https://github.com/oncotrialdesign/BOP2-FE-RShiny>. MIT
  license reported by repository metadata; inspect the repository license before
  any reuse.
- GBOP2: <https://github.com/Wanni-Lei/GBOP2>. License was not confirmed during
  scaffold creation; treat as a read-only methodological reference.

Before implementing comparisons, reproduce their published operating
characteristics independently and document the exact version, settings, prior,
analysis schedule, and calibration targets used.