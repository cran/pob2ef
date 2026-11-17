# Public API implementation plan:
#
# select_design(): exact grid search over lambda_1, lambda_2, gamma, and eta;
#   retain candidates satisfying Pr_p0(superiority) <= alpha and
#   Pr_p1(futility) <= beta; select the candidate minimizing the final
#   grey-zone probability at p1.
#
# decision_table(): response-count actions and posterior evidence at each look.
# plot_decision_boundaries(): graphical display of probability and integer
#   response-count boundaries.
#
# simulate_design(): reproducible Monte Carlo operating characteristics.
# plot_operating_characteristics(): figures from simulation or exact results.
#
# Functions are added only when their input validation, exact reference
# calculation, documentation, and tests are implemented together.
NULL