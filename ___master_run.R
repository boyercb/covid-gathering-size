source('2_code/0_packages.R')

# global parameters -------------------------------------------------------

# probably don't need.
SIMS <- 100000
rerun_simulation <- TRUE

# load helper functions ---------------------------------------------------

# figure out if we need any of these
source("0_functions/simulation_functions.R")

source("0_functions/plot_functions.R")

source("0_functions/alternative_model.R")


# run code ----------------------------------------------------------------

source("2_code/1_theory.R")

source("2_code/2_empirical_distributions.R")

source("2_code/3_empirical_restrictions.R")

