library(tidyverse)
library(deSolve)
library(rmutil)
library(hexbin)
library(gridExtra)
library(RColorBrewer)

# global parameters -------------------------------------------------------

SIMS <- 100000
rerun_simulation <- TRUE

# load helper functions ---------------------------------------------------

source("0_functions/simulation_functions.R")

source("0_functions/plot_functions.R")

source("0_functions/alternative_model.R")


# run code ----------------------------------------------------------------

source("2_code/1_load_bbc_data.R")

source("2_code/2_heterogeneity_in_q.R")

source("2_code/3_heterogeneity_in_S.R")

source("2_code/4_lorenz_curves.R")

source("2_code/5_unconstrained_outbreak.R")

source("2_code/6_how_low_to_set_c.R")

source("2_code/7_how_long_to_hold.R")

source("2_code/8_time_dependent_interventions.R")

source("2_code/9_sensitivity_analyses.R")
