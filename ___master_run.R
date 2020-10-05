library(tidyverse)
library(deSolve)
library(rmutil)


# global parameters -------------------------------------------------------

SIMS <- 100000
rerun_simulation <- TRUE

# load helper functions ---------------------------------------------------

source("0_functions/simulation_functions.R")

source("0_functions/plot_functions.R")


# run code ----------------------------------------------------------------

source("2_code/1_theory.R")

source("2_code/2_heterogeneity_in_q.R")

source("2_code/3_sampling.R")

source("2_code/4_heterogeneous_mixing.R")

source("2_code/5_load_bbc_data.R")

source("2_code/6_full_simulation.R")


