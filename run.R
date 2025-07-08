# script to run pipeline
library(targets)

targets::tar_make()
tar_load_everything()

targets::tar_visnetwork()

source("libraries.R")


# source data processing
#source("R/recruitment_data_processing.R")

# source analyses
#source("R/recruitment_analyses.R")