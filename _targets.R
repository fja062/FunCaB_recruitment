### TARGETS PIPELINE

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "dataDownloader", "vegan", "ggvegan", "Hmisc", "glue", "traitstrap", "dataDocumentation", "lme4", "broom", "broom.mixed", "lmerTest", "tidymodels", "sjPlot", "patchwork"), # packages that your targets need to run
  #format = "rds" # default storage format
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  download_plan,
  transformation_plan,
  analysis_plan,
  figure_plan#,
  #manuscript_plan
)
