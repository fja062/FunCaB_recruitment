# load data

# load SPEI data
load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/spei_20092019.RData")

#load seedling data
load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/composition_211123.RData")


#load biomass data
load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/communityBiomass_cleaned.RData")

#load community data
gs4_deauth() # works for googlesheets where link sharing is ON
dat <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1eDVV6pAoeVfwaUGzdQuWrmN2_9s7q3EhbYQwbtvnP_M/edit#gid=595311948", sheet = "FUNCAB_recruitment2019", col_types = "c")

# load dictionaries
source("~/Documents/research/FunCaB/dictionaries.R")