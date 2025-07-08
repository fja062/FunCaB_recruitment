#download_data
download_plan <- list(
  
  # download data
  # biomass
  tar_target(
    name = removed_biomass_download,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_biomass_2015-2021.csv",
                        path = "data",
                        remote_path = "1_Biomass_removal"),
    format = "file"
  ),
  
  # community composition
  tar_target(
    name = community_download,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_composition_2015-2019.csv",
                        path = "data",
                        remote_path = "3_Plant_composition"),
    format = "file"
  ),

  # community composition
  tar_target(
    name = recruitment_download,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_recruitment_2018-2019.csv",
                        path = "data",
                        remote_path = "4_Seedling_recruitment"),
    format = "file"
  ),
  
  # species corrections
#  tar_target(
#    name = species_corrections_download,
#    command =  get_file(node = "tx9r2",
#                        file = "FUNCAB_species_corrections.csv",
#                        path = "data",
#                        remote_path = "1_Vegetation/Raw_data"),
#    format = "file"
#  ),
  
  # plant functional traits
  tar_target(
    name = traits_download,
    command =  get_file(node = "npfa9",
                        file = "VCG_clean_trait_data_2012-2016.csv",
                        path = "data",
                        remote_path = "5_Trait_data"),
    format = "file"
  ),
  
  # climate data
  tar_target(
    name = gridded_climate_download,
    command =  get_file(node = "npfa9",
                        file = "VCG_clean_gridded_daily_climate_2008-2022.csv",
                        path = "data",
                        remote_path = "8_Environmental_data"),
    format = "file"
  ),
  
  # import data
  # biomass
  tar_target(
    name = removed_biomass_raw,
    command =  read_csv(removed_biomass_download)
  ),
  
  # community composition
  tar_target(
    name = community_raw,
    command =  read_csv(community_download)
  ),

    # recruitment
  tar_target(
    name = recruitment_raw,
    command =  read_csv(recruitment_download)
  ),
  
  # species corrections
#  tar_target(
#    name = species_corrections_raw,
#    command =  read_csv(species_corrections_download)
#  ),
  
  # plant functional traits
  tar_target(
    name = traits_raw,
    command =  read_csv(traits_download)
  ),
  
  # climate data
  tar_target(
    name = gridded_climate_raw,
    command =  read_csv(gridded_climate_download)
  )
)




## load data
#
## load SPEI data
#load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/spei_20092019.RData")
#
##load composition data
#load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/composition_211123.RData")
#
##load biomass data
#load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/communityBiomass_cleaned.RData")
#
##load seedling data
#gs4_deauth() # works for googlesheets where link sharing is ON
#dat <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1eDVV6pAoeVfwaUGzdQuWrmN2_9s7q3EhbYQwbtvnP_M/edit#gid=595311948", #sheet = "FUNCAB_recruitment2019", col_types = "c")
#
## load dictionaries
#source("~/Documents/research/FunCaB/dictionaries.R")