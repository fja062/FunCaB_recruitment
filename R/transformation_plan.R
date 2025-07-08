# data curation plan
transformation_plan <- list(
  
  # mean climate
  tar_target(
    name = gridded_climate,
    command = {
      dat <- gridded_climate_raw |>
        filter(variable %in% c("temperature", "precipitation"),
               year(date) %in% c(2008:2019)) |>
        pivot_wider(names_from = variable, values_from = value) |>
        mutate(year = year(date))
      
      left_join(dat |>
                  group_by(year, siteID) |>
                  summarise(precipitation = sum(precipitation, na.rm = TRUE)) |>
                  ungroup() |>
                  group_by(siteID) |>
                  summarise(precipitation = mean(precipitation, na.rm = TRUE)),
                dat |>
                  mutate(month = month(date)) |>
                  filter(month %in% c(6, 7, 8)) |>
                  group_by(year, month, siteID) |>
                  summarise(temperature = mean(temperature)) |>
                  ungroup() |>
                  group_by(siteID) |>
                  summarise(temperature = mean(temperature)),
                by = "siteID")
      
    }
  ),
  
  # prep biomass
  tar_target(
    name = biomass,
    command = removed_biomass_raw |>
      # Clean 2018 Ovstedalen duplicates
      clean_ovstedalen_2018_duplicates() |>
      mutate(plotID = if_else(
        treatment == "GF" & str_detect(plotID, "FG"),
        str_replace(plotID, "FG", "GF"),
        plotID)) %>%
      # fix blockID
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE)
  ),
  

  # standing biomass for 2016 controls
  ### PROBLEM WITH THIS DATA, ONE DUPLICATE???
  tar_target(
    name = standing_biomass,
    command = biomass |>
      # control plots in 2016
      filter(fg_removed == "XC") |>
      rename(standing_biomass = biomass)
  ),
  
  
  # make community data, impute missing cover values, construct FG cover coefficients
  tar_target(
    name = community,
    command = fg_cleaning(community_raw, gridded_climate
                          #, species_corrections_raw
                          ) %>%
      make_fg_cover_coefficients()
  ),
  
  # clean recruitment data
  tar_target(
    name = recruitment,
    command = recruitment_raw %>%
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE)
  ),
  
  # prep cover
  tar_target(
    name = cover_data,
    command = community |>
      # remove extra plots in 2016
      filter(fg_removed != "XC") |>
      select(year:fg_removed, species, cover, functional_group, temperature_level:fg_remaining)
  ),
  
  tar_target(
    name = fg_cover,
    command = community |>
      # remove extra plots in 2016
      filter(fg_removed != "XC") |>
      select(year:fg_removed, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes) |>
      tidylog::distinct() |>
      #remove duplicates
      dplyr::mutate(n = dplyr::n(), .by = c(year, siteID, blockID, plotID, removal, fg_removed)) |>
      tidylog::filter(!c(n == 2 & is.na(total_graminoids))) |>
      # remove last duplicate
      filter(!c(year == 2019 & plotID == "Alr3C" & total_bryophytes == 2))
    
  ),
  
  #merge biomass with community
  tar_target(
    name = remaining_biomass_merged,
    command = merge_community_biomass(community, standing_biomass)
  ),
  

  # make biomass coefficients
  tar_target(
    name = biomass_coefficients,
    command = {
      # get biomass coefficients
      base <- make_biomass_coefficients(remaining_biomass_merged)
      # get biomass in 2015
      biomass_2015 <- base |>
        filter(year == 2015) |>
        select(plotID, biomass_2015 = standing_biomass_calculated)
      # join 2015 biomass back to base
      base |>
        tidylog::left_join(biomass_2015, by = "plotID") |>
        mutate(delta_biomass = standing_biomass_calculated - biomass_2015)
    }
  ),
  
  
  tar_target(
    name = traits,
    command = traits_raw |>
      select(-date, -flag) |>
      
      # log transform size traits
      mutate(
        value_trans = if_else(
          trait %in% c(
            "height",
            "fresh_mass",
            "dry_mass",
            "leaf_area",
            "leaf_thickness"
          ),
          true = suppressWarnings(log(value)),# suppress warnings from log(-value) in isotopes (these are calculated but not kept)
          false = value
        ),
        trait_trans = recode(
          trait,
          "height" = "height_log",
          "fresh_mass" = "fresh_mass_log",
          "dry_mass" = "dry_mass_log",
          "leaf_area" = "leaf_area_log",
          "leaf_thickness" = "leaf_thickness_log"
        ),
        trait_trans = factor(trait_trans,
                             levels = c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log", "SLA", "LDMC", "C", "N", "CN_ratio", "d13C", "d15N"))) %>%
      make_fancy_data(., gridded_climate, fix_treatment = FALSE)
    
  ),
  
  # bootstrapping
  # trait imputation
  tar_target(
    name = imputed_traits,
    command = make_trait_impute(cover_data, traits)
  ),
  
  # bootstrapping for CWM
  tar_target(
    name = trait_means,
    command = make_bootstrapping(imputed_traits)
  )#,
  
  # join response and explanatory variables for analysis
#  tar_target(
#    name = analysis_data,
#    command = biomass_coefficients |>
#      
#      # join with removed biomass
#      # 147 biomass_coefficients plots do not join, because removed_biomass has no controls
#      tidylog::left_join(removed_biomass, by = join_by(siteID, plotID, fg_removed)) |>
#      # add missing blockID for control plots
#      mutate(blockID = if_else(is.na(blockID), str_sub(plotID, 1, 4), blockID)) |>
#      # rename to cumulative removed biomass
#      rename(cum_removed_biomass = removed_biomass) %>%
#      make_fancy_data(., gridded_climate, fix_treatment = FALSE)
    
    # join with diversity
    # tidylog::left_join(diversity |>
    #   ungroup() |>
    #   filter(year == 2019) |>
    #   select(-year, -removal, -c(temperature_level:temperature)),
    #     by = join_by(siteID, plotID, blockID, fg_removed, fg_remaining, functional_group))
    
#  ),
  
  # join response and explanatory variables for trait analysis
#  tar_target(
#    name = analysis_traits,
#    command = biomass_coefficients |>
#      # filter for only 2019 data
#      filter(year == 2019) |>
#      
#      # join with removed biomass
#      # 147 biomass_coefficients plots do not join, because removed_biomass has no controls
#      # 5 plots from removed_biomass do no join, because they are missing in biomass_coefficients
#      tidylog::left_join(removed_biomass, by = join_by(siteID, plotID, fg_removed)) |>
#      # add missing blockID for control plots
#      mutate(blockID = if_else(is.na(blockID), str_sub(plotID, 1, 4), blockID)) |>
#      
#      # join with traits
#      # WHY DO 720 NOT JOIN???!!!
#      tidylog::left_join(trait_means)
#    
#  )
  
)

