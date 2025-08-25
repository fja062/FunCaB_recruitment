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
  
  # clean funcab recruitment data
  tar_target(
    name = funcab_recruitment,
    command = 
      # standardise dataset
      funcab_recruitment_raw |> 
      prepare_funcab_recruitment() %>%
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE) %>% 
      clean_funcab_recruitment(., community) |>
      # select controls and bare plots
      filter(fg_removed %in% c("FGB", "C")) |> 
      #create treatment variable
      mutate(fg_removed = if_else(fg_removed == "FGB", "Gap", "Intact")) |> 
      select(-comment, -functional_group) |> 
      # prep for merge with seedclim data
      mutate(plotID = if_else(!is.na(turfID), paste(blockID,"RTC", sep = ""), plotID))
  ),

    # clean seedclim recruitment data
  tar_target(
    name = seedclim_recruitment,
    command = 
      # standardise dataset
      seedclim_recruitment_raw |> 
      clean_seedclim_recruitment() %>%
      #funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE) |> 
      rename(seedID = ID)
  ),

      # join funcab and seedclim recruitment data
  tar_target(
    name = combined_recruitment,
    command = 
      # standardise dataset
      seedclim_recruitment |> 
      tidylog::full_join(funcab_recruitment) |> 
      group_by(siteID, blockID, plotID, season, year) |> 
      mutate(count = sum(count)) |> 
      ungroup() |> 
      left_join(spei_raw |>  select(-season, -date), by = join_by(siteID, year, month))
      #funcabization(., convert_to = "Funder") %>%
      #make_fancy_data(., gridded_climate, fix_treatment = TRUE)
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
    
  )
  

  
)

