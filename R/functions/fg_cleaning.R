fg_cleaning <- function(community_raw, gridded_climate
                        #, species_corrections_raw
                        ) {
  transition_community <- community_raw %>%
    # cleaning blockIDs
    mutate(blockID = case_when(
      plotID == "Fau1XC" ~ "Fau1",
      plotID == "Fau4XC" ~ "Fau4",
      plotID == "Fau3XC" ~ "Fau5",
      plotID == "Gud1XC" ~ "Gud12",
      plotID == "Gud4XC" ~ "Gud15",
      plotID == "Ves4C" ~ "Ves4",
      TRUE ~ blockID
    )) |>
    # make species and cover corrections where necessary
#    left_join(species_corrections_raw |>
#                filter(!is.na(turfID)) |>
#                rename(species = old, plotID = turfID) |>
#                mutate(year = as.numeric(year)) |>
#                select(-functional_group, -cover),
#              by = c("year", "siteID", "plotID", "species")) |>
    # remove single occurence species
    filter(!species %in% c("Dan.dec", "Ver.ver")) |>
    # fix missing total_graminoids and total_forbs
    mutate(
      total_graminoids = case_when(
        is.na(total_graminoids) & plotID == "Fau1C" & species == "Hol.lan" ~ 70,
        is.na(total_graminoids) & plotID == "Fau2C" & species == "Hol.lan" ~ 60,
        is.na(total_graminoids) & plotID == "Ram5C" & species == "Fes.viv" ~ 50,
        is.na(total_graminoids) & plotID == "Vik5C" & species == "Hol.lan" ~ 45,
        TRUE ~ total_graminoids
      ),
      total_forbs = case_when(
        is.na(total_forbs) & plotID == "Fau1C" & species == "Hol.lan" ~ 70,
        is.na(total_forbs) & plotID == "Fau2C" & species == "Hol.lan" ~ 25,
        is.na(total_forbs) & plotID == "Ram5C" & species == "Fes.viv" ~ 40,
        is.na(total_forbs) & plotID == "Vik5C" & species == "Hol.lan" ~ 60,
        TRUE ~ total_forbs
      )
    ) |>
    # imputing 0s in moss height where moss cover is low
    mutate(moss_height = case_when(
      (is.na(moss_height) & total_bryophytes <= 5) ~ 0,
      TRUE ~ moss_height
    )) |>
    # create sum of covers
    group_by(year, plotID, functional_group) |>
    mutate(sum_cover = sum(cover)) |>
    # keep only the TTC controls in Alrust
    filter(!(plotID == "Alr3C" & is.na(turfID))) %>%
    ungroup() %>%
    # fix plotIDs
    funcabization(., convert_to = "Funder") %>%
    # make data fancy
    make_fancy_data(., gridded_climate, fix_treatment = TRUE) %>%
    select(-sumcover) |>
    ungroup()
  
  impute_ctrls <- transition_community |>
    left_join(
      transition_community |>
        select(year:fg_removed, fg_remaining, vegetation_height, moss_height,
               total_graminoids, total_forbs, total_bryophytes) |>
        filter(fg_removed == "C") |>
        distinct() |>
        group_by(plotID) |>
        mutate(moss_height2 = case_when(
          (is.na(moss_height)) ~ mean(moss_height, na.rm = TRUE),
          TRUE ~ moss_height
        ))
    ) |>
    mutate(moss_height = if_else((is.na(moss_height) & fg_removed == "C"), moss_height2, moss_height)) |>
    select(-moss_height2) |>
    ungroup()
  
  impute_trts <- impute_ctrls |>
    left_join(impute_ctrls |>
                select(year:fg_removed, fg_remaining, vegetation_height, moss_height,
                       total_graminoids, total_forbs, total_bryophytes) |>
                tidylog::distinct() |>
                # filter(fg_removed %in% c("C", "XC")) |>
                group_by(siteID, year) |>
                mutate(
                  site_moss_height = mean(moss_height, na.rm = TRUE),
                  moss_height2 = if_else(is.na(moss_height), site_moss_height, moss_height)
                )) |>
    mutate(
      moss_height = if_else((is.na(moss_height)), moss_height2, moss_height),
      imputed_height = if_else(moss_height == site_moss_height, TRUE, FALSE)
    ) |>
    select(-moss_height2, -site_moss_height) |>
    ungroup()
  
  impute_trts
}
