# biomass
# FG cover in XC plots from 2016
merge_community_biomass <- function(community, standing_biomass){
  
  ### 9 plots from biomass do not match, because there are no corresponding XC plots in the community data
  ### Fauske has only blocks 2 and 3 and is missing 1
  ### Gudmedal has only blocks 1 and 3 and is missing, 2 and 4
  standing_biomass_merged <- community |>
    select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes) |>
    
    # remove species level data and select for plot level
    tidylog::distinct() |>
    
    #remove duplicates
    dplyr::mutate(n = dplyr::n(), .by = c(year, siteID, blockID, plotID, removal, fg_removed)) |>
    tidylog::filter(!c(n == 2 & is.na(total_graminoids))) |>
    tidylog::filter(!c(plotID == "Alr3C" & year == 2016 & vegetation_height > 80)) |>
    tidylog::filter(!c(plotID == "Alr3C" & year == 2017 & vegetation_height > 83)) |>
    
    # remove last duplicate
    mutate(height_forbs = vegetation_height) |>
    rename(height_graminoids = vegetation_height) |>
    pivot_longer(c(height_graminoids, height_forbs, total_graminoids, total_bryophytes, total_forbs), names_to = "functional_group", values_to = "cover") |>
    separate_wider_delim(functional_group, delim = "_", names = c("trait", "functional_group")) |>
    pivot_wider(names_from = trait, values_from = cover) |>
    select(-blockID) |>
    tidylog::left_join(
      standing_biomass |>
        select(siteID, plotID, functional_group, standing_biomass) |> #blockID,
        filter(functional_group %in% c("graminoids", "forb" , "bryophytes")) |>
        mutate(functional_group = case_when(
          plotID == "Hog1XC" & standing_biomass == 29.44 ~ "graminoids",
          TRUE ~ functional_group
        )) |>
        mutate(standing_biomass = standing_biomass/0.0625,    # recalculate to g/m2
               functional_group = case_when(
                 functional_group == "forb" ~ "forbs",
                 TRUE ~ functional_group
               ))
    ) |>
    group_by(plotID, functional_group) |>
    mutate(cover_height = case_when(
      functional_group == "bryophytes" ~ total * moss_height,
      functional_group == "graminoids" ~  total * height,
      functional_group == "forbs" ~  total * height
    )
    ) |>
    ungroup()
  
}

######## biomass regressions
#standing_biomass_test |>
#  filter(functional_group == "bryophytes") |>
#  ggplot(aes(total, standing_biomass))+
#  geom_point()+
#  stat_smooth(method = "lm")
#
#standing_biomass_test |>
#  filter(functional_group == "bryophytes") |>
#  ggplot(aes(cover_height, standing_biomass))+
#  geom_point() +
#  stat_smooth(method = "lm")
#
#b2<-ggplot(composition_biomass, aes(moss_coverXheight, bryophytes))+
#  geom_point()+
#  stat_smooth(method = "lm")
#
#g1<-ggplot(composition_biomass, aes(graminoidCov, graminoids))+
#  geom_point()+
#  stat_smooth(method = "lm")
#
#g2<-ggplot(composition_biomass, aes(vegetationHeight, graminoids))+
#  geom_point()+
#  stat_smooth(method = "lm")
#
#f1<-ggplot(composition_biomass, aes(forbCov, forbs))+
#  geom_point()+
#  stat_smooth(method = "lm")
#
#f2<-ggplot(composition_biomass, aes(vegetationHeight, forbs))+
#  geom_point()+
#  stat_smooth(method = "lm")
#
## supplementary material fig of cover by height
## add r2 values
#plot_grid(b1, b2, g1, g2, f1, f2, labels = c('Bcover', 'BcoverXheight','Gcover', 'GcoverXheight', 'Fcover', 'FcoverXheight' ), label_size = 12)
#
#
#ggsave(filename = "~/OneDrive - University of Bergen/research/FunCaB/paper 2/figuressupFig_1#.jpg", dpi = 300, width = 9, height = 6)

make_biomass_coefficients <- function(standing_biomass_merged){
  
  # linear model by functional group
  lm_forb <- summary(lm(standing_biomass ~ 0 + total, data = standing_biomass_merged |> filter(functional_group == "forbs")))$coefficients %>% as_tibble()
  lm_bryophyte <- summary(lm(standing_biomass ~ 0 + cover_height, data = standing_biomass_merged |> filter(functional_group == "bryophytes")))$coefficients %>% as_tibble()
  lm_graminoid <- summary(lm(standing_biomass ~ 0 + total, data = standing_biomass_merged |> filter(functional_group == "graminoids")))$coefficients %>% as_tibble()
  
  # create biomass estimates
  biomass_coefficients <- standing_biomass_merged |>
    filter(!fg_removed == "XC") |>
    mutate(standing_biomass_calculated = case_when(
      functional_group == "graminoids" ~ total*lm_graminoid$Estimate,
      functional_group == "forbs" ~ total*lm_forb$Estimate,
      functional_group == "bryophytes" ~ total*lm_bryophyte$Estimate
    )
    )
}

# Clean 2018 Ovstedalen duplicates
# Remove duplicate entries from Ovstedalen 2018 while preserving legitimate duplicates from 2016-2017
clean_ovstedalen_2018_duplicates <- function(data) {
  # Remove duplicates from 2018 Ovstedalen
  # Keep only the first occurrence of each plot-functional group combination
  cleaned_data <- data |>
    filter(!(siteID == "Ovstedalen" & year == 2018)) |>
    bind_rows(
      data |>
        filter(siteID == "Ovstedalen", year == 2018) |>
        group_by(plotID, removed_fg) |>
        slice(1) |>
        ungroup()
    )
  
  return(cleaned_data)
}

