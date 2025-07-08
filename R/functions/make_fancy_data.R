# make fancy data

make_fancy_data <- function(data, gridded_climate, fix_treatment = TRUE){
  
  data2 <- data |>
    
    # add temperature and precipitation levels
    mutate(temperature_level = case_when(siteID %in% c("Ulvehaugen", "Skjelingahaugen", "Lavisdalen", "Gudmedalen") ~ "alpine",
                                         siteID %in% c("Alrust", "Veskre", "Rambera", "Hogsete") ~ "sub-alpine",
                                         TRUE ~ "boreal"),
           temperature_level = factor(temperature_level,
                                      levels = c("alpine", "sub-alpine", "boreal")),
           precipitation_level = case_when(siteID %in% c("Fauske", "Alrust", "Ulvehaugen") ~ 1,
                                           siteID %in% c("Vikesland", "Hogsete", "Lavisdalen") ~ 2,
                                           siteID %in% c("Arhelleren", "Rambera", "Gudmedalen") ~ 3,
                                           TRUE ~ 4)) |>
    
    # # add gridded climate data
    left_join(gridded_climate, by = "siteID")
  
  
  if(fix_treatment == TRUE){
    
    # convert treatment to FG present
    data2 <- data2 |>
      mutate(fg_remaining = case_when(
        treatment == "C" ~ "FGB",
        treatment == "FB" ~ "G",
        treatment == "FGB" ~ "bare",
        treatment == "GB" ~ "F",
        treatment == "F" ~ "GB",
        treatment == "GF" ~ "B",
        treatment == "B" ~ "GF",
        treatment == "G" ~ "FB",
        treatment == "XC" ~ "FGB",
        TRUE ~ NA_character_)) |>
      rename(fg_removed = treatment)
    
  } else {
    data2 <- data2
  }
  
  
}



#rename traits to fancy names for figures

fancy_trait_name_dictionary <- function(dat){
  
  dat <- dat %>%
    # make fancy names with units
    mutate(trait_fancy = recode(trait_trans,
                                height_log = "Height cm",
                                fresh_mass_log = "Wet mass g",
                                dry_mass_log = "Dry mass g",
                                leaf_area_log = "Area cm2",
                                leaf_thickness_log = "Thickness mm",
                                LDMC = "LDMC g/g",
                                SLA = "SLA cm2/g",
                                N = "N %",
                                C = "C %",
                                CN_ratio = "CN",
                                d13C = "δC13 ‰",
                                d15N = "δN15 ‰")) |>
    # make names that can be used in figures with unit
    mutate(figure_names = case_when(
      trait_trans == "height_log" ~ "Plant~height~(cm)",
      trait_trans == "fresh_mass_log" ~ "Leaf~wet~mass~(g)",
      trait_trans == "dry_mass_log" ~ "Leaf~dry~mass~(g)",
      trait_trans == "leaf_area_log" ~ "Leaf~area~(cm^2)",
      trait_trans == "leaf_thickness_log" ~ "Leaf~thickness~(mm)",
      trait_trans == "LDMC" ~ "LDMC~(gg^{-1})",
      trait_trans == "SLA" ~ "SLA~(cm^2*g^{-1})",
      trait_trans == "C" ~ "C~('%')",
      trait_trans == "N" ~ "N~('%')",
      trait_trans == "CN_ratio" ~ "CN",
      trait_trans == "d13C" ~ "δ^{13}~C~'(‰)'",
      trait_trans == "d15N" ~ "δ^{15}~N~'(‰)'"
    )) |>
    
    # order for trait_trans
    mutate(trait_trans = factor(trait_trans,
                                levels = c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log", "LDMC", "SLA", "C", "N", "CN_ratio", "d13C", "d15N"))) |>
    
    # order for figure names
    mutate(figure_names = factor(figure_names,
                                 levels = c("Plant~height~(cm)",
                                            "Leaf~dry~mass~(g)",
                                            "Leaf~area~(cm^2)",
                                            "Leaf~thickness~(mm)",
                                            "SLA~(cm^2*g^{-1})",
                                            "LDMC~(gg^{-1})",
                                            "C~('%')",
                                            "N~('%')",
                                            "CN",
                                            "δ^{13}~C~'(‰)'",
                                            "δ^{15}~N~'(‰)'"))) |>
    # add class
    mutate(class = case_when(trait_trans %in% c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log") ~ "Size",
                             trait_trans %in% c("d13C", "d15N") ~ "Isotopes",
                             TRUE ~ "Leaf economics"),
           class = factor(class, levels = c("Size", "Leaf economics", "Isotopes")))
  
}
