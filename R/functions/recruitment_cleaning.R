# funcab seedling data cleaning #
clean_funcab_recruitment <- function(data, community){
# create dataframe of all turfs
all_turfs <- community |> 
  filter(!fg_removed == "XC") |> 
  distinct(siteID, blockID, plotID, fg_removed, temperature_level, precipitation_level, temperature, precipitation, fg_remaining) |> 
  crossing(year = c(2018, 2019), round = c("early", "late")) |> 
  mutate(round = case_when(
    year == 2018 & round == "early" ~ 1,
    year == 2018 & round == "late" ~ 2,
    year == 2019 & round == "early" ~ 3,
    year == 2019 & round == "late" ~ 4
  ))



# split and clean seedling counts
data <- data |> 
  full_join(all_turfs) |>  
  mutate(presence = coalesce(presence, 0)) |> 
#  group_by(siteID, blockID, plotID, seedID) |> 
#  # fill in missing zeros
#  pivot_wider(names_from = round, values_from = presence) %>% 
#  mutate(season = if_else(`1` > 0, "spr_18", NA),
#         season = if_else(`2` > 0 & `1` == 0, "aut_18", season),
#         season = if_else(`3` > 0 & `2` == 0 & `1` == 0, "spr_19", season),
#         season = if_else(`4` > 0 & `3` == 0 & `2` == 0 & `1` == 0, "aut_19", season),
#         count = if_else(!is.na(season), 1, 0)) %>% 
#  rowwise() %>% 
#  mutate(sum = sum(`1`, `2`, `3`, `4`, na.rm = TRUE)) %>% 
#  ungroup()
# join onto complete turf list to catch turfs with zero seedlings


# create month variable
  mutate(month = month(date))

data
}





##############

site_dict <- site_dictionary() |> 
  select(old, new)

data2 <- seedclim_recruitment_raw |> 
  # filter for gaps and intact plots
  filter(Treat %in% c("RTG", "RTC"))  |>  
  # fix site names
  mutate(Site = case_when(
    grepl("\\vs", Site) ~ "Ovs", 
    grepl("^\\L", Site) ~ "Lav", 
    grepl("\\lr", Site) ~ "Alr",
    grepl("\\g", Site) ~ "Hog",
    TRUE ~ Site
  ),
  Site = trimws(Site)) |>
  # impute species id from other years
  mutate(Species_11_2 = if_else(is.na(Species_11_2), Species_11_1, Species_11_2),
         Species_11_2 = if_else(is.na(Species_11_2), Species_10_2_2, Species_11_2),
         Species_11_2 = if_else(is.na(Species_11_2), Species_09_2, Species_11_2)) |> 
  # impute missing presences when species is present but count is empty
  mutate(C_11_1 = case_when(is.na(C_11_1) & !is.na(Species_11_1) ~ "S", TRUE ~ C_11_1),
         C_11_2 = case_when(!is.na(Rec_11_2) & is.na(C_11_2) & !is.na(C_12_1) ~ "S", TRUE ~ C_11_2)#,
         #         C_10_1 = case_when(is.na(C_10_1) & !is.na(Species_10_1) ~ "S", TRUE ~ C_10_1),
         #         C_10_2 = case_when(is.na(C_10_2) & !is.na(Species_10_2_1) ~ "S", TRUE ~ C_10_2),
         #         #C_11_2 = case_when(is.na(C_11_2) & !is.na(Species_11_2) ~ "S", TRUE ~ C_11_2),
         #         C_12_1 = case_when(is.na(C_12_1) & !is.na(Species_12_1) ~ "S", TRUE ~ C_12_1),
  )  |> 
  # fix column names
  select(siteID = Site, treatment = Treat, blockID = Block, ID = ID...22, species = Species_11_2, aut_09 = C_09_2, spr_10 = C_10_1, aut_10 = C_10_2, spr_11 = C_11_1, aut_11 = C_11_2, spr_12 = C_12_1)  |>  
  mutate(blockID = as.numeric(case_when(
    blockID == "I" ~ 1,
    blockID == "II" ~ 2,
    blockID == "III" ~ 3,
    blockID == "IV" ~ 4,
    blockID == "V" ~ 5
  ))) |>  
  # join onto site dictionary and retain correct site names
  left_join(site_dict, by = c("siteID"  = "old"))  |>  
  select("plotID" = siteID, treatment:spr_12, "siteID" = new)  |>  
  # create plot and blockID and treatment term
  mutate(blockID = paste0(plotID, blockID),
         plotID = paste0(blockID, treatment)
         ) |> 
  mutate(treatment = if_else(treatment == "RTG", "gap", "intact"))

# complete turf list
rtc_turf_list <- data2 |> 
  distinct(siteID, blockID, plotID) |> 
  crossing(year = c(2009, 2010, 2011, 2012),
           season = c("early", "late")) |>  
  filter(!year == 2009 | !season == "early",
         !year == 2012 | !season == "late")



data2 |>  
  # clean species names and assign functional groups where missing
  mutate(species = trimws(species, which = "both"),
         species = gsub(" ", ".", species),
         functional_group = case_when(
           species %in% c("G", "Ant.odo", "Ave.fle") ~ "graminoid",
           grepl("Agr", species) ~ "graminoid",
           grepl("Car", species) ~ "graminoid", 
           grepl("Des", species) ~ "graminoid", 
           grepl("Fes", species) ~ "graminoid", 
           grepl("Poa", species) ~ "graminoid", 
           grepl("Luz", species) ~ "graminoid",
           TRUE ~ "forb"
         ))  |>  
# remove graminoid seedlings from analyses
  tidylog::filter(!functional_group == "graminoid")  |> 
  #filter(!species %in% c("Destroyed", "Destroyed ", "missing", "DAMAGED")) %>% 
  mutate(species = gsub(".cf", "", species),
         species = gsub(".CF", "", species)) %>% 
  #assign IDs to seedlings without IDs
  mutate(ID = case_when(
    is.na(ID) ~ paste0("ran-", floor(runif(nrow(is.na(.)), min = 2000, max = 5000))),
    TRUE ~ ID))  |>  
  pivot_longer(aut_09:spr_12, names_to = "season", values_to = "presence") |> 
  #harmonise seedling indications
  mutate(presence = substr(presence, 1,1),
         presence = case_when(
           presence == "s" ~ "S",
           presence == "H" ~ "S",
           presence == "?" ~ "T",
           presence == "C" ~ "S",
           presence == "E" ~ "S",
           presence == " " ~ "S",
           presence == "1" ~ "S",
           presence == "V" ~ "S",
           TRUE ~ presence))  |> 
  #create year variable
  #mutate(year = as.numeric(paste0("20", substr(season, 5,6))))


# correct errors where seedlings come back from the dead 
  distinct(siteID, blockID, plotID, treatment, ID, season, species, presence) |> 
  pivot_wider(names_from = season, values_from = presence) |> 
  # replace NAs with zeros
  mutate(across(aut_09:spr_12, ~ coalesce(., "0")))  |>  
  mutate(aut_11 = case_when(aut_11 =="0" & spr_12 %in% c("T", "D") ~ "S", TRUE ~ aut_11),
         spr_11 = case_when(spr_11 == "0" & aut_11 %in% c("T", "D") ~ "S", TRUE ~ spr_11),
         aut_10 = case_when(aut_10 == "0" & spr_11 %in% c("T", "D") ~ "S", TRUE ~ aut_10),
         spr_10 = case_when(spr_10 == "0" & aut_10 %in% c("T", "D") ~ "S", TRUE ~ spr_10)
  )  |> 
  # make seedling counts numeric
  mutate(across(.cols = contains("aut_")|contains("spr_"),
                .fns = ~ as.numeric(case_when(
                  . %in% c("T", "D") ~ 0,
                  . == "S" ~ 1,
                  TRUE ~ 0
                ))))  |>  
  # fill in missing counts in 2-year gaps
  mutate(
    spr_10 = case_when(aut_09 == 1 & spr_10 == 0 & spr_11 == 1 ~ 1, TRUE ~ spr_10),
    aut_10 = case_when(spr_10 == 1 & aut_10 == 0 & aut_11 == 1 ~ 1, TRUE ~ aut_10),
    spr_11 = case_when(aut_10 == 1 & spr_11 == 0 & spr_12 == 1 ~ 1, TRUE ~ spr_11), 
    # fill in missing counts in 1-year gaps
    spr_10 = case_when(aut_09 == 1 & spr_10 == 0 & aut_10 == 1 ~ 1, TRUE ~ spr_10),
    aut_10 = case_when(spr_10 == 1 & aut_10 == 0 & spr_11 == 1 ~ 1, TRUE ~ aut_10),
    spr_11 = case_when(aut_10 == 1 & spr_11 == 0 & aut_11 == 1 ~ 1, TRUE ~ spr_11),
    aut_11 = case_when(spr_11 == 1 & aut_11 == 0 & spr_12 == 1 ~ 1, TRUE ~ aut_11)) |> 


# calculate first occurrences of seedlings and remove 18 without observations
  group_by(siteID, blockID, plotID, treatment, ID, species) %>% 
  mutate(season = if_else(aut_09 == 1, "aut_09", NA),
         season = if_else(spr_10 == 1 & aut_09 == 0, "spr_10", season),
         season = if_else(aut_10 == 1 & aut_09 == 0 & spr_10 == 0, "aut_10", season),
         season = if_else(spr_11 == 1 & aut_09 == 0 & spr_10 == 0 & aut_10 == 0, "spr_11", season),
         season = if_else(aut_11 == 1 & aut_09 == 0 & spr_10 == 0 & aut_10 == 0 & spr_11 == 0, "aut_11", season),
         season = if_else(spr_12 == 1 & aut_09 == 0 & spr_10 == 0 & aut_10 == 0 & spr_11 == 0 & aut_11 == 0, "spr_12", season),
         count = 1
  )  |>  
  tidylog::filter(!is.na(season)) |> 

# calculate seedling counts and sums
  rowwise() |> 
  mutate(sum = sum(spr_12, aut_11, spr_11, aut_10, spr_10, aut_09)) |> 
  ungroup()


# create year and date columns

rtc_counts <- rtc_counts %>% 
  mutate(year = as.numeric(paste0("20",substr(season, 5,6))), 
         season = substr(season, 1, 3),
         season = case_when(
           season == "aut" ~ "late",
           season == "spr" ~ "early",
           TRUE ~ season
         )) %>%
  ungroup() %>% 
  mutate(date = dmy(case_when(
    season == "late" & year == 2009 ~ "01-09-2009",
    season == "early" & year == 2010 ~ "01-07-2010", 
    season == "late" & year == 2010 ~ "01-09-2010", 
    season == "early" & year == 2011 ~ "01-07-2011", 
    season == "late" & year == 2011 ~ "01-09-2011", 
    season == "early" & year == 2012 ~ "01-07-2012")),
    month = month(date),
    treatment = factor(treatment, labels = c(RTC = "Intact", RTG = "Gap")))

rtc_counts_join <- rtc_counts %>% 
  select(siteID:species, season:sum)

# attach to complete turf list
rtc_counts_join <- full_join(rtc_counts, rtc_turf_list, by = c("siteID", "blockID", "turfID", "year", "season", "treatment"))


## join with SPEI and FunCaB recruitment
seedling_counts_join <- seedling_counts_complete %>% 
  select(siteID, blockID, turfID, treatment, seedID, season, count, sum, date, year, month)

# rename treatment variable and harmonise
recruitment_0919 <- rtc_counts_join %>%
  rename(seedID = ID) %>% 
  select(-species) %>% 
  full_join(seedling_counts_complete) %>%
  mutate(month = case_when(
    season == "early" ~ 7,
    season == "late" ~ 8,
    TRUE ~ month
  ))


recruitment_0919 <- recruitment_0919 %>%
  mutate(count = coalesce(count, 0)) %>% 
  group_by(siteID, blockID, turfID, season, month, year, treatment) %>% 
  reframe(sumcount = sum(count))

save(recruitment_0919, file = "~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/recruitment_0919_spei.RData")
















####### trait data ############
# load seed mass trait data
con <- src_sqlite(path = "~/OneDrive - University of Bergen/Research/FunCaB/seedclim.sqlite", create = FALSE)



######### data preparation ###########
seedMass <- tbl(con, "numeric_traits") %>% 
  filter(trait == "seedMass") %>% 
  collect()

all_turfs <- all_turfs %>% 
  rename("siteID" = "site") %>% 
  mutate(round = case_when(
    round %in% c("1", "3") ~ "early",
    round %in% c("2", "4") ~ "late",
    TRUE ~ round
  ))

turfDict <- recruitment_biomass_spei %>% 
  #filter(fg_presence %in% c("FGB", "F", "G", "B", "C")) %>% 
  distinct(siteID, blockID, Treatment, turfID, fg_presence, round, year) %>% 
  full_join(all_turfs)


# merge composition data with seed mass data
seedcomp <- comp2 %>% 
  filter(year > 2017) %>% 
  left_join(seedMass) %>% 
  group_by(siteID, blockID, turfID, treatment, year, vegetation_height, moss_height, total_bryophytes, total_forbs, total_graminoids, functional_group) %>% 
  summarise(seedMass = weighted.mean(value, cover)) %>% 
  ungroup()

