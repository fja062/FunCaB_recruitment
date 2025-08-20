# funcab seedling data cleaning #
prepare_funcab_recruitment <- function(funcab_recruitment_raw){

recruitment <- funcab_recruitment_raw  |> 
  mutate(site = recode(site,
                       "Ovstedal" = "Ovstedalen",
                       "Skjellingahaugen" = "Skjelingahaugen",
                       "Ulvhaugen" = "Ulvehaugen")) |> 

# generate random seedling IDs for missing seedIDs
  filter(is.na(NS),
         !Comment %in% c("out of plot"),
         !is.na(presence1) | !is.na(presence2) | !is.na(presence3) |!is.na(presence4)) |> 
  mutate(seedID = case_when(
    is.na(seedID) ~ paste0("ran", row_number()),
    TRUE ~ seedID))

# seedlings with duplicate IDs
# should be empty
recruitment %>%
  group_by(turfID, seedID) %>%
  filter(n() > 1) %>%
  distinct(turfID, seedID)


recruitment1 <- recruitment %>%
  mutate(seedID = paste0(seedID,"_", turfID),
         presence1 = as.character(presence1)) %>%
  pivot_longer(cols = unID:Viola,
               names_to = "species",
               values_to = "temp_present") %>%
  mutate(temp_present = as.character(temp_present),
         temp_present = if_else(!is.na(temp_present), species, temp_present)) |> 
  pivot_wider(names_from = species, values_from = temp_present) |> 
  # no duplicates here
  # group_by(site, blockID, treatment, turfID, seedID, species) %>%
  # summarise(n = n()) %>% filter(n > 1)
  rowwise() |> 
  # coerce species into one column and delete individual species columns
  mutate(species = coacross(unID:Viola)) |> 
  select(-c(unID:Viola)) |> 
  pivot_longer(cols = matches("\\d"),
               names_to = c(".value", "round"),
               names_pattern = "(.*)(\\d$)") |> 
  # fixing one wrongly formatted date
  mutate(date = if_else(date == "8/82019", "8/8/2019", date)) %>%
  mutate(date = dmy(date),
         year = if_else(round %in% c(1, 2), 2018, 2019),
         round = as.numeric(round)
  ) %>%
  select(year, date, siteID = site, blockID, plotID = turfID, treatment, seedID, round, species, presence, x, y, recorder = Obs, comment = Comment, Tttreat) %>%
  mutate(presence = case_when(
    presence %in% c("NF", "not found") ~ "0",
    presence %in% c("1?") ~"1",
    #presence %in% c("225") ~"25",
    is.na(presence) ~ "0",
    TRUE ~ presence
  ),
  presence = as.numeric(presence)) |> 
  group_by(plotID) |> 
  tidylog::filter(!sum(presence) < 1) |> 
  ungroup()



# fix species
recruitment1 <- recruitment1 %>%
  mutate(species = str_replace(species, "_", "."),
         species = recode(species,
                          "Anem.sp" = "Ane.sp",
                          "Bellis" = "Bel.sp",
                          "Betula" = "Bet.sp",
                          "Epil.sp" = "Epi.sp",
                          "Galium" = "Gal.sp",
                          "Hier.sp" = "Hie.sp",
                          "Leon.sp" = "Leo.sp",
                          "Leuc.sp" = "Leu.sp",
                          "Oma" = "Oma.sp",
                          "Ranu.sp" = "Ran.sp",
                          "Rume.sp" = "Rum.sp",
                          "Stel.sp" = "Ste.sp",
                          "Tara.sp" = "Tar.sp",
                          "Trif.sp" = "Tri.sp",
                          "unID" = "NID.seedling",
                          "Veronica" = "Ver.sp",
                          "Gal sax" = "Gal.sax",
                          "Viola" = "Vio.sp"))

# make multiple seedlings into separate observation and add unique seedID
# check if this sum is the same as uncount
#recruitment1 %>% filter(presence > 1) %>% summarise(sum(presence))
# YES
new_seedlings19 <- uncount(data = recruitment1  |> 
                             filter(presence > 1), weights = presence) %>%
  mutate(seedID = paste0("s", row_number(), "_", plotID)) |> 
  mutate(presence = 1)

# load TTC turf dictionary
dictionary_TTC_turf <- dict_TTC_turf()

recruitment2 <- recruitment1 %>%
  filter(presence < 2) %>%
  bind_rows(new_seedlings19) %>%
  # fix turfID
  tidylog::left_join(dictionary_TTC_turf, by = "plotID") %>%
  rename(turfID = TTtreat) %>%
  select(-Tttreat) %>%
  mutate(blockID = paste0(substr(siteID, 1, 3), blockID),
         functional_group = "forb") %>%
  # fix comment and coordinates
  mutate(y = if_else(y == "205/105", "205", y),
         y = as.numeric(y),
         comment = if_else(x > 250 | y > 250, "coordinate outside plot", comment))
}



clean_funcab_recruitment <- function(data, community){
# create dataframe of all turfs
all_turfs <- community |> 
  filter(!fg_removed == "XC") |> 
  distinct(siteID, blockID, plotID, fg_removed) |> 
  crossing(year = c(2018, 2019), round = c("early", "late")) |> 
  mutate(round = case_when(
    year == 2018 & round == "early" ~ 1,
    year == 2018 & round == "late" ~ 2,
    year == 2019 & round == "early" ~ 3,
    year == 2019 & round == "late" ~ 4
  ))

# extract seedling IDs, dates and their corresponding species
speciesID <- data |> distinct(date, year, round, seedID, species)

# split and clean seedling counts
data |> 
  tidylog::full_join(all_turfs) |>  
  
  # ALL PLOTS are present in dataset - no actual need for join to full turf list
  mutate(presence = coalesce(presence, 0)) |> 
  
  # remove coordinates
  select(-x, -y, -species, -recorder, -date, -year) |> 
  group_by(siteID, blockID, plotID, seedID) |> 
  
#  # fill in missing zeros
  tidylog::pivot_wider(names_from = round, values_from = presence)  |>  
  mutate(season = if_else(`1` > 0, "spr_18", NA),
         season = if_else(`2` > 0 & `1` == 0, "aut_18", season),
         season = if_else(`3` > 0 & `2` == 0 & `1` == 0, "spr_19", season),
         season = if_else(`4` > 0 & `3` == 0 & `2` == 0 & `1` == 0, "aut_19", season),
         survival_duration = rowSums(across(`1`:`4`))
         ) |>  
  ungroup() |> 
  
  # remove seedlings with no season associated
  tidylog::filter(!is.na(season)) |> 
  
  # gather rounds and join dates and species back to the data
  tidylog::pivot_longer(cols = c(`1`:`4`), names_to = "round", values_to = "presence") |> 
  mutate(round = as.numeric(round)) |> 
  tidylog::left_join(speciesID, by = join_by(seedID, round)) |>

# create month variable
  mutate(month = month(date))

}





##################

# seedclim recruitment cleaning

clean_seedclim_recruitment <- function(seedclim_recruitment_raw){
  # extract site names
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
  mutate(treatment = if_else(treatment == "RTG", "Gap", "Intact")) |> 

# complete turf list
#rtc_turf_list <- data2 |> 
#  distinct(siteID, blockID, plotID, treatment) #|> 
#  crossing(year = c(2009, 2010, 2011, 2012),
#           season = c("early", "late")) |>  
#  filter(!year == 2009 | !season == "early",
#         !year == 2012 | !season == "late")

# ALL PLOTS are present in dataset - no need for join to full turf list


# attach to complete turf list -- now redundant
#  tidylog::full_join(rtc_turf_list, by = c("siteID", "blockID", "plotID", "treatment")) |> 
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

# correct errors where seedlings come back from the dead 
  tidylog::distinct(siteID, blockID, plotID, treatment, ID, season, species, presence) |> 
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
         count = 1,
         survival_duration = rowSums(across(aut_09:spr_12))
  )  |>  
  tidylog::filter(!is.na(season)) |> 

# create year, month, season and date columns
  mutate(first_occurrence = season,
         year = as.numeric(paste0("20",substr(season, 5,6))), 
         season = substr(season, 1, 3),
         season = if_else(season == "aut", "late", "early")) |> 
  mutate(date = dmy(case_when(
    season == "late" & year == 2009 ~ "01-09-2009",
    season == "early" & year == 2010 ~ "01-07-2010", 
    season == "late" & year == 2010 ~ "01-09-2010", 
    season == "early" & year == 2011 ~ "01-07-2011", 
    season == "late" & year == 2011 ~ "01-09-2011", 
    season == "early" & year == 2012 ~ "01-07-2012")),
    month = month(date)) |> 
  # remove season-specific count columns for join
  select(-c(aut_09:spr_12)) |> 
  ungroup()
}




#########
## join with SPEI and FunCaB recruitment
#seedling_counts_join <- seedling_counts_complete %>% 
#  select(siteID, blockID, turfID, treatment, seedID, season, count, sum, date, year, month)
#
## rename treatment variable and harmonise
#recruitment_0919 <- rtc_counts_join %>%
#  rename(seedID = ID) %>% 
#  select(-species) %>% 
#  full_join(seedling_counts_complete) %>%
#  mutate(month = case_when(
#    season == "early" ~ 7,
#    season == "late" ~ 8,
#    TRUE ~ month
#  ))
#
#
#recruitment_0919 <- recruitment_0919 %>%
#  mutate(count = coalesce(count, 0)) %>% 
#  group_by(siteID, blockID, turfID, season, month, year, treatment) %>% 
#  reframe(sumcount = sum(count))
#
#save(recruitment_0919, file = "~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/recruitment_0919_spei.RData"#)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
######## trait data ############
## load seed mass trait data
#con <- src_sqlite(path = "~/OneDrive - University of Bergen/Research/FunCaB/seedclim.sqlite", create = FALSE)
#
#
#
########## data preparation ###########
#seedMass <- tbl(con, "numeric_traits") %>% 
#  filter(trait == "seedMass") %>% 
#  collect()
#
#all_turfs <- all_turfs %>% 
#  rename("siteID" = "site") %>% 
#  mutate(round = case_when(
#    round %in% c("1", "3") ~ "early",
#    round %in% c("2", "4") ~ "late",
#    TRUE ~ round
#  ))
#
#turfDict <- recruitment_biomass_spei %>% 
#  #filter(fg_presence %in% c("FGB", "F", "G", "B", "C")) %>% 
#  distinct(siteID, blockID, Treatment, turfID, fg_presence, round, year) %>% 
#  full_join(all_turfs)
#
#
## merge composition data with seed mass data
#seedcomp <- comp2 %>% 
#  filter(year > 2017) %>% 
#  left_join(seedMass) %>% 
#  group_by(siteID, blockID, turfID, treatment, year, vegetation_height, moss_height, total_bryophytes, total_forbs, #total_graminoids, functional_group) %>% 
#  summarise(seedMass = weighted.mean(value, cover)) %>% 
#  ungroup()
#
#