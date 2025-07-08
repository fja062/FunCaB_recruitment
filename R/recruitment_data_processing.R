# seedling data cleaning #

# biomass data from 2015/2016
biomass <- community_1516 %>%
  select(siteID, blockID, turfID, biomassGram, biomassForb, biomassBryo, Treatment) %>% 
  mutate(blockID = paste0(substr(siteID, 1,3), blockID))

# harmonise blockIDs
comp2 <- comp2 %>% 
  mutate(blockID = paste0(substr(siteID, 1, 3), gsub("\\D", "", blockID)))

# create dataframe of all turfs
all_turfs <- comp2 %>% 
  filter(treatment != "XC") %>% 
  distinct(site = siteID, blockID, turfID) %>% 
  crossing(round = c("1", "2", "3", "4"))

# generate random seedling IDs for missing seedIDs
recruitment <- dat %>% 
  filter(is.na(NS),
         !Comment %in% c("out of plot"),
         !is.na(presence1) | !is.na(presence2) | !is.na(presence3) |!is.na(presence4)) %>% 
  mutate(seedID = case_when(
    is.na(seedID) ~ paste0("r", floor(runif(nrow(is.na(.)), min = 2000, max = 10000))),
    TRUE ~ seedID),
    blockID = paste0(substr(site, 1,3), blockID)) %>% 
  left_join(dict_Site, by = c("site"  = "old2")) %>% 
  rename(siteID = new) %>% 
  select(-site)

# seedlings with duplicate IDs
# should be empty
recruitment %>% 
  group_by(turfID, seedID) %>% 
  filter(n() > 1) %>% 
  distinct(turfID, seedID)


coalesce_df <- function(df) {
  dplyr::coalesce(!!!df)
} 

recruitment %>% 
  mutate(species = coalesce_df(pick(Ach_mil:Viola)))



recruitment <- recruitment %>% pivot_longer(cols = Ach_mil:Viola, names_to = "species") %>% 
  filter(!is.na(value))

#### split the dataframe into 1) dates + round and 2) presence+round
# split and clean dates
recruitment_dates <- recruitment %>% 
  mutate(seedID = paste0(seedID,"_", turfID)) %>% 
  select(siteID, turfID, seedID, date1:date4) %>% 
  pivot_longer(date1:date4,
               names_to = c("category", "season"),
               names_pattern = "(^.{0,8})([1-4])",
               values_to = "date") %>% 
  mutate(date = str_replace_all(date, "\\.", "/"),
         date = case_when(
           date == "8/82019" ~ "08/08/2019",
           TRUE ~ date
         ),
         date = dmy(date)) %>% 
  select(siteID, season, date) %>% 
  reframe(siteID, season, date) %>% 
  filter(!is.na(date)) %>% 
  group_by(siteID, season) %>% 
  filter(date == last(date)) %>% 
  ungroup() %>% 
  distinct(siteID, season, date) %>% 
  mutate(season = case_when(
    season == 1 ~ "spr_18",
    season == 2 ~ "aut_18",
    season == 3 ~ "spr_19",
    season == 4 ~ "aut_19",
  ))
  
# split and clean seedling counts
seedling_counts <- recruitment %>% 
  mutate(seedID = paste0(seedID,"_", turfID)) %>% 
  select(siteID, blockID, turfID, seedID, presence1:presence4, species) %>% 
  pivot_longer(presence1:presence4,
               names_to = c("category", "round"),
               names_pattern = "(^.{0,8})([1-4])",
               values_to = "count") %>% 
  select(-category) %>% 
  full_join(all_turfs) %>% 
  mutate(count = coalesce(count, "0")) %>% # fill in missing zeros
  mutate(count = case_when(
    count %in% c("NF", "not found", "NA") ~ "0",
    count %in% c("1?") ~"1",
    is.na(count) ~ "0",
    TRUE ~ count
  ),
  count = as.numeric(count)) %>% 
  pivot_wider(names_from = round, values_from = count) %>% 
  mutate(season = if_else(`1` > 0, "spr_18", NA),
         season = if_else(`2` > 0 & `1` == 0, "aut_18", season),
         season = if_else(`3` > 0 & `2` == 0 & `1` == 0, "spr_19", season),
         season = if_else(`4` > 0 & `3` == 0 & `2` == 0 & `1` == 0, "aut_19", season),
         count = if_else(!is.na(season), 1, 0)) %>% 
  rowwise() %>% 
  mutate(sum = sum(`1`, `2`, `3`, `4`, na.rm = TRUE)) %>% 
  ungroup()
# join onto complete turf list to catch turfs with zero seedlings


# make total seedling count per turf and per observation round
#seedling_counts <- seedling_counts %>% 
#  group_by(siteID, blockID, turfID, season) %>% 
#  summarise(count = sum(count, na.rm = TRUE)) %>% 
#  ungroup()



# create treatment term
seedling_counts <- seedling_counts %>% 
  filter(grepl("C", turfID) | grepl("FGB", turfID)) %>% 
  mutate(treatment = case_when(
    grepl("C", turfID) ~ "Intact",
    grepl("FGB", turfID) ~ "Gap",
    TRUE ~ "0"
  ))

turf_test <- all_turfs %>% 
  filter(grepl("C", turfID) | grepl("FGB", turfID)) %>% 
  mutate(season = case_when(
    round == 1 ~ "spr_18",
    round == 2 ~ "aut_18",
    round == 3 ~ "spr_19",
    round == 4 ~ "aut_19",
  )) %>% 
  mutate(treatment = case_when(
    grepl("C", turfID) ~ "Intact",
    grepl("FGB", turfID) ~ "Gap",
    TRUE ~ "0"
  )) %>% 
  select(siteID = site, blockID, turfID, season, treatment)

seedling_counts_complete <- full_join(seedling_counts, turf_test, by = c("siteID", "blockID", "turfID", "season", "treatment"))

# create month, year and round variable
seedling_counts_complete <- seedling_counts_complete %>% 
  left_join(recruitment_dates) %>% 
  mutate(year = as.numeric(paste0(20, substr(season, 5,6))),
         month = month(date),
         season = case_when(
           grepl("spr", season) ~ "early",
           grepl("aut", season) ~ "late",
           TRUE ~ season
         )) %>% 
  filter(!is.na(date))

seedling_counts_complete <- seedling_counts_complete %>% 
  mutate(`2` = case_when(
    `1` == 1 & `2` == 0 & `3`== 1 ~ 1,
    TRUE ~ `2`),
    `3` = case_when(
      `2` == 1 & `3` == 0 & `4`== 1 ~ 1,
      TRUE ~ `3`)
    )

# join to biomass data
#recruitment_test <- seedling_counts %>% 
#  left_join(biomass, by = c("site" = "siteID", "turfID")) %>% 
#  rename(siteID = site)
  
#save(recruitment1, file = "~/OneDrive - University of Bergen/research/FunCaB/Data/secondary/recruitment_cleaned.csv")

# replace biomasses with real NAs according to the treatments
#recruitment_biomass <- recruitment_test %>% 
#  mutate(biomassForb = case_when(
#    grepl("F", Treatment) ~ NA_real_,
#    TRUE ~ biomassForb),
#    biomassGram = case_when(
#      grepl("G", Treatment) ~ NA_real_,
#      TRUE ~ biomassGram),
#    biomassBryo = case_when(
#      grepl("B", Treatment) ~ NA_real_,
#      TRUE ~ biomassBryo)) %>%
#  mutate(fg_presence = recode(Treatment, "B" = "GF", "G" = "FB", "F" = "GB", "GF" = "B", "FB" = "G", "GB" = #"F")) %>% 
#  filter(fg_presence %in% c("FGB", "F", "G", "B", "C")) 




######## ---- 2009/2010 ----- ###########
# D = dead, assign alive if found alive in following year
# S = seedling
# TP = toothpick missing; essentially missing data. assign alive if found alive in following year
# E = established - two pairs of opposite leaves, or two alternate leaves
# + = fertile


# trying with John's cleaned data
recruitment_fj <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/primary/veg_recruitment/recr_for_francesca.csv", delim = ";", col_types = cols(.default = "c"))

recruitment_fj %>% 
  filter(treat %in% c("RTC", "RTG"))







##############
rc_rtc <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/primary/veg_recruitment/PM_rawdata_1112.csv", delim = ",", col_types = cols(.default = "c"))

 
rc_rtc <- rc_rtc %>% 
   filter(Treat %in% c("RTG", "RTC")) %>% 
   mutate(Site = case_when(
     grepl("\\vs", Site) ~ "Ovs", 
     grepl("^\\L", Site) ~ "Lav", 
     grepl("\\lr", Site) ~ "Alr",
     grepl("\\g", Site) ~ "Hog",
     TRUE ~ Site
     ),
     Site = trimws(Site)) %>% 
   mutate(Species_11_2 = if_else(is.na(Species_11_2), Species_11_1, Species_11_2),
          Species_11_2 = if_else(is.na(Species_11_2), Species_10_2_2, Species_11_2),
          Species_11_2 = if_else(is.na(Species_11_2), Species_09_2, Species_11_2)) %>% 
  mutate(C_11_1 = case_when(is.na(C_11_1) & !is.na(Species_11_1) ~ "S", TRUE ~ C_11_1),
         C_11_2 = case_when(!is.na(Rec_11_2) & is.na(C_11_2) & !is.na(C_12_1) ~ "S", TRUE ~ C_11_2)#,
#         C_10_1 = case_when(is.na(C_10_1) & !is.na(Species_10_1) ~ "S", TRUE ~ C_10_1),
#         C_10_2 = case_when(is.na(C_10_2) & !is.na(Species_10_2_1) ~ "S", TRUE ~ C_10_2),
#         #C_11_2 = case_when(is.na(C_11_2) & !is.na(Species_11_2) ~ "S", TRUE ~ C_11_2),
#         C_12_1 = case_when(is.na(C_12_1) & !is.na(Species_12_1) ~ "S", TRUE ~ C_12_1),
         ) %>%
  select(siteID = Site, treatment = Treat, blockID = Block, ID = ID...22, species = Species_11_2, aut_09 = C_09_2, spr_10 = C_10_1, aut_10 = C_10_2, spr_11 = C_11_1, aut_11 = C_11_2, spr_12 = C_12_1) %>% 
   mutate(blockID = as.numeric(case_when(
     blockID == "I" ~ 1,
     blockID == "II" ~ 2,
     blockID == "III" ~ 3,
     blockID == "IV" ~ 4,
     blockID == "V" ~ 5
   ))) %>% 
   left_join(dict_Site, by = c("siteID"  = "old")) %>% 
   select(treatment:spr_12, "siteID" = new) %>% 
# create turfID and blockID
  mutate(turfID = paste0(substr(siteID, 1, 3), blockID, treatment),
         blockID = paste0(substr(siteID, 1, 3), blockID))



# complete turf list
rtc_turf_list <- rc_rtc %>%
  distinct(siteID, blockID, turfID) %>% 
  crossing(year = c(2009, 2010, 2011, 2012),
           season = c("early", "late")) %>% 
  filter(!year == 2009 | !season == "early") %>% 
  filter(!year == 2012 | !season == "late") %>% 
  mutate(treatment = case_when(
    grepl("RTC", turfID) ~ "Intact",
    grepl("RTG", turfID) ~ "Gap",
    TRUE ~ "0"
  ))

# remove graminoid seedlings from analyses and assign IDs to seedlings without IDs
rc_rtcSum <- rc_rtc %>% 
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
          )) %>% 
  filter(!functional_group == "graminoid") %>%
  #filter(!species %in% c("Destroyed", "Destroyed ", "missing", "DAMAGED")) %>% 
  mutate(species = gsub(".cf", "", species),
         species = gsub(".CF", "", species)) %>% 
  mutate(ID = case_when(
    is.na(ID) ~ paste0("ran-", floor(runif(nrow(is.na(.)), min = 2000, max = 5000))),
    TRUE ~ ID)) %>% 
  gather(spr_12, aut_11, spr_11, aut_10, spr_10, aut_09, key = season, value = seed, na.rm = TRUE)
 

# recode seedling presence
rc_rtcSum <- rc_rtcSum %>% 
  mutate(seed = substr(seed, 1,1),
         seed = case_when(
           seed == "s" ~ "S",
           seed == "H" ~ "S",
           seed == "?" ~ "T",
           seed == "C" ~ "S",
           seed == "E" ~ "S",
           seed == " " ~ "S",
           seed == "1" ~ "S",
           seed == "V" ~ "S",
           TRUE ~ seed)) %>%
  mutate(year = as.numeric(paste0("20", substr(season, 5,6))))


# correct errors where seedlings come back from the dead 
rtc_counts <- rc_rtcSum %>% 
  distinct(siteID, blockID, turfID, treatment, ID, season, species, seed) %>% 
  pivot_wider(names_from = season, values_from = seed) %>% 
  mutate(aut_09 = coalesce(aut_09, "0"),
         spr_10 = coalesce(spr_10, "0"),
         aut_10 = coalesce(aut_10, "0"),
         spr_11 = coalesce(spr_11, "0"),
         aut_11 = coalesce(aut_11, "0"),
         spr_12 = coalesce(spr_12, "0")
  ) %>% 
  mutate(aut_11 = case_when(aut_11 =="0" & spr_12 %in% c("T", "D") ~ "S",TRUE ~ aut_11),
    spr_11 = case_when(spr_11 == "0" & aut_11 %in% c("T", "D") ~ "S", TRUE ~ spr_11),
    aut_10 = case_when(aut_10 == "0" & spr_11 %in% c("T", "D") ~ "S", TRUE ~ aut_10),
    spr_10 = case_when(spr_10 == "0" & aut_10 %in% c("T", "D") ~ "S", TRUE ~ spr_10)
  ) %>%
  mutate(across(.cols = contains("aut_")|contains("spr_"),
                .fns = ~as.numeric(case_when(
                  . %in% c("T", "D") ~ 0,
                  . == "S" ~ 1,
                  TRUE ~ 0
                )))) %>% 
  # fill in missing counts in 2-year gaps
  mutate(
  spr_10 = case_when(aut_09 == 1 & spr_10 == 0 & spr_11 == 1 ~ 1, TRUE ~ spr_10),
  aut_10 = case_when(spr_10 == 1 & aut_10 == 0 & aut_11 == 1 ~ 1, TRUE ~ aut_10),
  spr_11 = case_when(aut_10 == 1 & spr_11 == 0 & spr_12 == 1 ~ 1, TRUE ~ spr_11), 
  # fill in missing counts in 1-year gaps
  spr_10 = case_when(aut_09 == 1 & spr_10 == 0 & aut_10 == 1 ~ 1, TRUE ~ spr_10),
  aut_10 = case_when(spr_10 == 1 & aut_10 == 0 & spr_11 == 1 ~ 1, TRUE ~ aut_10),
  spr_11 = case_when(aut_10 == 1 & spr_11 == 0 & aut_11 == 1 ~ 1, TRUE ~ spr_11),
  aut_11 = case_when(spr_11 == 1 & aut_11 == 0 & spr_12 == 1 ~ 1, TRUE ~ aut_11))


# calculate first occurrences of seedlings and remove 18 without observations
rtc_counts <- rtc_counts %>% 
  group_by(siteID, blockID, turfID, treatment, ID, species) %>% 
  mutate(season = if_else(aut_09 == 1, "aut_09", NA),
         season = if_else(spr_10 == 1 & aut_09 == 0, "spr_10", season),
         season = if_else(aut_10 == 1 & aut_09 == 0 & spr_10 == 0, "aut_10", season),
         season = if_else(spr_11 == 1 & aut_09 == 0 & spr_10 == 0 & aut_10 == 0, "spr_11", season),
         season = if_else(aut_11 == 1 & aut_09 == 0 & spr_10 == 0 & aut_10 == 0 & spr_11 == 0, "aut_11", season),
         season = if_else(spr_12 == 1 & aut_09 == 0 & spr_10 == 0 & aut_10 == 0 & spr_11 == 0 & aut_11 == 0, "spr_12", season),
         count = 1
         ) %>% 
  filter(!is.na(season))

# calculate seedling counts and sums
rtc_counts <- rtc_counts %>% 
  rowwise() %>% 
  mutate(sum = sum(spr_12, aut_11, spr_11, aut_10, spr_10, aut_09)) %>% 
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



######### ------ abundance ------ ###########
recruitment_biomass_spei <- recruitment_biomass_spei %>% 
  distinct(siteID, blockID, round, date, year, Treatment, turfID, count, spei) %>% 
  right_join(turfDict)

######### ----- survival ----- ###########
survival <- seed %>%
  filter(Round == 1) %>% 
  select(siteID, blockID, Treatment, turfID, survival) %>% 
  mutate(survival = recode(survival, "dead" = 0, "alive" = 1, "not found" = 0, "1" = 1, "dezd" = 0)) %>%
  group_by(Treatment, turfID, blockID, siteID) %>% 
  summarise(tot = n(),
            totS = sum(survival),
            survival = (totS/tot)*100) %>% 
  right_join(turfDict %>% mutate(Round = as.factor(Round))) %>% 
  mutate_at(tot = case_when(is.na(tot) ~ 0L, 
                            TRUE ~ tot)) %>% 
  mutate(totS = case_when(is.na(totS) ~ 0, 
                          TRUE ~ totS)) %>% 
  mutate(survival = case_when(is.na(survival) ~ 0, 
                              TRUE ~ survival)) %>%
  left_join(weather) %>% 
  ungroup()

