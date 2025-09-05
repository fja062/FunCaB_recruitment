# source dictionaries and data
source("~/Documents/research/FunCaB/climate/weather.R")
source("~/Documents/research/FunCaB/dictionaries.R")


weather <- weather %>% 
  mutate(siteID = case_when(
    siteID == "Ovstedal" ~ "Ovstedalen",
    siteID == "Skjellingahaugen" ~ "Skjelingahaugen",
    siteID == "Ulvhaugen" ~ "Ulvehaugen",
    TRUE ~ siteID
  ))

load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/climate.Rdata")
#load("~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/soilMoisture2018.RData")


#site information
site_info <- read_csv("~/Documents/research/FunCaB/climate/data/VCG_site_info.csv") %>% 
  arrange(siteID)

#gridded precipitation
precip_gridded <- read_csv("~/Documents/research/FunCaB/climate/data/VCG_clean_gridded_daily_climate_2008-2022.csv") %>% 
  filter(variable == "precipitation", date > "2008-12-31", date < "2020-11-01") %>% 
  mutate(year = year(date), month = month(date), date = date(date), mdate = format(date, "%Y-%m")) %>%
  group_by(mdate, year, month, siteID) %>% 
  summarise(precip_sum = sum(value))


# gudmedalen fix
gud_temp <- read_csv("~/Documents/research/FunCaB/climate/data/VCG_clean_temperature.csv") %>%
  filter(logger %in% c("temp200cm", "temp30cm"), !is.na(value), date > "2009-01-01 00:00:00", siteID == "Gudmedalen") %>%
  mutate(year = year(date), month = month(date), date = date(date), day = day(date), mdate = format(date, "%Y-%m"))



#site temperature
temperature_site <- read_csv("~/Documents/research/FunCaB/climate/data/VCG_clean_temperature.csv") %>%
  filter(logger %in% c("temp200cm", "temp30cm"), !is.na(value), date > "2009-01-01 00:00:00") %>% 
  mutate(year = year(date), month = month(date), date = date(date), day = day(date), mdate = format(date, "%Y-%m")) %>%
  group_by(date, year, month, day, siteID, logger) %>% 
  mutate(temp_max = max(value),
         temp_min = min(value)) %>% 
  group_by(mdate, year, month, siteID,logger) %>% 
  summarise(temp_max = mean(temp_max, na.rm = TRUE),
            temp_min = mean(temp_min, na.rm = TRUE),
            temp_mean = mean(value, na.rm = TRUE), .groups = "drop")

maxtemp <- temperature_site %>%
  ungroup() %>%
  select(siteID, year, month, temp_max, logger) %>%
  pivot_wider(names_from = logger, values_from = temp_max) %>% 
  mutate(temp_max = if_else(is.na(temp200cm), temp30cm, temp200cm)) %>% 
  complete(siteID, year, month) %>% 
  left_join(site_info) %>% 
  group_by(Biogeographic_zone, year, month) %>% 
  mutate(regional_temp_max = mean(temp_max)) %>% 
  ungroup() %>% 
  mutate(temp_max = if_else(is.na(temp_max), regional_temp_max, temp_max)) %>% 
  select(-Code, -Biogeographic_zone, -Latitude, -Longitude, -Elevation, -regional_temp_max, -temp200cm, -temp30cm)

maxtemp_spei <- maxtemp %>% 
  pivot_wider(names_from = siteID, values_from = temp_max) %>% 
  select(-year, -month)





mintemp <- temperature_site %>% 
  ungroup() %>% 
  select(siteID, year, month, temp_min, logger) %>%
  pivot_wider(names_from = logger, values_from = temp_min) %>% 
  mutate(temp_min = if_else(is.na(temp200cm), temp30cm, temp200cm)) %>% 
  complete(siteID, year, month) %>% 
  left_join(site_info) %>% 
  group_by(Biogeographic_zone, year, month) %>% 
  mutate(regional_temp_min = mean(temp_min)) %>% 
  ungroup() %>% 
  mutate(temp_min = if_else(is.na(temp_min), regional_temp_min, temp_min)) %>% 
  select(-Code, -Biogeographic_zone, -Latitude, -Longitude, -Elevation, -regional_temp_min, -temp200cm, -temp30cm)

mintemp_spei <- mintemp %>% 
  pivot_wider(names_from = siteID, values_from = temp_min) %>% 
  select(-year, -month)


#mean temperature
meantemp <- temperature_site %>% 
  ungroup() %>% 
  select(siteID, year, month, temp_mean, logger) %>%
  pivot_wider(names_from = logger, values_from = temp_mean) %>% 
  mutate(temp_mean = if_else(is.na(temp200cm), temp30cm, temp200cm)) %>% 
  complete(siteID, year, month) %>% 
  left_join(site_info) %>% 
  group_by(Biogeographic_zone, year, month) %>% 
  mutate(regional_temp_mean = mean(temp_mean)) %>% 
  ungroup() %>% 
  mutate(temp_mean = if_else(is.na(temp_mean), regional_temp_mean, temp_mean)) %>% 
  select(-Code, -Biogeographic_zone, -Latitude, -Longitude, -Elevation, -regional_temp_mean, -temp200cm, -temp30cm)


temperature_site_min_mean_max <- full_join(mintemp, maxtemp) %>% 
  full_join(meantemp) %>% 
  pivot_longer(cols = c("temp_min", "temp_mean", "temp_max"), names_to = "temperature_aggregate", values_to = "value")



# gridded precipitation
precip <- precip_gridded %>% 
  ungroup() %>% 
  filter(year %in% c(2009:2019)) %>% 
  select(siteID, year, month, precip_sum) %>%
  pivot_wider(names_from = siteID, values_from = precip_sum) %>% 
  select(-year, -month)
precip_calc <- as.matrix(precip, dimnames = NULL) %>% unname()

#latitude
lat <- site_info %>% select(Latitude)
lat_calc <- lat$Latitude

# mininmum temperature
mintemp_calc <- mintemp %>% 
  filter(year %in% c(2009:2019)) %>% 
  pivot_wider(values_from = temp_min, names_from = siteID) %>%  
  select(-year, -month)

mintemp_calc <- mintemp_calc %>% 
  as.matrix(dimnames = NULL) %>% 
  unname()

# maximum temperature
maxtemp_calc <- maxtemp %>% 
  filter(year %in% c(2009:2019)) %>% 
  pivot_wider(values_from = temp_max, names_from = siteID) %>%  
  select(-year, -month)

maxtemp_calc <- maxtemp_calc %>% 
  as.matrix(dimnames = NULL) %>% unname()


# calculating monthly SPEI
pet <- hargreaves(mintemp_calc, maxtemp_calc, lat = lat_calc, na.rm = TRUE)
cwbal <- precip_calc - pet
spei_all <- spei(as.matrix(cwbal), scale = 1, na.rm = TRUE)$fitted %>%
  as_tibble() %>% 
  pivot_longer(c(`Series 1`:`Series 12`), names_to = "series", values_to = "spei")


spei_all_seasonal <- spei(as.matrix(cwbal), scale = 3, na.rm = TRUE)$fitted %>%
  as_tibble()

#spei_all_seasonal <- precip_gridded %>% 
#  ungroup() %>% 
#  distinct(year, month) %>% 
#  bind_cols(spei_all_seasonal) %>% 
#  pivot_longer(c("Alrust": "Vikesland"), names_to = "siteID", values_to #= "spei_seasonal") %>% 
#  mutate(date = as_date(paste(year, month, "1", sep = "-")))

spei_sites <- mintemp %>% 
  filter(year %in% c(2009:2019)) %>% 
  select(-temp_min) %>% 
  arrange(month, year) %>% 
  bind_cols(spei_all) %>% 
  mutate(date = my(paste(month, year, sep = "/"))) %>% 
  mutate(season = case_when(
    between(month, 3,5) ~ "2",
    between(month, 6,7)  ~ "early",
    between(month, 8,11)  ~ "late",
    TRUE ~ "1"
  )) %>% 
  group_by(season, year) %>% 
  mutate(xmin = min(date),
         xmax = max(date + 29)) %>% 
  ungroup() %>% 
  select(-series)



# soil moisture
soil_moisture <- read_csv("~/Documents/research/FunCaB/climate/data/VCG_clean_soil_moisture.csv")

soil_moisture <- soil_moisture %>% 
  filter(is.na(flag)) %>%
  filter(!is.na(value)) %>% 
  rename(date_time = date) %>% 
  mutate(date = date(date_time),
         year = year(date),
         month = month(date))


no_site <- soil_moisture %>%
  filter(is.na(siteID)) %>% 
  mutate(new_site = sub("_.*", "", file)) %>% 
  mutate(new_site = sub(" .*", "", new_site)) %>% 
  mutate(new_site = sub("-.*", "", new_site)) %>% 
  mutate(new_site = case_when(
    new_site == "ITAS" ~ "Ulvehaugen",
    TRUE ~ new_site)) %>% 
  mutate(new_site = case_when(
    new_site == "Skjellingavatnet" ~ "Skjellingahaugen",
    new_site == "Skjeldingahaugen" ~ "Skjellingahaugen",
    new_site == "Ulvhaugen" ~ "Ulvehaugen",
    TRUE ~ new_site
  )) %>%
  mutate(siteID = if_else(!is.na(new_site), new_site, siteID)) %>%
  select(-new_site)

soil_moisture <- soil_moisture %>% 
  filter(!is.na(siteID)) %>% 
  full_join(no_site) %>%
  group_by(date, year, month, siteID) %>% 
  reframe(value = mean(value))



soil_moisture <- soil_moisture %>%
  group_by(siteID) %>% 
  mutate(sm_total_mean = mean(value)) %>% 
  group_by(siteID, year) %>% 
  mutate(sm_annual_mean = mean(value)) %>% 
  group_by(siteID, month, year, sm_annual_mean, sm_total_mean) %>% 
  reframe(soil_moisture_month_mean = mean(value, na.rm = TRUE))




spei_temp_precip_sites <- spei_sites %>% 
  select(-xmin, -xmax) %>% 
  left_join(meantemp) %>% 
  left_join(mintemp) %>% 
  left_join(maxtemp) %>% 
  left_join(soil_moisture)

save(spei_sites, file = "~/OneDrive - University of Bergen/research/FunCaB/Data/secondary/spei_20092019.RData")

save(spei_temp_precip_sites, file = "~/OneDrive - University of Bergen/research/FunCaB/Data/secondary/spei_t_sm_20092019.RData")
write_csv(spei_temp_precip_sites, file = "~/OneDrive - University of Bergen/research/FunCaB/Data/secondary/spei_t_sm_20092019.csv")




####################

subClim <- climate %>% 
  ungroup() %>% 
  mutate(year = year(date), date = date(date), hour = hour(date), day = lubridate::yday(date)) %>%
  filter(year > 2008, 
         logger %in% c("jordf1", "jordf2", "jordfukt2", "nedbor", "sm300 1", "sm300 2", "soil moisture 1", "soil moisture 2", "soil temp", "temp1", "temp2", "temp200cm", "temp30cm", "veg temp", "veg. temp", "rain", "temperature", "temperature2"))

subClim <- subClim %>% 
  left_join(dict_Site, by = c("site" = "v3")) %>% 
  rename(siteID = new)


# long-term daily temperature average
LTtemp <- subClim %>% 
  filter(logger %in% c("temp200cm")) %>% 
  group_by(day, siteID) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(day = day - 1, 
         date = as.Date(day, origin = "2018-01-01")) %>% 
  left_join(weather)

LTtempPlot <- LTtemp %>% 
  filter(tempLevel == 8.5, between(day, 152, 274))

# long-term daily precipitation average
LTprecip <- subClim %>% 
  filter(logger %in% c("jordf1", "jordf2", "jordfukt2", "soil moisture 1", "soil moisture 2", "sm300 2", "sm300 1")) %>% 
  group_by(day, siteID) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(day = day - 1, 
         date = as.Date(day, origin = "2018-01-01")) %>% 
  left_join(weather)

LTprecipPlot <- LTprecip %>% 
  filter(tempLevel == 8.5, between(day, 152, 274))


# 2018 daily precipitation and temperature
mSubClim <- subClim %>% 
  group_by(date, day, logger, siteID, year) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  left_join(weather) %>% 
  ungroup()

soilM <- mSubClim %>% 
  filter(year == 2018, logger %in% c("jordf1", "jordf2", "jordfukt2", "soil moisture 1", "soil moisture 2", "sm300 2", "sm300 1"))



# long-term temperature and precipitation trend

mSubClimPlot <- mSubClim %>%
  mutate(day = day - 1) %>% 
  left_join(LTtemp %>% select(-date, LTval = value)) %>% 
  mutate(tAnom = value - LTval,
         sign = if_else(tAnom > 0, "red", "blue")) %>% 
  filter(logger %in% c("temp200cm"), 
         tempLevel == 8.5,
         dplyr::between(date, ymd("2009-03-01"), ymd("2018-12-31")))



AnnualLTtemp <- LTtemp %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(6,7,8,9)) %>% 
  group_by(siteID) %>% 
  summarise(LTval = mean(value)) %>% 
  ungroup()

AnnualLTprecip <- LTprecip %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(6,7,8,9)) %>%  
  group_by(siteID) %>% 
  summarise(LTPval = mean(value)) %>% 
  ungroup()

LTtemp <- LTtemp %>% 
  mutate(month = month(date),
         monthN = if_else(month %in% c(6,7), "summer", if_else(month %in% c(8,9), "autumn", NA))) %>% 
  filter(!is.na(monthN)) %>% 
  group_by(monthN, siteID) %>% 
  summarise(value = mean(value)) %>% 
  select(LTtval = value, monthN, siteID) %>% 
  ungroup()

LTprecip <- LTprecip %>% 
  mutate(month = month(date),
         monthN = if_else(month %in% c(6,7), "spr",
                          if_else(month %in% c(8,9), "aut", NA))) %>% 
  filter(!is.na(monthN)) %>% 
  group_by(monthN, siteID) %>% 
  summarise(value = mean(value)) %>% 
  select(LTPval = value, monthN, siteID)


## monthly temp anomalies
soilM <- mSubClim %>% 
  filter(logger %in% c("jordf1", "jordf2")) %>% 
  group_by(date, siteID) %>% 
  summarise(soilM = mean(value, na.rm = TRUE))

sm2018 <- SM2018 %>% 
  filter(siteID %in% c("Fauske", "Lavisdalen", "Skjellingahaugen", "Veskre"), 
         Treatment == "C") %>%
  group_by(siteID) %>% 
  summarise(soilM = mean(SM)/100) %>% 
  mutate(year = 2018,
         monthN = case_when(
           siteID == "Fauske" ~ "spr",
           siteID == "Veskre" ~ "spr",
           siteID == "Lavisdalen" ~ "spr",
           siteID == "Skjellingahaugen" ~ "aut"
         ))

# calculate monthly averages for drought and non-drought conditions
monthAv <- mSubClim %>%
  filter(logger == "temp200cm") %>%
  rename(soilT = value) %>% 
  left_join(soilM) %>% 
  gather(soilM, soilT, key = logger, value = value) %>% 
  mutate(month = month(date),
         monthN = if_else(month %in% c(6,7), "spr",
                          if_else(month %in% c(8,9), "aut", ""))) %>% 
  filter(!monthN == "") %>% 
  group_by(monthN, year, siteID, logger) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  spread(key = logger, value = value) %>% 
  left_join(LTtemp) %>% 
  left_join(LTprecip) %>% 
  left_join(sm2018, by = c("siteID", "year", "monthN"), suffix = c("", ".new")) %>% 
  mutate(soilM = if_else(is.na(soilM), soilM.new, soilM)) %>% 
  mutate(tAnom = soilT - LTtval,
         pAnom = soilM - LTPval) %>% 
  select(monthN, year, siteID, tAnom, pAnom, soilT, soilM)


# calculate summer averages
AnnualMonthAv <- mSubClim %>%
  filter(logger == "temp200cm") %>%
  rename(soilT = value) %>% 
  left_join(soilM) %>% 
  gather(soilM, soilT, key = logger, value = value) %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(6,7,8,9)) %>% 
  group_by(year, siteID, logger) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  spread(key = logger, value = value) %>% 
  left_join(AnnualLTtemp) %>% 
  left_join(AnnualLTprecip) %>% 
  mutate(tAnom = soilT - LTval,
         pAnom = soilM - LTPval) %>% 
  select(year, siteID, tAnom, pAnom, soilT, soilM)


#save(AnnualMonthAv, file = "~/Documents/FunCaB/climate/data/AnnualMonthAv.RData")