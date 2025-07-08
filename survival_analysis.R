# libraries
#library("rjags")
#library("R2jags")
library("tidyverse")
library("tidybayes")
library("DHARMa")
library("lme4")
library("glmmTMB")
library("broom.mixed")
library("sjPlot")
library("ggeffects")
library("performance")
library("brms")
library("rstan")

# load data


# analyses
# READ THIS:
# https://stats.stackexchange.com/questions/396336/r-glmm-for-unbalanced-zero-inflated-data-glmmtmb
# DHARMA vignette -> https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

# These options help Stan run faster:
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# for colour palettes:
show_sjplot_pals()

survival_1819 <- seedling_counts_complete %>% 
  select(siteID, blockID, turfID, seedID, treatment, `1`:`4`) %>%
  pivot_longer(`1`:`4`, names_to = "season", values_to = "count") %>% 
  mutate(year = case_when(
    season %in% c("1", "2") ~ 2018,
    season %in% c("3", "4") ~ 2019
    ),
    season = case_when(
      season %in% c("1", "3") ~ "early",
      season %in% c("2", "4") ~ "late"
    ))


lost_seedlings <- seedling_counts_complete %>% anti_join(survival_1819) %>% 
  select(-c(site:`4`), -count, -date, -month) %>% 
  rename(count = sum)
  
seasons <- recruitment_dates %>% 
  mutate(month = month(date),
         year = year(date),
         season = case_when(
           grepl("spr", season) ~ "early",
           grepl("aut", season) ~ "late",
           TRUE ~ season
         ))

survival_1819 <- survival_1819 %>% 
  left_join(lost_seedlings) %>% 
  left_join(seasons %>% select(-date)) %>% 
  mutate(month = case_when(
    season == "early" ~ 7,
    season == "late" ~ 8,
    TRUE ~ month
  ))


survival_0912 <- rtc_counts %>% 
  select(siteID, blockID, turfID, seedID = ID, treatment, spr_12:aut_09) %>% 
  pivot_longer(spr_12:aut_09, names_to = "season", values_to = "count") %>% 
  mutate(year = case_when(
    grepl("09", season) ~ 2009,
    grepl("10", season) ~ 2010,
    grepl("11", season) ~ 2011,
    grepl("12", season) ~ 2012,
  ),
  season = case_when(
    grepl("spr", season) ~ "early",
    grepl("aut", season) ~ "late"
  ), 
  month = case_when(
    season == "early" ~ 7,
    season == "late" ~ 8
  ))


combined_survival <- full_join(survival_0912, survival_1819) %>% 
  mutate(date = my(paste(month, year, sep = "-"))) %>% 
  select(-month, -year) %>% 
  mutate(censor = as.factor(case_when(
    count == 1 & date == "2012-07-01" ~ "right",
    count == 1 & date == "2019-08-01" ~ "right",
    TRUE ~ "interval"
  ))) %>% 
  group_by(siteID, blockID, turfID, seedID, treatment) %>% 
  mutate(survival_duration = sum(count)) %>% 
  left_join(spei_temp_precip_sites %>% select(siteID, date, spei, tempmean = temp_mean, tempmax = temp_max, tempmin = temp_min, smmonthly = soil_moisture_month_mean))
  
last_dead <- combined_survival %>% 
  mutate(mean_spei = mean(spei)) %>% 
  arrange(siteID, blockID, turfID, seedID, date) %>%
  group_by(siteID, blockID, turfID, seedID) %>% 
  filter((count==0 & lag(count)==1) | (count == 1 & date %in% c("2019-08-01", "2012-07-01")))


combined_survival <- combined_survival %>% 
  filter(!count == 0) %>% 
  mutate(mean_spei = mean(spei)) %>% 
  arrange(siteID, blockID, turfID, seedID, date) %>%
  group_by(siteID, blockID, turfID, seedID)

first_germ <- combined_survival %>%
  slice(1) %>% select(-censor)

last_germ <- combined_survival %>%
  slice(n())


months_til_dead <- last_dead %>% 
  left_join(first_germ, join_by(siteID, blockID, treatment, turfID, seedID), suffix = c("_last", "_first")) %>% 
  mutate(months_til_dead = as.period(interval(start = date_first, end = date_last), unit = "months")) %>% 
  select(siteID:treatment, months_til_dead)

# pivot longer to gather first and last occurrence details for each seedlingc        
combined_survival <- full_join(first_germ, last_germ, join_by(siteID, blockID, treatment, turfID, seedID, count, survival_duration, mean_spei), suffix = c("_first", "_last")) %>% 
  filter(count < 2) %>% 
  mutate(months_til_alive = as.period(interval(start = date_first, end = date_last), unit = "months")) %>% 
  left_join(months_til_dead) %>% 
  mutate(months_til_dead = time_length(months_til_dead, unit = "months"),
         months_til_alive = time_length(months_til_alive, unit = "months"))

combined_survival <- combined_survival %>% 
  distinct(across(everything())) %>% 
  select(-count) %>% 
  pivot_longer(c(season_first:smmonthly_last, -survival_duration, -mean_spei, -censor),
               names_to = c("Var", ".value"),
               names_sep = "_",values_transform = as.character) %>% 
  pivot_longer(first:last,
               names_to = "occurrence", values_to = "value") %>% 
  pivot_wider(values_from = value, names_from = Var) %>% 
  mutate(spei = as.numeric(spei), 
         tempmin = as.numeric(tempmin), 
         tempmax = as.numeric(tempmax), 
         tempmean = as.numeric(tempmean), 
         smmonthly = as.numeric(smmonthly))

combined_survival_weather <- combined_survival %>% 
  left_join(weather) %>% 
  ungroup()

combined_survival_weather <- combined_survival_weather %>% 
  dplyr::mutate(sprecip7010 = as.numeric(scale(precip7010/1000, scale = FALSE)),
         sprecip0916 = as.numeric(scale(precip0916/1000, scale = FALSE)),
         stemp7010 = as.numeric(scale(temp7010, scale = FALSE)),
         stemp0916 = as.numeric(scale(temp0916, scale = FALSE)),
         stempmean = as.numeric(scale(tempmean, scale = FALSE)),
         stempmax = as.numeric(scale(tempmax, scale = FALSE)),
         stempmin = as.numeric(scale(tempmin, scale = FALSE)),
         ssmmonthly = as.numeric(scale(smmonthly, scale = FALSE)),
         treatment = relevel(as.factor(treatment), ref = "Intact"))

combined_survival_weather <- filter(combined_survival_weather, months_til_alive<=months_til_dead)

########################################
########### figures ####################

# older seedlings tend to appear during drier conditions, and die during wetter conditions
combined_survival_weather %>% 
  ggplot(aes(x = spei, y = months_til_dead, fill = occurrence, colour = occurrence)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = "glm", formula = "y ~ poly(x, 2)") +
  facet_grid(.~treatment) +
  scale_color_manual(values = c("#C45B46", "#F1B749", "#664458")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#C45B46", "#F1B749", "#664458")) +
  scale_shape_manual(values = c(21, 22)) + #sjplot colour palette "warm"
  theme_ggeffects()




combined_survival %>% 
  filter(occurrence == "last") %>% 
  ggplot(aes(x = mean_spei, y = months_til_dead, colour = treatment)) +
  geom_point(shape = 21, position = position_jitter(width = 0.05), alpha = 0.3) +
  geom_smooth(method = "glm", formula = 'y ~ poly(x, 2)') +
  scale_color_manual(values = c("#C45B46", "#F1B749", "#664458")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#C45B46", "#F1B749", "#664458")) + #sjplot colour palette "warm"
  theme_ggeffects()


## analysis:


combined_survival_weather %>% 
  filter(occurrence == "last") %>% 
  ggplot(aes(x = mean_spei, y = months_til_dead, colour = treatment, fill = treatment)) +
  geom_point(shape = 21, position = position_jitter(width = 0.05), alpha = 0.3) +
  geom_smooth(method = "glm", formula = "y ~ poly(x, 2)") +
  scale_color_manual(values = c("#C45B46", "#F1B749", "#664458")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#C45B46", "#F1B749", "#664458")) + #sjplot colour palette "warm"
  theme_ggeffects() +
  facet_wrap(.~tempLevel)

test_dataset <- combined_survival_weather %>%
  filter(occurrence == "last")

mod1 <- brm(months_til_alive|cens(censor, months_til_dead) ~ poly(mean_spei, 2) + (1|siteID),
            family = negbinomial(),
            control = list(adapt_delta = 0.99),
            cores = 3,
            chains = 2,
            data = test_dataset,
            iter = 1000, warmup = 500)


summary(mod1)
plot(mod1)
pp_check(mod1, type = "dens_overlay_grouped", ndraws = 100, group = "spei")
pp_check(mod1, ndraws = 100, type = "ecdf_overlay")
plot(conditional_effects(mod1), points = TRUE, ask = FALSE)

glmer_surv <- glmer(survival_duration ~ mean_spei + treatment + stemp7010 + sprecip7010 + treatment:mean_spei + treatment:stemp7010 + (1|siteID), family = "compois", data = combined_survival_weather)

glmer_surv <- lmer(survival_duration ~ mean_spei + poly(mean_spei, 2) + treatment + stemp7010 + sprecip7010 + treatment:mean_spei + treatment:poly(mean_spei, 2) + (1|blockID), data = combined_survival_weather)


glmer_surv <- glmmTMB(survival_duration ~ poly(mean_spei, 2) + treatment + stemp7010 + sprecip7010 + treatment:poly(mean_spei, 2) + treatment:stemp7010 + (1|siteID/blockID),
                      ziformula = ~1,
                      family = "compois",
                      data = test_dataset)

summary(glmer_surv)
res <- simulateResiduals(glmer_surv, n = 500, plot = TRUE)
testZeroInflation(res)
testDispersion(res)

predictions_precip <- ggpredict(glmer_surv, terms = c("mean_spei", "sprecip7010", "treatment"))

predictions_temp <- ggpredict(glmer_surv, terms = c("mean_spei[all]", "stemp7010[all]", "treatment")) %>% 
  as_tibble() %>% 
  rename(mean_spei = x, treatment = facet, stemp7010 = group, survival_duration = predicted) %>% 
  mutate(stemp7010 = as.numeric(as.character(stemp7010)))

plot(predictions_precip)
ggpredict(glmer_surv, terms = c("mean_spei", "stemp7010", "treatment")) %>%
  plot()

plot_model(glmer_surv)
test_predictions(glmer_surv)

combined_survival_weather %>% 
  ggplot(aes(x = mean_spei, y = survival_duration, colour = factor(stemp7010), fill = factor(stemp7010))) +
  geom_point() +
  geom_line(data = predictions_temp, aes(y = survival_duration)) +
  facet_grid(.~ treatment) +
  theme_ggeffects()


glmer_surv1 <- glmer(survival_duration ~ spei + I(spei^2) + occurrence + treatment + I(spei^2):occurrence + spei:occurrence + treatment:spei + treatment:I(spei^2) + (1|siteID/blockID), family = poisson, data = combined_survival_weather)

summary(glmer_surv1)
res <- simulateResiduals(glmer_surv1, n = 500, plot = TRUE)

ggpredict(glmer_surv1, terms = c("spei", "treatment", "occurrence")) %>%
  plot()
