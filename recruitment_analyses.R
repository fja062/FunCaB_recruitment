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
library("modelr")


# analyses
# READ THIS:
# https://stats.stackexchange.com/questions/396336/r-glmm-for-unbalanced-zero-inflated-data-glmmtmb
# DHARMA vignette -> https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html



# Q 1: we investigated the relationship between recruitment success and drought over several growing seasons in gaps versus closed vegetation (Figure 1). In this first experiment we hypothesise that (i) the presence of neighbouring vegetation will promote recruitment during drought conditions

load(file = "~/OneDrive - University of Bergen/Research/FunCaB/Data/secondary/recruitment_0919_spei.RData")
load(file = "~/OneDrive - University of Bergen/research/FunCaB/Data/secondary/spei_t_sm_20092019.RData")
source("~/Documents/research/FunCaB/climate/weather.R")

# prepare dataset
long_term_seed <- recruitment_0919 %>% 
  distinct(siteID, turfID, blockID, season, year, month, treatment, sumcount) %>%
  left_join(spei_temp_precip_sites %>%  select(-season)) %>% 
  left_join(weather) %>% 
  filter(sumcount < 190) %>% 
  #relevel treatment factor to contrast with intact vegetation in analyses
  mutate(treatment = relevel(as.factor(treatment), ref = "Intact")) %>% 
  # scale explanatory variables
  mutate(sprecip7010 = as.numeric(scale(precip7010/100, scale = FALSE)),
         sprecip0916 = as.numeric(scale(precip0916, scale = FALSE)),
         stemp7010 = as.numeric(scale(temp7010, scale = FALSE)),
         stemp0916 = as.numeric(scale(temp0916, scale = FALSE)),
         stempmean = as.numeric(scale(temp_mean, scale = FALSE)),
         stempmax = as.numeric(scale(temp_max, scale = FALSE)),
         stempmin = as.numeric(scale(temp_min, scale = FALSE)),
         ssmmonthly = as.numeric(scale(soil_moisture_month_mean, scale = FALSE)),
         season = as.factor(season),
         year1 = year-2008)


# exploration
#long_term_seed %>% 
#  ggplot(aes(x = spei, y = total_seedlings, colour = factor#(tempLevel))) +
#  geom_point() + 
#  geom_smooth(method = "lm") +
#  facet_grid(.~Treatment)

recruitment_0919 %>% 
  ggplot(aes(x = sumcount)) +
  geom_density()

# correlation between variables
long_term_seed %>% 
  select(spei:precip7010, -date) %>% 
  cor(use = "complete.obs")

# check correlation
long_term_seed %>% 
  select(-turfID, -blockID) %>% 
  GGally::ggpairs()


# Temperature

mod2 <- brm(sumcount ~ treatment + stemp7010 + sprecip7010 + spei + I(spei^2) + season + treatment:spei + treatment:I(spei^2) + treatment:stemp7010 + stemp7010:spei + stemp7010:I(spei^2) + sprecip7010:treatment + stemp7010:spei:treatment + (1|siteID/blockID),
            chains = 2,
            cores = 4,
            family = negbinomial(), 
            iter = 2500, warmup = 1000,
            prior = priors,
            data = long_term_seed)
#Uninformative priors for 1-6 (zero-centred normal distribution with low precision parameterisation: 0.001) and r were assumed (wide gamma distribution with low shape and scale parameterisation: 0.001 for both parameters). The specification for the random intercepts of Nsite,i and Nsiteblock,i were assumed to follow a normal distribution: Nsite ~ dnorm(0, τ) where  had an uninformative gamma distribution (with both the shape and scale parameters set to 0.001) prior specification.

priors <- c(prior(normal(0, 10), class = b, coef = IspeiE2),
           prior(normal(0, 10), class = b, coef = seasonlate),
           prior(normal(0, 10), class = b, coef = spei),
           prior(normal(0, 10), class = b, coef = sprecip7010),
           prior(normal(0, 10), class = b, coef = stemp7010),
           prior(normal(0, 10), class = b, coef = `stemp7010:IspeiE2`),
           prior(normal(0, 10), class = b, coef = `stemp7010:spei`),
           prior(normal(0, 10), class = b, coef = treatmentGap),
           prior(normal(0, 10), class = b, coef = `treatmentGap:IspeiE2`),
           prior(normal(0, 10), class = b, coef = `treatmentGap:spei`),
           prior(normal(0, 10), class = b, coef = `treatmentGap:sprecip7010`),
           prior(normal(0, 10), class = b, coef = `treatmentGap:stemp7010`),
           prior(normal(0, 10), class = b, coef = `treatmentGap:stemp7010:spei`)
           )


default_prior(sumcount ~ treatment + stemp7010 + sprecip7010 + spei + I(spei^2) + season + treatment:spei + treatment:I(spei^2) + treatment:stemp7010 + stemp7010:spei + stemp7010:I(spei^2) + sprecip7010:treatment + stemp7010:spei:treatment + (1|siteID/blockID),
               chains = 2,
               cores = 4,
               family = negbinomial(), 
               iter = 2500, warmup = 1000,
               data = long_term_seed)

summary(mod2)
plot(mod2)
pp_check(mod2)
pp_check(mod2, type = "dens_overlay_grouped", ndraws = 100, group = "treatment")
pp_check(mod2, ndraws = 100, type = "ecdf_overlay")
plot(conditional_effects(mod2), points = TRUE, ask = FALSE)
conditions <- make_conditions(mod2, "treatment")
plot(conditional_effects(mod2, "stemp7010:spei", conditions = conditions, select_points = 0.75), points = TRUE, theme = theme_ggeffects())

#ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1a_long_term_seed_spei.jpg", dpi = 300, height = 4, width = 7)



preds_long <- long_term_seed %>%
    #group_by(treatment) %>%
  data_grid(treatment, season, stemp7010 = seq_range(stemp7010, n = 50), sprecip7010, spei = c(-1.2, 0 , 1.2)) %>% 
    add_epred_draws(mod2, re_formula = NA)

preds_long %>% 
  ggplot(aes(x = stemp7010, y = sumcount, color = ordered(spei), fill = ordered(spei))) +
    stat_lineribbon(aes(y = .epred), .width = c(.95, .80, .50), alpha = 1/4) +
  facet_grid(.~treatment) +
    #geom_point(data = long_term_seed) +
    theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(colour = "Drought Index (SPEI)", y = "Seedling count", x = "Temperature (ºC)", fill = "Drought Index (SPEI)")

#ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1abc_long_term_seed_spei.jpg", dpi = 300, height = 3.5, width = 7)


long_term_seed %>%
  data_grid(treatment, season, stemp7010, spei) %>% 
  add_predicted_draws(mod2, re_formula = NA) %>%
  ggplot(aes(y = treatment, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = sumcount), data = long_term_seed) +
  scale_color_brewer()

long_term_seed %>%
  data_grid(treatment, season, stemp7010, spei) %>% 
  add_epred_draws(mod2, re_formula = NA) %>%
  pivot_longer(treatment:spei, names_to = "explanatories", values_to = "value", values_transform = as.character) %>% 
  ggplot(aes(x = .epred, y = explanatories)) +
  stat_pointinterval(.width = c(.66, .95))

#extract coefficient names
vars_for_coefs <- get_variables(mod2) %>% 
  as_tibble() %>% 
  filter(grepl("b_", value))

# gather draws to make coefficients plot
gather_draws(mod2, 
   #`b_Intercept`,                                    
   `b_treatmentGap`,                                  
   `b_stemp7010`,                                 
   `b_sprecip7010`,                                   
   `b_spei`,                                  
   `b_IspeiE2`,                                       
   `b_seasonlate`,                                    
   `b_treatmentGap:spei`,                             
   `b_treatmentGap:IspeiE2`,                          
   `b_treatmentGap:stemp7010`,                        
   `b_stemp7010:spei`,                       
   `b_stemp7010:IspeiE2`,                             
   `b_treatmentGap:sprecip7010`,                      
   `b_treatmentGap:stemp7010:spei`) %>%  
  mutate(var = str_split(.variable, "_", simplify = TRUE)[, 2]) %>% 
  mutate(var = str_replace_all(var, ":", " x ")) %>% 
  ggplot(aes(x = reorder(var, .value), y = .value)) +
  geom_vline(xintercept = 0, colour = "grey80") +
  stat_halfeye() +
  theme_sjplot() +
  labs(y = "Coefficients", x = "") +
  coord_flip()

#ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1abcd_long_term_seed_spei.jpg", dpi = 300, height = 5, width = 7)


# SPEI
glmer1 <- glmmTMB(sumcount ~ treatment + stemp7010 + spei + I(spei^2) + season + treatment:spei + treatment:stemp7010 + season:spei + stemp7010:spei + (1|siteID/blockID),
                  ziformula = ~ 1,
                  family = nbinom1, 
                  data = long_term_seed)

summary(glmer1)
res <- simulateResiduals(glmer1, n = 500, plot = TRUE)

testDispersion(res)
plotResiduals(res, form = long_term_seed$treatment)
plotResiduals(res, form = long_term_seed$spei)
plotResiduals(res, form = long_term_seed$temp_max)
plotResiduals(res, form = long_term_seed$season)

testZeroInflation(res)
testOutliers(res, type = "bootstrap")

ggpredict(glmer1, terms = c("stemp7010", "spei")) %>% 
  plot()

ggpredict(glmer1, terms = c("spei", "stemp7010[-2, 0, 2]", "treatment")) %>%
  plot()


plot_model(glmer1, vline.color = "grey", title = "", 
           #axis.labels = c("Treatment x SPEI x temperature", "Treatment x SPEI x round", "SPEI x temperature", "SPEI x round", "Treatment x round", "Treatment x temperature", "Treatment x SPEI", "round", "temperature", "SPEI", "Treatment"), 
           transform = "exp", show.values = TRUE, value.offset = 0.3, sort.est = TRUE, show.zeroinf = FALSE) +
  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1b_long_term_seed_temp.jpg", dpi = 300, height = 5, width = 7)

predictions_1 <- predict_response(glmer1, back_transform = TRUE, type = "fe", terms = c("spei", "stemp7010[2,0,-2]", "treatment"))
print(predictions_1, collapse_table = TRUE)

#sjplot_pal(pal = "warm")
ggplot(predictions_1, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(x = "SPEI", y = "seedling count", colour = "Temperature (ºC)", fill = "Temperature (ºC)")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1_long_term_seed_temp.jpg", dpi = 300, height = 5, width = 10)

# output
tidy(glmer1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(across(.cols = 4:9, ~round(., 3))) %>% 
  write_csv(file = "~/Documents/research/FunCaB/bioticInteractions/recruitment/results/glmer1.csv")


##################
#### max temperature ####
##################

mod3 <- brm(sumcount ~ treatment + stemp7010 + sprecip7010 + stempmax + I(stempmax^2) + stempmin + I(stempmin^2) + stempmax:treatment + stempmin:treatment + I(stempmin^2):treatment + (1|siteID/blockID),
            chains = 2,
            cores = 4,
            family = negbinomial(), 
            iter = 2500, warmup = 1000,
            prior = priors,
            data = long_term_seed)

default_prior(sumcount ~ treatment + stemp7010 + sprecip7010 + stempmax + I(stempmax^2) + stempmin + I(stempmin^2) + stempmax:treatment + I(stempmax^2):treatment + stempmin:treatment + I(stempmin^2):treatment + (1|siteID/blockID),
              chains = 2,
              cores = 4,
              family = negbinomial(), 
              iter = 2500, warmup = 1000,
              #prior = priors,
              data = long_term_seed)
#Uninformative priors for 1-6 (zero-centred normal distribution with low precision parameterisation: 0.001) and r were assumed (wide gamma distribution with low shape and scale parameterisation: 0.001 for both parameters). The specification for the random intercepts of Nsite,i and Nsiteblock,i were assumed to follow a normal distribution: Nsite ~ dnorm(0, τ) where  had an uninformative gamma distribution (with both the shape and scale parameters set to 0.001) prior specification.

priors <- c(prior(normal(0, 10), class = b, coef = sprecip7010),
            prior(normal(0, 10), class = b, coef = stemp7010),
            prior(normal(0, 10), class = b, coef = `stempmax`),
            prior(normal(0, 10), class = b, coef = `IstempmaxE2`),
            prior(normal(0, 10), class = b, coef = `stempmin`),
            prior(normal(0, 10), class = b, coef = `IstempminE2`),
            prior(normal(0, 10), class = b, coef = treatmentGap),
            prior(normal(0, 10), class = b, coef = `treatmentGap:IstempminE2`),
            prior(normal(0, 10), class = b, coef = `treatmentGap:stempmax`),
            prior(normal(0, 10), class = b, coef = `treatmentGap:stempmin`)
)

summary(mod3)
plot(conditional_effects(mod3), points = TRUE, ask = FALSE)
plot(mod3, ask = FALSE)
pp_check(mod3)
pp_check(mod3, type = "dens_overlay_grouped", ndraws = 100, group = "treatment")
pp_check(mod3, ndraws = 100, type = "ecdf_overlay")

get_variables(mod3)
mod3 %>% gather_draws(
#`b_Intercept`,
`b_treatmentGap`,                                 
`b_stemp7010`,
`b_sprecip7010`,                                
`b_stempmax`, 
`b_IstempmaxE2`,
`b_stempmin`, 
`b_IstempminE2`,
`b_treatmentGap:stempmax`,
`b_treatmentGap:stempmin`,        
`b_treatmentGap:IstempminE2`
  ) %>%  
  mutate(var = str_split(.variable, "_", simplify = TRUE)[, 2]) %>% 
  mutate(var = str_replace_all(var, ":", " x ")) %>% 
  ggplot(aes(x = reorder(var, .value), y = .value)) +
  geom_vline(xintercept = 0, colour = "grey80") +
  stat_halfeye() +
  theme_sjplot() +
  labs(y = "Coefficients", x = "") +
  coord_flip()


#ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_2b_long_term_seed_maxmin.jpg", dpi = 300, height = 5, width = 7)



preds_long <- long_term_seed %>%
  #group_by(treatment) %>%
  data_grid(treatment, stempmax = seq_range(stempmax, n = 10), stempmin = seq_range(stempmin, n = 10), stemp7010, sprecip7010) %>% 
  add_epred_draws(mod3, re_formula = NA)

preds_long %>% 
  ggplot(aes(x = stemp7010, y = sumcount, color = ordered(stempmax), fill = ordered(stempmax))) +
  stat_lineribbon(aes(y = .epred), .width = c(.95, .80, .50), alpha = 1/4) +
  facet_grid(.~treatment) +
  #geom_point(data = long_term_seed) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(colour = "Maximum temperature (ºC)", y = "Seedling count", x = "Temperature (ºC)", fill = "Maximum temperature (ºC)")

#ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1abc_long_term_seed_spei.jpg", dpi = 300, height = 3.5, width = 7)




glmer2 <- glmmTMB(sumcount ~ treatment + stempmax + I(stempmax^2) + spei + I(spei^2) + season + treatment:stempmax + stempmax:spei + stempmax:I(spei^2) + (1|siteID/blockID),
                  ziformula = ~ treatment,
                  family = nbinom1, data = long_term_seed)




summary(glmer2)
res <- simulateResiduals(glmer2, n = 500, plot = TRUE)

testDispersion(res)
plotResiduals(res, form = long_term_seed$treatment)
plotResiduals(res, form = long_term_seed$spei)
plotResiduals(res, form = long_term_seed$temp_max)
plotResiduals(res, form = long_term_seed$season)

testZeroInflation(res)
testOutliers(res, type = "bootstrap")

ggpredict(glmer2, terms = c("stempmax[all]", "treatment")) %>% 
  plot()

glmer1_predictions <- predict((glmer1)) %>% as_tibble()
ggpredict(glmer1, terms = c("spei", "stemp7010[-2, 0, 2]", "treatment")) %>%
  plot()


plot_model(glmer2, vline.color = "grey", title = "", 
           #axis.labels = c("Treatment x SPEI x temperature", "Treatment x SPEI x round", "SPEI x temperature", "SPEI x round", "Treatment x round", "Treatment x temperature", "Treatment x SPEI", "round", "temperature", "SPEI", "Treatment"), 
           transform = "exp", show.values = TRUE, value.offset = 0.3, sort.est = TRUE, show.zeroinf = FALSE) +
  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1cb_long_term_seed_maxtemp.jpg", dpi = 300, height = 5, width = 7)

predictions_1 <- predict_response(glmer2, back_transform = TRUE, type = "fe", terms = c("stempmax[all]", "treatment"))
print(predictions_1, collapse_table = TRUE)

#sjplot_pal(pal = "warm")
ggplot() +
  geom_line(data = predictions_1, aes(x = x, y = predicted, colour = group)) +
  geom_ribbon(data = predictions_1, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  #facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(x = "Maximum temperature (ºC)", y = "seedling count (N)", colour = "", fill = "") +
  geom_point(data = long_term_seed, aes(x = stempmax, y= sumcount, colour = treatment)) +
  coord_cartesian(ylim = c(0,75)) +
  scale_x_continuous(breaks = c(-5, 0, 5, 10), labels = c(10, 15, 20, 25))

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1c_long_term_seed_maxtemp.jpg", dpi = 300, height = 5, width = 10)

# output
tidy(glmer1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(across(.cols = 4:9, ~round(., 3))) %>% 
  write_csv(file = "~/Documents/research/FunCaB/bioticInteractions/recruitment/results/glmer1.csv")





## min temperature
glmer3 <- glmmTMB(sumcount ~ treatment + stempmin + treatment + I(stempmin^2) + season + spei + stempmin:spei + stempmin:season + treatment:stempmin + (1|siteID/blockID),
                  ziformula = ~ 1,
                  family = nbinom2, data = long_term_seed)

summary(glmer3)
res <- simulateResiduals(glmer3, n = 500, plot = TRUE)

testDispersion(res)
plotResiduals(res, form = long_term_seed$treatment)
plotResiduals(res, form = long_term_seed$spei)
plotResiduals(res, form = long_term_seed$temp_max)
plotResiduals(res, form = long_term_seed$season)

testZeroInflation(res)
testOutliers(res, type = "bootstrap")

ggpredict(glmer3, terms = c("stempmin[all]", "treatment")) %>% 
  plot()

long_term_seed %>% ggplot(aes(x = temp_min, y = sumcount, colour = treatment)) + geom_point() 

plot_model(glmer3, vline.color = "grey", title = "", 
           #axis.labels = c("Treatment x SPEI x temperature", "Treatment x SPEI x round", "SPEI x temperature", "SPEI x round", "Treatment x round", "Treatment x temperature", "Treatment x SPEI", "round", "temperature", "SPEI", "Treatment"), 
           transform = "exp", show.values = TRUE, value.offset = 0.3, sort.est = TRUE, show.zeroinf = FALSE) +  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1db_long_term_seed_maxtemp.jpg", dpi = 300, height = 5, width = 7)

predictions_1 <- predict_response(glmer3, back_transform = TRUE, type = "fe", terms = c("stempmin[all]", "treatment"))
print(predictions_1, collapse_table = TRUE)

#sjplot_pal(pal = "warm")
ggplot() +
  geom_line(data = predictions_1, aes(x = x, y = predicted, colour = group)) +
  geom_ribbon(data = predictions_1, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  #facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(x = "Minimum temperature (ºC)", y = "seedling count (N)", colour = "", fill = "") +
  geom_point(data = long_term_seed, aes(x = stempmin, y= sumcount, colour = treatment)) +
  coord_cartesian(ylim = c(0,75)) +
  scale_x_continuous(breaks = c(-2.5, 0, 2.5, 5), labels = c(6, 8, 10, 12))

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1d_long_term_seed_mintemp.jpg", dpi = 300, height = 5, width = 10)







##########
# second analysis block, this time with precipitation
# Precipitation
##########
sm_long_term_seed <- filter(long_term_seed, !is.na(ssmmonthly))

mod4 <- brm(sumcount ~ treatment + ssmmonthly + spei + sprecip7010 + stemp7010 + I(ssmmonthly^2) + ssmmonthly:treatment + I(ssmmonthly^2):treatment + stemp7010:treatment + ssmmonthly:stemp7010 + I(ssmmonthly^2):stemp7010 + sprecip7010:ssmmonthly + ssmmonthly:spei + ssmmonthly:stemp7010:treatment + I(ssmmonthly^2):stemp7010:treatment + (1|siteID/blockID), #ssmmonthly:sprecip7010:treatment + I(ssmmonthly^2):sprecip7010:treatment + 
            chains = 2,
            cores = 4,
            family = negbinomial(), 
            iter = 2500, warmup = 1000,
            prior = priors,
            data = long_term_seed)

default_prior(sumcount ~ treatment + stemp7010 + sprecip7010 + stempmax + I(stempmax^2) + stempmin + I(stempmin^2) + stempmax:treatment + I(stempmax^2):treatment + stempmin:treatment + I(stempmin^2):treatment + (1|siteID/blockID),
              chains = 2,
              cores = 4,
              family = negbinomial(), 
              iter = 2500, warmup = 1000,
              #prior = priors,
              data = long_term_seed)
#Uninformative priors for 1-6 (zero-centred normal distribution with low precision parameterisation: 0.001) and r were assumed (wide gamma distribution with low shape and scale parameterisation: 0.001 for both parameters). The specification for the random intercepts of Nsite,i and Nsiteblock,i were assumed to follow a normal distribution: Nsite ~ dnorm(0, τ) where  had an uninformative gamma distribution (with both the shape and scale parameters set to 0.001) prior specification.

#### must add the rest of the priors!!! ###
priors <- c(prior(normal(0, 10), class = b, coef = sprecip7010),
            #prior(normal(0, 10), class = b, coef = stemp7010),
            prior(normal(0, 10), class = b, coef = `ssmmonthly`),
            prior(normal(0, 10), class = b, coef = `spei`),
            prior(normal(0, 10), class = b, coef = treatmentGap)
)

summary(mod4)
plot(conditional_effects(mod4), points = TRUE, ask = FALSE)
plot(mod4, ask = FALSE)
pp_check(mod4)
pp_check(mod4, type = "dens_overlay_grouped", ndraws = 100, group = "treatment")
pp_check(mod4, ndraws = 100, type = "ecdf_overlay")

get_variables(mod4)
mod4 %>% gather_draws(
  `b_treatmentGap`,
  `b_ssmmonthly`,
  `b_spei`,
  `b_sprecip7010`,  
  `b_IssmmonthlyE2`,
  `b_treatmentGap:ssmmonthly`,
  `b_treatmentGap:IssmmonthlyE2`,
  `b_treatmentGap:sprecip7010`,
  `b_ssmmonthly:sprecip7010`,
  `b_IssmmonthlyE2:precip7010`,
  `b_ssmmonthly:spei`,
  `b_spei:IssmmonthlyE2`) %>%  
  mutate(var = str_split(.variable, "_", simplify = TRUE)[, 2]) %>% 
  mutate(var = str_replace_all(var, ":", " x ")) %>% 
  ggplot(aes(x = reorder(var, .value), y = .value)) +
  geom_vline(xintercept = 0, colour = "grey80") +
  stat_halfeye() +
  theme_sjplot() +
  labs(y = "Coefficients", x = "") +
  coord_flip()


#ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_2b_long_term_seed_maxmin.jpg", dpi = 300, height = 5, width = 7)



preds_long <- long_term_seed %>%
  #group_by(treatment) %>%
  data_grid(treatment, stemp7010 = seq_range(stemp7010, n = 3), spei = c(-1.2, 0 , 1.2), sprecip7010 = seq_range(sprecip7010, n = 4), ssmmonthly = seq_range(ssmmonthly, n = 50)) %>% 
  add_epred_draws(mod4, re_formula = NA)

preds_long %>% 
  ggplot(aes(x = ssmmonthly, y = sumcount, color = ordered(stemp7010), fill = ordered(stemp7010))) +
  stat_lineribbon(aes(y = .epred), .width = c(.95, .80, .50), alpha = 1/4) +
  facet_grid(.~treatment) +
  #geom_point(data = long_term_seed) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(colour = "Temperature (ºC)", y = "Seedling count", x = "Soil moisture", fill = "Temperature (ºC)")

#ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_1abc_long_term_seed_spei.jpg", dpi = 300, height = 3.5, width = 7)



ggplot(long_term_seed, aes(x = sprecip7010, y = sumcount, colour = treatment)) + geom_point() + facet_grid(.~ season)

glmer4 <- glmmTMB(sumcount ~ treatment + spei + season + sprecip7010 + season:treatment + (1|siteID/blockID), 
                  #ziformula = ~season,
                  family = nbinom1,
                  data = long_term_seed)

summary(glmer4)
res <- simulateResiduals(glmer4, n = 500, plot = TRUE)


testDispersion(glmer2)
plotResiduals(res, form = long_term_seed$treatment)
plotResiduals(res, form = long_term_seed$spei)
plotResiduals(res, form = long_term_seed$sprecip7010)
plotResiduals(res, form = long_term_seed$season)
plotResiduals(res, form = long_term_seed$year)
testZeroInflation(res)
testOutliers(res, type = "bootstrap")


ggpredict(glmer4, terms = c("sprecip7010", "treatment"), back_transform = TRUE) %>% 
  plot()

predictions_2 <- predict_response(glmer4, back_transform = TRUE, type = "fe", terms = c("sprecip7010", "season", "treatment"))
print(predictions_2, collapse_table = TRUE)

ggplot(predictions_2, aes(x = x, y = predicted, colour = facet)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = facet), alpha = 0.3, colour = NA) +
  facet_wrap(~group, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#004D80", "#37848E", "#9BC2B6")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#004D80", "#37848E", "#9BC2B6")) +
  labs(x = "precipitation", y = "seedling count", colour = "Treatment", fill = "Treatment")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_2a_long_term_seed_precip.jpg", dpi = 300, height = 5, width = 10)


plot_model(glmer4, vline.color = "grey", title = "", 
           #axis.labels = c("Treatment x SPEI x round", "SPEI x round", "Treatment x round", "Treatment x precipitation", "Treatment x SPEI", "round", "precipitation", "SPEI", "Treatment"), 
           transform = "exp", show.values = TRUE, value.offset = 0.3, show.zeroinf = FALSE, sort.est = TRUE) +
  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_2b_long_term_seed_precip.jpg", dpi = 300, height = 5, width = 7)

# no interactive effect of treatment and precipitation




# soil moisture
sm_long_term_seed <- filter(long_term_seed, !is.na(ssmmonthly))

ggplot(long_term_seed, aes(x = ssmmonthly, y = sumcount, colour = treatment)) + geom_point() + facet_grid(.~ season)

glmer5 <- glmmTMB(sumcount ~ treatment + spei + season + poly(ssmmonthly, 2) + treatment:poly(ssmmonthly, 2) + (1|siteID/blockID), 
                  #ziformula = ~1,
                  family = nbinom1,
                  data = sm_long_term_seed)

summary(glmer5)
res <- simulateResiduals(glmer5, n = 500, plot = TRUE)


testDispersion(glmer5)
testDispersion(res)
plotResiduals(res, form = sm_long_term_seed$treatment)
plotResiduals(res, form = sm_long_term_seed$spei)
plotResiduals(res, form = sm_long_term_seed$sprecip7010)
plotResiduals(res, form = sm_long_term_seed$season)
plotResiduals(res, form = sm_long_term_seed$year)
testZeroInflation(res)
testOutliers(res, type = "bootstrap")


ggpredict(glmer5, terms = c("ssmmonthly[all]", "treatment"), back_transform = TRUE) %>% 
  plot()

predictions_2 <- predict_response(glmer5, back_transform = TRUE, type = "fe", terms = c("ssmmonthly[all]", "treatment"))
print(predictions_2, collapse_table = TRUE)

ggplot() +
  geom_line(data = predictions_2, aes(x = x, y = predicted, colour = group)) +
  geom_ribbon(data = predictions_2, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
#  facet_wrap(~group, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#004D80", "#37848E", "#9BC2B6")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#004D80", "#37848E", "#9BC2B6")) +
  labs(x = "Monthly soil moisture", y = "seedling count", colour = "Treatment", fill = "Treatment") +
  geom_point(data = long_term_seed, aes(x = ssmmonthly, y = sumcount, colour = treatment)) +
  coord_cartesian(ylim = c(0, 90))
  

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_3a_long_term_seed_soilmoisture.jpg", dpi = 300, height = 5, width = 10)


plot_model(glmer5, vline.color = "grey", title = "", 
           #axis.labels = c("Treatment x SPEI x round", "SPEI x round", "Treatment x round", "Treatment x precipitation", "Treatment x SPEI", "round", "precipitation", "SPEI", "Treatment"), 
           transform = "exp", show.values = TRUE, value.offset = 0.3, show.zeroinf = FALSE, sort.est = TRUE) +
  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_3b_long_term_seed_precip.jpg", dpi = 300, height = 5, width = 7)












##############################################################
##############################################################
##############################################################
##############################################################
##############################################################


# Q2: we tested whether any one particular functional group determines recruitment during drought, and whether this differs according to the local climate. Using a reciprocal removal experiment of three dominant functional groups we hypothesise that ii) during drought the plant functional group biomass will promote seedling abundance because of higher water retention under the canopy.

# load data
load("~/OneDrive - University of Bergen/research/FunCaB/Data/secondary/recruitment_spei_cleaned.csv")



##############################################################
##############################################################
##############################################################
# functional group analyses - NOT USED
##############################################################
##############################################################
##############################################################


#### Seedling abundance ####

recruitment_biomass_spei <- recruitment_biomass_spei %>% 
  mutate(blockID = substr(turfID, 1,4)) %>% 
  group_by(siteID, turfID, year) %>% 
  mutate(annual_count = sum(count)) %>% 
  ungroup() %>% 
  mutate(sprecip7010 = precip7010/1000,
         stemp7010 = temp7010/10)

# bryophytes
# exploration
recruitment_biomass_spei %>% 
  filter(fg_presence == "B", year == 2018) %>% 
  ggplot(aes(x = biomassBryo, y = count, colour = factor(round))) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(year~factor(tempLevel))

rec_test <- recruitment_biomass_spei %>% 
  filter(fg_presence == "B")

rec_test %>% 
  ggplot(aes(x = biomassBryo, y = count, colour = factor(tempLevel))) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, colour = "grey") +
  #coord_cartesian(ylim = c(-20,40)) +
  labs(y = "∂ seedling N (late-early)") +
  theme_bw() +
  facet_grid(~year, scales = "free_y")

#ggsave(filename = "~/Documents/FunCaB/bioticInteractions/recruitment/figures/bryophyte_seed_n.jpg", dpi = 300, height = 5, width = 7)



glmer1_bryo <- recruitment_biomass_spei %>% 
  mutate(sbiomassBryo = biomassBryo/10) %>% 
  filter(fg_presence == "B",
         !is.na(spei),
         year == 2018)


# variable dependence/independence
rms::vif()

# Temperature
glmer3 <- glmmTMB(count ~ sbiomassBryo + round + stemp7010 + round:stemp7010 + (1|siteID/blockID),
                  family = poisson, 
                  data = glmer1_bryo)

performance::check_collinearity(glmer3)
summary(glmer3)
res <- simulateResiduals(glmer3)
plot(res)
plotResiduals(res, glmer1_bryo$spei)
plotResiduals(res, glmer1_bryo$round)
plotResiduals(res, glmer1_bryo$sbiomassBryo)
performance::check_model(glmer3)

testDispersion(res)
testZeroInflation(res)
testQuantiles(res)


tidy(glmer3)
ggpredict(glmer3, terms = c("stemp7010[0.65, 0.85, 1.05]", "round")) %>% 
  plot()

glmer3_predictions <- predict((glmer3)) %>% as_tibble()

predictions_3 <- predict_response(glmer3, back_transform = TRUE, type = "fe", terms = c("stemp7010", "round"))
print(predictions_3, collapse_table = TRUE)

sjplot_pal(pal = "warm")
ggplot(predictions_3, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  #facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
labs(x = "Temperature", y = "seedling count", colour = "round", fill = "round")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_3_bryoophyte_temp.jpg", dpi = 300, height = 5, width = 9)

plot_model(glmer3, vline.color = "grey", title = "", axis.labels = c("temperature x round", "temperature", "round", "biomass"), transform = "exp", show.values = TRUE, value.offset = 0.3) +
  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_3b_bryophyte_temp.jpg", dpi = 300, height = 5, width = 7)


# Precipitation
glmer4 <- glmmTMB(count ~ sbiomassBryo + sprecip7010 + round + sbiomassBryo:sprecip7010 + sbiomassBryo:round + sprecip7010:round + sprecip7010:sbiomassBryo:round + (1|siteID/blockID), family = poisson, data = glmer1_bryo)

summary(glmer4)
res <- simulateResiduals(glmer4)
plot(res)
plotResiduals(res, glmer1_bryo$spei)
plotResiduals(res, glmer1_bryo$round)
plotResiduals(res, glmer1_bryo$sbiomassBryo)

testDispersion(res)
testZeroInflation(res)
testQuantiles(res)


tidy(glmer4)
ggpredict(glmer4, terms = c("sbiomassBryo", "sprecip7010[0.6, 1.2, 2, 2.7]", "round")) %>% 
  plot()
ggpredict(glmer4, terms = c("sbiomassBryo", "round")) %>% 
  plot()

predictions_4 <- predict_response(glmer4, back_transform = TRUE, type = "fe", terms = c("sbiomassBryo", "sprecip7010[0.6, 1.2, 2, 2.7]", "round"))
print(predictions_4, collapse_table = TRUE)

sjplot_pal(pal = "us")
ggplot(predictions_4, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#004D80", "#37848E", "#9BC2B6", "#B5D2C0")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#004D80", "#37848E", "#9BC2B6", "#B5D2C0")) +
  labs(x = "Biomass", y = "seedling count", colour = "Precipitation", fill = "Precipitation")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_4_bryoophyte_precip.jpg", dpi = 300, height = 5, width = 9)

plot_model(glmer4, vline.color = "grey", title = "", axis.labels = c("Biomass x precipitation x round", "precipitation x round", "Biomass x round", "Biomass x precipitation", "round", "precipitation", "Biomass"), transform = "exp", show.values = TRUE, value.offset = 0.3) +
  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_4b_bryophyte_precip.jpg", dpi = 300, height = 5, width = 7)


#  ggplot(aes(x, predicted)) + 
#  geom_line(aes(colour = group), size = 1) + 
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha #= 0.15) + 
#  stat_summary(data = subset(glmer1_bro), aes(sbiomassBryo, #total_seedlings, colour = as.factor(tempLevel)), geom = "point", fun = #"mean", na.rm = TRUE, shape = 21, size = 2) +
#  labs(x = bquote('Biomass '(g/m^2)), y = "", title = "")


################################################################
################################################################
################################################################

# graminoids
#exploration
recruitment_biomass_spei %>% 
  filter(fg_presence == "G",
         year == 2018) %>% 
  ggplot(aes(x = biomassGram, y = count, colour = factor(precipLevel))) +
  geom_point() + 
  facet_grid(year~round) +
  geom_smooth(method = "lm")

rec_test <- recruitment_biomass_spei %>% 
  filter(fgPresence == "G", !is.na(total_seedlings)) %>% 
  distinct(siteID, turfID, year, biomassGram, spei, tempLevel, precipLevel, precip7010, round_py, total_seedlings)

rec_test %>% 
  ggplot(aes(x = biomassGram, y = delta_seed_tot, colour = factor(tempLevel))) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, colour = "grey") +
  coord_cartesian(ylim = c(-20,40)) +
  labs(y = "∂ seedling N (late-early)") +
  theme_bw()

ggsave(filename = "~/Documents/FunCaB/bioticInteractions/recruitment/figures/graminoid_seed_n.jpg", dpi = 300, height = 5, width = 7)


# analyses
glmer1_gra <- recruitment_biomass_spei %>% 
  mutate(sbiomassGram = biomassGram/10) %>% 
  filter(fg_presence == "G",
         !is.na(spei),
         year == 2018)

# temperature
glmer5 <- glmmTMB(count ~ sbiomassGram + stemp7010 + round + sbiomassGram:stemp7010 + round:stemp7010 + (1|siteID), family = nbinom1, data = glmer1_gra)

summary(glmer5)
tidy(glmer5, exponentiate = TRUE)
res <- simulateResiduals(glmer5, plot = TRUE)
plotResiduals(res, glmer1_gra$stemp7010)
plotResiduals(res, glmer1_gra$round)
plotResiduals(res, glmer1_gra$sbiomassGram)

testDispersion(res)
testZeroInflation(res)
testQuantiles(res)

ggpredict(glmer5, terms = c("stemp7010", "round"), type = "fe") %>% 
  plot()

predictions_5 <- predict_response(glmer5, back_transform = TRUE, type = "fe", terms = c("stemp7010", "round"))
print(predictions_5, collapse_table = TRUE)

sjplot_pal(pal = "warm")
ggplot(predictions_5, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  #facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(x = "Temperature", y = "seedling count", colour = "round", fill = "round")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_5_graminoid_temp.jpg", dpi = 300, height = 5, width = 9)


plot_model(glmer5, vline.color = "grey", title = "", axis.labels = c("temperature x round", "Biomass x temperature", "round", "precipitation", "Biomass"), transform = "exp", show.values = TRUE, value.offset = 0.3) +
  theme_sjplot(base_size = 14)

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_4b_bryophyte_precip.jpg", dpi = 300, height = 5, width = 7)


# precipitation
glmer6 <- glmmTMB(count ~ sprecip7010 + (1|siteID/blockID), family = nbinom1, data = glmer1_gra)

summary(glmer6)
res <- simulateResiduals(glmer6, plot = TRUE)
plotResiduals(res, glmer1_gra$spei)
plotResiduals(res, glmer1_gra$round_py)
plotResiduals(res, glmer1_gra$sbiomassGram)

testDispersion(res)
testZeroInflation(res)
testQuantiles(res)


ggpredict(glmer6, terms = c("sprecip7010"), type = "fe") %>% 
  plot()

predictions_6 <- predict_response(glmer6, back_transform = TRUE, type = "fe", terms = c("sprecip7010"))
print(predictions_4, collapse_table = TRUE)

sjplot_pal(pal = "us")
ggplot(predictions_6, aes(x = x, y = predicted)) +
  theme_sjplot() +
  geom_line(colour= "#37848E") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, colour = NA, fill = "#37848E") +
  labs(x = "Precipitation (m)", y = "seedling count")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_6_graminoid_precip.jpg", dpi = 300, height = 5, width = 9)

tidy(glmer6)




################################################################
################################################################
################################################################

# forbs
#exploration
recruitment_biomass_spei %>% 
  filter(fg_presence == "F") %>% 
  ggplot(aes(x = biomassForb, y = count, colour = factor(tempLevel))) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(.~round)

rec_test <- recruitment_biomass_spei %>% 
  filter(fgPresence == "F", !is.na(total_seedlings)) %>% 
  distinct(siteID, turfID, year, biomassForb, spei, tempLevel, precipLevel, precip7010, round_py, total_seedlings) %>% 
  group_by(year, siteID, turfID, biomassForb, tempLevel, precipLevel, precip7010) %>%
  summarise(delta_seed_tot = diff(total_seedlings)) %>%
  ungroup()

rec_test %>% 
  ggplot(aes(x = biomassForb, y = count, colour = factor(tempLevel))) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, colour = "grey") +
  coord_cartesian(ylim = c(-50,50)) +
  labs(y = "∂ seedling N (late-early)") +
  theme_bw()

ggsave(filename = "~/Documents/FunCaB/bioticInteractions/recruitment/figures/forb_seed_n.jpg", dpi = 300, height = 5, width = 7)


# analyses
glmer1_forb <- recruitment_biomass_spei %>% 
  mutate(sbiomassForb = biomassForb/100) %>% 
  filter(fg_presence == "F",
         !is.na(spei))


# Temperature
glmer7 <- glmmTMB(count ~ sbiomassForb + spei + stemp7010 + sbiomassForb:spei + spei:stemp7010 + sbiomassForb:round + spei:round + round +  (1|siteID/blockID), family = nbinom1, data = glmer1_forb)

summary(glmer7)

res <- simulateResiduals(glmer7, plot = TRUE)
plot(res)
plotResiduals(res, glmer1_forb$spei)
plotResiduals(res, glmer1_forb$round)
plotResiduals(res, glmer1_forb$sbiomassBryo)

testDispersion(res)
testZeroInflation(res)
testQuantiles(res)


ggpredict(glmer7, terms = c("spei", "round"), type = "fe") %>% 
  plot()

predictions_7 <- predict_response(glmer7, back_transform = TRUE, type = "fe", terms = c("spei", "round"))
print(predictions_7, collapse_table = TRUE)

sjplot_pal(pal = "warm")
ggplot(predictions_7, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  #facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(x = "SPEI", y = "seedling count", colour = "round", fill = "round")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_7_forb_temp.jpg", dpi = 300, height = 5, width = 9)


# Precipitatoin
glmer8 <- glmmTMB(count ~ sbiomassForb + sprecip7010 + spei + sprecip7010:spei + (1|siteID/blockID), family = nbinom1, data = glmer1_forb)

summary(glmer8)

res <- simulateResiduals(glmer8, plot = TRUE)
plot(res)
plotResiduals(res, glmer1_forb$spei)
plotResiduals(res, glmer1_forb$round)
plotResiduals(res, glmer1_forb$sbiomassBryo)

testDispersion(res)
testZeroInflation(res)
testQuantiles(res)


ggpredict(glmer8, terms = c("spei", "sprecip7010"), type = "fe") %>% 
  plot()

predictions_8 <- predict_response(glmer8, back_transform = TRUE, type = "fe", terms = c("spei", "sprecip7010"))
print(predictions_7, collapse_table = TRUE)

sjplot_pal(pal = "warm")
ggplot(predictions_7, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, colour = NA) +
  #facet_wrap(~facet, ncol = 2) +
  theme_sjplot() +
  scale_color_manual(values = c("#664458", "#C45B46", "#F1B749")) + #sjplot colour palette "warm"
  scale_fill_manual(values = c("#664458", "#C45B46", "#F1B749")) +
  labs(x = "SPEI", y = "seedling count", colour = "round", fill = "round")

ggsave(filename = "~/Documents/research/FunCaB/bioticInteractions/recruitment/figures/Figure_7_forb_temp.jpg", dpi = 300, height = 5, width = 9)
