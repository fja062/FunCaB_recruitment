# data analysis plan
analysis_plan <- list(

  tar_target(
    name = bayes_model,
    command = fit_bayesian_model(data_prepped)
  ),
  
  tar_target(
    name = bayes_model_summary,
    command = broom.mixed::tidy(bayes_model, effects = "fixed")
  ),
  
  tar_target(
    name = pp_check_plot,
    command = make_pp_check_plot(bayes_model)
  ),
  
  tar_target(
    name = bayes_diag_plots,
    command = make_diagnostics_plots(bayes_model)
  ),
  
  tar_target(
    name = model_predictions,
    command = make_predictions(bayes_model, data_prepped)
  )

#  tar_target(
#    name = recruitment_analysis,
#    command = {
#      # prepare data
#      dat <- combined_recruitment |> 
#        group_by(siteID, blockID, plotID, season, year, month, fg_removed, temperature_level, precipitation_level, spei) |> 
#        summarise(count = sum(count), .groups = "drop")
#        left_join(spei_raw %>%  select(-season)) %>% 
#        left_join(weather) %>% 
#        filter(sumcount < 190) %>% 
#        #relevel treatment factor to contrast with intact vegetation in analyses
#        mutate(treatment = relevel(as.factor(treatment), ref = "Intact")) %>% 
#        # scale explanatory variables
#        mutate(sprecip7010 = as.numeric(scale(precip7010/100, scale = FALSE)),
#               sprecip0916 = as.numeric(scale(precip0916, scale = FALSE)),
#               stemp7010 = as.numeric(scale(temp7010, scale = FALSE)),
#               stemp0916 = as.numeric(scale(temp0916, scale = FALSE)),
#               stempmean = as.numeric(scale(temp_mean, scale = FALSE)),
#               stempmax = as.numeric(scale(temp_max, scale = FALSE)),
#               stempmin = as.numeric(scale(temp_min, scale = FALSE)),
#               ssmmonthly = as.numeric(scale(soil_moisture_month_mean, scale = FALSE)),
#               season = as.factor(season),
#               year1 = year-2008)
#      
#    }
    
    
  )
#  ## 
#  
#  # functional group richness
#  tar_target(
#    name = fg_richness_analysis,
#    command = {
#      # prepare data
#      dat <- standing_biomass_22 |>
#        # sum biomass by fg_removed and remaining
#        group_by(siteID, blockID, plotID, fg_removed, fg_remaining, fg_richness, fg_status, temperature_level, precipitation_level, #temperature_scaled, precipitation_scaled) |>
#        summarise(standing_biomass = sum(biomass), .groups = "drop")
#      
#      # compare full vs 2-way model
#      results <- compare_full_vs_2way_lmer(
#        data = dat,
#        response = "standing_biomass",
#        predictor = "fg_richness"
#      )
#    }
#  ),
#  tar_target(
#    name = fg_richness_tidy,
#    command = clean_model_terms(tidy_model(fg_richness_analysis$model_2way))
#  ),
#  
#  # functional group identity
#  tar_target(
#    name = fg_identity_analysis,
#    command = {
#      # compare full vs 2-way model
#      results <- compare_full_vs_2way_lmer(
#        data = standing_biomass_22,
#        response = "biomass",
#        # should this be fg_removed x fg_richness. Or should it be identidy (fg_removed) x removed_fg?
#        predictor = "fg_removed"
#      )
#    }
#  ),
#  tar_target(
#    name = fg_identity_tidy,
#    command = clean_model_terms(tidy_model(fg_identity_analysis$model_2way))

#  ),
  
#  
#  # PART 2: Single group effects
#  # Graminoids present
#  
#  # Create a long dataset for each focal functional group present in fg_remaining
#  tar_target(
#    name = standing_biomass_by_focal_fg,
#    command = {
#      fgs <- c("G" = "graminoids", "F" = "forbs", "B" = "bryophytes")
#      purrr::map_dfr(names(fgs), function(fg_code) {
#        standing_biomass_22 %>%
#          filter(str_detect(fg_remaining, fg_code)) %>%
#          mutate(focal_fg = fgs[[fg_code]]) %>%
#          group_by(siteID, blockID, plotID, fg_removed, fg_remaining, fg_richness, fg_status, temperature_level, precipitation_level, #temperature_scaled, precipitation_scaled, focal_fg) %>%
#          summarise(standing_biomass = sum(biomass), .groups = "drop")
#      })
#    }
#  ),
#  
#  # functional group richness by focal group (graminoids, forbs, bryophytes)
#  tar_target(
#    name = fg_richness_focal_analysis,
#    command = {
#      split_data <- split(standing_biomass_by_focal_fg, standing_biomass_by_focal_fg$focal_fg)
#      purrr::imap(split_data, function(dat, fg) {
#        compare_full_vs_2way_lmer(
#          data = dat,
#          response = "standing_biomass",
#          predictor = "fg_richness"
#        )
#      })
#    }
#  ),
#  
#  tar_target(
#    name = fg_richness_focal_tidy,
#    command = {
#      purrr::imap_dfr(fg_richness_focal_analysis, function(res, fg) {
#        clean_model_terms(tidy_model(res$model_2way)) %>%
#          dplyr::mutate(focal_fg = fg)
#      })
#    }
#  ),
#  
#  # functional group identity by focal group (graminoids, forbs, bryophytes)
#  tar_target(
#    name = fg_identity_focal_analysis,
#    command = {
#      split_data <- split(standing_biomass_by_focal_fg, standing_biomass_by_focal_fg$focal_fg)
#      purrr::imap(split_data, function(dat, fg) {
#        compare_full_vs_2way_lmer(
#          data = dat,
#          response = "standing_biomass",
#          predictor = "fg_removed"
#        )
#      })
#    }
#  ),
#  
#  tar_target(
#    name = fg_identity_focal_tidy,
#    command = {
#      purrr::imap_dfr(fg_identity_focal_analysis, function(res, fg) {
#        clean_model_terms(tidy_model(res$model_2way)) %>%
#          dplyr::mutate(focal_fg = fg)
#      })
#    }
#  )
#  

#  

