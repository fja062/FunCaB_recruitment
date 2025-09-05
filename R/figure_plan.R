# Figure plan for FunCaB Community Analysis
figure_plan <- list(
  
  tar_target(
    name = fig_test,
    command = combined_recruitment |> 
      ggplot(aes(x = spei, y = count, group = temperature_level, colour = temperature_level, fill = temperature_level)) + 
      geom_point() + 
      geom_smooth(method = "glm", formula = 'y ~ x + I(x^2)') + 
      facet_wrap(~fg_removed) +
      theme_classic()
  ),
  
  tar_target(
    name = prediction_plot,
    command = plot_predictions(model_predictions)
  )#,
  
#  tar_target(
#    export_prediction_plot,
#    ggsave("outputs/prediction_plot.png", prediction_plot, width = 8, height = 5),
#    format = "file"
#  )
  
#  tar_target(
#    name = fig_fg_richness_effects,
#    command = plot_model_effects(fg_richness_analysis$model_2way)
#  ),
#  
#  tar_target(
#    name = fig_fg_identity_effects,
#    command = plot_model_effects(fg_identity_analysis$model_2way)
#  ),
#  
#  tar_target(
#    name = fig_fg_identity_interactions,
#    command = {
#      p1 <- plot_model(fg_identity_analysis$model_2way, type = "pred", terms = c("precipitation_scaled", "fg_removed"))
#      p2 <- plot_model(fg_identity_analysis$model_2way, type = "pred", terms = c("temperature_scaled", "fg_removed"))
#      
#      p1 + p2 + plot_layout(guides = "collect") & theme_bw()
#    }
#  ),
#  
#  tar_target(
#    name = fig_fg_biomass_effects,
#    command = plot_model_effects(fg_biomass_analysis)
#  ),
#  
#  tar_target(
#    name = fig_fg_biomass_interactions,
#    command = {
#      p1 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("cumulative_removed_biomass", "fg_removed"))
#      p2 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("temperature_scaled", "fg_removed"))
#      p3 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("precipitation_scaled", "fg_removed"))
#      
#      p1 + p2 + p3 + plot_layout(guides = "collect") & theme_bw()
#    }
#  ),
#  
#  ## Part 2
#  
#  # fg richness focals
#  tar_target(
#    name = fig_fg_richness_focal_effects,
#    command = plot_tidy_effects_facet(data = fg_richness_focal_tidy, 
#                                      facet_var = "focal_fg", 
#                                      term_order = rev(c("fg_richness", "T", "P", "fg_richness:T", "fg_richness:P", "P:T")))
#  ),
#  
#  # fg identity focals
#  tar_target(
#    name = fig_fg_identity_focal_effects,
#    command = plot_tidy_effects_facet(data = fg_identity_focal_tidy, 
#                                      facet_var = "focal_fg")
#  ),
#  
#  tar_target(
#    name = fig_fg_identity_focal_interactions,
#    command = {
#      p1 <- plot_model(fg_identity_focal_analysis$bryophytes$model_2way,
#                       type = "pred", 
#                       terms = c("precipitation_scaled", "fg_removed"),
#                       title = "Bryophytes")
#      p2 <- plot_model(fg_identity_focal_analysis$forbs$model_2way, 
#                       type = "pred", 
#                       terms = c("precipitation_scaled", "fg_removed"),
#                       title = "Forbs")
#      p3 <- plot_model(fg_identity_focal_analysis$graminoids$model_2way, 
#                       type = "pred", 
#                       terms = c("precipitation_scaled", "fg_removed"),
#                       title = "Graminoids")
#      p4 <- plot_model(fg_identity_focal_analysis$bryophytes$model_2way, 
#                       type = "pred", 
#                       terms = c("temperature_scaled", "fg_removed"),
#                       title = "Bryophytes")
#      p5 <- plot_model(fg_identity_focal_analysis$forbs$model_2way, 
#                       type = "pred", 
#                       terms = c("temperature_scaled", "fg_removed"),
#                       title = "Forbs")
#      p6 <- plot_model(fg_identity_focal_analysis$graminoids$model_2way, 
#                       type = "pred", 
#                       terms = c("temperature_scaled", "fg_removed"),
#                       title = "Graminoids")
#      
#      (p1 + p2 + p3) / (p4 + p5 + p6) + plot_layout(guides = "collect") & theme_bw()
#    }
#  )
#  
#  
#  
#  
  
)