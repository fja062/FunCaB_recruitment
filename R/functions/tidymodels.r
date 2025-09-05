# -------------------------
# Data Preparation
# -------------------------
prepare_data <- function(df) {
  df %>%
    mutate(
      temperature_scaled = scale(temperature)[, 1],
      precipitation_scaled = scale(precipitation)[, 1],
      spei_scaled = scale(spei)[, 1],
      spei_sq = spei_scaled^2
    )
}

# -------------------------
# Model Fitting
# -------------------------
fit_bayesian_model <- function(df) {
  rstanarm::stan_glmer(
    count ~ treatment * spei_scaled + treatment * spei_sq +
      treatment * temperature_scaled + treatment * precipitation_scaled +
      temperature_scaled * spei_scaled +
      temperature_scaled * spei_sq +
      (1 | siteID/blockID),
    data = df,
    family = neg_binomial_2(),
    chains = 2,
    cores = 4,
    iter = 2500,
    warmup = 1000,
    prior = normal(0, 2.5),
    prior_intercept = normal(0, 5),
    adapt_delta = 0.95
  )
}

# -------------------------
# Diagnostics
# -------------------------
make_pp_check_plot <- function(model) {
  bayesplot::pp_check(model) +
    ggtitle("Posterior Predictive Check")
}

make_diagnostics_plots <- function(model) {
  bayesplot::mcmc_trace(as.matrix(model)) +
    ggtitle("MCMC Trace Plot")
}

# -------------------------
# Predictions
# -------------------------
make_predictions <- function(model, data) {
  preds <- ggeffects::ggpredict(
    model,
    terms = c("treatment", "spei_scaled")
  ) %>%
    as.data.frame()
  
  preds <- preds %>%
    left_join(
      data %>%
        select(treatment, spei, temperature, precipitation) %>%
        distinct(),
      by = "treatment"
    )
  
  return(preds)
}

# -------------------------
# Prediction Plot
# -------------------------
plot_predictions <- function(pred_df) {
  ggplot(pred_df, aes(x = x, y = predicted, color = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
    labs(
      x = "SPEI (scaled)",
      y = "Predicted Count",
      color = "Treatment",
      fill = "Treatment"
    ) +
    theme_minimal()
}
