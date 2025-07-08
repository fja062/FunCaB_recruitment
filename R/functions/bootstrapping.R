## Bootstrapping

# trait imputation
make_trait_impute <- function(cover_data, traits){
  
  #prepare community data
  comm <- cover_data |> 
    filter(year == 2019,
           !is.na(cover))
  
  #prepare trait data
  trait <- traits
  
  #set seed for bootstrapping repeatability
  set.seed(1525)
  trait_fill(
    comm = comm, 
    traits = trait,
    scale_hierarchy = c("siteID"),
    taxon_col = "species", 
    value_col = "value_trans",
    trait_col = "trait_trans", 
    abundance_col = "cover",
    other_col = c("blockID", "plotID", "fg_removed", "fg_remaining"),
    min_n_in_sample = 2
  )
  
  
}



# bootstrapping
make_bootstrapping <- function(imputed_traits){
  
  # trait_imp <- wg_trait_impute
  CWM <- trait_np_bootstrap(imputed_traits, nrep = 100, sample_size = 200)
  
  CWM_mean <- trait_summarise_boot_moments(CWM) %>%
    ungroup() |>
    select(global, siteID:mean, var, skew, kurt, -n)
  
  # #prepare bootstrapped trait data for analyses
  traitMean <- CWM_mean %>%
    fancy_trait_name_dictionary(.) |>
    ungroup()
  
}