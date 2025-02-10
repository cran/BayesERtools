## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(dplyr)
library(BayesERtools)

ggplot2::theme_set(ggplot2::theme_bw(base_size = 12))

## -----------------------------------------------------------------------------
data(d_sim_binom_cov)

head(d_sim_binom_cov) |>
  gt::gt() |>
  gt::fmt_number(n_sigfig = 3)

d_sim_binom_cov_2 <-
  d_sim_binom_cov |>
  mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10, BHBA1C_5 = BHBA1C / 5,
    Dose = paste(Dose_mg, "mg")
  )

# Grade 2+ hypoglycemia
df_er_ae_hgly2 <- d_sim_binom_cov_2 |> filter(AETYPE == "hgly2")
# Grade 2+ diarrhea
df_er_ae_dr2 <- d_sim_binom_cov_2 |> filter(AETYPE == "dr2")

## -----------------------------------------------------------------------------
var_resp <- "AEFLAG"
# HbA1c & glucose are only relevant for hyperglycemia
var_cov_ae_hgly2 <-
  c("BAGE_10", "BWT_10", "RACE", "VISC", "BHBA1C_5", "BGLUC")
var_cov_ae_dr2 <-
  c("BAGE_10", "BWT_10", "RACE", "VISC")

## -----------------------------------------------------------------------------
set.seed(1234)
ermod_bin <- dev_ermod_bin(
  data = df_er_ae_hgly2,
  var_resp = var_resp,
  var_exposure = "AUCss_1000"
)
ermod_bin

## -----------------------------------------------------------------------------
# Using `*` instead of `+` so that scale can be
# applied for both panels (main plot and boxplot)
plot_er_gof(ermod_bin, var_group = "Dose", show_coef_exp = TRUE) *
  xgxr::xgx_scale_x_log10()

## -----------------------------------------------------------------------------
draws_df <- as_draws_df(ermod_bin)

draws_df_summary <-
  posterior::summarize_draws(draws_df)

draws_df_summary |>
  gt::gt() |>
  gt::fmt_number(n_sigfig = 3)

## -----------------------------------------------------------------------------
ersim_med_qi <- sim_er_new_exp(
  ermod_bin,
  exposure_to_sim_vec = 1:3,
  output_type = "median_qi"
)
ersim_med_qi

## -----------------------------------------------------------------------------
set.seed(1234)
ermod_bin_exp_sel <-
  dev_ermod_bin_exp_sel(
    # Use reduced N to make the example run faster
    data = slice_sample(df_er_ae_hgly2, n = 100),
    var_resp = var_resp,
    var_exp_candidates = c("AUCss_1000", "Cmaxss", "Cminss"),
    # Use reduced iter to make the example run faster
    iter = 1000
  )
ermod_bin_exp_sel

## -----------------------------------------------------------------------------
plot_er_exp_sel(ermod_bin_exp_sel) +
  xgxr::xgx_scale_x_log10()

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(1234)
#  ermod_bin_cov_sel <-
#    dev_ermod_bin_cov_sel(
#      data = df_er_ae_hgly2,
#      var_resp = var_resp,
#      var_exposure = "AUCss_1000",
#      var_cov_candidate = var_cov_ae_hgly2,
#      verbosity_level = 2
#    )

## ----eval = FALSE, include = FALSE--------------------------------------------
#  # Save the output in vignettes/data to avoid running it during the build.
#  saveRDS(ermod_bin_cov_sel, file = "data/ermod_bin_cov_sel.rds")

## ----include = FALSE----------------------------------------------------------
ermod_bin_cov_sel <- readRDS("data/ermod_bin_cov_sel.rds")

## -----------------------------------------------------------------------------
ermod_bin_cov_sel

## -----------------------------------------------------------------------------
plot_submod_performance(ermod_bin_cov_sel)

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(1234)
#  ermod_bin_cov_sel_kfold <-
#    dev_ermod_bin_cov_sel(
#      data = df_er_ae_hgly2,
#      var_resp = var_resp,
#      var_exposure = "AUCss_1000",
#      var_cov_candidate = var_cov_ae_hgly2,
#      cv_method = "kfold",
#      validate_search = TRUE,
#      verbosity_level = 2
#    )

## ----eval = FALSE, include = FALSE--------------------------------------------
#  # Save the output in vignettes/data to avoid running it during the build.
#  saveRDS(ermod_bin_cov_sel_kfold, file = "data/ermod_bin_cov_sel_kfold.rds")

## ----include = FALSE----------------------------------------------------------
ermod_bin_cov_sel_kfold <- readRDS("data/ermod_bin_cov_sel_kfold.rds")

## -----------------------------------------------------------------------------
ermod_bin_cov_sel_kfold
plot_submod_performance(ermod_bin_cov_sel_kfold)
plot_var_ranking(ermod_bin_cov_sel_kfold)

## -----------------------------------------------------------------------------
ersim_new_exp_marg_med_qi <- sim_er_new_exp_marg(
  ermod_bin_cov_sel,
  exposure_to_sim_vec = c(2:6),
  output_type = "median_qi"
)
ersim_new_exp_marg_med_qi

plot_er(ersim_new_exp_marg_med_qi, marginal = TRUE)

## ----fig.width = 5, fig.height = 4.5------------------------------------------
coveffsim <- sim_coveff(ermod_bin_cov_sel)
plot_coveff(coveffsim)
print_coveff(coveffsim)

