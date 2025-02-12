# Read d_sim_binom_cov data, which is exported to the package
d_sim_binom_cov <- d_sim_binom_cov

id_to_sample <- seq(1, max(d_sim_binom_cov$ID), by = 1)
id_to_sample2 <- seq(1, max(d_sim_binom_cov$ID), by = 5)

df_er_ae_hgly2 <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "hgly2",
    ID %in% id_to_sample
  ) |>
  dplyr::mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10, BHBA1C_5 = BHBA1C / 5
  )

df_er_dr2 <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "dr2",
    ID %in% id_to_sample
  ) |>
  dplyr::mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10
  )

var_resp <- "AEFLAG"
var_exp_candidates <- c("AUCss_1000", "Cmaxss", "Cminss")


# develop models --------------------------------------------------------------

set.seed(1234)
ermod_bin <- dev_ermod_bin(
  data = df_er_ae_hgly2,
  var_resp = var_resp,
  var_exposure = "AUCss_1000",
  var_cov = NULL,
  verbosity_level = 0,
  # Below option to make the test fast
  chains = 2, iter = 1000
)

set.seed(1234)
ermod_bin_w_cov <- dev_ermod_bin(
  data = df_er_ae_hgly2,
  var_resp = var_resp,
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
  verbosity_level = 0,
  # Below option to make the test fast
  chains = 2, iter = 1000
)

set.seed(1234)
ermod_bin_exp_sel <-
  dev_ermod_bin_exp_sel(
    data = df_er_ae_hgly2,
    var_resp = var_resp,
    var_exp_candidates = var_exp_candidates,
    verbosity_level = 0,
    # Below option to make the test fast
    chains = 2, iter = 1000
  )

# Simulate responses ----------------------------------------------------------
ersim_curve <- sim_er_curve(
  ermod_bin,
  n_draws_sim = 200,
  num_exposures = 31,
  output_type = "draws"
)

ersim_curve_med_qi <- sim_er_curve(
  ermod_bin,
  n_draws_sim = 200,
  num_exposures = 31,
  output_type = "median_qi",
  qi_width = 0.95
)

ersim_curve_2_med_qi <- sim_er_curve(
  ermod_bin_w_cov,
  data_cov = dplyr::tibble(BHBA1C_5 = 8),
  num_exposures = 31,
  output_type = "median_qi",
  qi_width = 0.95
)

ersim_curve_3_med_qi <- sim_er_curve(
  ermod_bin_w_cov,
  data_cov = dplyr::tibble(BHBA1C_5 = c(4, 8)),
  num_exposures = 31,
  output_type = "median_qi",
  qi_width = 0.95
)

ersim_new_exp_marg_med_qi <- sim_er_new_exp_marg(
  ermod_bin_w_cov,
  exposure_to_sim_vec = c(2, 2:6),
  data_cov = dplyr::tibble(BHBA1C_5 = 4:10, AUCss_1000 = 4:10),
  n_subj_sim = NULL,
  n_draws_sim = 200,
  output_type = "median_qi"
)

# Test ------------------------------------------------------------------------
test_that("plot_er.ermod", {
  if (requireNamespace("xgxr")) {
    g1 <- plot_er(ermod_bin, show_orig_data = TRUE)
    g2 <- plot_er(ermod_bin_w_cov,
      show_orig_data = TRUE, marginal = TRUE,
      n_draws_sim = 50, num_exposures = 31, exposure_range = c(1, 3)
    )

    expect_equal(nrow(g1$data), 51)
    expect_equal(g2$data$AUCss_1000, seq(1, 3, length.out = 31))
  }
  plot_er(ermod_bin_w_cov) |>
    expect_error("Model has covariate\\(s\\), and you cannot use this")
})

test_that("plot_er.ersim", {
  g1 <- plot_er(ersim_curve)
  g2 <- plot_er(ersim_curve_med_qi)
  if (requireNamespace("xgxr")) {
    g3 <- plot_er(ersim_curve_med_qi, show_orig_data = TRUE)
  }

  expect_equal(nrow(g1$data), 31) # Make sure sim_er_curve worked fine
  expect_equal(g2$labels$x, "AUCss_1000")

  expect_silent(plot(g1))

  if (requireNamespace("xgxr")) {
    plot_er(ersim_curve_2_med_qi, show_orig_data = TRUE) |>
      expect_warning("Model has covariate\\(s\\), and only one covariate data")
  }
  plot_er(ersim_curve_3_med_qi) |>
    expect_error("Model has covariate\\(s\\) and multiple covariate data rows")
})

test_that("plot_er with groups", {
  if (requireNamespace("xgxr")) {
    plot_er(ermod_bin,
      show_orig_data = TRUE,
      options_orig_data = list(var_group = "AUCss_1000")
    ) |>
      expect_error("Column `AUCss_1000` is numeric and has > 10 unique values")

    plot_er(ermod_bin,
      show_orig_data = TRUE,
      options_orig_data = list(var_group = "Dose_mg")
    ) |>
      expect_silent()

    plot_er(ermod_bin,
      show_orig_data = TRUE,
      options_orig_data = list(var_group = "Dose_mg", add_boxplot = TRUE)
    ) |>
      expect_silent()

    plot_er(ermod_bin,
      show_orig_data = TRUE,
      options_orig_data = list(add_boxplot = TRUE)
    ) |>
      expect_silent()
  }
})

test_that("plot_er add CI", {
  plot_er(ermod_bin,
    show_coef_exp = TRUE,
    options_coef_exp = list(size = 6)
  ) |>
    expect_silent()
})

test_that("plot_er show caption", {
  if (requireNamespace("xgxr")) {
    plot_er(
      ersim_curve_med_qi,
      show_orig_data = TRUE,
      show_coef_exp = TRUE,
      show_caption = TRUE,
      options_coef_exp = list(size = 6),
      options_caption = list(orig_data = TRUE, orig_data_summary = TRUE)
    ) |>
      expect_silent()
  }

  plot_er(ermod_bin,
    show_coef_exp = TRUE,
    show_caption = TRUE,
    options_coef_exp = list(size = 6),
    options_caption = list(coef_exp = TRUE)
  ) |>
    expect_silent()
})

test_that("plot_er_gof", {
  if (requireNamespace("xgxr")) {
    plot_er_gof(ermod_bin) |>
      expect_silent()
    plot_er_gof(ermod_bin, var_group = "Dose_mg") |>
      expect_silent()
    plot_er_gof(ermod_bin, show_coef_exp = TRUE, show_caption = TRUE) |>
      expect_silent()
  }
})
