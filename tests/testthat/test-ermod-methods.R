d_sim_binom_cov <- d_sim_binom_cov

id_to_sample <- seq(1, max(d_sim_binom_cov$ID), by = 5)

df_er_ae_covsel_test <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "ae_covsel_test",
    ID %in% id_to_sample
  ) |>
  dplyr::mutate(AUCss_1000 = AUCss / 1000, BHBA1C_5 = BHBA1C / 5)

data_er_cont <- rstanemax::exposure.response.sample
data_er_cont_cov <- rstanemax::exposure.response.sample.with.cov
data_er_bin <- rstanemax::exposure.response.sample.binary

# Create  ermod_bin objects for testing ---------------------------------------
set.seed(1234)
ermod_bin <- dev_ermod_bin(
  data = df_er_ae_covsel_test,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
  verbosity_level = 0,
  # Below option to make the test fast
  chains = 2, iter = 1000
)

ermod_bin_wo_cov <- dev_ermod_bin(
  data = df_er_ae_covsel_test,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  verbosity_level = 0,
  # Below option to make the test fast
  chains = 2, iter = 1000
)

set.seed(1234)
ermod_emax_w_cov <-
  dev_ermod_emax(
    data = data_er_cont_cov,
    var_exposure = "conc",
    var_resp = "resp",
    l_var_cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
    verbosity_level = 0,
    chains = 2,
    iter = 1000,
    seed = 1
  )

ermod_bin_emax <-
  suppressWarnings(dev_ermod_bin_emax(
    data = df_er_ae_covsel_test,
    var_resp = "AEFLAG",
    var_exposure = "AUCss_1000",
    verbosity_level = 0,
    # Increase iter for better convergence as exact reproducibility
    # over different machines doesn't seem realistic
    chains = 2,
    iter = 1000,
    seed = 1
  ))

# Test ------------------------------------------------------------------------
test_that("loo", {
  loo_ermod_bin <- loo(ermod_bin)
  loo_ermod_emax_w_cov <- suppressWarnings(loo(ermod_emax_w_cov))
  loo_ermod_bin_emax <- loo(ermod_bin_emax)

  expect_equal(
    loo_ermod_bin$estimates[, 1],
    c(elpd_loo = -38.528662, p_loo = 3.325979, looic = 77.057323)
  )
  expect_equal(
    loo_ermod_emax_w_cov$estimates[, 1],
    c(elpd_loo = -216.539552, p_loo = 7.765598, looic = 433.079103),
    tolerance = 0.1
  )
  expect_equal(
    loo_ermod_bin_emax$estimates[, 1],
    c(elpd_loo = -60.787274, p_loo = 1.427765, looic = 121.574548),
    tolerance = 0.1
  )

  expect_silent(loo::loo_compare(loo_ermod_bin, loo_ermod_bin_emax))
})

test_that("as_draws", {
  as_draws_df(ermod_bin)$AUCss_1000[1:10] |>
    expect_equal(
      c(
        0.5200371, 0.6083116, 0.7032639, 0.8712742, 0.4418383,
        0.9465231, 1.0348286, 0.5353301, 0.5980297, 0.5244656
      ),
      tolerance = 0.001
    )
  as_draws_df(ermod_emax_w_cov) |>
    names() |>
    expect_equal(c(
      "ec50[C1]", "ec50[C0]", "sigma", "gamma", "e0[A0]", "e0[A1]",
      "emax[B0]", "emax[B2]", "emax[B3]", ".chain", ".iteration", ".draw"
    ))
  as_draws_rvars(ermod_bin_emax) |> expect_s3_class("draws_rvars")
})


# extract_coef_exp_ci
test_that("extract_coef_exp_ci", {
  extract_coef_exp_ci(ermod_bin) |>
    expect_equal(
      c(.lower = 0.1717, .upper = 1.01064),
      tolerance = 0.001
    )
  expect_error(extract_coef_exp_ci(ermod_emax_w_cov), "extract_coef_exp_ci")
})


test_that("print.ermod prints correct information", {
  out <- cli::cli_fmt({
    print(ermod_bin)
  })
  expect_true(any(grepl("Binary ER model", out)))
  expect_true(any(grepl("Use \\`plot_er\\(\\)\\` to visualize ER curve", out)))
  expect_true(any(grepl("Developed model", out)))
})

# plot.ermod_bin
test_that("plot.ermod_bin calls plot_er", {
  if (.if_run_ex_plot_er()) {
    expect_silent(plot.ermod_bin(ermod_bin_wo_cov))
  }
})

# Additional extract function tests
test_that("extract functions", {
  expect_equal(extract_data.ermod(ermod_bin), ermod_bin$data)
  expect_equal(extract_mod.ermod(ermod_bin), ermod_bin$mod)
  expect_equal(extract_var_resp.ermod(ermod_bin), ermod_bin$var_resp)
  expect_equal(extract_var_exposure.ermod(ermod_bin), ermod_bin$var_exposure)
  expect_equal(extract_var_cov.ermod(ermod_bin), ermod_bin$var_cov)
  expect_equal(
    extract_var_cov.ermod(ermod_emax_w_cov),
    unlist(ermod_emax_w_cov$l_var_cov)
  )
  expect_equal(
    extract_exp_sel_list_model.ermod_exp_sel(ermod_bin_emax),
    ermod_bin_emax$l_mod_exposures
  )
  expect_equal(
    extract_exp_sel_comp.ermod_exp_sel(ermod_bin_emax),
    ermod_bin_emax$loo_comp_exposures
  )
  expect_equal(
    extract_var_selected.ermod_cov_sel(ermod_bin_emax),
    ermod_bin_emax$var_selected
  )
})

# get_mod_type_name
test_that("get_mod_type_name", {
  expect_equal(get_mod_type_name(ermod_emax_w_cov), "Emax model")
  expect_equal(get_mod_type_name(ermod_bin_emax), "Binary Emax model")
  expect_error(get_mod_type_name("1"), "Unknown model type")
})
