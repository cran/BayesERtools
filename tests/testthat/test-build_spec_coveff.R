data(d_sim_binom_cov_hgly2)

set.seed(1234)

ermod_bin <- suppressWarnings(dev_ermod_bin(
  data = d_sim_binom_cov_hgly2, var_resp = "AEFLAG",
  var_exposure = "AUCss_1000", var_cov = c("BGLUC", "RACE"),
  verbosity_level = 0,
  # Below option to make the example run fast
  chains = 2, iter = 1000
))


spec_coveff <- build_spec_coveff(ermod_bin)

spec_new_bgluc <- build_spec_coveff_one_variable(
  "BGLUC", seq(4, 8, by = 0.1),
  qi_width_cov = 0.8, show_ref_value = TRUE
)
spec_var_race <- build_spec_coveff_one_variable(
  "RACE", c("Black", "Black", "Asian", "White"),
  show_ref_value = TRUE
)

spec_coveff_new <- replace_spec_coveff(
  spec_coveff,
  dplyr::bind_rows(spec_var_race, spec_new_bgluc)
)
spec_coveff_no_keep_ref <-
  replace_spec_coveff(spec_coveff, spec_new_bgluc, replace_ref_value = TRUE)
spec_coveff_no_keep_ref_race <-
  replace_spec_coveff(spec_coveff, spec_var_race, replace_ref_value = TRUE)

coveffsim1 <- sim_coveff(ermod_bin, spec_coveff = spec_coveff_new)
coveffsim2 <- sim_coveff(ermod_bin, spec_coveff = spec_coveff_no_keep_ref)
coveffsim3 <- sim_coveff(ermod_bin, spec_coveff = spec_coveff_no_keep_ref_race)


# Tests -----------------------------------------------------------------------
test_that("input col & row error", {
  expect_error(replace_spec_coveff(
    spec_coveff, spec_new_bgluc |> dplyr::select(-value_cont)
  ), "At least one of value_cont or value_cat columns must be present.")

  expect_error(replace_spec_coveff(
    spec_coveff, spec_new_bgluc |> dplyr::mutate(test = "a")
  ), "`spec_new` expected to have the")
  expect_error(replace_spec_coveff(
    spec_coveff, spec_new_bgluc |> dplyr::select(-is_ref_value)
  ), "`spec_new` expected to have the")

  expect_error(replace_spec_coveff(
    spec_coveff, spec_new_bgluc |> dplyr::mutate(var_name = "TEST")
  ), "Following var_name in spec_new do not")
})


test_that("sim with modified spec", {
  expect_equal(coveffsim1$.odds_ratio,
    c(
      0.5362096, 1, 4.1778986, 1, 0.2176921, 0.9138167, 3.8359728, 1,
      1.0926579, 1.8544957, 1
    ),
    tolerance = 1e-6
  )
  expect_equal(coveffsim2$.odds_ratio,
    c(
      0.5362096, 1, 4.1778986, 0.2382229, 1, 4.1977486, 1, 1.8544957,
      1.0926579
    ),
    tolerance = 1e-6
  )
  expect_equal(coveffsim3$.odds_ratio,
    c(
      0.5362096, 1, 4.1778986, 0.2242438, 1, 3.8026690, 1, 1.7090927,
      0.9151996
    ),
    tolerance = 1e-6
  )
})
