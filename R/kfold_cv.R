# TODO: see if the model works when not all the factor levels
# are present in the training dataset

# TODO: Warn when newdata in sim_er has categorical covariate with
# new levels not present in the training dataset

#' Run k-fold cross-validation
#'
#' This function performs k-fold cross-validation using the appropriate model
#' development function based on the class of the `ermod` object.
#'
#' @param ermod An `ermod` object containing the model and data.
#' @param newdata Optional new dataset to use instead of the original data.
#' Default is NULL.
#' @param k The number of folds for cross-validation. Default is 5.
#' @param seed Random seed for reproducibility. Default is NULL.
#'
#' @return A `kfold_cv_ermod` class object containing the fitted models and
#' holdout predictions for each fold.
#'
#' @examplesIf BayesERtools:::.if_run_ex_eval_mod()
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' ermod_bin <- dev_ermod_bin(
#'   data = d_sim_binom_cov_hgly2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000",
#'   var_cov = "BHBA1C_5",
#'   # Settings to make the example run faster
#'   chains = 2,
#'   iter = 1000
#' )
#'
#' cv_results <- run_kfold_cv(ermod_bin, k = 3, seed = 123)
#'
#' print(cv_results)
#' }
#'
#' @export
run_kfold_cv <- function(ermod, newdata = NULL, k = 5, seed = NULL) {
  rlang::check_installed("rsample")

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Use newdata if provided, otherwise use data from ermod object
  data <- if (!is.null(newdata)) newdata else ermod$data

  # Convert non-numeric columns to factors
  # Need to do this first, otherwise there can be discrepancies in categorical
  # levels between training and testing data
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(~ !is.numeric(.)), as.factor)) |>
    dplyr::mutate(.row_orig = dplyr::row_number())

  # Determine the model development function based on the class of the ermod
  if (inherits(ermod, "ermod_emax")) {
    model_dev_fun <- dev_ermod_emax
  } else if (inherits(ermod, "ermod_bin_emax")) {
    model_dev_fun <- dev_ermod_bin_emax
  } else if (inherits(ermod, "ermod_lin")) {
    model_dev_fun <- dev_ermod_lin
  } else if (inherits(ermod, "ermod_bin")) {
    model_dev_fun <- dev_ermod_bin
  } else {
    stop("Unsupported model class")
  }

  # Create k-fold cross-validation splits
  folds <- rsample::vfold_cv(data, v = k)

  # Function to fit the model and make predictions
  fit_and_sim <- function(split, fold_id) {
    # Extract analysis and assessment data
    train_data <- rsample::analysis(split)
    test_data <- rsample::assessment(split)

    # Prepare arguments for the model development function
    dev_args <- c(
      list(
        data = train_data,
        var_resp = ermod$var_resp,
        var_exposure = ermod$var_exposure
      ),
      if (inherits(ermod, "ermod_lin") || inherits(ermod, "ermod_bin")) {
        list(var_cov = ermod$var_cov)
      } else {
        list(l_var_cov = ermod$l_var_cov)
      },
      ermod$input_args
    )

    ermod_k <- do.call(model_dev_fun, dev_args)
    ersim_k <- sim_er(ermod_k, newdata = test_data)

    d_truth_k <- test_data |>
      dplyr::select(.row = .row_orig, truth = !!rlang::sym(ermod$var_resp)) |>
      dplyr::mutate(fold_id = fold_id)

    d_sim_k <- ersim_k |>
      dplyr::ungroup() |>
      dplyr::select(.row = .row_orig, .draw, pred = .epred) |>
      dplyr::mutate(fold_id = fold_id)

    list(
      ermod = ermod_k,
      d_truth = d_truth_k,
      d_sim = d_sim_k
    )
  }

  # Apply the fit_and_sim function to each fold
  results <- purrr::map2(folds$splits, seq_along(folds$splits), fit_and_sim)

  # Reformat so that the results are easier to access
  results <- purrr::list_transpose(results)

  # Rename ermod to l_ermod
  results$l_ermod <- results$ermod
  results$ermod <- NULL

  # row_bind the results together for d_truth and d_sim
  results$d_truth <-
    dplyr::bind_rows(results$d_truth) |>
    dplyr::arrange(.row) |>
    dplyr::group_by(fold_id)
  results$d_sim <-
    dplyr::bind_rows(results$d_sim) |>
    dplyr::arrange(.row) |>
    dplyr::group_by(fold_id, .row)

  # Store metadata
  results$k <- k

  # Assign class
  class(results) <- c("kfold_cv_ermod", "list")

  return(results)
}

#' @export
print.kfold_cv_ermod <- function(x, ...) {
  cli::cli({
    cli::cli_h1("k-fold Cross-Validation for ermod object")
    cli::cli_alert_info(paste(
      "Number of folds: ", x$k
    ))
    cli::cli_h2("Structure of the object:")
    cli::cli_li("$l_ermod: list of ermod objects")
    cli::cli_li("$d_truth: data frame with true response values")
    cli::cli_li("$d_sim: data frame with holdout predictions")
  })
}
