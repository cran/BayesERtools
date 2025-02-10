# `ermod_bin*` S3 methods for print, plot, ... --------------------------------
## Regular `ermod` class ------------------------------------------------------

#' S3 methods for the classes \code{ermod_*}
#' @name ermod_method
#' @param x An object of class \code{ermod_*}
#' @param object An object of class \code{ermod_*}
#' @param digits Number of digits to print
#' @param ... Additional arguments passed to functions
#' @return
#' - `print()` and `plot()`: No return value, called for side effects
#' - `coef()`: Coefficients of the model
#' - `summary()`: Summary of the model
NULL


get_mod_type_name <- function(mod) {
  if (inherits(mod, "ermod_bin")) {
    return("Binary ER model")
  } else if (inherits(mod, "ermod_lin")) {
    return("Linear ER model")
  } else if (inherits(mod, "ermod_emax")) {
    return("Emax model")
  } else if (inherits(mod, "ermod_bin_emax")) {
    return("Binary Emax model")
  } else {
    stop("Unknown model type")
  }
}

#' @export
#' @rdname ermod_method
print.ermod <- function(x, digits = 2, ...) {
  mod_type_name <- get_mod_type_name(x)

  cli::cli({
    cli::cli_h1(mod_type_name)
    cli::cli_alert_info(paste(
      "Use `plot_er()` to visualize ER curve"
    ))
    cli::cli_h2("Developed model")
    print(x$mod, digits = digits, ...) |>
      utils::capture.output() |>
      cli::cli_code()
  })
}

#' @export
#' @rdname ermod_method
#' @inheritParams plot_er
plot.ermod_bin <- function(x, show_orig_data = FALSE, ...) {
  plot_er(x, show_orig_data = show_orig_data)
}

#' @export
#' @rdname ermod_method
coef.ermod <- function(object, ...) {
  if (inherits(object, "ermod_emax")) {
    stop("coef() not supported for ermod_emax object")
  }

  stats::coef(object$mod, ...)
}

#' @export
#' @rdname ermod_method
summary.ermod <- function(object, ...) {
  if (inherits(object, "ermod_emax")) {
    stop("summary() not supported for ermod_emax object")
  }

  summary(object$mod, ...)
}

## ermod_exp_sel class ----------------------------------------------------

#' S3 methods for the classes `ermod_exp_sel`
#'
#' @export
#' @name ermod_exp_sel_method
#' @inheritParams ermod_method
#' @param x An object of class `ermod_bin_exp_sel`
#' @return No return value, called for print or plot side effects
print.ermod_exp_sel <- function(x, digits = 2, ...) {
  mod_type_name <- get_mod_type_name(x)

  cli::cli({
    cli::cli_h1(paste(mod_type_name, "& exposure metric selection"))
    cli::cli_alert_info(paste(
      "Use `plot_er_exp_sel()` for ER curve of all exposure metrics"
    ))
    cli::cli_alert_info(paste(
      "Use `plot_er()` with `show_orig_data = TRUE` for ER curve of the",
      "selected exposure metric"
    ))

    if (length(x$var_exp_candidates) > 1) {
      cli::cli_h2("Exposure metrics comparison")
      print(x$loo_comp_exposures, digits = digits, ...) |>
        utils::capture.output() |>
        cli::cli_code()
    } else {
      cli::cli_alert_info(paste0(
        "Only `",
        x$var_exp_candidates,
        "` was provided, no selection done"
      ))
    }

    cli::cli_h2("Selected model")
    print(x$mod, digits = digits, ...) |>
      utils::capture.output() |>
      cli::cli_code()
  })
}

#' @export
#' @rdname ermod_exp_sel_method
plot.ermod_exp_sel <- function(x, ...) {
  plot_er_exp_sel(x, ...)
}

## ermod_cov_sel class ----------------------------------------------------

#' S3 methods for the classes `ermod_bin_cov_sel`
#' @export
#' @name ermod_cov_sel_method
#' @inheritParams ermod_method
#' @param x An object of class `ermod_bin_cov_sel`
#' @return No return value, called for print or plot side effects
print.ermod_cov_sel <- function(x, digits = 2, ...) {
  mod_type_name <- get_mod_type_name(x)

  cli::cli({
    cli::cli_h1(paste(mod_type_name, "& covariate selection"))

    cli::cli_alert_info(paste(
      "Use `plot_submod_performance()` to see variable",
      "selection performance\n"
    ))
    if (!is.null(x$rk$foldwise)) {
      cli::cli_alert_info(paste(
        "Use `plot_var_ranking()` to see variable ranking"
      ))
    }
    cli::cli_alert_info(paste(
      "Use `plot_er()` with `marginal = TRUE` to visualize",
      "marginal ER curve"
    ))

    cli::cli_h2("Selected model")
    print(x$mod, digits = digits, ...) |>
      utils::capture.output() |>
      cli::cli_code()
  })
}

#' @export
#' @rdname ermod_cov_sel_method
plot.ermod_cov_sel <- function(x, ...) {
  plot_submod_performance(x, ...)
}


# Extract elements ------------------------------------------------------------

#' Extract elements from an object of class \code{ermod_*}
#'
#' @export
#' @name extract_ermod
#' @keywords internal
#' @inherit extract_method return
#' @param x An object of class \code{ermod_*}
#'
extract_data.ermod <- function(x) x$data

#' @export
#' @rdname extract_ermod
extract_mod.ermod <- function(x) x$mod

#' @export
#' @rdname extract_ermod
extract_var_resp.ermod <- function(x) x$var_resp

#' @export
#' @rdname extract_ermod
extract_var_exposure.ermod <- function(x) x$var_exposure

#' @export
#' @rdname extract_ermod
extract_var_cov.ermod <- function(x) {
  if (inherits(x, c("ermod_bin", "ermod_lin"))) {
    return(x$var_cov)
  } else if (inherits(x, c("ermod_emax", "ermod_bin_emax"))) {
    return(x$l_var_cov |> unlist())
  } else {
    stop(
      "extract_var_cov() only supported for `ermod_bin`, `ermod_lin`",
      "`ermod_emax`, and `ermod_bin_emax`, and their subclasses"
    )
  }
}

#' @export
#' @rdname extract_ermod
extract_exp_sel_list_model.ermod_exp_sel <- function(x) x$l_mod_exposures

#' @export
#' @rdname extract_ermod
extract_exp_sel_comp.ermod_exp_sel <- function(x) x$loo_comp_exposures

#' @export
#' @rdname extract_ermod
extract_var_selected.ermod_cov_sel <- function(x) x$var_selected


#' Extract credible interval of the exposure coefficient
#'
#' @export
#' @param x An object of class `ermod_bin` or `ermod_lin`
#' @param ci_width Width of the credible interval
#' @return A named vector of length 2 with the lower and upper bounds of the
#'  credible interval (.lower, .upper)
#'
extract_coef_exp_ci <- function(x, ci_width = 0.95) {
  # Check that input x is ermod object
  if (!inherits(x, c("ermod_bin", "ermod_lin"))) {
    stop("extract_coef_exp_ci() only supported for linear models")
  }

  coef_exp_ci <-
    stats::quantile(
      x$coef_exp_draws,
      c(0.5 - ci_width / 2, 0.5 + ci_width / 2)
    )

  names(coef_exp_ci) <- c(".lower", ".upper")

  return(coef_exp_ci)
}


# LOO -------------------------------------------------------------------------

#' Efficient approximate leave-one-out cross-validation (LOO)
#'
#' See [loo::loo()] for details.
#'
#' @name loo
#' @param x An object of class `ermod`
#' @param ... Additional arguments passed to `loo::loo()`
#' @importFrom loo loo
#'
#' @return An object of class `loo`
#' @export
loo::loo

#' @rdname loo
#' @export
loo.ermod <- function(x, ...) {
  loo::loo(x$mod, ...)
}

#' @rdname loo
#' @export
loo.ermod_emax <- function(x, ...) {
  rlang::check_installed("digest")

  out <- loo::loo(x$mod$stanfit, ...)

  y <- x$mod$standata$response
  attributes(y) <- NULL
  attr(out, "yhash") <- digest::sha1(y)

  return(out)
}

#' @rdname loo
#' @export
loo.ermod_bin_emax <- function(x, ...) {
  rlang::check_installed("digest")

  out <- loo::loo(x$mod$stanfit, ...)

  y <- x$mod$standata$response
  attributes(y) <- NULL
  attr(out, "yhash") <- digest::sha1(y)

  return(out)
}

# as_draws --------------------------------------------------------------------

#' @rdname as_draws
#' @importFrom posterior as_draws
#' @export
posterior::as_draws

#' @rdname as_draws
#' @importFrom posterior as_draws_list
#' @export
posterior::as_draws_list

#' @rdname as_draws
#' @importFrom posterior as_draws_array
#' @export
posterior::as_draws_array

#' @rdname as_draws
#' @importFrom posterior as_draws_df
#' @export
posterior::as_draws_df

#' @rdname as_draws
#' @importFrom posterior as_draws_matrix
#' @export
posterior::as_draws_matrix

#' @rdname as_draws
#' @importFrom posterior as_draws_rvars
#' @export
posterior::as_draws_rvars


#' Transform to `draws` objects
#'
#' See [posterior::as_draws()] for details.
#'
#' @param x An object of class `ermod`
#' @param ... Arguments passed to individual methods (if applicable).
#' @return A draws object from the `posterior` package.
#' @name as_draws
NULL

#' @rdname as_draws
#' @export
as_draws.ermod <- function(x, ...) {
  posterior::as_draws(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_list.ermod <- function(x, ...) {
  posterior::as_draws_list(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_array.ermod <- function(x, ...) {
  posterior::as_draws_array(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_df.ermod <- function(x, ...) {
  posterior::as_draws_df(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_matrix.ermod <- function(x, ...) {
  posterior::as_draws_matrix(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_rvars.ermod <- function(x, ...) {
  posterior::as_draws_rvars(x$mod, ...)
}
