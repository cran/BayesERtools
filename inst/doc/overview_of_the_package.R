## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, warning=FALSE, message=FALSE-------------------------------
library(gt)
library(dplyr)
set.seed(1234) # Needed to stablize div id

# Saved csv from README.Rmd
tab_mod <- read.csv("data/supported_models.csv")

tab_mod |>
  select(-.row_id) |>
  gt() |>
  fmt_markdown() |>
  fmt_url(
    columns = !1,
    rows = 2,
    label = "🔗",
    show_underline = FALSE
  ) |>
  tab_spanner(
    label = "Binary endpoint",
    columns = c(lin_logit, emax_logit)
  ) |>
  tab_spanner(
    label = "Continuous endpoint",
    columns = c(linear, emax)
  ) |>
  cols_label(
    feature_name = "",
    lin_logit = "Linear  \n(logit)",
    emax_logit = md("E<sub/>max</sub> (logit)"),
    linear = "Linear",
    emax = md("E<sub/>max</sub>"),
  ) |>
  tab_style(
    style = cell_text(v_align = "top", align = "center"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(v_align = "middle", align = "center"),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(v_align = "middle", align = "right"),
    locations = cells_body(columns = feature_name)
  ) |>
  tab_footnote(
    footnote = paste(
      "✅ Available",
      "🟡 In plan/under development",
      "❌ Not in a current plan",
      sep = ", "
    )
  )

