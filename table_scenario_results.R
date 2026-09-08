suppressPackageStartupMessages({
  library(utils)
})

format_count = function(value) {
  format(
    round(value), big.mark = ".", decimal.mark = ",",
    scientific = FALSE, trim = TRUE
  )
}

format_scenario_value = function(values, include_ui = TRUE) {
  if (!include_ui) {
    return(format_count(values[["med"]]))
  }

  sprintf(
    "%s (%s-%s)",
    format_count(values[["med"]]),
    format_count(values[["low"]]),
    format_count(values[["high"]])
  )
}

format_ratio = function(value) {
  format(
    round(value, 3), nsmall = 3, decimal.mark = ".",
    scientific = FALSE, trim = TRUE
  )
}

format_ratio_value = function(values, include_ui = TRUE) {
  if (!include_ui) {
    return(format_ratio(values[["med"]]))
  }

  sprintf(
    "%s (%s-%s)",
    format_ratio(values[["med"]]),
    format_ratio(values[["low"]]),
    format_ratio(values[["high"]])
  )
}

format_percent = function(value) {
  paste0(format(round(value, 1), nsmall = 1, decimal.mark = "."), "%")
}

format_percent_value = function(values, include_ui = TRUE) {
  if (!include_ui) {
    return(format_percent(values[["med"]]))
  }

  sprintf(
    "%s (%s-%s)",
    format_percent(values[["med"]]),
    format_percent(values[["low"]]),
    format_percent(values[["high"]])
  )
}

summarise_period = function(data, group_name, date_start, date_end) {
  period_data = data[
    data$group == group_name &
      data$date >= date_start &
      data$date <= date_end,
  ]

  if (nrow(period_data) == 0) {
    stop("Missing group in scenario data: ", group_name)
  }

  value_columns = c("CI_low_values", "CI_med_values", "CI_high_values")
  if (!all(value_columns %in% names(period_data))) {
    if (!"value" %in% names(period_data)) {
      stop("Scenario data must contain CI columns or a value column")
    }
    period_data$CI_low_values = period_data$value
    period_data$CI_med_values = period_data$value
    period_data$CI_high_values = period_data$value
  }

  if ("simulation_values" %in% names(period_data)) {
    simulation_matrix = do.call(rbind, lapply(period_data$simulation_values, as.numeric))
    simulation_totals = vapply(seq_len(ncol(simulation_matrix)), function(j) {
      values = simulation_matrix[, j]
      if (grepl("cumulative", group_name, fixed = TRUE)) {
        tail(values, 1) - values[1]
      } else {
        sum(values, na.rm = TRUE) - values[1]
      }
    }, numeric(1))

    return(c(
      low = as.numeric(quantile(simulation_totals, probs = 0.025)),
      med = as.numeric(quantile(simulation_totals, probs = 0.5)),
      high = as.numeric(quantile(simulation_totals, probs = 0.975))
    ))
  }

  c(
    low = sum(period_data$CI_low_values, na.rm = TRUE) - period_data$CI_low_values[1],
    med = sum(period_data$CI_med_values, na.rm = TRUE) - period_data$CI_med_values[1],
    high = sum(period_data$CI_high_values, na.rm = TRUE) - period_data$CI_high_values[1]
  )
}

build_result_rows = function(scenario_data, date_start, date_end, fact100k) {
  outcomes = data.frame(
    Outcome = c(
      "Hospitalisations", "Hospitalisations",
      "ICU admissions", "ICU admissions",
      "Deaths", "Deaths"
    ),
    Population = rep(c("All ages", ">=65 years"), 3),
    Group = c(
      "hosp in", "hosp in age 65+",
      "icu in", "icu in age 65+",
      "deaths: daily", "deaths: daily age 65+"
    ),
    stringsAsFactors = FALSE
  )

  scenario_order = c("S4", "S1", "S3", "S2", "S5", "S6")
  scenario_headers = c(
    "S0 Observed rollout",
    "S1 No vaccination",
    "S2 0.7 x observed",
    "S3 1.3 x observed",
    "S4 >=65 only, observed",
    "S5 >=65 only, 0.7 x observed"
  )

  missing_scenarios = setdiff(scenario_order, names(scenario_data))
  if (length(missing_scenarios) > 0) {
    stop("Missing scenario data: ", paste(missing_scenarios, collapse = ", "))
  }

  totals = outcomes[, c("Outcome", "Population")]
  rates = outcomes[, c("Outcome", "Population")]
  relative_change = outcomes[, c("Outcome", "Population")]
  percentage_change = outcomes[, c("Outcome", "Population")]

  baseline_values = vapply(outcomes$Group, function(group_name) {
    summarise_period(scenario_data[["S4"]], group_name, date_start, date_end)[["med"]]
  }, numeric(1))

  for (i in seq_along(scenario_order)) {
    scenario_name = scenario_order[[i]]
    header = scenario_headers[[i]]
    include_ui = scenario_name != "S4"

    totals[[header]] = vapply(outcomes$Group, function(group_name) {
      values = summarise_period(
        scenario_data[[scenario_name]], group_name, date_start, date_end
      )
      format_scenario_value(values, include_ui)
    }, character(1))

    rates[[header]] = vapply(outcomes$Group, function(group_name) {
      values = summarise_period(
        scenario_data[[scenario_name]], group_name, date_start, date_end
      ) / fact100k
      format_scenario_value(values, include_ui)
    }, character(1))

    relative_change[[header]] = vapply(seq_along(outcomes$Group), function(j) {
      values = summarise_period(
        scenario_data[[scenario_name]], outcomes$Group[[j]], date_start, date_end
      )
      ratio_values = values / baseline_values[[j]]
      format_ratio_value(ratio_values, include_ui)
    }, character(1))

    percentage_change[[header]] = vapply(seq_along(outcomes$Group), function(j) {
      values = summarise_period(
        scenario_data[[scenario_name]], outcomes$Group[[j]], date_start, date_end
      )
      ratio_values = values / baseline_values[[j]]
      percentage_values = (1 - ratio_values) * 100

      # Reverse the interval bounds because larger counterfactual values
      # correspond to smaller percentages relative to S0.
      percentage_values = c(
        low = percentage_values[["high"]],
        med = percentage_values[["med"]],
        high = percentage_values[["low"]]
      )
      format_percent_value(percentage_values, include_ui)
    }, character(1))
  }

  list(
    totals = totals,
    rates = rates,
    relative_change = relative_change,
    percentage_change = percentage_change
  )
}

markdown_table = function(data) {
  header = paste(names(data), collapse = " | ")
  separator = paste(c("---", rep("---:", ncol(data) - 1)), collapse = " | ")
  rows = apply(data, 1, function(row) paste(row, collapse = " | "))

  paste0(
    "| ", header, " |\n",
    "| ", separator, " |\n",
    paste0("| ", rows, " |", collapse = "\n")
  )
}

write_scenario_tables = function(
    scenario_data,
    output_file = file.path(getwd(), "results", "simulation_table_results.md"),
    start_date = as.Date("2021-01-01"),
    end_date = as.Date("2021-12-31"),
    fact100k = 21.01462765957446808510
) {
  scenario_data = scenario_data[c("S1", "S2", "S3", "S4", "S5", "S6")]
  rows = build_result_rows(scenario_data, start_date, end_date, fact100k)

  output = paste(
    "## Total, n (95% UI for counterfactual scenarios)",
    "",
    markdown_table(rows$totals),
    "",
    "## Rate per 100.000 population (95% UI for counterfactual scenarios)",
    "",
    markdown_table(rows$rates),
    "",
    "## Relative ratio to S0 (95% UI for counterfactual scenarios)",
    "",
    markdown_table(rows$relative_change),
    "",
    "## Percentage difference relative to S0 (95% UI for counterfactual scenarios)",
    "",
    "Percentage difference was calculated as `(S0 - Sx) / S0 * 100`. Positive values indicate fewer outcomes than S0; negative values indicate additional outcomes.",
    "",
    markdown_table(rows$percentage_change),
    sep = "\n"
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(output, output_file, useBytes = TRUE)
  invisible(output_file)
}

write_scenario_tables(
  list(S1 = spdat_scen1, S2 = spdat_scen2, S3 = spdat_scen3,
       S4 = spdat_scen4, S5 = spdat_scen5, S6 = spdat_scen6),
  output_file = file.path(
    if (exists("project_dir", inherits = TRUE)) project_dir else getwd(),
    "results", "simulation_table_results.md"
  )
)
