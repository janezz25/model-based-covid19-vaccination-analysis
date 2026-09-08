suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

plot_scenario_metric = function(scenario_data, group_name, title, y_label,
                                output_file, start_date, end_date, y_max = NULL) {
  scenario_labels = c(
    S1 = "S1: no vaccination",
    S2 = "S2: increased vaccination",
    S3 = "S3: reduced vaccination",
    S4 = "S0: observed vaccination",
    S5 = "S5: vaccination age 65+ only",
    S6 = "S6: reduced vaccination age 65+"
  )
  colours = c("#aa374a", "#2fa8ee", "#009E73", "#316ca3", "#cad621", "#E69F00")

  plot_data = do.call(rbind, lapply(names(scenario_data), function(name) {
    data = scenario_data[[name]][scenario_data[[name]]$group == group_name, ]
    data$scenario = scenario_labels[[name]]
    data
  }))

  plot_data$scenario = factor(
    plot_data$scenario,
    levels = unname(scenario_labels)
  )

  plot_data$CI_med_values[
    plot_data$date < start_date | plot_data$date > end_date
  ] = NA
  plot_data$CI_low_values[
    plot_data$date < start_date | plot_data$date > end_date
  ] = NA
  plot_data$CI_high_values[
    plot_data$date < start_date | plot_data$date > end_date
  ] = NA

  plot_data$observed[
    plot_data$date < start_date | plot_data$date > end_date
  ] = NA

  y_limits = if (is.null(y_max)) {
    c(0, max(plot_data$CI_high_values, na.rm = TRUE) * 1.05)
  } else {
    c(0, y_max)
  }

  plot = ggplot(plot_data, aes(x = date, group = scenario)) +
    geom_ribbon(
      aes(ymin = CI_low_values, ymax = CI_high_values, fill = scenario),
      alpha = 0.18, na.rm = TRUE
    ) +
    geom_line(aes(y = CI_med_values, colour = scenario), linewidth = 1.1) +
    geom_point(
      data = plot_data[plot_data$scenario == "S0: observed vaccination", ],
      aes(y = observed), colour = "black", size = 1.1, na.rm = TRUE
    ) +
    scale_colour_manual(values = colours, drop = FALSE) +
    scale_fill_manual(values = colours, drop = FALSE) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%m/%d/%y",
      limits = c(start_date, end_date)
    ) +
    scale_y_continuous(
      labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
    ) +
    coord_cartesian(ylim = y_limits) +
    labs(title = title, x = NULL, y = y_label, colour = "Scenario", fill = "Scenario") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      text = element_text(size = 13),
      legend.position = "bottom"
    )

  if (!exists("produce_graphical_output", inherits = TRUE) || isTRUE(produce_graphical_output)) {
    print(plot)
  }
  ggsave(output_file, plot = plot, dpi = 300, width = 10, height = 5)
  invisible(plot)
}

plot_scenario_results = function(
    scenario_data,
    output_dir = getwd(),
    start_date = as.Date("2021-01-01"),
    end_date = as.Date("2021-12-31")
) {
  scenario_data = scenario_data[c("S1", "S2", "S3", "S4", "S5", "S6")]

  plot_scenario_metric(
    scenario_data, "hospitalizations", "Hospital occupancy by scenario",
    "Hospitalized patients", file.path(output_dir, "hospital_occupancy_by_scenario.png"),
    start_date, end_date, 12000
  )
  plot_scenario_metric(
    scenario_data, "icu", "ICU occupancy by scenario",
    "ICU patients", file.path(output_dir, "icu_occupancy_by_scenario.png"),
    start_date, end_date, 3000
  )
  plot_scenario_metric(
    scenario_data, "deaths: daily", "Daily deaths by scenario",
    "Deaths per day", file.path(output_dir, "daily_deaths_by_scenario.png"),
    start_date, end_date, 200
  )
}

plot_scenario_results(
  list(S1 = spdat_scen1, S2 = spdat_scen2, S3 = spdat_scen3,
       S4 = spdat_scen4, S5 = spdat_scen5, S6 = spdat_scen6),
  output_dir = file.path(
    if (exists("project_dir", inherits = TRUE)) project_dir else getwd(),
    "results"
  )
)
