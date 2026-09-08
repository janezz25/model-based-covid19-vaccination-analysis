suppressPackageStartupMessages({
  library(ggplot2)
  library(plotly)
  library(scales)
})

plot_calibration_ggplotly = function(
    data, group, title, ylab, start_date, end_date,
    maxy = NULL, smooth_observed = FALSE, show_raw_observed = TRUE,
    model_label = "model"
) {
  plot_data = data[
    data$group == group &
      data$date >= start_date & data$date <= end_date,
  ]

  if (nrow(plot_data) == 0)
    stop("No data available for group: ", group)

  if (smooth_observed) {
    plot_data$observed_smoothed = as.numeric(
      stats::filter(plot_data$observed, rep(1 / 7, 7), sides = 2)
    )
  }

  y_values = c(
    plot_data$CI_low_values,
    plot_data$CI_high_values,
    plot_data$observed
  )
  y_values = y_values[is.finite(y_values)]
  y_max = if (is.null(maxy)) max(y_values, na.rm = TRUE) * 1.05 else maxy

  smoothed_layer = if (smooth_observed) {
    geom_line(
      aes(y = observed_smoothed, colour = "smoothed actual data"),
      linewidth = 1.0, na.rm = TRUE
    )
  } else {
    NULL
  }

  observed_layer = if (show_raw_observed) {
    geom_point(
      aes(y = observed, colour = "actual data"),
      size = 1.2, na.rm = TRUE
    )
  } else {
    NULL
  }

  pg = ggplot(plot_data, aes(x = date)) +
    geom_ribbon(
      aes(ymin = CI_low_values, ymax = CI_high_values),
      fill = "#2f6fa3", alpha = 0.2
    ) +
    geom_line(
      aes(y = CI_med_values, colour = model_label),
      linewidth = 1.1
    ) +
    observed_layer +
    smoothed_layer +
    scale_colour_manual(
      values = c(
        "model" = "#2f6fa3",
        "BW optimized model" = "#2f6fa3",
        "calibrated model" = "#2f6fa3",
        "actual data" = "#d62728",
        "smoothed actual data" = "#222222"
      ),
      name = NULL
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%d/%y") +
    scale_y_continuous(
      labels = scales::format_format(
        big.mark = " ", decimal.mark = ",", scientific = FALSE
      )
    ) +
    coord_cartesian(
      xlim = c(start_date, end_date),
      ylim = c(0, y_max)
    ) +
    labs(title = title, x = "", y = ylab) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      text = element_text(size = 13),
      legend.position = "bottom"
    )

  ggplotly(pg, tooltip = c("x", "y", "colour"))
}

plot_calibration_set = function(
    data, start_date, end_date, title_prefix = "Calibration",
    model_label = "calibrated model"
) {
  plots = list(
    hosp = plot_calibration_ggplotly(
      data, "hospitalizations", paste(title_prefix, "Hospitalizations"),
      "Hospitalizations", start_date, end_date, model_label = model_label
    ),
    icu = plot_calibration_ggplotly(
      data, "icu", paste(title_prefix, "ICU occupancy"),
      "ICU occupancy", start_date, end_date, model_label = model_label
    ),
    infections = plot_calibration_ggplotly(
      data, "infections: daily", paste(title_prefix, "Daily infections"),
      "Daily infections", start_date, end_date, model_label = model_label
    ),
    deaths = plot_calibration_ggplotly(
      data, "deaths: daily", paste(title_prefix, "Daily deaths"),
      "Daily deaths", start_date, end_date, smooth_observed = TRUE,
      model_label = model_label
    )
  )

  all_plot = subplot(
    plots$hosp, plots$icu, plots$deaths,
    nrows = 3, shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.04
  )
  layout(all_plot, title = list(text = title_prefix, x = 0.5))
}

plot_model_fit_publication = function(
    data, start_date, end_date, title, output_file = NULL
) {
  group_labels = c(
    "hospitalizations" = "A  Hospital occupancy",
    "icu" = "B  ICU occupancy",
    "deaths: daily" = "C  Daily deaths"
  )

  plot_data = data[
    data$group %in% names(group_labels) &
      data$date >= start_date & data$date <= end_date,
  ]
  if (nrow(plot_data) == 0) {
    stop("No model-fit data available in the requested date range.")
  }

  plot_data$outcome = factor(
    unname(group_labels[plot_data$group]),
    levels = unname(group_labels)
  )
  plot_data$observed_smoothed = NA_real_
  death_rows = plot_data$group == "deaths: daily"
  plot_data$observed_smoothed[death_rows] = as.numeric(stats::filter(
    plot_data$observed[death_rows], rep(1 / 7, 7), sides = 2
  ))

  plot = ggplot(plot_data, aes(x = date)) +
    geom_point(
      aes(y = observed, colour = "Observed"),
      size = 0.75, alpha = 0.55, na.rm = TRUE
    ) +
    geom_line(
      aes(y = CI_med_values, colour = "Model"),
      linewidth = 0.9, lineend = "round", na.rm = TRUE
    ) +
    geom_line(
      data = plot_data[death_rows, ],
      aes(y = observed_smoothed, colour = "Observed, 7-day mean"),
      linewidth = 0.65, na.rm = TRUE
    ) +
    facet_wrap(vars(outcome), ncol = 1, scales = "free_y") +
    scale_colour_manual(
      values = c(
        "Model" = "#0072B2",
        "Observed" = "#D55E00",
        "Observed, 7-day mean" = "#303030"
      ),
      breaks = c("Model", "Observed", "Observed, 7-day mean"),
      name = NULL
    ) +
    scale_x_date(
      breaks = seq(
        as.Date(format(start_date, "%Y-%m-01")), end_date, by = "1 month"
      ),
      date_labels = "%b",
      limits = c(start_date, end_date),
      expand = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ",", accuracy = 1),
      expand = expansion(mult = c(0, 0.06))
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      strip.text = element_text(size = 11, face = "bold", hjust = 0),
      strip.background = element_rect(fill = "#F1F4F6", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E5E8EA", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#D9DDE0", linewidth = 0.35),
      axis.text = element_text(colour = "#303030"),
      legend.position = "bottom",
      legend.justification = "left",
      legend.box.just = "left",
      legend.margin = margin(t = 2),
      panel.spacing = grid::unit(0.7, "lines"),
      plot.margin = margin(10, 12, 8, 10)
    ) +
    guides(colour = guide_legend(override.aes = list(
      linewidth = c(0.9, 0, 0.65),
      shape = c(NA, 16, NA),
      alpha = c(1, 0.7, 1)
    )))

  if (!is.null(output_file)) {
    output_dir = dirname(output_file)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    ggsave(
      output_file, plot = plot, width = 7.2, height = 7.6,
      units = "in", dpi = 400, bg = "white"
    )
  }

  plot
}
