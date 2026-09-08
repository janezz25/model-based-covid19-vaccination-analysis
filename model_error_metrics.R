# Error metrics for comparing actual and modelled data.

check_metric_inputs <- function(actual, modelled) {
  if (length(actual) != length(modelled)) {
    stop("'actual' and 'modelled' must have the same length.", call. = FALSE)
  }
}

get_metric_dates <- function(actual, modelled) {
  actual_dates <- names(actual)
  modelled_dates <- names(modelled)

  if (is.null(actual_dates) && is.null(modelled_dates)) {
    stop(
      "Date filtering requires dates as names on 'actual' or 'modelled'.",
      call. = FALSE
    )
  }

  if (!is.null(actual_dates) && !is.null(modelled_dates) &&
      !identical(actual_dates, modelled_dates)) {
    stop("Date names on 'actual' and 'modelled' must match.", call. = FALSE)
  }

  dates <- if (!is.null(actual_dates)) actual_dates else modelled_dates
  dates <- as.Date(dates)

  if (any(is.na(dates))) {
    stop("Date names must be coercible to Date values.", call. = FALSE)
  }

  dates
}

filter_metric_time <- function(actual, modelled, time = NULL) {
  check_metric_inputs(actual, modelled)

  if (is.null(time)) {
    return(list(actual = actual, modelled = modelled))
  }

  if (length(time) != 2) {
    stop("'time' must be NULL or a two-value vector c(t1, t2).", call. = FALSE)
  }

  time <- as.Date(time)
  if (any(is.na(time))) {
    stop("'time' values must be coercible to Date values.", call. = FALSE)
  }

  if (time[1] > time[2]) {
    stop("'time' start must not be after 'time' end.", call. = FALSE)
  }

  dates <- get_metric_dates(actual, modelled)

  keep <- dates >= time[1] & dates <= time[2]

  if (!any(keep, na.rm = TRUE)) {
    stop("No observations found in the requested 'time' window.", call. = FALSE)
  }

  list(actual = actual[keep], modelled = modelled[keep])
}

normalization_denominator <- function(actual, normalization = "mean", na.rm = TRUE) {
  denominator <- switch(
    normalization,
    mean = mean(actual, na.rm = na.rm),
    range = max(actual, na.rm = na.rm) - min(actual, na.rm = na.rm),
    sd = stats::sd(actual, na.rm = na.rm),
    stop("Unknown normalization. Use 'mean', 'range', or 'sd'.", call. = FALSE)
  )

  if (is.na(denominator) || denominator == 0) {
    stop("Normalization denominator is zero or NA.", call. = FALSE)
  }

  denominator
}

rmse <- function(actual, modelled, time = NULL, na.rm = TRUE) {
  data <- filter_metric_time(actual, modelled, time = time)
  sqrt(mean((data$actual - data$modelled)^2, na.rm = na.rm))
}

nrmse <- function(actual, modelled, time = NULL, normalization = "mean",
                  na.rm = TRUE) {
  data <- filter_metric_time(actual, modelled, time = time)
  rmse(data$actual, data$modelled, na.rm = na.rm) /
    normalization_denominator(data$actual, normalization = normalization, na.rm = na.rm)
}

mae <- function(actual, modelled, time = NULL, na.rm = TRUE) {
  data <- filter_metric_time(actual, modelled, time = time)
  mean(abs(data$actual - data$modelled), na.rm = na.rm)
}

nmae <- function(actual, modelled, time = NULL, normalization = "mean",
                 na.rm = TRUE) {
  data <- filter_metric_time(actual, modelled, time = time)
  mae(data$actual, data$modelled, na.rm = na.rm) /
    normalization_denominator(data$actual, normalization = normalization, na.rm = na.rm)
}

# Positive values indicate model overestimation; negative values indicate
# underestimation. The result is expressed as a percentage of observed totals.
pbias <- function(actual, modelled, time = NULL, na.rm = TRUE) {
  data <- filter_metric_time(actual, modelled, time = time)
  keep <- rep(TRUE, length(data$actual))

  if (na.rm) {
    keep <- !is.na(data$actual) & !is.na(data$modelled)
  }

  denominator <- sum(data$actual[keep], na.rm = FALSE)
  if (is.na(denominator) || denominator == 0) {
    stop("PBIAS denominator is zero or NA.", call. = FALSE)
  }

  100 * sum(data$modelled[keep] - data$actual[keep], na.rm = FALSE) /
    denominator
}
