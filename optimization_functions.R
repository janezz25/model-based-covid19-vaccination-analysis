###############################################################################
##### Transmission-parameter optimization ####################################
###############################################################################

# Calculate normalized RMSE while ignoring missing observations.
nrmse = function(observed, modeled) {
  keep = !is.na(observed) & !is.na(modeled)

  if (!any(keep))
    return(Inf)

  denominator = mean(observed[keep])
  if (!is.finite(denominator) || denominator == 0)
    return(Inf)

  sqrt(mean((observed[keep] - modeled[keep])^2)) / abs(denominator)
}

# Aggregate one output across the model's five age groups.
sum_model_groups = function(pdat, group_prefix) {
  groups = paste0(group_prefix, " ", 1:5)
  values = lapply(groups, function(group) pdat$value[pdat$group == group])

  if (any(vapply(values, length, integer(1)) == 0))
    stop("Missing model output group for ", group_prefix)

  Reduce(`+`, values)
}

optimize_Bw = function(
    Bw0, win_len, observed_data, duration_time, w, param,
    optimization_start_date = as.Date("2021-01-01"),
    optimization_end_date = as.Date("2021-12-31"),
    optimize_indices = seq_along(Bw0)
) {
  # Optimize only transmission while keeping empirical corrections disabled.
  if (length(Bw0) == 0 || any(!is.finite(Bw0)))
    stop("Bw0 must contain finite values")

  if (length(win_len) != length(Bw0))
    stop("win_len must have the same length as Bw0")

  if (any(!is.finite(win_len)) || any(win_len <= 0))
    stop("win_len must contain positive finite values")

  if (optimization_start_date > optimization_end_date)
    stop("optimization_start_date must not be after optimization_end_date")

  optimize_indices = as.integer(optimize_indices)
  if (length(optimize_indices) == 0 || any(is.na(optimize_indices)) ||
      any(optimize_indices < 1) || any(optimize_indices > length(Bw0)))
    stop("optimize_indices must contain valid Bw indices")
  optimize_indices = sort(unique(optimize_indices))

  param_obj = param
  param_obj$calibrate_hosp = FALSE
  param_obj$calibrate_icu = FALSE
  param_obj$calibrate_death = FALSE
  param_obj$calibrate_infected = FALSE

  weights = list(infections = 0.5, hosp = 1.0, icu = 1.0, deaths = 1.0)
  if (!is.null(param$Bw_opt_weights))
    weights = modifyList(weights, param$Bw_opt_weights)
  if (any(!is.finite(unlist(weights))) || any(unlist(weights) < 0))
    stop("Bw_opt_weights must contain non-negative finite values")

  lower = if (is.null(param$Bw_lower)) 0.000001 else param$Bw_lower
  upper = if (is.null(param$Bw_upper)) 10 * max(Bw0) else param$Bw_upper
  lower = rep(lower, length.out = length(Bw0))
  upper = rep(upper, length.out = length(Bw0))
  if (any(!is.finite(lower)) || any(!is.finite(upper)) || any(lower > upper))
    stop("Invalid Bw optimization bounds")

  lower_opt = lower[optimize_indices]
  upper_opt = upper[optimize_indices]
  Bw_fixed = Bw0

  show_progress = is.null(param$Bw_opt_progress) || isTRUE(param$Bw_opt_progress)
  maxit = if (is.null(param$Bw_opt_maxit)) 10 else param$Bw_opt_maxit
  if (length(maxit) != 1 || !is.finite(maxit) || maxit < 1)
    stop("Bw_opt_maxit must be one positive finite value")
  maxit = as.integer(maxit)
  factr = if (is.null(param$Bw_opt_factr)) 1e7 else param$Bw_opt_factr
  if (length(factr) != 1 || !is.finite(factr) || factr <= 0)
    stop("Bw_opt_factr must be one positive finite value")
  evaluation = 0L

  # Evaluate weighted mismatch over the requested calendar-date window.
  objective = function(Bw_opt_values) {
    evaluation <<- evaluation + 1L
    value = tryCatch({
      Bw = Bw_fixed
      Bw[optimize_indices] = Bw_opt_values
      Bfun = Bt_rect_time(duration_time, Bw, win_len)
      pdat = calculate_model_observed_vaccination_waning(
        Bfun, observed_data, duration_time, w, param_obj
      )

      modeled = list(
        infections = sum_model_groups(pdat, "infections: daily"),
        hosp = sum_model_groups(pdat, "hospitalizations"),
        icu = sum_model_groups(pdat, "ICU"),
        deaths = sum_model_groups(pdat, "deaths: daily")
      )

      target = function(group, modeled_values) {
        group_mask = pdat$group == group
        # Apply the date window after model/observed data have been aligned.
        date_mask = pdat$date[group_mask] >= optimization_start_date &
          pdat$date[group_mask] <= optimization_end_date
        list(
          observed = pdat$observed[group_mask][date_mask],
          modeled = modeled_values[date_mask]
        )
      }
      targets = list(
        infections = target("infections: daily 1", modeled$infections),
        hosp = target("hospitalizations 1", modeled$hosp),
        icu = target("ICU 1", modeled$icu),
        deaths = target("deaths: daily 1", modeled$deaths)
      )

      loss = weights$infections * nrmse(targets$infections$observed, targets$infections$modeled) +
        weights$hosp * nrmse(targets$hosp$observed, targets$hosp$modeled) +
        weights$icu * nrmse(targets$icu$observed, targets$icu$modeled) +
        weights$deaths * nrmse(targets$deaths$observed, targets$deaths$modeled)

      if (is.finite(loss)) loss else 1e100
      }, error = function(error) {
        1e100
      })

    value = as.numeric(value)
    if (show_progress) {
      message(sprintf(
        "Bw optimization objective evaluation %d: loss = %.6g",
        evaluation, value
      ))
      flush.console()
    }

    value
  }

  fit = optim(
    par = Bw0[optimize_indices],
    fn = objective,
    method = "L-BFGS-B",
    lower = lower_opt,
    upper = upper_opt,
    control = list(
      maxit = maxit,
      factr = factr,
      trace = if (show_progress) 1 else 0,
      REPORT = 1
    )
  )

  Bw_opt = Bw_fixed
  Bw_opt[optimize_indices] = fit$par
  list(
    Bw = Bw_opt,
    Bfun = Bt_rect_time(duration_time, Bw_opt, win_len),
    fit = fit,
    maxit = maxit,
    factr = factr,
    objective_evaluations = evaluation,
    win_len = win_len,
    optimize_indices = optimize_indices
  )
}

run_Bw_optimization = function(
    Bw0, win_len, observed_data, duration_time, w, param,
    optimization_start_date = as.Date("2021-01-01"),
    optimization_end_date = as.Date("2021-12-31"),
    optimize_indices = seq_along(Bw0)
) {
  # Run transmission optimization and return data for the next calibration step.
  optimization = optimize_Bw(
    Bw0, win_len, observed_data, duration_time, w, param,
    optimization_start_date, optimization_end_date, optimize_indices
  )

  param$Bw = optimization$Bw
  param$Bw_optimization = optimization

  list(
    pdat = calculate_model_observed_vaccination_waning(
      optimization$Bfun, observed_data, duration_time, w, param
    ),
    Bfun = optimization$Bfun,
    Bw = optimization$Bw,
    optimization = optimization,
    param = param
  )
}
