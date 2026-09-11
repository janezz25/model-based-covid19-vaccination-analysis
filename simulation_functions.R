translate_group_labels = function(data) {
  labels = data$group
  labels = sub("^infections", "infections", labels)
  labels = sub("^hospitalizations", "hospitalizations", labels)
  labels = sub("^deaths: daily", "deaths: daily", labels)
  labels = sub("^deaths: cumulative", "deaths: cumulative", labels)
  labels = sub("^vaccinated", "vaccinated", labels)
  labels = sub("^hospitalizations in", "hospitalizations in", labels)
  labels = sub("^ICU in", "ICU in", labels)
  labels = sub("^ICU", "ICU", labels)
  labels = sub(" 65", " age 65+", labels, fixed = TRUE)
  labels = sub("^infections age 65+: daily", "infections age 65+: daily", labels)
  labels = sub("^infections: daily", "infections: daily", labels)
  labels = sub("^deaths: daily", "deaths: daily", labels)
  labels = sub("^deaths: cumulative", "deaths: cumulative", labels)
  labels = sub("^deaths: daily", "deaths: daily", labels)
  labels = sub("^deaths: cumulative", "deaths: cumulative", labels)
  labels = sub("^hospitalizations in", "hospitalizations in", labels)
  data$group = labels
  data
}

combine_simulation_projections_over65 = function(pdat) 
{
  odat = pdat[pdat$group == "infections: daily 1",]
  odat$value = pdat[pdat$group == "infections: daily 1","value"] + 
    pdat[pdat$group == "infections: daily 2","value"] + 
    pdat[pdat$group == "infections: daily 3","value"] + 
    pdat[pdat$group == "infections: daily 4","value"] + 
    pdat[pdat$group == "infections: daily 5","value"]
  
  odat$group = "infections: daily"

  
  odat65 = pdat[pdat$group == "infections: daily 1",]
  odat65$value = 
    pdat[pdat$group == "infections: daily 4","value"] + 
    pdat[pdat$group == "infections: daily 5","value"]
  
  odat65$group = "infections 65: daily"


  
  
  hdat = pdat[pdat$group == "hospitalizations 1",]
  hdat$value = pdat[pdat$group == "hospitalizations 1","value"] + 
    pdat[pdat$group == "hospitalizations 2","value"] + 
    pdat[pdat$group == "hospitalizations 3","value"] + 
    pdat[pdat$group == "hospitalizations 4","value"] + 
    pdat[pdat$group == "hospitalizations 5","value"]
  
  hdat$group = "hospitalizations"


  hdat65 = pdat[pdat$group == "hospitalizations 1",]
  hdat65$value = 
    pdat[pdat$group == "hospitalizations 4","value"] + 
    pdat[pdat$group == "hospitalizations 5","value"]
  
  hdat65$group = "hospitalizations 65"

  
  hindat = pdat[pdat$group == "hospitalizations in 1",]
  hindat$value = pdat[pdat$group == "hospitalizations in 1","value"] + 
    pdat[pdat$group == "hospitalizations in 2","value"] + 
    pdat[pdat$group == "hospitalizations in 3","value"] + 
    pdat[pdat$group == "hospitalizations in 4","value"] + 
    pdat[pdat$group == "hospitalizations in 5","value"]
  
  hindat$group = "hosp in"
  
  hindat65 = pdat[pdat$group == "hospitalizations in 1",]
  hindat65$value = 
    pdat[pdat$group == "hospitalizations in 4","value"] + 
    pdat[pdat$group == "hospitalizations in 5","value"]
  
  hindat65$group = "hosp in 65"
  

  
  idat = pdat[pdat$group == "ICU 1",]
  idat$value = pdat[pdat$group == "ICU 1","value"] + 
    pdat[pdat$group == "ICU 2","value"] + 
    pdat[pdat$group == "ICU 3","value"] + 
    pdat[pdat$group == "ICU 4","value"] + 
    pdat[pdat$group == "ICU 5","value"]
  
  idat$group = "icu"

  idat65 = pdat[pdat$group == "ICU 1",]
  idat65$value = 
    pdat[pdat$group == "ICU 4","value"] + 
    pdat[pdat$group == "ICU 5","value"]
  
  idat65$group = "icu 65"
  
  
  iindat = pdat[pdat$group == "ICU in 1",]
  iindat$value = pdat[pdat$group == "ICU in 1","value"] + 
    pdat[pdat$group == "ICU in 2","value"] + 
    pdat[pdat$group == "ICU in 3","value"] + 
    pdat[pdat$group == "ICU in 4","value"] + 
    pdat[pdat$group == "ICU in 5","value"]
  
  iindat$group = "icu in"
  
  
  iindat65 = pdat[pdat$group == "ICU in 1",]
  iindat65$value = 
    pdat[pdat$group == "ICU in 4","value"] + 
    pdat[pdat$group == "ICU in 5","value"]
  
  iindat65$group = "icu in 65"
  
  
  
  dudat = pdat[pdat$group == "deaths: daily 1",]
  dudat$value = pdat[pdat$group == "deaths: daily 1","value"] + 
    pdat[pdat$group == "deaths: daily 2","value"] + 
    pdat[pdat$group == "deaths: daily 3","value"] + 
    pdat[pdat$group == "deaths: daily 4","value"] + 
    pdat[pdat$group == "deaths: daily 5","value"]
  
  dudat$group = "deaths: daily"

  dudat65 = pdat[pdat$group == "deaths: daily 1",]
  dudat65$value = 
    pdat[pdat$group == "deaths: daily 4","value"] + 
    pdat[pdat$group == "deaths: daily 5","value"]
  
  dudat65$group = "deaths: daily 65"
  
  
  kudat = pdat[pdat$group == "deaths: cumulative 1",]
  kudat$value = pdat[pdat$group == "deaths: cumulative 1","value"] + 
    pdat[pdat$group == "deaths: cumulative 2","value"] + 
    pdat[pdat$group == "deaths: cumulative 3","value"] + 
    pdat[pdat$group == "deaths: cumulative 4","value"] + 
    pdat[pdat$group == "deaths: cumulative 5","value"]
  
  kudat$group = "deaths: cumulative"
  
  kudat65 = pdat[pdat$group == "deaths: cumulative 1",]
  kudat65$value = 
    pdat[pdat$group == "deaths: cumulative 4","value"] + 
    pdat[pdat$group == "deaths: cumulative 5","value"]
  
  kudat65$group = "deaths: cumulative 65"
  


  spdat = rbind(odat, hdat, idat, dudat, kudat, hindat, iindat,
                odat65, hdat65, idat65, dudat65, kudat65, hindat65, iindat65)
  
  return(translate_group_labels(spdat))
}


combine_simulation_projections_over65_s56 = function(pdat) 
{
  odat = pdat[pdat$group == "infections: daily 1",]
  odat$value = pdat[pdat$group == "infections: daily 1","value"] + 
    pdat[pdat$group == "infections: daily 2","value"] + 
    pdat[pdat$group == "infections: daily 3","value"] + 
    pdat[pdat$group == "infections: daily 4","value"] + 
    pdat[pdat$group == "infections: daily 5","value"]
  
  odat$group = "infections: daily"

  
  odat65 = pdat[pdat$group == "infections: daily 1",]
  odat65$value = 
    pdat[pdat$group == "infections: daily 4","value"] + 
    pdat[pdat$group == "infections: daily 5","value"]
  
  odat65$group = "infections 65: daily"


  
  
  hdat = pdat[pdat$group == "hospitalizations 1",]
  hdat$value = 0.8*pdat[pdat$group == "hospitalizations 1","value"] + 
    0.8*pdat[pdat$group == "hospitalizations 2","value"] + 
    0.8*pdat[pdat$group == "hospitalizations 3","value"] + 
    pdat[pdat$group == "hospitalizations 4","value"] + 
    pdat[pdat$group == "hospitalizations 5","value"]
  
  hdat$group = "hospitalizations"


  hdat65 = pdat[pdat$group == "hospitalizations 1",]
  hdat65$value = 
    pdat[pdat$group == "hospitalizations 4","value"] + 
    pdat[pdat$group == "hospitalizations 5","value"]
  
  hdat65$group = "hospitalizations 65"

  
  hindat = pdat[pdat$group == "hospitalizations in 1",]
  hindat$value = 0.8*pdat[pdat$group == "hospitalizations in 1","value"] + 
    0.8*pdat[pdat$group == "hospitalizations in 2","value"] + 
    0.8*pdat[pdat$group == "hospitalizations in 3","value"] + 
    pdat[pdat$group == "hospitalizations in 4","value"] + 
    pdat[pdat$group == "hospitalizations in 5","value"]
  
  hindat$group = "hosp in"
  
  hindat65 = pdat[pdat$group == "hospitalizations in 1",]
  hindat65$value = 
    pdat[pdat$group == "hospitalizations in 4","value"] + 
    pdat[pdat$group == "hospitalizations in 5","value"]
  
  hindat65$group = "hosp in 65"
  

  
  idat = pdat[pdat$group == "ICU 1",]
  idat$value = 0.8*pdat[pdat$group == "ICU 1","value"] + 
    0.8*pdat[pdat$group == "ICU 2","value"] + 
    0.8*pdat[pdat$group == "ICU 3","value"] + 
    pdat[pdat$group == "ICU 4","value"] + 
    pdat[pdat$group == "ICU 5","value"]
  
  idat$group = "icu"

  idat65 = pdat[pdat$group == "ICU 1",]
  idat65$value = 
    pdat[pdat$group == "ICU 4","value"] + 
    pdat[pdat$group == "ICU 5","value"]
  
  idat65$group = "icu 65"
  
  
  iindat = pdat[pdat$group == "ICU in 1",]
  iindat$value = 0.8*pdat[pdat$group == "ICU in 1","value"] + 
    0.8*pdat[pdat$group == "ICU in 2","value"] + 
    0.8*pdat[pdat$group == "ICU in 3","value"] + 
    pdat[pdat$group == "ICU in 4","value"] + 
    pdat[pdat$group == "ICU in 5","value"]
  
  iindat$group = "icu in"
  
  
  iindat65 = pdat[pdat$group == "ICU in 1",]
  iindat65$value = 
    pdat[pdat$group == "ICU in 4","value"] + 
    pdat[pdat$group == "ICU in 5","value"]
  
  iindat65$group = "icu in 65"
  
  
  
  dudat = pdat[pdat$group == "deaths: daily 1",]
  dudat$value = 0.8*pdat[pdat$group == "deaths: daily 1","value"] + 
    0.8*pdat[pdat$group == "deaths: daily 2","value"] + 
    0.8*pdat[pdat$group == "deaths: daily 3","value"] + 
    pdat[pdat$group == "deaths: daily 4","value"] + 
    pdat[pdat$group == "deaths: daily 5","value"]
  
  dudat$group = "deaths: daily"

  dudat65 = pdat[pdat$group == "deaths: daily 1",]
  dudat65$value = 
    pdat[pdat$group == "deaths: daily 4","value"] + 
    pdat[pdat$group == "deaths: daily 5","value"]
  
  dudat65$group = "deaths: daily 65"
  
  
  kudat = pdat[pdat$group == "deaths: cumulative 1",]
  kudat$value = 0.8*pdat[pdat$group == "deaths: cumulative 1","value"] + 
    0.8*pdat[pdat$group == "deaths: cumulative 2","value"] + 
    0.8*pdat[pdat$group == "deaths: cumulative 3","value"] + 
    pdat[pdat$group == "deaths: cumulative 4","value"] + 
    pdat[pdat$group == "deaths: cumulative 5","value"]
  
  kudat$group = "deaths: cumulative"
  
  kudat65 = pdat[pdat$group == "deaths: cumulative 1",]
  kudat65$value = 
    pdat[pdat$group == "deaths: cumulative 4","value"] + 
    pdat[pdat$group == "deaths: cumulative 5","value"]
  
  kudat65$group = "deaths: cumulative 65"
  


  spdat = rbind(odat, hdat, idat, dudat, kudat, hindat, iindat,
                odat65, hdat65, idat65, dudat65, kudat65, hindat65, iindat65)
  
  return(translate_group_labels(spdat))
}




run_simulation_model = function(scenario_parameters, calibration_data, Bfun, observed_data, duration_time, w)
{

  # Do not calibrate; use calibration from the baseline scenario.
  scenario_parameters$calibrate_infected = FALSE
  scenario_parameters$calibrate_hosp     = FALSE
  scenario_parameters$calibrate_icu      = FALSE
  scenario_parameters$calibrate_death    = FALSE

  ## weights za other scenarije iz osnovnega
  wt1 = calibration_data$calib_wt_hosp
  scenario_parameters$p_hosp1 = wt1 * scenario_parameters$p_hosp1
  scenario_parameters$p_hosp2 = wt1 * scenario_parameters$p_hosp2
  scenario_parameters$p_hosp3 = wt1 * scenario_parameters$p_hosp3
  scenario_parameters$p_hosp4 = wt1 * scenario_parameters$p_hosp4
  scenario_parameters$p_hosp5 = wt1 * scenario_parameters$p_hosp5

  wt1 = calibration_data$calib_wt_icu
  scenario_parameters$p_ICU1 = wt1 * scenario_parameters$p_ICU1
  scenario_parameters$p_ICU2 = wt1 * scenario_parameters$p_ICU2
  scenario_parameters$p_ICU3 = wt1 * scenario_parameters$p_ICU3
  scenario_parameters$p_ICU4 = wt1 * scenario_parameters$p_ICU4
  scenario_parameters$p_ICU5 = wt1 * scenario_parameters$p_ICU5

  calibration_data_scen = calibrate_model_observed_vaccination_waning(Bfun, observed_data, duration_time, w, scenario_parameters)

  wt1 = calibration_data$calib_wt_infected

  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 1"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 1"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 2"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 2"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 3"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 3"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 4"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 4"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 5"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="infections: daily 5"]

  wt1 = calibration_data$calib_wt_dead

  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 1"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 1"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 2"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 2"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 3"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 3"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 4"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 4"]
  calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 5"] = wt1 * calibration_data_scen$pdat$value[calibration_data_scen$pdat$group=="deaths: daily 5"]

  # Keep scenario cumulative deaths consistent with the calibrated daily series.
  calibration_data_scen$pdat = recalculate_cumulative_deaths(calibration_data_scen$pdat)


  # combine projekcije
  pdat_scen = calibration_data_scen$pdat
  spdat_scen = combine_simulation_projections_over65(pdat_scen)

  return(spdat_scen)


}



compute_ts_CI = function(sim_results, CI_low=0.25, CI_high=0.75)
{

  # Each simulation has five columns; column 2 contains its value.
  # Already summarised data (for example the baseline) has one value column.
  if (ncol(sim_results) >= 5 && ncol(sim_results) %% 5 == 0) {
    sel_col = seq(2, ncol(sim_results), 5)
    tdat = sim_results[, sel_col, drop = FALSE]
  } else {
    tdat = sim_results[, "value", drop = FALSE]
  }

  # Keep the simulation paths so period totals can be calculated per
  # simulation rather than by adding pointwise interval bounds.
  simulation_values = lapply(seq_len(nrow(tdat)), function(i) {
    as.numeric(tdat[i, ])
  })

  CI_low_values <- apply(tdat, 1, function(x) quantile(x, probs = CI_low))
  CI_high_values <- apply(tdat, 1, function(x) quantile(x, probs = CI_high))
  CI_med_values <- apply(tdat, 1, function(x) quantile(x, probs = 0.5))

  # print(CI_high_values)
  # print(CI_med_values)
  # print(CI_high_values)

  outdat = sim_results[,1:5]
  outdat = cbind(outdat, CI_low_values, CI_med_values, CI_high_values)
  outdat$simulation_values = I(simulation_values)

  return(outdat)
} 



# Calculate cumulative totals for the period.
compute_tot_sum = function(res, date_start="2021-01-01", date_end="2021-12-31", fact100k = 21.01462765957446808510)
{

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")

  group = unique(res$group)
  #print(group)

  mask = res$date >= date_start & res$date <= date_end
  tdat = res[mask,]

  #print(tdat)

  totals = do.call(rbind, lapply(group, function(skp) {
    mask = (tdat$group == skp)
    ttdat = tdat[mask,]

    cs = sum(ttdat$value, na.rm = TRUE) - ttdat$value[1]

    data.frame(
      group = skp,
      total = sprintf("%.0f", cs),
      per_100k = sprintf("%.0f", cs/fact100k),
      stringsAsFactors = FALSE
    )
  }))

  print(totals, row.names = FALSE)
  invisible(totals)

}



# Calculate cumulative totals for the period.
compute_tot_sum_CI = function(CIres, date_start="2021-01-01", date_end="2021-12-31", fact100k = 21.01462765957446808510)
{

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")

  group = unique(CIres$group)
  #print(group)

  mask = CIres$date >= date_start & CIres$date <= date_end
  tdat = CIres[mask,]

  #print(tdat)

  totals = do.call(rbind, lapply(group, function(skp) {
    mask = (tdat$group == skp)
    ttdat = tdat[mask,]

    if ("simulation_values" %in% names(ttdat)) {
      simulation_matrix = do.call(rbind, lapply(ttdat$simulation_values, as.numeric))
      simulation_totals = vapply(seq_len(ncol(simulation_matrix)), function(j) {
        values = simulation_matrix[, j]
        if (grepl("cumulative", skp, fixed = TRUE)) {
          tail(values, 1) - values[1]
        } else {
          sum(values, na.rm = TRUE) - values[1]
        }
      }, numeric(1))

      cs1 = quantile(simulation_totals, probs = 0.025)
      cs2 = quantile(simulation_totals, probs = 0.5)
      cs3 = quantile(simulation_totals, probs = 0.975)
    } else {
      stop("CI result is missing per-simulation values")
    }

    data.frame(
      group = skp,
      total = sprintf("%.0f [%.0f, %.0f]", cs2, cs1, cs3),
      per_100k = sprintf("%.0f [%.0f, %.0f]", cs2/fact100k, cs1/fact100k, cs3/fact100k),
      stringsAsFactors = FALSE
    )
  }))

  print(totals, row.names = FALSE)
  invisible(totals)

}
