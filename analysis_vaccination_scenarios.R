suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(deSolve)
  library(scales)
  library(plotly)
  library(foreach)
  library(doParallel)
})


log_progress = function(text) {
  message(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text))
}

analysis_seed = 2505
RNGkind("L'Ecuyer-CMRG")
set.seed(analysis_seed)

log_progress("Starting analysis")


# Set to TRUE for interactive plotting, FALSE when running as a non-graphical script.
produce_graphical_output = FALSE



project_dir = getwd()
required_files = c(
  "model_functions.R",
  "model_data.R",
  "model_parameters.R",
  "initial_bw_parameters.R",
  "simulation_functions.R",
  "optimization_functions.R",
  "model_error_metrics.R",
  "plot_calibration_results.R",
  "plot_scenario_results.R",
  "table_scenario_results.R",
  file.path("data", "population_shares.csv"),
  file.path("data", "slo-covid-19_stats.csv"),
  file.path("data", "nijz_vw_lusy_okuzeni.csv"),
  file.path("data", "sledilnik_vaccination-by_age.csv")
)
missing_files = required_files[!file.exists(file.path(project_dir, required_files))]
if (length(missing_files) > 0) {
  stop("Run this script from the public source directory. Missing: ",
       paste(missing_files, collapse = ", "))
}


############################################
##### functions ####
############################################


# model functions
source(file.path(project_dir, "model_functions.R"))

# model data
source(file.path(project_dir, "model_data.R"))

# parameters
source(file.path(project_dir, "model_parameters.R"))
source(file.path(project_dir, "initial_bw_parameters.R"))
Bw4_initial = Bw4

# simulations
source(file.path(project_dir, "simulation_functions.R"))
source(file.path(project_dir, "optimization_functions.R"))
source(file.path(project_dir, "model_error_metrics.R"))
source(file.path(project_dir, "plot_calibration_results.R"))

log_progress("Model functions, data, parameters, and simulation helpers loaded")





last_date = Sys.Date()

start_date = as.Date("2021-01-01")
end_date = as.Date("2021-12-31")




##################################################################################
## Optimization of BW parameters (very demanding task)
##################################################################################


Bw_optimization_file = file.path(project_dir, "Bw_optimization.RData")
if (!file.exists(Bw_optimization_file)) {
  stop("Missing Bw_optimization.RData. Run run_Bw_optimization.R first.")
}

log_progress("Loading optimized Bw values")
load(Bw_optimization_file)

## optimized Bf used for further modelling
# Use the exact beta function saved by the optimization step.
Bfun4 = Bw_optimization$Bfun
param$Bw = Bw4



# Plot the BW-optimized model before empirical calibration is applied.
param_bw = param

pdat_bw = calculate_model_observed_vaccination_waning(
  Bfun4, observed_data, duration_time, w, param_bw
)
spdat_bw = combine_simulation_projections_over65(pdat_bw)
spdat_bw$CI_low_values = spdat_bw$value
spdat_bw$CI_med_values = spdat_bw$value
spdat_bw$CI_high_values = spdat_bw$value
bw_optimization_publication_plot = plot_model_fit_publication(
  spdat_bw, start_date, end_date,
  title = "Model fit after transmission optimization",
  output_file = file.path(project_dir, "results", "optimization-fit.png")
)
if (produce_graphical_output) {
  print(bw_optimization_publication_plot)
}

bw_metric_groups = c("hospitalizations", "icu", "deaths: daily")
bw_metric_names = c("Hospitalizations", "ICU", "Deaths")

bw_optimization_metrics = do.call(rbind, lapply(seq_along(bw_metric_groups), function(i) {
  metric_data = spdat_bw[
    spdat_bw$group == bw_metric_groups[i] &
      spdat_bw$date >= start_date &
      spdat_bw$date <= end_date,
  ]

  data.frame(
    Outcome = bw_metric_names[i],
    RMSE = rmse(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    NRMSE = nrmse(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    MAE = mae(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    NMAE = nmae(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    PBIAS = pbias(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    )
  )
}))

bw_metric_columns = c("RMSE", "NRMSE", "MAE", "NMAE", "PBIAS")
bw_optimization_metrics[, bw_metric_columns] = round(
  bw_optimization_metrics[, bw_metric_columns],
  4
)

print("BW optimization metrics, model vs actual data")
print(bw_optimization_metrics, row.names = FALSE)


#### comparison between init and optimized betas / R

# # Bw4 is stored as beta; convert both series back to reproduction-number scale.
# R_comparison = data.frame(
#   Window = seq_along(Bw4_initial),
#   Change_date = as.Date("2020-03-04") + beta_change_days,
#   Initial = Bw4_initial * param$D_infectious,
#   Optimized = Bw4 * param$D_infectious
# )

# R_comparison_2021 = R_comparison[
#   R_comparison$Change_date >= start_date &
#     R_comparison$Change_date <= end_date,
# ]

# R_comparison_plot = ggplot(R_comparison_2021, aes(x = Change_date)) +
#   geom_step(aes(y = Initial, colour = "Initial"), linewidth = 0.8) +
#   geom_point(aes(y = Initial, colour = "Initial"), size = 1.5) +
#   geom_step(aes(y = Optimized, colour = "Optimized"), linewidth = 0.8) +
#   geom_point(aes(y = Optimized, colour = "Optimized"), size = 1.5) +
#   scale_colour_manual(values = c(Initial = "grey35", Optimized = "firebrick")) +
#   labs(
#     title = "Initial versus optimized R values",
#     x = "Date",
#     y = "R",
#     colour = "Values"
#   ) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))

# R_comparison_plot = ggplotly(
#   R_comparison_plot,
#   tooltip = c("x", "y", "colour")
# )

# print(R_comparison_plot)









##################################################################################
## Parameters for alternative scenarios
##################################################################################

scenario_parameters1 = param

###################################################################
### no vaccination ####
###################################################################


# group 5

vaccination_weights = c(0.0, 0.0,  0.0)
vaccination_window_lengths = c(vaccination_start_offset, rep(1, length(vaccination_data_by_age$vaccination_group5.2nd)), 10)

vfun5 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)
vfun4 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)
vfun3 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)
vfun2 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)
vfun1 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)



scenario_parameters1$vaccfun1 = vfun1
scenario_parameters1$vaccfun2 = vfun2
scenario_parameters1$vaccfun3 = vfun3
scenario_parameters1$vaccfun4 = vfun4
scenario_parameters1$vaccfun5 = vfun5




###################################################################
### vaccination increased by factor 1.30 ####
###################################################################

# This corresponds to approximately 70% vaccination coverage with current data.
# Weights are set so that ending coverage is approximately 70%.
scenario_parameters2 = param

scenario_parameters2$vaccfun1 = 1.45 * param$vaccfun1
scenario_parameters2$vaccfun2 = 1.4 * param$vaccfun2
scenario_parameters2$vaccfun3 = 1.25 * param$vaccfun3
scenario_parameters2$vaccfun4 = 1.15 * param$vaccfun4
scenario_parameters2$vaccfun5 = 1.075 * param$vaccfun5



###################################################################
### vaccination reduced by factor 0.70 ####
###################################################################

# This corresponds to approximately 37% vaccination coverage with current data.
scenario_parameters3 = param

scenario_parameters3$vaccfun1 = 0.7 * param$vaccfun1
scenario_parameters3$vaccfun2 = 0.7 * param$vaccfun2
scenario_parameters3$vaccfun3 = 0.7 * param$vaccfun3
scenario_parameters3$vaccfun4 = 0.7 * param$vaccfun4
scenario_parameters3$vaccfun5 = 0.7 * param$vaccfun5



###################################################################
### vaccination only for age 65+ ####
###################################################################

# This corresponds to approximately 70% vaccination coverage with current data.
# Weights are set so that ending coverage is approximately 70%.
scenario_parameters5 = param

scenario_parameters5$vaccfun1 = 0.001 * param$vaccfun1
scenario_parameters5$vaccfun2 = 0.001 * param$vaccfun2
scenario_parameters5$vaccfun3 = 0.001 * param$vaccfun3
scenario_parameters5$vaccfun4 = 1.0 * param$vaccfun4
scenario_parameters5$vaccfun5 = 1.0 * param$vaccfun5



###################################################################
### vaccination increased by factor 0.7, only for age 65+ ####
###################################################################

# This corresponds to approximately 70% vaccination coverage with current data.
# Weights are set so that ending coverage is approximately 70%.
scenario_parameters6 = param

scenario_parameters6$vaccfun1 = 0.001 * param$vaccfun1
scenario_parameters6$vaccfun2 = 0.001 * param$vaccfun2
scenario_parameters6$vaccfun3 = 0.001 * param$vaccfun3
scenario_parameters6$vaccfun4 = 0.7 * param$vaccfun4
scenario_parameters6$vaccfun5 = 0.7 * param$vaccfun5




##################################################################################
## Model calculation: observed state and calibration
##################################################################################



#### Calibration and computation ######################

param$calibrate_infected = TRUE
param$calibrate_hosp     = TRUE
param$calibrate_icu      = TRUE
param$calibrate_death    = TRUE

log_progress("Starting baseline calibration")

# pdat_scen4 = calculate_model_observed_vaccination_waning(Bfun4, observed_data, duration_time, w, param)
calibration_data =  calibrate_model_observed_vaccination_waning(Bfun4, observed_data, duration_time, w, param)

  log_progress("Baseline calibration complete")

pdat_scen4  = calibration_data$pdat

spdat_scen4 = combine_simulation_projections_over65(pdat_scen4)

#this is done, although we do not have simulations of S0 to obtain the same structure
spdat_scen4 = compute_ts_CI(spdat_scen4, 0.025, 0.975)

calibration_publication_plot = plot_model_fit_publication(
  spdat_scen4, start_date, end_date,
  title = "Model fit after empirical calibration",
  output_file = file.path(project_dir, "results", "calibration-fit.png")
)



if (produce_graphical_output) {
  print(calibration_publication_plot)
}

calibration_metrics = do.call(rbind, lapply(seq_along(bw_metric_groups), function(i) {
  metric_data = spdat_scen4[
    spdat_scen4$group == bw_metric_groups[i] &
      spdat_scen4$date >= start_date &
      spdat_scen4$date <= end_date,
  ]

  data.frame(
    Outcome = bw_metric_names[i],
    RMSE = rmse(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    NRMSE = nrmse(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    MAE = mae(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    NMAE = nmae(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    ),
    PBIAS = pbias(
      if (bw_metric_groups[i] == "deaths: daily")
        as.numeric(stats::filter(metric_data$observed, rep(1 / 7, 7), sides = 2))
      else metric_data$observed,
      metric_data$value
    )
  )
}))

calibration_metrics[, bw_metric_columns] = round(
  calibration_metrics[, bw_metric_columns],
  4
)

print("Calibration metrics, model vs actual data")
print(calibration_metrics, row.names = FALSE)

results_dir = file.path(project_dir, "results")
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

metrics_to_markdown = function(metrics_table) {
  c(
    "| Outcome | RMSE | NRMSE | MAE | NMAE | PBIAS |",
    "|---|---:|---:|---:|---:|---:|",
    apply(metrics_table, 1, function(row) {
      sprintf(
        "| %s | %.4f | %.4f | %.4f | %.4f | %.4f |",
        row[["Outcome"]],
        as.numeric(row[["RMSE"]]),
        as.numeric(row[["NRMSE"]]),
        as.numeric(row[["MAE"]]),
        as.numeric(row[["NMAE"]]),
        as.numeric(row[["PBIAS"]])
      )
    })
  )
}

writeLines(
  c(
    "# Optimization and Calibration Evaluation",
    "",
    "## BW Optimization Metrics",
    "",
    "Model vs actual data.",
    "",
    metrics_to_markdown(bw_optimization_metrics),
    "",
    "## Calibration Metrics",
    "",
    "Model vs actual data.",
    "",
    metrics_to_markdown(calibration_metrics)
  ),
  file.path(results_dir, "optimization_and_calibration_evaluation.md")
)


print("##################################################")
print("Scenario S0: baseline")
print("##################################################")

compute_tot_sum(spdat_scen4, date_start="2021-01-01", date_end="2021-12-31")






##################################################
#### scenario 1: no vaccination
##################################################


# spdat_scen1 = run_simulation_model(scenario_parameters1, calibration_data, Bfun4, observed_data, duration_time, w)

# paralelizacija

# Simulation settings
Nsim = 500
eps_pert = 0.10
numCores = max(1, parallel::detectCores() - 2)

##################################################

log_progress(sprintf("Starting parallel simulations: %d simulations per scenario", Nsim))


cl <- makeCluster(numCores) # create a cluster
registerDoParallel(cl)
parallel::clusterSetRNGStream(cl, analysis_seed + 1)

log_progress(sprintf("Scenario S1 started using %d worker cores", numCores))

nB = length(Bw4)


param_pert = scenario_parameters1

results = foreach(i=1:Nsim, .combine=cbind, .export=c("lsoda"), .verbose=TRUE ) %dopar% {
    set.seed(analysis_seed + 1000 + i)
    
    pert_vec = runif(nB, min = -1, max = 1)
    Bw4_pert = pmax(Bw4 * (1 + eps_pert * pert_vec), 0.005)

    Bfun_pert = Bt_rect_time(duration_time, Bw4_pert, beta_window_lengths)
  
    #perturbacija VE
    param_pert$infectiousness_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters1$infectiousness_factor_vaccinated, sd = 2))
    param_pert$hospitalization_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters1$hospitalization_factor_vaccinated, sd = 2))
    param_pert$icu_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters1$icu_factor_vaccinated, sd = 2))
    param_pert$mortality_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters1$mortality_factor_vaccinated, sd = 2))
  
    run_simulation_model(param_pert, calibration_data, Bfun_pert, observed_data, duration_time, w)
}

stopCluster(cl)
  log_progress("Scenario S1 simulations complete")

# calculation intervalov zaupanja CI95
spdat_scen1 = compute_ts_CI(results, 0.025, 0.975)



print("##################################################")
print("Scenario S1: no vaccination")
print("##################################################")

# calculation total numbers za periode
compute_tot_sum_CI(spdat_scen1, date_start="2021-01-01", date_end="2021-12-31")






##################################################
#### scenario 2: vaccination increased by factor 1.30
##################################################


# spdat_scen2 = run_simulation_model(scenario_parameters2, calibration_data, Bfun4, observed_data, duration_time, w)


# paralelizacija

cl <- makeCluster(numCores) # create a cluster
registerDoParallel(cl)
parallel::clusterSetRNGStream(cl, analysis_seed + 2)

log_progress(sprintf("Scenario S2 started using %d worker cores", numCores))

nB = length(Bw4)
 
param_pert = scenario_parameters2

results = foreach(i=1:Nsim, .combine=cbind, .export=c("lsoda"), .verbose=TRUE ) %dopar% {
    set.seed(analysis_seed + 2000 + i)
    
    pert_vec = runif(nB, min = -1, max = 1)
    Bw4_pert = pmax(Bw4 * (1 + eps_pert * pert_vec), 0.005)

    Bfun_pert = Bt_rect_time(duration_time, Bw4_pert, beta_window_lengths)
  
    #perturbacija VE
    param_pert$infectiousness_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters2$infectiousness_factor_vaccinated, sd = 2))
    param_pert$hospitalization_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters2$hospitalization_factor_vaccinated, sd = 2))
    param_pert$icu_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters2$icu_factor_vaccinated, sd = 2))
    param_pert$mortality_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters2$mortality_factor_vaccinated, sd = 2))
  
    run_simulation_model(param_pert, calibration_data, Bfun_pert, observed_data, duration_time, w)
}

stopCluster(cl)
  log_progress("Scenario S2 simulations complete")

# calculation intervalov zaupanja
spdat_scen2 = compute_ts_CI(results, 0.025, 0.975)


print("##################################################")
print("Scenario S2: vaccination increased by factor 1.30")
print("##################################################")

# calculation total numbers za periode
compute_tot_sum_CI(spdat_scen2, date_start="2021-01-01", date_end="2021-12-31")








##################################################
#### scenario 3: vaccination reduced by factor 0.70
##################################################

# spdat_scen3 = run_simulation_model(scenario_parameters3, calibration_data, Bfun4, observed_data, duration_time, w)


# paralelizacija

cl <- makeCluster(numCores) # create a cluster
registerDoParallel(cl)
parallel::clusterSetRNGStream(cl, analysis_seed + 3)

log_progress(sprintf("Scenario S3 started using %d worker cores", numCores))

nB = length(Bw4)
 
param_pert = scenario_parameters3

results = foreach(i=1:Nsim, .combine=cbind, .export=c("lsoda"), .verbose=TRUE ) %dopar% {
    set.seed(analysis_seed + 3000 + i)
    
    pert_vec = runif(nB, min = -1, max = 1)
    Bw4_pert = pmax(Bw4 * (1 + eps_pert * pert_vec), 0.005)

    Bfun_pert = Bt_rect_time(duration_time, Bw4_pert, beta_window_lengths)
  
    #perturbacija VE
    param_pert$infectiousness_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters3$infectiousness_factor_vaccinated, sd = 2))
    param_pert$hospitalization_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters3$hospitalization_factor_vaccinated, sd = 2))
    param_pert$icu_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters3$icu_factor_vaccinated, sd = 2))
    param_pert$mortality_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters3$mortality_factor_vaccinated, sd = 2))
  
    run_simulation_model(param_pert, calibration_data, Bfun_pert, observed_data, duration_time, w)
}

stopCluster(cl)
  log_progress("Scenario S3 simulations complete")

# calculation intervalov zaupanja
spdat_scen3 = compute_ts_CI(results, 0.025, 0.975)


print("##################################################")
print("Scenario 3: vaccination reduced by factor 0.70")
print("##################################################")

# calculation total numbers za periode
compute_tot_sum_CI(spdat_scen3, date_start="2021-01-01", date_end="2021-12-31")







##################################################
#### scenario 5: vaccination only for age 65+
##################################################

# spdat_scen5 = run_simulation_model(scenario_parameters5, calibration_data, Bfun4, observed_data, duration_time, w)

# paralelizacija

cl <- makeCluster(numCores) # create a cluster
registerDoParallel(cl)
parallel::clusterSetRNGStream(cl, analysis_seed + 5)

log_progress(sprintf("Scenario S5 started using %d worker cores", numCores))

nB = length(Bw4)
 
param_pert = scenario_parameters5

results = foreach(i=1:Nsim, .combine=cbind, .export=c("lsoda"), .verbose=TRUE ) %dopar% {
    set.seed(analysis_seed + 5000 + i)
    
    pert_vec = runif(nB, min = -1, max = 1)
    Bw4_pert = pmax(Bw4 * (1 + eps_pert * pert_vec), 0.005)

    Bfun_pert = Bt_rect_time(duration_time, Bw4_pert, beta_window_lengths)
  
    #perturbacija VE
    param_pert$infectiousness_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters5$infectiousness_factor_vaccinated, sd = 2))
    param_pert$hospitalization_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters5$hospitalization_factor_vaccinated, sd = 2))
    param_pert$icu_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters5$icu_factor_vaccinated, sd = 2))
    param_pert$mortality_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters5$mortality_factor_vaccinated, sd = 2))
  
    run_simulation_model(param_pert, calibration_data, Bfun_pert, observed_data, duration_time, w)
}

stopCluster(cl)
  log_progress("Scenario S5 simulations complete")

# calculation intervalov zaupanja
spdat_scen5 = compute_ts_CI(results, 0.025, 0.975)


print("##################################################")
print("Scenario 5: vaccination only for age 65+")
print("##################################################")

# calculation total numbers za periode
compute_tot_sum_CI(spdat_scen5, date_start="2021-01-01", date_end="2021-12-31")









##################################################
#### scenario 6: vaccination increased by factor 0.7, only for age 65+
##################################################

# spdat_scen6 = run_simulation_model(scenario_parameters6, calibration_data, Bfun4, observed_data, duration_time, w)


# paralelizacija

cl <- makeCluster(numCores) # create a cluster
registerDoParallel(cl)
parallel::clusterSetRNGStream(cl, analysis_seed + 6)

log_progress(sprintf("Scenario S6 started using %d worker cores", numCores))

nB = length(Bw4)
 
param_pert = scenario_parameters6

results = foreach(i=1:Nsim, .combine=cbind, .export=c("lsoda"), .verbose=TRUE ) %dopar% {
    set.seed(analysis_seed + 6000 + i)
    
    pert_vec = runif(nB, min = -1, max = 1)
    Bw4_pert = pmax(Bw4 * (1 + eps_pert * pert_vec), 0.005)

    Bfun_pert = Bt_rect_time(duration_time, Bw4_pert, beta_window_lengths)
  
    #perturbacija VE
    param_pert$infectiousness_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters6$infectiousness_factor_vaccinated, sd = 2))
    param_pert$hospitalization_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters6$hospitalization_factor_vaccinated, sd = 2))
    param_pert$icu_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters6$icu_factor_vaccinated, sd = 2))
    param_pert$mortality_factor_vaccinated = max(1, rnorm(1, mean = scenario_parameters6$mortality_factor_vaccinated, sd = 2))
  
    run_simulation_model(param_pert, calibration_data, Bfun_pert, observed_data, duration_time, w)
}

stopCluster(cl)
  log_progress("Scenario S6 simulations complete")

# calculation intervalov zaupanja
spdat_scen6 = compute_ts_CI(results, 0.025, 0.975)


print("##################################################")
print("Scenario 6: vaccination increased by factor 0.7, only for age 65+")
print("##################################################")

# calculation total numbers za periode
compute_tot_sum_CI(spdat_scen6, date_start="2021-01-01", date_end="2021-12-31")



##################################################
#### Save key data
##################################################

save(spdat_scen1, spdat_scen2, spdat_scen3, spdat_scen4, spdat_scen5, spdat_scen6,
        file="scenario_simulation_data.RData")

source(file.path(project_dir, "plot_scenario_results.R"))
source(file.path(project_dir, "table_scenario_results.R"))

log_progress("Analysis complete; data, plots, and tables saved")
