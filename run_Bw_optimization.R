suppressPackageStartupMessages({
  library(deSolve)
})

log_progress = function(text) {
  message(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text))
}

log_progress("Starting Bw optimization")

project_dir = getwd()
required_files = c(
  "model_functions.R",
  "model_data.R",
  "model_parameters.R",
  "initial_bw_parameters.R",
  "optimization_functions.R"
)
missing_files = required_files[!file.exists(file.path(project_dir, required_files))]
if (length(missing_files) > 0) {
  stop("Run this script from the public source directory. Missing: ",
       paste(missing_files, collapse = ", "))
}

source(file.path(project_dir, "model_functions.R"))
source(file.path(project_dir, "model_data.R"))
source(file.path(project_dir, "model_parameters.R"))
source(file.path(project_dir, "initial_bw_parameters.R"))
source(file.path(project_dir, "optimization_functions.R"))

Bw_optimization = optimize_Bw(
  Bw0 = Bw4,
  win_len = beta_window_lengths,
  observed_data = observed_data,
  duration_time = duration_time,
  w = w,
  param = param,
  optimization_start_date = as.Date("2021-01-01"),
  optimization_end_date = as.Date("2021-12-31"),
  optimize_indices = 17:length(Bw4)
)

Bw4 = Bw_optimization$Bw
Bfun4 = Bw_optimization$Bfun
save(
  Bw4,
  Bfun4,
  beta_change_days,
  beta_window_lengths,
  Bw_optimization,
  file = file.path(project_dir, "Bw_optimization.RData")
)

log_progress(sprintf(
  "Bw optimization complete: loss = %.4f, convergence = %d, objective evaluations = %d",
  Bw_optimization$fit$value,
  Bw_optimization$fit$convergence,
  Bw_optimization$objective_evaluations
))
log_progress("Saved optimized Bw values to Bw_optimization.RData")
