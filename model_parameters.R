############################################
##### Model parameters ####
############################################

duration_time = 870

param = list()


param$N = 2100000
param$initial_cases = 25
param$D_incubation = 5.2
param$D_infectious = 2.9


param$D_recovery_mild = 12
param$D_recovery_hosp = 12
param$D_recovery_ICU  = 14
param$D_death = 14

# waning parameters
param$D_waning_R = 365
param$D_waning_V = 365


# Weight modeled infections relative to observed infections
param$w_inf_daily_t = rep(0.4, duration_time)



# Risk-reduction factors for vaccinated people
param$infectiousness_factor_vaccinated = 5
param$hospitalization_factor_vaccinated = 10
param$icu_factor_vaccinated = 5
param$mortality_factor_vaccinated = 10




# leave this unchanged
p_fatal_a_t = rep(0, duration_time)
p_fatal_a_t[1:40]    = rep(10.0/100, 40)
p_fatal_a_t[41:100]  = rep(0.5/100, 60)
p_fatal_a_t[101:215] = rep(2.5/100, 115)
p_fatal_a_t[216:275] = rep(5.1/100, 60)
p_fatal_a_t[276:300] = rep(2.8/100, 25)
p_fatal_a_t[301:350] = rep(1.4/100, 50)
p_fatal_a_t[351:390] = rep(0.8/100, 40)
p_fatal_a_t[391:duration_time] = rep(0.2/100, duration_time-390)

p_fatal_a_t1 = 0.0 * p_fatal_a_t
p_fatal_a_t2 = 0.0 * p_fatal_a_t
p_fatal_a_t3 = 0.0 * p_fatal_a_t
p_fatal_a_t4 = 0.8 * p_fatal_a_t
p_fatal_a_t5 = 2.0 * p_fatal_a_t



p_fatal_s_t = rep(20/100, duration_time)
p_fatal_s_t[550:duration_time] = 25/100 
p_fatal_s_t[600:duration_time] = 40/100
p_fatal_s_t[636:duration_time] = 20/100

# Apply an age-specific odds ratio to the baseline ICU fatality probability.
# An odds ratio of 3.5 means that the odds of dying after ICU are 3.5 times
# the baseline odds. For a baseline probability of 40%, this gives 70%.
apply_odds_ratio = function(probability, odds_ratio) {
  odds_ratio * probability / (1 - probability + odds_ratio * probability)
}


p_fatal_s_t1 = apply_odds_ratio(p_fatal_s_t, 0.0)
p_fatal_s_t2 = apply_odds_ratio(p_fatal_s_t, 0.0)
p_fatal_s_t3 = apply_odds_ratio(p_fatal_s_t, 0.0)
p_fatal_s_t4 = apply_odds_ratio(p_fatal_s_t, 0.8)
p_fatal_s_t5 = apply_odds_ratio(p_fatal_s_t, 2.0)

# Adjust mortality in younger age groups for the ANG variant.
# Asymptomatic deaths are unchanged because they are assumed to occur in hospital.
p_fatal_s_t3[370:duration_time] = apply_odds_ratio(p_fatal_s_t[370:duration_time], 1.0)
p_fatal_s_t4[370:duration_time] = apply_odds_ratio(p_fatal_s_t[370:duration_time], 1.75)
p_fatal_s_t5[370:duration_time] = apply_odds_ratio(p_fatal_s_t[370:duration_time], 4.0)

p_fatal_s_t3[430:duration_time] = apply_odds_ratio(p_fatal_s_t[430:duration_time], 1.0)
p_fatal_s_t4[430:duration_time] = apply_odds_ratio(p_fatal_s_t[430:duration_time], 1.75)
p_fatal_s_t5[430:duration_time] = apply_odds_ratio(p_fatal_s_t[430:duration_time], 2.0)

p_fatal_s_t3[500:duration_time] = apply_odds_ratio(p_fatal_s_t[500:duration_time], 2.0)
p_fatal_s_t4[500:duration_time] = apply_odds_ratio(p_fatal_s_t[500:duration_time], 3.5)
p_fatal_s_t5[500:duration_time] = apply_odds_ratio(p_fatal_s_t[500:duration_time], 4.0)


param$p_fatal_a1 = p_fatal_a_t1
param$p_fatal_a2 = p_fatal_a_t2
param$p_fatal_a3 = p_fatal_a_t3
param$p_fatal_a4 = p_fatal_a_t4
param$p_fatal_a5 = p_fatal_a_t5

param$p_fatal_s1 = p_fatal_s_t1
param$p_fatal_s2 = p_fatal_s_t2
param$p_fatal_s3 = p_fatal_s_t3
param$p_fatal_s4 = p_fatal_s_t4
param$p_fatal_s5 = p_fatal_s_t5




p_icu_t = rep(15/100, duration_time)
p_icu_t[381:duration_time] = rep(23/100, duration_time-380)
p_icu_t[501:duration_time] = rep(25/100, duration_time-500)
p_icu_t[601:duration_time] = rep(25/100, duration_time-600)
p_icu_t[611:duration_time] = rep(25/100, duration_time-610)

p_icu_t1 = 0.0 * p_icu_t
p_icu_t2 = 0.0 * p_icu_t
p_icu_t3 = 0.0 * p_icu_t
p_icu_t4 = 0.9 * p_icu_t
p_icu_t5 = 1.3 * p_icu_t

# Increase hospitalization share for group 3 from the ANG variant onward
p_icu_t3[400:duration_time] = 1.0 * p_icu_t[400:duration_time]
p_icu_t4[400:duration_time] = 1.0 * p_icu_t[400:duration_time]
p_icu_t5[400:duration_time] = 1.0 * p_icu_t[400:duration_time]

p_icu_t3[445:duration_time] = 0.9 * p_icu_t[445:duration_time]
p_icu_t4[445:duration_time] = 1.0 * p_icu_t[445:duration_time]
p_icu_t5[445:duration_time] = 1.0 * p_icu_t[445:duration_time]

p_icu_t3[550:duration_time] = 1.0 * p_icu_t[550:duration_time]
p_icu_t4[550:duration_time] = 1.0 * p_icu_t[550:duration_time]
p_icu_t5[550:duration_time] = 1.0 * p_icu_t[550:duration_time]

p_icu_t3[600:duration_time] = 1.0 * p_icu_t[600:duration_time]
p_icu_t4[600:duration_time] = 1.0 * p_icu_t[600:duration_time]
p_icu_t5[600:duration_time] = 1.0 * p_icu_t[600:duration_time]


param$p_ICU1 = p_icu_t1
param$p_ICU2 = p_icu_t2
param$p_ICU3 = p_icu_t3
param$p_ICU4 = p_icu_t4
param$p_ICU5 = p_icu_t5



p_hosp_t = rep(10/100, duration_time)
p_hosp_t[601:duration_time] = 9/100
p_hosp_t[611:duration_time] = 9/100

p_hosp_t1 = rep(0, duration_time)
p_hosp_t2 = rep(0, duration_time)
p_hosp_t3 = 0.1 * p_hosp_t
p_hosp_t4 = 1.0 * p_hosp_t
p_hosp_t5 = 1.5 * p_hosp_t


p_hosp_t3[320:duration_time] = 0.1*p_hosp_t[320:duration_time]
p_hosp_t4[320:duration_time] = 1.0*p_hosp_t[320:duration_time]
p_hosp_t5[320:duration_time] = 1.2*p_hosp_t[320:duration_time]


# Increase hospitalization share for group 3 from the ANG variant onward

p_hosp_t3[390:duration_time] = 0.3*p_hosp_t[390:duration_time]
p_hosp_t4[390:duration_time] = 1.0*p_hosp_t[390:duration_time]
p_hosp_t5[390:duration_time] = 1.5*p_hosp_t[390:duration_time]

p_hosp_t3[430:duration_time] = 0.3*p_hosp_t[430:duration_time]
p_hosp_t4[430:duration_time] = 1.0*p_hosp_t[430:duration_time]
p_hosp_t5[430:duration_time] = 1.5*p_hosp_t[430:duration_time]

p_hosp_t3[550:duration_time] = 0.4*p_hosp_t[550:duration_time]
p_hosp_t4[550:duration_time] = 1.0*p_hosp_t[550:duration_time]
p_hosp_t5[550:duration_time] = 1.3*p_hosp_t[550:duration_time]


param$p_hosp1 = p_hosp_t1
param$p_hosp2 = p_hosp_t2
param$p_hosp3 = p_hosp_t3
param$p_hosp4 = p_hosp_t4
param$p_hosp5 = p_hosp_t5


param$hospitalization_shift = 6
param$icu_shift  = 5
param$death_shift = 0


############################################
##### Mixing matrices ####
############################################

# Mixing matrix, transformed after assembly
mix_mat1 = c(1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)

mix_mat2 = c(1.50, 1.25, 1.20, 1.05, 1.05,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)


# Increase mixing from 2021-09-01
mix_mat3 = c(1.75, 1.30, 1.25, 1.05, 1.05,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)


# Increase mixing from 2021-10-01
mix_mat4 = c(3.00, 1.30, 1.25, 1.05, 1.05,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)


transition_days1 = as.numeric(as.Date("2021-02-01") - as.Date("2020-03-04"))
transition_days2 = as.numeric(as.Date("2021-09-01") - as.Date("2021-02-01"))
transition_days3 = as.numeric(as.Date("2021-10-10") - as.Date("2021-09-01"))

# Assemble matrices over time
m1 = array(mix_mat1, dim=c(5,5,transition_days1))
m2 = array(mix_mat2, dim=c(5,5,transition_days2))
m3 = array(mix_mat3, dim=c(5,5,transition_days3))
m4 = array(mix_mat4, dim=c(5,5,duration_time-transition_days3))

mix_array = abind::abind(m1, m2, m3, m4)

param$MixMat = mix_array






###################################################################
### vaccination ####
###################################################################



## Shape vaccination to match the observed situation

# group 5

vaccination_data = vaccination_data_by_age$vaccination_group5.2nd
vaccination_data[is.na(vaccination_data)] = 0
# daily increase as a population share
vaccination_rate = diff(vaccination_data) / population_5 
vaccination_start_offset = as.numeric(vaccination_data_by_age$date[1] - observed_data$date[1]) -  5

vaccination_weights = c(0.0, w5 * vaccination_rate, 0.0)
vaccination_window_lengths = c(vaccination_start_offset, rep(1, length(vaccination_rate)), 10)

vfun5 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)


# group 4

vaccination_data = vaccination_data_by_age$vaccination_group4.2nd
vaccination_data[is.na(vaccination_data)] = 0
#vsakodnevni prirastek v share
vaccination_rate = diff(vaccination_data) / population_4 
vaccination_start_offset = as.numeric(vaccination_data_by_age$date[1] - observed_data$date[1]) -  5

vaccination_weights = c(0.0, w4 * vaccination_rate, 0.0)
vaccination_window_lengths = c(vaccination_start_offset, rep(1, length(vaccination_rate)), 10)

vfun4 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)


# group 3

vaccination_data = vaccination_data_by_age$vaccination_group3.2nd
vaccination_data[is.na(vaccination_data)] = 0
#vsakodnevni prirastek v share
vaccination_rate = diff(vaccination_data) / population_3 
vaccination_start_offset = as.numeric(vaccination_data_by_age$date[1] - observed_data$date[1]) -  5

vaccination_weights = c(0.0, w3 * vaccination_rate, 0.0)
vaccination_window_lengths = c(vaccination_start_offset, rep(1, length(vaccination_rate)), 10)

vfun3 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)



# group 2

vaccination_data = vaccination_data_by_age$vaccination_group2.2nd
vaccination_data[is.na(vaccination_data)] = 0
#vsakodnevni prirastek v share
vaccination_rate = diff(vaccination_data) / population_2 
vaccination_start_offset = as.numeric(vaccination_data_by_age$date[1] - observed_data$date[1]) -  5

vaccination_weights = c(0.0, w2 * vaccination_rate, 0.0)
vaccination_window_lengths = c(vaccination_start_offset, rep(1, length(vaccination_rate)), 10)

vfun2 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)



# group 1


vaccination_data = vaccination_data_by_age$vaccination_group1.2nd
vaccination_data[is.na(vaccination_data)] = 0
# daily increase as a population share
vaccination_rate = diff(vaccination_data) / population_1 
vaccination_start_offset = as.numeric(vaccination_data_by_age$date[1] - observed_data$date[1]) -  5

vaccination_weights = c(0.0, w1 * vaccination_rate, 0.0)
vaccination_window_lengths = c(vaccination_start_offset, rep(1, length(vaccination_rate)), 10)

vfun1 = Bt_rect_time(duration_time, vaccination_weights, vaccination_window_lengths)




param$vaccfun1 = vfun1
param$vaccfun2 = vfun2
param$vaccfun3 = vfun3
param$vaccfun4 = vfun4
param$vaccfun5 = vfun5


###################################################################
### optimization and calibration ####
###################################################################

# Optimization parameters. Bw is stored as beta, so express epidemiological
# bounds on the reproduction-number scale and convert them to beta.
# Each L-BFGS-B iteration requires many model runs because the gradient of all
# optimized coefficients is calculated numerically.
param$Bw_opt_maxit = 20
param$Bw_opt_factr = 1e7
param$Bw_lower = 0.5 / param$D_infectious
param$Bw_upper = 8.0 / param$D_infectious



# calibrate infections
param$calibrate_infected = FALSE
param$calibrate_hosp     = FALSE
param$calibrate_icu      = FALSE
param$calibrate_death    = FALSE

param$calib_tolerance    = 0.15


# confidence intervals
param$compute_CI = FALSE
param$CL = 0.25
param$CU = 0.15
