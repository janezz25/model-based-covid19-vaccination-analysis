############################################
##### observed Sledilnik data ####
############################################

data_dir = if (exists("project_dir", inherits = TRUE)) project_dir else getwd()
data_dir = file.path(data_dir, "data")

tdat = read.csv(
  file.path(data_dir, "slo-covid-19_stats.csv"),
  header = TRUE, stringsAsFactors = FALSE
)


tdat = tdat[5:nrow(tdat),]
tdat$date = as.Date(tdat$date)

tracker_data = tdat

observed_data = data.frame(date = tdat$date, 
                   tests.positive = tdat$cases.confirmed,
                   state.in_hospital = tdat$state.in_hospital, 
                   state.deceased.todate = tdat$state.deceased.todate,
                   state.icu = tdat$state.icu,
                   cases.active = tdat$cases.active,
                   vaccination.administered.todate = tdat$vaccination.administered.todate,
                   vaccination.administered2nd.todate = tdat$vaccination.administered2nd.todate)

observed_data = observed_data[!is.na(observed_data$date), ]



############################################
##### NIJZ mortality data ####
############################################


tdat = read.csv(
  file.path(data_dir, "nijz_vw_lusy_okuzeni.csv"),
  header = TRUE, stringsAsFactors = FALSE
)

tdat$date = as.Date(tdat$datum_izvida)

nijz_data = tdat



# deaths
# Align NIJZ deaths by date instead of assuming both live data sources have
# the same length and historical starting date.
death_match = match(observed_data$date, tdat$date)
observed_data$deceased = ifelse(
  is.na(death_match),
  0,
  tdat$stevilo_umrlih[death_match]
)
observed_data$deceased[is.na(observed_data$deceased)] = 0

observed_data$state.deceased.todate = cumsum(observed_data$deceased)



###################################################################
### age groups ####
###################################################################

sdata = read.csv(file.path(data_dir, "population_shares.csv"), sep = ";")


# w1 =sum( sdata$share[1:6])
# w2 =sum( sdata$share[7:10])
# w3 =sum( sdata$share[11:14])
# w4 =sum( sdata$share[15:16])
# w5 =sum( sdata$share[17:19])

population_1 =sum( sdata[1:25])
population_2 =sum( sdata[26:45])
population_3 =sum( sdata[46:65])
population_4 =sum( sdata[66:75])
population_5 =sum( sdata[76:86])

total_population = population_1+population_2+population_3+population_4+population_5

w1 =population_1 / total_population
w2 =population_2 / total_population
w3 =population_3 / total_population
w4 =population_4 / total_population
w5 =population_5 / total_population



w = c(w1, w2, w3, w4, w5)
dfw = data.frame(group = c("0-24", "25-44", "45-64", "65-74", "75+"),
                 "count" = c(population_1, population_2, population_3, population_4, population_5),
                 "share" = sprintf("%.2f%%", w * 100 ))







###################################################################
### vaccination ####
###################################################################


### Vaccination data

tdat = read.csv(
  file.path(data_dir, "sledilnik_vaccination-by_age.csv"),
  header = TRUE, stringsAsFactors = FALSE
)


tdat$date = as.Date(tdat$date)


# Build the model age groups
vaccination_group1.1st = tdat$vaccination.age.0.11.1st.todate + tdat$vaccination.age.12.17.1st.todate + tdat$vaccination.age.18.24.1st.todate
vaccination_group1.2nd = tdat$vaccination.age.0.11.2nd.todate + tdat$vaccination.age.12.17.2nd.todate + tdat$vaccination.age.18.24.2nd.todate

vaccination_group2.1st = tdat$vaccination.age.25.29.1st.todate + tdat$vaccination.age.30.34.1st.todate + 
  tdat$vaccination.age.35.39.1st.todate + tdat$vaccination.age.40.44.1st.todate
vaccination_group2.2nd = tdat$vaccination.age.25.29.2nd.todate + tdat$vaccination.age.30.34.2nd.todate + 
  tdat$vaccination.age.35.39.2nd.todate + tdat$vaccination.age.40.44.2nd.todate

vaccination_group3.1st = tdat$vaccination.age.45.49.1st.todate + tdat$vaccination.age.50.54.1st.todate + 
  tdat$vaccination.age.55.59.1st.todate + tdat$vaccination.age.60.64.1st.todate
vaccination_group3.2nd = tdat$vaccination.age.45.49.2nd.todate + tdat$vaccination.age.50.54.2nd.todate + 
  tdat$vaccination.age.55.59.2nd.todate + tdat$vaccination.age.60.64.2nd.todate

vaccination_group4.1st = tdat$vaccination.age.65.69.1st.todate + tdat$vaccination.age.70.74.1st.todate
vaccination_group4.2nd = tdat$vaccination.age.65.69.2nd.todate + tdat$vaccination.age.70.74.2nd.todate

vaccination_group5.1st = tdat$vaccination.age.75.79.1st.todate + tdat$vaccination.age.80.84.1st.todate + 
  tdat$vaccination.age.85.89.1st.todate + tdat$vaccination.age.90..1st.todate
vaccination_group5.2nd = tdat$vaccination.age.75.79.2nd.todate + tdat$vaccination.age.80.84.2nd.todate + 
  tdat$vaccination.age.85.89.2nd.todate + tdat$vaccination.age.90..2nd.todate


vaccination_data_by_age = data.frame(date = tdat$date, vaccination_group1.1st, vaccination_group1.2nd, vaccination_group2.1st, vaccination_group2.2nd, vaccination_group3.1st, vaccination_group3.2nd, vaccination_group4.1st, vaccination_group4.2nd, vaccination_group5.1st, vaccination_group5.2nd)
