#load('latitudes.RData')
#load('avg_temps.RData')
#load('stringency_diffs.RData')
#load('all3_1stDec_250221.RData')
#load('cases_deaths_peak_doy.RData')
#load('test_peak_doy.RData')
#load('conf_data.RData')

load('Data/resistance_resilience_reduction.RData')

# calculate day of year for each peak
cases_peak_doy <- rep(NA, 190)
tests_peak_doy <- rep(NA, 190)
deaths_peak_doy <- rep(NA, 190)
cases_peak_doy2 <- rep(NA, 190)
tests_peak_doy2 <- rep(NA, 190)
deaths_peak_doy2 <- rep(NA, 190)

for (i in 1:190) {
        try(cases_peak_doy[i] <- diff(c(as.Date('2020-01-01'),cases_dates[[i]][cases_peak_loc_1[i]])), silent=T)
        try(tests_peak_doy[i] <- diff(c(as.Date('2020-01-01'),tests_dates[[i]][tests_peak_loc_1[i]])), silent=T)
        try(deaths_peak_doy[i] <- diff(c(as.Date('2020-01-01'),deaths_dates[[i]][deaths_peak_loc_1[i]])), silent=T)
        try(cases_peak_doy2[i] <- diff(c(as.Date('2020-01-01'),cases_dates[[i]][cases_peak_loc_comb_pt2[i]])), silent=T)
        try(tests_peak_doy[i] <- diff(c(as.Date('2020-01-01'),tests_dates[[i]][tests_peak_loc_1[i]])), silent=T)
        try(deaths_peak_doy2[i] <- diff(c(as.Date('2020-01-01'),deaths_dates[[i]][deaths_peak_loc_comb_pt2[i]])), silent=T)
}

# extract the raw values from the OWID table (country size is created as population/population_density)
# most do not change so the first value can be used, but stringency needs to have the mean taken
population <- rep(NA, 190)
population_density <- rep(NA, 190)
gdp_per_capita <- rep(NA, 190)
median_age <- rep(NA, 190)
life_expectancy <- rep(NA, 190)
human_development_index <- rep(NA, 190)
hospital_beds_per_thousand <- rep(NA, 190)
stringency_mean <- rep(NA, 190)

for (i in 1:190) {
	population[i] <- datatable$population[which(datatable$iso_code == unique_iso[i])[1]]
	population_density[i] <- datatable$population_density[which(datatable$iso_code == unique_iso[i])[1]]
	gdp_per_capita[i] <- datatable$gdp_per_capita[which(datatable$iso_code == unique_iso[i])[1]]
	median_age[i] <- datatable$median_age[which(datatable$iso_code == unique_iso[i])[1]]
	life_expectancy[i] <- datatable$life_expectancy[which(datatable$iso_code == unique_iso[i])[1]]
	human_development_index[i] <- datatable$human_development_inde[which(datatable$iso_code == unique_iso[i])[1]]
	hospital_beds_per_thousand[i] <- datatable$hospital_beds_per_thousand[which(datatable$iso_code == unique_iso[i])[1]]
	stringency_mean[i] <- mean(datatable$stringency_index[which(datatable$iso_code == unique_iso[i])], na.rm=T)
}

# more complex stringency variables are calculated in 'stringency_measures.R' and are loading in below
load('Data/stringency_measures.RData')

# and load in social data (Hofstede and WVS data)
social_data <- read.csv('Data/social_data.csv')

# removed the marked reductions that are equal to -1 (those where the minimum has not reached 0 by the end of the time series)
cases_reduction_all <- c(cases_reduction_1, cases_reduction_2)
cases_reduction_all_rm <- cases_reduction_all
cases_reduction_all_rm[which(cases_reduction_all_rm == -1)] <- NA

tests_reduction_all <- c(tests_reduction_1, tests_reduction_2)
tests_reduction_all_rm <- tests_reduction_all
tests_reduction_all_rm[which(tests_reduction_all_rm == -1)] <- NA

deaths_reduction_all <- c(deaths_reduction_1, deaths_reduction_2)
deaths_reduction_all_rm <- deaths_reduction_all
deaths_reduction_all_rm[which(deaths_reduction_all_rm == -1)] <- NA


# create a data frame that contains all the variables, such that each country has two rows (all first peaks first, then all second peaks)
# resistance is reverse signed such that correlations with other variables are in the correct direction (higher resistance value/peak means less resistance)
# resilience is reverse signed as the model output is negative and also needs to match with other variables for correlation

full_data_combined <- data.frame('ISO'=c(unique_iso, unique_iso),
'Cases_Resistance'=-c(cases_resistance_1,cases_resistance_2), 'Cases_Resilience'=-c(cases_resilience_1,cases_resilience_2), 'Cases_Reduction'=cases_reduction_all_rm, 'Cases_R2_fit'=c(cases_resilience_1_r2_val,cases_resilience_2_r2_val),
'Tests_Resistance'=-c(tests_resistance_1,tests_resistance_2), 'Tests_Resilience'=-c(tests_resilience_1,tests_resilience_2), 'Tests_Reduction'=tests_reduction_all_rm, 'Tests_R2_fit'=c(tests_resilience_1_r2_val,tests_resilience_2_r2_val),
'Deaths_Resistance'=-c(deaths_resistance_1,deaths_resistance_2), 'Deaths_Resilience'=-c(deaths_resilience_1,deaths_resilience_2), 'Deaths_Reduction'=deaths_reduction_all_rm, 'Deaths_R2_fit'=c(deaths_resilience_1_r2_val,deaths_resilience_2_r2_val),
'Cases_Peak_DOY'=c(cases_peak_doy,cases_peak_doy2), 'Tests_Peak_DOY'=c(tests_peak_doy,tests_peak_doy2), 'Deaths_Peak_DOY'=c(deaths_peak_doy,deaths_peak_doy2),
'Stringency_Mean'=c(stringency_mean,stringency_mean), 'Stringency_Decay'=c(stringency_decay_1,stringency_decay_2), 'Stringency_Background'=c(stringency_background,stringency_background), 'Stringency_Adaptive'=c(stringency_adaptive_1,stringency_adaptive_2),
'Population'=c(population,population), 'Pop_Dens'=c(population_density,population_density), 'Country_Size'=c(population/population_density,population/population_density), 'Median_Age'=c(median_age,median_age), 'GDP_per_Capita'=c(gdp_per_capita,gdp_per_capita), 'Hospital_Beds_per_1000'=c(hospital_beds_per_thousand,hospital_beds_per_thousand), 'Life_Expectancy'=c(life_expectancy,life_expectancy), 'Human_Development_Index'=c(human_development_index,human_development_index),
'Trust'=c(social_data$Most_Trusted,social_data$Most_Trusted), 'Power_Distance'=c(social_data$Power_Distance,social_data$Power_Distance), 'Individualism'=c(social_data$Individualism,social_data$Individualism), 'Masculinity'=c(social_data$Masculinity,social_data$Masculinity), 'Uncertainty_Avoidance'=c(social_data$Uncertainty_Avoidance,social_data$Uncertainty_Avoidance), 'Long_Term_Orientation'=c(social_data$Long_Term_Orientation,social_data$Long_Term_Orientation), 'Indulgence'=c(social_data$Indulgence,social_data$Indulgence))

save.image('Data/full_data.RData')

write.csv(full_data_combined, file='Data/full_data.csv')

