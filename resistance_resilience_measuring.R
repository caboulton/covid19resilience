# Chris A. Boulton - University of Exeter
# c.a.boulton@exeter.ac.uk

# this script calculates the resistance, resilience and reduction for the cases/capita (cases), cases/tests (tests) and deaths/capita (deaths) time series

datatable <- read.csv('Data/OWID_covid_daily_011220.csv')
unique_iso <- unique(datatable$iso_code)

unique_iso <- unique_iso[-c(73,192)] #remove Hong Kong which has no data, and the missing ISO code at the end

# reads in the script that determines where to look for peaks and which to ignore
source('peak_min_locations.R')

# determine how many peaks to look for (determined by authors based on manually reviewing the time series)
# works alongside the 'ignore' indices in the peak_min_locations.R script as not all time series have the same number of peaks
num_peaks <- rep(0, 190)
num_peaks[which(unique_iso %in% c(
'AFG','AGO','ALB','ARE','ARG','ARM','AUT','AZE','BEN','BGD','BGR','BLR','BOL','BRA','BRB','BRN','CAF','CAN','CHE','CHL','CHN','CIV','CMR','COD','COL',
'CPV','CRI','CYP','CZE','DEU','DJI','DNK','ECU','EGY','ESP','EST','ETH','FIN','FJI','FRA','GAB','GBR','GHA','GMB','GNB','GRC','GTM','GUY','HND','HRV','HTI','HUN','IDN','IND',
'IRQ','ITA','KAZ','KEN','LBN','LIE','LKA','LSO','LTU','LVA','MAR','MCO','MDA','MDG','MEX','MLI','MLT','MMR','MOZ','MRT',
'MUS','MWI','MYS','NAM','NER','NGA','NIC','NLD','NOR','NPL','NZL','PAK','PHL','PNG','PRT','PRY','QAT','RUS','RWA','SAU','SDN','SEN','SGP','SLE','SLV','SMR','SOM',
'SSD','SUR','SVK','SVN','SWE','SWZ','SYR','THA','TJK','TUN','TUR','TWN','URY','VEN','YEM','ZAF','ZWE'
))] <- 1
num_peaks[which(unique_iso %in% c(
'AND','AUS','BEL','BFA','BHR','BHS','BIH','BTN',
'CUB','DOM','DZA','ERI',
'IRL','IRN','ISL','ISR','JAM','JPN','KOR','KWT','LBR','LUX','MDV','MKD',
'OMN','PAN','PER','PSE','ROU',
'SRB','TCD','TTO','UGA','USA','UZB','VNM','OWID_KOS','ZMB','OWID_WRL'
))] <- 2

# for each peak the following are calculated and stored 
cases_resistance_1 <- rep(NA, length(unique_iso))	#the peak value
cases_resilience_1 <- rep(NA, length(unique_iso))	#the decay rate from peak to min
cases_resilience_1_interp <- rep(NA, length(unique_iso))	#the intercept from the decay rate model (used for plotting)
cases_resilience_1_r2_val <- rep(NA, length(unique_iso))	#the r-squared value of the decay rate model fit
cases_reduction_1 <- rep(NA, length(unique_iso))	#the proportion of reduction from peak to min
cases_0flag_1 <- rep(NA, length(unique_iso))	#flag to determine if min reaches 0 (not used in further analysis)
cases_dates <- list()	#dates from time series (for plotting)
cases_timeseries <- list() 	#time series (for plotting)
cases_smoothed <- list()	#smoothed time series (for plotting)
cases_peak_loc_1 <- rep(NA, length(unique_iso))	#location of peak
cases_min_loc_1 <- rep(NA, length(unique_iso))	#location of following minimum

tests_resistance_1 <- rep(NA, length(unique_iso))
tests_resilience_1 <- rep(NA, length(unique_iso))
tests_resilience_1_interp <- rep(NA, length(unique_iso))
tests_resilience_1_r2_val <- rep(NA, length(unique_iso))
tests_reduction_1 <- rep(NA, length(unique_iso))
tests_0flag_1 <- rep(NA, length(unique_iso))
tests_dates <- list()
tests_timeseries <- list()
tests_smoothed <- list()
tests_peak_loc_1 <- rep(NA, length(unique_iso))
tests_min_loc_1 <- rep(NA, length(unique_iso))

deaths_resistance_1 <- rep(NA, length(unique_iso))
deaths_resilience_1 <- rep(NA, length(unique_iso))
deaths_resilience_1_interp <- rep(NA, length(unique_iso))
deaths_resilience_1_r2_val <- rep(NA, length(unique_iso))
deaths_reduction_1 <- rep(NA, length(unique_iso))
deaths_0flag_1 <- rep(NA, length(unique_iso))
deaths_dates <- list()
deaths_timeseries <- list()
deaths_smoothed <- list()
deaths_peak_loc_1 <- rep(NA, length(unique_iso))
deaths_min_loc_1 <- rep(NA, length(unique_iso))

cases_resistance_2 <- rep(NA, length(unique_iso))
cases_resilience_2 <- rep(NA, length(unique_iso))
cases_resilience_2_interp <- rep(NA, length(unique_iso))
cases_resilience_2_r2_val <- rep(NA, length(unique_iso))
cases_reduction_2 <- rep(NA, length(unique_iso))
cases_0flag_2 <- rep(NA, length(unique_iso))
cases_peak_loc_2 <- rep(NA, length(unique_iso))
cases_min_loc_2 <- rep(NA, length(unique_iso))

tests_resistance_2 <- rep(NA, length(unique_iso))
tests_resilience_2 <- rep(NA, length(unique_iso))
tests_resilience_2_interp <- rep(NA, length(unique_iso))
tests_resilience_2_r2_val <- rep(NA, length(unique_iso))
tests_reduction_2 <- rep(NA, length(unique_iso))
tests_0flag_2 <- rep(NA, length(unique_iso))
tests_peak_loc_2 <- rep(NA, length(unique_iso))
tests_min_loc_2 <- rep(NA, length(unique_iso))

deaths_resistance_2 <- rep(NA, length(unique_iso))
deaths_resilience_2 <- rep(NA, length(unique_iso))
deaths_resilience_2_interp <- rep(NA, length(unique_iso))
deaths_resilience_2_r2_val <- rep(NA, length(unique_iso))
deaths_reduction_2 <- rep(NA, length(unique_iso))
deaths_0flag_2 <- rep(NA, length(unique_iso))
deaths_peak_loc_2 <- rep(NA, length(unique_iso))
deaths_min_loc_2 <- rep(NA, length(unique_iso))

# the if statements in the for loop below follow the same format but only the first is fully commented unless there are extra steps later on

for (i in 1:length(unique_iso)){
	print(c(i,unique_iso[i]))
	if (num_peaks[i] > 0) {
		# create a table of data relevent to the country in question
		country_table <- datatable[which(datatable$iso_code == unique_iso[i]),]
		# store the dates
		dates <- as.Date(country_table$date)
		# get new cases per capita ($population does not change throughout the table)
		new_cases_per_pop <- country_table$new_cases/country_table$population
		# use a Kernal smoothing function to smooth the time series
		smoothed_new_cases_per_pop <- ksmooth(c(1:length(new_cases_per_pop)), new_cases_per_pop, bandwidth=25, x.points=c(1:length(new_cases_per_pop)))$y
		# in some countries, negative cases can occur due to removal of some new cases (very rare)
		new_cases_per_pop[which(new_cases_per_pop < 0)] <- 0
		# smoothing may introduce negative values which are set to 0
		smoothed_new_cases_per_pop[which(smoothed_new_cases_per_pop < 0)] <- NA
		# store the values in the lists created above
		cases_dates[[i]] <- dates
		cases_timeseries[[i]] <- new_cases_per_pop
		cases_smoothed[[i]] <- smoothed_new_cases_per_pop
		
		if (cases_ignore_1[i] == 0) {
			# detect the max and min values in between the locations from the other script and store them
			peak_loc <- which.max(smoothed_new_cases_per_pop[peak_location_1[i,1]:peak_location_1[i,2]]) + peak_location_1[i,1] - 1
			min_loc <- which.min(smoothed_new_cases_per_pop[min_location_1[i,1]:min_location_1[i,2]]) + min_location_1[i,1] - 1
			
			cases_peak_loc_1[i] <- peak_loc
			cases_min_loc_1[i] <- min_loc
			
			# create the time series to calculate the decay rate on and check flag (not used in further analysis)
			decay_ts <- smoothed_new_cases_per_pop[peak_loc:min_loc]
			if (length(which(decay_ts == 0) > 0)) {
				cases_0flag_1[i] <- 1
			} else {
				cases_0flag_1[i] <- 0
			}
			# remove 0 values which will change the decay model below
			decay_ts[which(decay_ts == 0)] <- NA
			
			# fit the model on the log of the above time series
			exp_mod <- lm(log(decay_ts) ~ c(1:length(decay_ts)))
			
			# record values
			cases_resistance_1[i] <- smoothed_new_cases_per_pop[peak_loc]
			cases_resilience_1[i] <- exp_mod$coefficients[2]
			cases_reduction_1[i] <- 1-(smoothed_new_cases_per_pop[min_loc]/smoothed_new_cases_per_pop[peak_loc])
			
			cases_resilience_1_interp[i] <- exp_mod$coefficients[1]
			cases_resilience_1_r2_val[i] <- summary(exp_mod)$r.squared
			
			# marker for if minimum is not reached by end of time series (and as such is unfair to determine reduction with this)
			if (min_loc == length(smoothed_new_cases_per_pop)) {
				cases_reduction_1[i] <- -1
			}
		}
		
		if (num_peaks[i] == 2 && cases_ignore_2[i] == 0) {
			peak_loc2 <- which.max(smoothed_new_cases_per_pop[peak_location_2[i,1]:peak_location_2[i,2]]) + peak_location_2[i,1] - 1
			min_loc2 <- which.min(smoothed_new_cases_per_pop[min_location_2[i,1]:min_location_2[i,2]]) + min_location_2[i,1] - 1
			
			cases_peak_loc_2[i] <- peak_loc2
			cases_min_loc_2[i] <- min_loc2

			decay_ts2 <- smoothed_new_cases_per_pop[peak_loc2:min_loc2]
			if (length(which(decay_ts2 == 0) > 0)) {
				cases_0flag_2[i] <- 1
			} else {
				cases_0flag_2[i] <- 0
			}
			decay_ts2[which(decay_ts2 == 0)] <- NA
		
			exp_mod2 <- lm(log(decay_ts2) ~ c(1:length(decay_ts2)))
			
			cases_resistance_2[i] <- smoothed_new_cases_per_pop[peak_loc2]
			cases_resilience_2[i] <- exp_mod2$coefficients[2]
			cases_reduction_2[i] <- 1-(smoothed_new_cases_per_pop[min_loc2]/smoothed_new_cases_per_pop[peak_loc2])
			
			cases_resilience_2_interp[i] <- exp_mod2$coefficients[1]
			cases_resilience_2_r2_val[i] <- summary(exp_mod2)$r.squared
			
			if (min_loc2 == length(smoothed_new_cases_per_pop)) {
				cases_reduction_2[i] <- -1
			}
		}
		
		# the below does the same as above but on the time series created by dividing the smoothed new cases time series by new tests
		new_cases <- country_table$new_cases
		new_tests <- country_table$new_tests
		try(new_tests <- approx(c(1:length(new_tests)), new_tests, c(1:length(new_tests)))$y, silent=T)
		smoothed_new_cases <- ksmooth(c(1:length(new_cases)), new_cases, bandwidth=25, x.points=c(1:length(new_cases)))$y
		smoothed_new_tests <- ksmooth(c(1:length(new_tests)), new_tests, bandwidth=25, x.points=c(1:length(new_tests)))$y
		smoothed_new_cases_per_tests <- smoothed_new_cases/smoothed_new_tests
		
		tests_dates[[i]] <- dates
		tests_smoothed[[i]] <- smoothed_new_cases_per_tests
		
		if (length(which(!is.na(smoothed_new_cases_per_tests[peak_location_1[i,1]:peak_location_1[i,2]]))) > 0 && tests_ignore_1[i] == 0) {
			peak_loc <- which.max(smoothed_new_cases_per_tests[peak_location_1[i,1]:peak_location_1[i,2]]) + peak_location_1[i,1] - 1
			min_loc <- which.min(smoothed_new_cases_per_tests[min_location_1[i,1]:min_location_1[i,2]]) + min_location_1[i,1] - 1
			
			tests_peak_loc_1[i] <- peak_loc
			tests_min_loc_1[i] <- min_loc

			decay_ts <- smoothed_new_cases_per_tests[peak_loc:min_loc]
			if (length(which(decay_ts == 0) > 0)) {
				tests_0flag_1[i] <- 1
			} else {
				tests_0flag_1[i] <- 0
			}
			decay_ts[which(decay_ts == 0)] <- NA
		
			exp_mod <- lm(log(decay_ts) ~ c(1:length(decay_ts)))
			
			tests_resistance_1[i] <- smoothed_new_cases_per_tests[peak_loc]
			tests_resilience_1[i] <- exp_mod$coefficients[2]
			tests_reduction_1[i] <- 1-(smoothed_new_cases_per_tests[min_loc]/smoothed_new_cases_per_tests[peak_loc])
			
			tests_resilience_1_interp[i] <- exp_mod$coefficients[1]
			tests_resilience_1_r2_val[i] <- summary(exp_mod)$r.squared
			
			if (min_loc == length(smoothed_new_cases_per_tests)) {
				tests_reduction_1[i] <- -1
			}
		}
		
		if (num_peaks[i] == 2 && length(which(!is.na(smoothed_new_cases_per_tests[peak_location_2[i,1]:peak_location_2[i,2]]))) > 0 && tests_ignore_2[i] == 0) {
			peak_loc2 <- which.max(smoothed_new_cases_per_tests[peak_location_2[i,1]:peak_location_2[i,2]]) + peak_location_2[i,1] - 1
			min_loc2 <- which.min(smoothed_new_cases_per_tests[min_location_2[i,1]:min_location_2[i,2]]) + min_location_2[i,1] - 1
				
			tests_peak_loc_2[i] <- peak_loc2
			tests_min_loc_2[i] <- min_loc2

			decay_ts2 <- smoothed_new_cases_per_tests[peak_loc2:min_loc2]
			if (length(which(decay_ts2 == 0) > 0)) {
				tests_0flag_2[i] <- 1
			} else {
				tests_0flag_2[i] <- 0
			}
			decay_ts2[which(decay_ts2 == 0)] <- NA
		
			exp_mod2 <- lm(log(decay_ts2) ~ c(1:length(decay_ts2)))
			
			tests_resistance_2[i] <- smoothed_new_cases_per_tests[peak_loc2]
			tests_resilience_2[i] <- exp_mod2$coefficients[2]
			tests_reduction_2[i] <- 1-(smoothed_new_cases_per_tests[min_loc2]/smoothed_new_cases_per_tests[peak_loc2])
			
			tests_resilience_2_interp[i] <- exp_mod2$coefficients[1]
			tests_resilience_2_r2_val[i] <- summary(exp_mod2)$r.squared
			
			if (min_loc2 == length(smoothed_new_cases_per_tests)) {
				tests_reduction_2[i] <- -1
			}
		}
		
		# calculating for deaths/capita
		new_deaths_per_pop <- country_table$new_deaths/country_table$population
		new_deaths_per_pop[which(new_deaths_per_pop < 0)] <- 0
		smoothed_new_deaths_per_pop <- ksmooth(c(1:length(new_deaths_per_pop)), new_deaths_per_pop, bandwidth=25, x.points=c(1:length(new_deaths_per_pop)))$y
		deaths_dates[[i]] <- dates
		deaths_timeseries[[i]] <- new_deaths_per_pop
		deaths_smoothed[[i]] <- smoothed_new_deaths_per_pop
		
		if (length(which(!is.na(smoothed_new_deaths_per_pop[peak_location_1[i,1]:peak_location_1[i,2]]))) > 0 && deaths_ignore_1[i] == 0) {
			peak_loc <- which.max(smoothed_new_deaths_per_pop[peak_location_1[i,1]:peak_location_1[i,2]]) + peak_location_1[i,1] - 1
			min_loc <- which.min(smoothed_new_deaths_per_pop[min_location_1[i,1]:min_location_1[i,2]]) + min_location_1[i,1] - 1
			
			deaths_peak_loc_1[i] <- peak_loc
			deaths_min_loc_1[i] <- min_loc

			decay_ts <- smoothed_new_deaths_per_pop[peak_loc:min_loc]
			if (length(which(decay_ts == 0) > 0)) {
				deaths_0flag_1[i] <- 1
			} else {
				deaths_0flag_1[i] <- 0
			}
			decay_ts[which(decay_ts == 0)] <- NA
		
			exp_mod <- lm(log(decay_ts) ~ c(1:length(decay_ts)))
			
			deaths_resistance_1[i] <- smoothed_new_deaths_per_pop[peak_loc]
			deaths_resilience_1[i] <- exp_mod$coefficients[2]
			deaths_reduction_1[i] <- 1-(smoothed_new_deaths_per_pop[min_loc]/smoothed_new_deaths_per_pop[peak_loc])
			
			deaths_resilience_1_interp[i] <- exp_mod$coefficients[1]
			deaths_resilience_1_r2_val[i] <- summary(exp_mod)$r.squared
			if (min_loc == length(smoothed_new_deaths_per_pop)) {
				deaths_reduction_1[i] <- -1
			}
		}
		
		if (num_peaks[i] == 2 && length(which(!is.na(smoothed_new_deaths_per_pop[peak_location_2[i,1]:peak_location_2[i,2]]))) > 0 && deaths_ignore_2[i] == 0) {
			peak_loc2 <- which.max(smoothed_new_deaths_per_pop[peak_location_2[i,1]:peak_location_2[i,2]]) + peak_location_2[i,1] - 1
			min_loc2 <- which.min(smoothed_new_deaths_per_pop[min_location_2[i,1]:min_location_2[i,2]]) + min_location_2[i,1] - 1
			
			deaths_peak_loc_2[i] <- peak_loc2
			deaths_min_loc_2[i] <- min_loc2

			decay_ts2 <- smoothed_new_deaths_per_pop[peak_loc2:min_loc2]
			if (length(which(decay_ts2 == 0) > 0)) {
				deaths_0flag_2[i] <- 1
			} else {
				deaths_0flag_2[i] <- 0
			}
			decay_ts2[which(decay_ts2 == 0)] <- NA
	
			exp_mod2 <- lm(log(decay_ts2) ~ c(1:length(decay_ts2)))
				
			deaths_resistance_2[i] <- smoothed_new_deaths_per_pop[peak_loc2]
			deaths_resilience_2[i] <- exp_mod2$coefficients[2]
			deaths_reduction_2[i] <- 1-(smoothed_new_deaths_per_pop[min_loc2]/smoothed_new_deaths_per_pop[peak_loc2])
			
			deaths_resilience_2_interp[i] <- exp_mod2$coefficients[1]
			deaths_resilience_2_r2_val[i] <- summary(exp_mod2)$r.squared
			if (min_loc2 == length(smoothed_new_deaths_per_pop)) {
				deaths_reduction_2[i] <- -1
			}
		}
	}
}

# reductions are not included in further analysis if the minimum has not been found by the end of the time series
# denoted by the marker
cases_reduction_1_rm <- cases_reduction_1
cases_reduction_1_rm[which(cases_reduction_1_rm == -1)] <- NA
tests_reduction_1_rm <- tests_reduction_1
tests_reduction_1_rm[which(tests_reduction_1_rm == -1)] <- NA
deaths_reduction_1_rm <- deaths_reduction_1
deaths_reduction_1_rm[which(deaths_reduction_1_rm == -1)] <- NA

cases_reduction_2_rm <- cases_reduction_2
cases_reduction_2_rm[which(cases_reduction_2_rm == -1)] <- NA
tests_reduction_2_rm <- tests_reduction_2
tests_reduction_2_rm[which(tests_reduction_2_rm == -1)] <- NA
deaths_reduction_2_rm <- deaths_reduction_2
deaths_reduction_2_rm[which(deaths_reduction_2_rm == -1)] <- NA

full_data <- data.frame('ISO'=unique_iso, 'Cases_Resistance_1'=-cases_resistance_1, 'Cases_Resilience_1'=-cases_resilience_1, 'Cases_Reduction_1'=cases_reduction_1_rm, 'Cases_R2_fit_1'=cases_resilience_1_r2_val, 'Tests_Resistance_1'=-tests_resistance_1, 'Tests_Resilience_1'=-tests_resilience_1, 'Tests_Reduction_1'=tests_reduction_1_rm, 'Tests_R2_fit_1'=tests_resilience_1_r2_val, 'Deaths_Resistance_1'=-deaths_resistance_1, 'Deaths_Resilience_1'=-deaths_resilience_1, 'Deaths_Reduction_1'=deaths_reduction_1_rm, 'Deaths_R2_fit_1'=deaths_resilience_1_r2_val,'Cases_Resistance_2'=-cases_resistance_2, 'Cases_Resilience_2'=-cases_resilience_2, 'Cases_Reduction_2'=cases_reduction_2_rm, 'Cases_R2_fit_2'=cases_resilience_2_r2_val, 'Tests_Resistance_2'=-tests_resistance_2, 'Tests_Resilience_2'=-tests_resilience_2, 'Tests_Reduction_2'=tests_reduction_2_rm, 'Tests_R2_fit_2'=tests_resilience_2_r2_val, 'Deaths_Resistance_2'=-tests_resistance_2, 'Deaths_Resilience_2'=-deaths_resilience_2, 'Deaths_Reduction_2'=deaths_reduction_2_rm, 'Deaths_R2_fit_2'=deaths_resilience_2_r2_val)

write.csv(full_data, file='Data/resistance_resilience_reduction.csv', row.names=F)

save.image('Data/resistance_resilience_reduction.RData')





















