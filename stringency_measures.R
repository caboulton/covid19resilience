# Chris A. Boulton - University of Exeter
# c.a.boulton@exeter.ac.uk

# script to calculate stringency measures

load('Data/resistance_resilience_reduction.RData')

stringency_background <- rep(NA, length(unique_iso))
stringency_pre_1 <- rep(NA, length(unique_iso))
stringency_decay_1 <- rep(NA, length(unique_iso))
stringency_pre_2 <- rep(NA, length(unique_iso))
stringency_decay_2 <- rep(NA, length(unique_iso))

for (i in 1:length(unique_iso)) {
	print(i)
	countrytable <- datatable[which(datatable$iso_code == unique_iso[i]),]
	stringency_index <- countrytable$stringency_index
	if (!is.na(cases_peak_loc_1[i]) && !is.na(cases_peak_loc_2[i])) {
		stringency_background[i] <- mean(stringency_index[-c(cases_peak_loc_1[i]:cases_min_loc_1[i],cases_peak_loc_2[i]:cases_min_loc_2[i])], na.rm=T)
		stringency_pre_1[i] <- mean(stringency_index[c(1:(cases_peak_loc_1[i]-1))], na.rm=T)
		stringency_decay_1[i] <- mean(stringency_index[c(cases_peak_loc_1[i]:cases_min_loc_1[i])], na.rm=T)
		stringency_pre_2[i] <- mean(stringency_index[c((cases_min_loc_1[i]+1):(cases_peak_loc_2[i]-1))], na.rm=T)
		stringency_decay_2[i] <- mean(stringency_index[c(cases_peak_loc_2[i]:cases_min_loc_2[i])], na.rm=T)
	}
	if (!is.na(cases_peak_loc_1[i]) && is.na(cases_peak_loc_2[i])) {
		stringency_background[i] <- mean(stringency_index[-c(cases_peak_loc_1[i]:cases_min_loc_1[i])], na.rm=T)
		stringency_pre_1[i] <- mean(stringency_index[c(1:(cases_peak_loc_1[i]-1))], na.rm=T)
		stringency_decay_1[i] <- mean(stringency_index[c(cases_peak_loc_1[i]:cases_min_loc_1[i])], na.rm=T)
	}
	if (is.na(cases_peak_loc_1[i]) && !is.na(cases_peak_loc_2[i])) {
		stringency_background[i] <- mean(stringency_index[-c(cases_peak_loc_2[i]:cases_min_loc_2[i])], na.rm=T)
		stringency_pre_2[i] <- mean(stringency_index[c(1:(cases_peak_loc_2[i]-1))], na.rm=T)
		stringency_decay_2[i] <- mean(stringency_index[c(cases_peak_loc_2[i]:cases_min_loc_2[i])], na.rm=T)
	}
}

stringency_adaptive_1 <- stringency_decay_1 - stringency_pre_1
stringency_adaptive_2 <- stringency_decay_2 - stringency_pre_2



save.image('Data/stringency_measures.RData')

