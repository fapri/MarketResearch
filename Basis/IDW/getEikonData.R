# Get basis data from Refinitiv

library(usethis)
library(devtools)
library(eikonapir)

set_app_id("")

locations = get_data(instruments = list("0#GGC-CORN-MO"),
                     fields = list("CF_NAME"))

allBasis = data.frame(matrix(ncol = 5, nrow = 0))
temp = data.frame()
for (i in 1:nrow(locations)) {
  temp = get_data(instruments = list(locations$Instrument[i]),
                  fields = list("CF_NAME", "CF_BID", "CF_DATE", "GEN_TEXT16", "ROW80_1", "ROW80_2", "GN_TXT16_2", "GN_TXT16_4", "ROW80_3"))
  allBasis = rbind(allBasis, cbind(temp, "Location" = rep(levels(droplevels(locations$CF_NAME[i])), length.out = nrow(temp))))
  temp = data.frame()
  print(i)
}

spotOnly = allBasis[which(grepl("SPOT", allBasis$CF_NAME) == TRUE), ]

# write.csv(spotOnly, 
#           file = paste("/Users/ensxvd/Desktop/refinitivData/", "cornSpotOnly", format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""), 
#           row.names = FALSE)
# write.csv(allBasis, 
#           file = paste("/Users/ensxvd/Desktop/refinitivData/", "cornAllBasis", format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""), 
#           row.names = FALSE)





spotOnly$Instrument = as.character(spotOnly$Instrument)

# October
oct = data.frame()

for (i in 1:nrow(spotOnly)) {
  oct = rbind(oct, cbind("Instrument" = spotOnly$Instrument[i], "octAvg" = mean(as.numeric(get_timeseries(spotOnly$Instrument[i],
                                                                                                          list("*"),
                                                                                                          start_date = "2019-10-01T15:04:05",
                                                                                                          end_date = "2019-10-31T15:04:05")$VALUE), na.rm = TRUE)))
  print(i)
}

oct$Instrument = as.character(oct$Instrument)
oct$octAvg = as.numeric(levels(oct$octAvg))[oct$octAvg]

# January
jan = data.frame()

for (i in 1:nrow(spotOnly)) {
  jan = rbind(jan, cbind("Instrument" = spotOnly$Instrument[i], "janAvg" = mean(as.numeric(get_timeseries(spotOnly$Instrument[i],
                                                                                                          list("*"),
                                                                                                          start_date = "2020-01-01T15:04:05",
                                                                                                          end_date = "2020-01-31T15:04:05")$VALUE), na.rm = TRUE)))
  print(i)
}

jan$Instrument = as.character(jan$Instrument)
jan$janAvg = as.numeric(levels(jan$janAvg))[jan$janAvg]


# Merge Oct and Jan to existing data
spotOnly = merge(x = spotOnly, y = oct, by = "Instrument", all.x = TRUE)
spotOnly = merge(x = spotOnly, y = jan, by = "Instrument", all.x = TRUE)

# write.csv(spotOnly, file = "/Users/ensxvd/Desktop/cornSpotJanOct.csv", row.names = FALSE)



#####################
# SOYBEANS
#####################




# Get basis data from Refinitiv

library(usethis)
library(devtools)
library(eikonapir)

set_app_id("")

locations = get_data(instruments = list("0#GGC-SOYB-MO"),
                     fields = list("CF_NAME"))

allBasis = data.frame(matrix(ncol = 5, nrow = 0))
temp = data.frame()
for (i in 1:nrow(locations)) {
  temp = get_data(instruments = list(locations$Instrument[i]),
                  fields = list("CF_NAME", "CF_BID", "CF_DATE", "GEN_TEXT16", "ROW80_1", "ROW80_2", "GN_TXT16_2", "GN_TXT16_4", "ROW80_3"))
  allBasis = rbind(allBasis, cbind(temp, "Location" = rep(levels(droplevels(locations$CF_NAME[i])), length.out = nrow(temp))))
  temp = data.frame()
  print(i)
}

spotOnly = allBasis[which(grepl("SPOT", allBasis$CF_NAME) == TRUE), ]

# write.csv(spotOnly, 
#           file = paste("/Users/ensxvd/Desktop/refinitivData/", "soybeanSpotOnly", format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""), 
#           row.names = FALSE)
# write.csv(allBasis, 
#           file = paste("/Users/ensxvd/Desktop/refinitivData/", "soybeanAllBasis", format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""), 
#           row.names = FALSE)










spotOnly$Instrument = as.character(spotOnly$Instrument)

# October
oct = data.frame()

for (i in 1:nrow(spotOnly)) {
  oct = rbind(oct, cbind("Instrument" = spotOnly$Instrument[i], "octAvg" = mean(as.numeric(get_timeseries(spotOnly$Instrument[i],
                                                                                                          list("*"),
                                                                                                          start_date = "2019-10-01T15:04:05",
                                                                                                          end_date = "2019-10-31T15:04:05")$VALUE), na.rm = TRUE)))
  print(i)
}

oct$Instrument = as.character(oct$Instrument)
oct$octAvg = as.numeric(levels(oct$octAvg))[oct$octAvg]

# January
jan = data.frame()

for (i in 1:nrow(spotOnly)) {
  jan = rbind(jan, cbind("Instrument" = spotOnly$Instrument[i], "janAvg" = mean(as.numeric(get_timeseries(spotOnly$Instrument[i],
                                                                                                          list("*"),
                                                                                                          start_date = "2020-01-01T15:04:05",
                                                                                                          end_date = "2020-01-31T15:04:05")$VALUE), na.rm = TRUE)))
  print(i)
}

jan$Instrument = as.character(jan$Instrument)
jan$janAvg = as.numeric(levels(jan$janAvg))[jan$janAvg]


# Merge Oct and Jan to existing data
spotOnly = merge(x = spotOnly, y = oct, by = "Instrument", all.x = TRUE)
spotOnly = merge(x = spotOnly, y = jan, by = "Instrument", all.x = TRUE)

# write.csv(spotOnly, file = "/Users/ensxvd/Desktop/soybeanSpotJanOct.csv", row.names = FALSE)



