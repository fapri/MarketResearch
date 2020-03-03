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
                  fields = list("CF_NAME", "CF_BID", "CF_DATE", "ROW80_1", "ROW80_2", "GN_TXT16_2", "GN_TXT16_4", "ROW80_3"))
  allBasis = rbind(allBasis, cbind(temp, "Location" = rep(levels(droplevels(locations$CF_NAME[i])), length.out = nrow(temp))))
  temp = data.frame()
  print(i)
}

spotOnly = allBasis[which(grepl("SPOT", allBasis$CF_NAME) == TRUE), ]

# saveRDS(spotOnly, file = "/Users/ensxvd/Desktop/spotOnly.rds")
# saveRDS(allBasis, file = "/Users/ensxvd/Desktop/allBasis.rds")





########################################################################
# Compare Corn and soybean addresses
########################################################################

# cornSpotOnly = read_csv("Basis/refinitivData/cornSpotOnly02282020.csv")
# # Remove extra columns
# cornSpotOnly = subset(cornSpotOnly, select = -c(GEN_TEXT16, Location))
# # Change column names
# colnames(cornSpotOnly) = c("instrument", "contractName", "basis", "date", "terminalName", "address", "county", "cropType", "phoneNumber")
# # Get zip codes
# cornSpotOnly$zipCode = str_extract(cornSpotOnly$phoneNumber, "\\d{5}")
# # Paste zip codes to address to send to geocoder
# cornSpotOnly$geoFormatAddress = paste(cornSpotOnly$address, cornSpotOnly$zipCode, sep = ", ")
# 
# soybeanSpotOnly = read_csv("Basis/refinitivData/soybeanSpotOnly02282020.csv")
# # Remove extra columns
# soybeanSpotOnly = subset(soybeanSpotOnly, select = -c(GEN_TEXT16, Location))
# # Change column names
# colnames(soybeanSpotOnly) = c("instrument", "contractName", "basis", "date", "terminalName", "address", "county", "cropType", "phoneNumber")
# # Get zip codes
# soybeanSpotOnly$zipCode = str_extract(soybeanSpotOnly$phoneNumber, "\\d{5}")
# # Paste zip codes to address to send to geocoder
# soybeanSpotOnly$geoFormatAddress = paste(soybeanSpotOnly$address, soybeanSpotOnly$zipCode, sep = ", ")
# 
# 
# # Get important information
# cornSubset = cornSpotOnly[, c("instrument", "geoFormatAddress")]
# 
# # # Confirm that soybean terminal names are the same as corn
# # soybeanSubset = soybeanSpotOnly
# # soybeanSubset = gsub('SOYB', 'CORN', soybeanSubset$instrument)
# # checkAddress = merge(x = soybeanSubset, y = cornSubset, by = c("geoFormatAddress", "instrument"), all.x = TRUE)
# 
# # Merge the sets by address (before being cleaned)
# # All addresses matched up to corn, meaning that the
# # errors we saw in corn are consistent with soybeans
# checkAddress = merge(x = soybeanSpotOnly, y = cornSubset, by = c("geoFormatAddress"), all.y = TRUE)
# 
# # Get the instruments which do not carry from corn and
# # ensure they are not in cleaning functions
# notInSoybeans = which(is.na(checkAddress$instrument.x))
# checkAddress$instrument.y[notInSoybeans]