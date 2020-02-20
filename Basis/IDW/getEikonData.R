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

# saveRDS(spotOnly, file = "/Users/ensxvd/Desktop/spotOnly.rds")
# saveRDS(allBasis, file = "/Users/ensxvd/Desktop/allBasis.rds")
