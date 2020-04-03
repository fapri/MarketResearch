

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(svDialogs)
library(stringr)

# Load data
DcData = read_excel("Decoupling/DecouplingLitData-X.xlsx", 
                    col_names = FALSE)
template = read.csv("Decoupling/template.csv", header = FALSE)
row.names(template) = template$V1
template$V1 = NA


# Renumber columns

colnames(DcData) = paste0("c", seq(from = 1, to = ncol(DcData), by = 1))

DcData$c1 = gsub("NEW SECTION", NA, DcData$c1)
DcData$c1 = gsub(" - NOT USED FOR NOW", "", DcData$c1)
DcData = DcData[rowSums(is.na(DcData)) != ncol(DcData),]
DcData = DcData[-grep("Notes", DcData$c2), ]

DcData$c1[grep("Nature of supply variable", DcData$c2)] = "Nature of supply variable"
DcData$c2[grep("Nature of supply variable", DcData$c2)] = NA
DcData$c1[grep("Cross effects among crops other than", DcData$c2)] = "Other cross effects"
DcData$c2[grep("Cross effects among crops other than", DcData$c2)] = NA

for (row in 1:(nrow(DcData) - 1)) {
  if (!is.na(DcData$c1[row]) & is.na(DcData$c1[row + 1])) {
    DcData$c1[row + 1] = DcData$c1[row]
  }
}

DcData = DcData[-(which(is.na(DcData$c2[9:nrow(DcData)])) + 8), ]

# x = DcData[,c(1,2)]

DcData$c2 = gsub("\\s*\\([^\\)]+\\)","", as.character(DcData$c2))
DcData$c1 = gsub("[[:punct:]]+","", DcData$c1)
DcData$c2 = gsub("[[:punct:]]+","", DcData$c2)

DcData$c1 = str_to_title(DcData$c1)
DcData$c2 = str_to_title(DcData$c2)
DcData$c1 = gsub(" ", "", DcData$c1)
DcData$c2 = gsub(" ", "", DcData$c2)

DcData$c1[grep("ProgramToWhichResultsRelate", DcData$c1)] = "Program"
DcData$c1[grep("NatureOfSupplyVariable", DcData$c1)] = "NatureOfSupply"
DcData$c2[grep("Peerreviewed", DcData$c2)] = "PeerReviewed"
DcData$c2[grep("EstimatedMarketData", DcData$c2)] = "EstMarketData"
DcData$c2[grep("EstimatedSurveyData", DcData$c2)] = "EstSurveyData"
DcData$c2[grep("EstimatedBalancedPanelData", DcData$c2)] = "EstBalPanelData"
DcData$c2[grep("EstimatedUnbalancedPanelData", DcData$c2)] = "EstUnbalPanelData"
DcData$c2[grep("AllOfUnitedStates", DcData$c2)] = "AllOfUS"
DcData$c2[grep("SomePartOfUnitedStates", DcData$c2)] = "PartOfUS"
DcData$c2[grep("IfSoStateTwodigitPostCode", DcData$c2)] = "StatePostCode"
DcData$c2[grep("OtherCountryOrCountries", DcData$c2)] = "OtherCountries"
DcData$c2[grep("ListCropOrCrops", DcData$c2)] = "CropOrCrops"
DcData$c2[grep("PercentChangeInOutputPerunitPaymentDollarOrEuro", DcData$c2)] = "PctChangeOutputPerunitPmt"
DcData$c2[grep("RatioOfPaymentImpactToMarketImpact", DcData$c2)] = "RatioOfPmtImpToMarketImp"



DcData$c3 = paste(DcData$c1, DcData$c2, sep = "_")



###############################################################


choices = c("All during period",
            "Crop insurance",
            "ARC",
            "PLC",
            "SCO",
            "CCP",
            "ACRE",
            "Market loss assistance",
            "Fixed direct payment (contract payment)",
            "Marketing Loan program",
            "Milk Income Loss Contract (MILC)",
            "Margin Protection Program",
            "Pre-1996 US policy",
            "Other"
)

selectedProgram = dlgList(choices, preselect = NULL, multiple = FALSE,
                          title = "Program Type")$res

selectedProgram = switch(selectedProgram, 
                         "All during period" = "AllDuringPeriod",
                         "Crop insurance" = "CropInsurance",
                         "ARC" = "Arc",
                         "PLC" = "Plc",
                         "SCO" = "Sco",
                         "CCP" = "Ccp",
                         "ACRE"  = "Acre",
                         "Market loss assistance" = "MarketLossAssistance",
                         "Fixed direct payment (contract payment)" = "FixedDirectPayment",
                         "Marketing Loan program" = "MarketingLoanProgram",
                         "Milk Income Loss Contract (MILC)" = "MilkIncomeLossContract",
                         "Margin Protection Program" = "MarginProtectionProgram",
                         "Pre-1996 US policy" = "Pre1996UsPolicy",
                         "Other" = "Other"
)

DcData = data.frame(DcData)
DcData[is.na(DcData)] = 0

programMaster = as.numeric(as.vector(DcData[which(DcData$c1 == "Program" & 
                                                    DcData$c2 == selectedProgram), 4:ncol(DcData)]))

# Convert the data to a list for key/value access
tDf = t(DcData)
colnames(tDf) = row.names(tDf) = NULL

DcList = lapply(seq_len(ncol(tDf[4:nrow(tDf), ])), function(i) tDf[4:nrow(tDf), ][,i])
names(DcList) = tDf[3, ]

DcList = lapply(DcList, function(col) {
  if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
    as.numeric(as.character(col))
  } else {
    col
  }
})

# Initialize data frames
allDf = estDf = notEstDf = usNatDf = otherDf = template















