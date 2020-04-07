

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(svDialogs)
library(stringr)
library(testit)
library(tidyr)

# Load data
DcData = read_excel("Decoupling/DecouplingLitData-X.xlsx", 
                    col_names = FALSE)
templateDf = read.csv("Decoupling/template.csv", header = FALSE)
row.names(templateDf) = templateDf$V1
templateDf$V1 = NA


templateList <- list()
for (name in row.names(templateDf)) {
  templateList[[name]] = list("data" = NA, "count" = NA, "average" = NA)
}

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

# selectedProgram = dlgList(choices, preselect = NULL, multiple = FALSE,
#                           title = "Program Type")$res
selectedProgram = 1

selectedProgram = switch(selectedProgram, 
                         "All during period" = "Program_AllDuringPeriod",
                         "Crop insurance" = "Program_CropInsurance",
                         "ARC" = "Program_Arc",
                         "PLC" = "Program_Plc",
                         "SCO" = "Program_Sco",
                         "CCP" = "Program_Ccp",
                         "ACRE"  = "Program_Acre",
                         "Market loss assistance" = "Program_MarketLossAssistance",
                         "Fixed direct payment (contract payment)" = "Program_FixedDirectPayment",
                         "Marketing Loan program" = "Program_MarketingLoanProgram",
                         "Milk Income Loss Contract (MILC)" = "Program_MilkIncomeLossContract",
                         "Margin Protection Program" = "Program_MarginProtectionProgram",
                         "Pre-1996 US policy" = "Program_Pre1996UsPolicy",
                         "Other" = "Program_Other"
)

# Convert the data to a list for key/value access
tDf = t(DcData)
colnames(tDf) = row.names(tDf) = NULL

DcList = lapply(seq_len(ncol(tDf[4:nrow(tDf), ])), function(i) tDf[4:nrow(tDf), ][,i])
names(DcList) = tDf[3, ]

# Convert to numeric where applicable
DcList = lapply(DcList, function(col) {
  if (!has_warning(as.numeric(as.character(col)))) {
    as.numeric(as.character(col))
  } else {
    col
  }
})

# # Convert to list
# templateDf = setNames(as.list(as.data.frame(t(templateDf))), rownames(templateDf))

# Initialize lists
all = est = notEst = usNat = other = templateList

# Extract dummy variable for what type of program is used
programMaster = DcList[[selectedProgram]]

# Match list names
names(DcList)[grep("DecouplingEffect_PriceEffect", names(DcList))] = "DecouplingEffect_priceEffect"
names(DcList)[grep("DecouplingEffect_RiskAversion", names(DcList))] = "DecouplingEffect_riskAversion"
names(DcList)[grep("DecouplingEffect_Wealth", names(DcList))] = "DecouplingEffect_wealth"
names(DcList)[grep("DecouplingEffect_UpdatingAndExpectations", names(DcList))] = "DecouplingEffect_updating"
names(DcList)[grep("DecouplingEffect_ExemptionsOrExclusions", names(DcList))] = "DecouplingEffect_exemptions"
names(DcList)[grep("DecouplingEffect_CreditLiquidity", names(DcList))] = "DecouplingEffect_liquidity"
names(DcList)[grep("DecouplingEffect_Labor", names(DcList))] = "DecouplingEffect_labor"
names(DcList)[grep("DecouplingEffect_EntryOrExit", names(DcList))] = "DecouplingEffect_entryExit"
names(DcList)[grep("DecouplingEffect_Other", names(DcList))] = "DecouplingEffect_other"
names(DcList)[grep("DecouplingEffect_All", names(DcList))] = "DecouplingEffect_all"

# Create a subsection of the calculations
calcLists = function(subList, id) {
  if (id == "all") {
    extraFactors = 1
  } else if (id == "est") {
    extraFactors = DcList[["Method_Estimation"]]
  } else if (id == "notEst") {
    notEstRelavancy = as.numeric(gsub(1, 5, DcList[["Method_Estimation"]]))
    notEstRelavancy = replace_na(notEstRelavancy, 1)
    notEstRelavancy = as.numeric(gsub(1, NA, DcList[["Method_Estimation"]]))
    extraFactors = notEstRelavancy
  } else if (id == "usNat") {
    extraFactors = DcList[["Region_AllOfUS"]]
  } else if (id == "other") {
    otherRelavancy = as.numeric(gsub(1, 5, DcList[["Region_AllOfUS"]]))
    otherRelavancy = replace_na(otherRelavancy, 1)
    otherRelavancy = as.numeric(gsub(5, NA, DcList[["Region_AllOfUS"]]))
    extraFactors = otherRelavancy
  }
  
  for (i in grep("Relavancy", names((subList)))) {
    avenue = which((sub("\\_.*", "", names(subList))) == sub("\\_.*", "", names((subList[i]))))
    
    Relavancy = grep("Relavancy", names(subList[avenue]))
    PCOPUP = grep("PCOPUP", names(subList[avenue]))
    PI2MI = grep("PI2MI", names(subList[avenue]))
    
    subList[[avenue[Relavancy]]][["data"]] = programMaster * 
      extraFactors * 
      DcList[[which((sub(".*_", "", names(DcList))) == sub("\\_.*", "", names((subList[i]))))]]
    subList[[avenue[PCOPUP]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["ImpactOfPayments_PctChangeOutputPerunitPmt"]]
    subList[[avenue[PI2MI]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["Ratio_RatioOfPmtImpToMarketImp"]]
  }
  return(subList)
}


all = calcLists(all, "all")
est = calcLists(est, "est")
notEst = calcLists(notEst, "notEst")
usNat = calcLists(usNat, "usNat")
other = calcLists(other, "other")


calcStats = function(subList) {
  for (i in names(subList)) {
    subList[[i]]$count = length(which(!is.na(subList[[i]][["data"]])))
    if (subList[[i]]$count > 0) {
      subList[[i]]$average = mean(subList[[i]][["data"]], na.rm = TRUE)
    } else {
      subList[[i]]$average = NA
    }
  }
  return(subList)
}


all = calcStats(all)
est = calcStats(est)
notEst = calcStats(notEst)
usNat = calcStats(usNat)
other = calcStats(other)


