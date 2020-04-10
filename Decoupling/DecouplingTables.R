# Emily Scully
# Created March 31, 2020
# Project: Decoupling Data Organization and Tables

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(svDialogs)
library(stringr)
library(testit)
library(tidyr)
library(flextable)
library(officer)

# Load data
DcData = read_excel("Decoupling/DecouplingLitData-X.xlsx", 
                    col_names = FALSE)

# Renumber columns
colnames(DcData) = paste0("c", seq(from = 1, to = ncol(DcData), by = 1))

# Clean data
DcData$c1 = gsub("NEW SECTION", NA, DcData$c1)
DcData$c1 = gsub(" - NOT USED FOR NOW", "", DcData$c1)
DcData = DcData[rowSums(is.na(DcData)) != ncol(DcData),]
DcData = DcData[-grep("Notes", DcData$c2), ]
DcData$c1[grep("Nature of supply variable", DcData$c2)] = "Nature of supply variable"
DcData$c2[grep("Nature of supply variable", DcData$c2)] = NA
DcData$c1[grep("Cross effects among crops other than", DcData$c2)] = "Other cross effects"
DcData$c2[grep("Cross effects among crops other than", DcData$c2)] = NA

# fills in main categories so they can be matched to subcategories and to remove NAs
for (row in 1:(nrow(DcData) - 1)) {
  if (!is.na(DcData$c1[row]) & is.na(DcData$c1[row + 1])) {
    DcData$c1[row + 1] = DcData$c1[row]
  }
}

# Remove rows with only NAs
DcData = DcData[-(which(is.na(DcData$c2[9:nrow(DcData)])) + 8), ]

# removes () and any info in them
DcData$c2 = gsub("\\s*\\([^\\)]+\\)","", as.character(DcData$c2))

# removes punctuation
DcData$c1 = gsub("[[:punct:]]+","", DcData$c1)
DcData$c2 = gsub("[[:punct:]]+","", DcData$c2)

# formatting/standardizing
DcData$c1 = str_to_title(DcData$c1)
DcData$c2 = str_to_title(DcData$c2)
DcData$c1 = gsub(" ", "", DcData$c1)
DcData$c2 = gsub(" ", "", DcData$c2)

# Shortening names that are excessively long
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

# merge the two categories of labels together, separated by _
DcData$c3 = paste(DcData$c1, DcData$c2, sep = "_")

# Convert the data to a list for key/value access
tDf = t(DcData)
colnames(tDf) = row.names(tDf) = NULL

# Converts each data frame row into a list
# This is helpful for being able to call the variables by 
# their key, rather than col/row notation
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

# Create temp vector of the avenues
tempAvenues = c("PriceEffect_",
                "RiskAversion_",
                "Wealth_",
                "UpdatingAndExpectations_",
                "ExemptionsOrExclusions_",
                "CreditLiquidity_",
                "Labor_",
                "EntryOrExit_",
                "Other_",
                "All_")

# Create vector of values needed for table
factors = c("Relavancy",
            "PCOPUP",
            "PI2MI")

# match each avenue to all factors
avenues = vector()
for (i in seq(length(tempAvenues))) {
  avenues = c(avenues, paste0(tempAvenues[i], factors))
}

# Convert to a list of lists for key/value access
templateList = list()
for (i in avenues) {
  templateList[[i]] = list("data" = NA, "count" = NA, "average" = NA)
}

# Initialize lists
all = est = notEst = usNat = other = templateList

###############################################################

# Set program choices for user
programChoices = c("All during period",
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

# Set supply choices for user
supplyChoices = c("Area of a Crop or Crops",
                  "Yield",
                  "Production of a Crop or Crops",
                  "All Supply Side Variables")

# # Dialogue boxes for user input
# selectedProgramText = dlgList(programChoices, preselect = NULL, multiple = FALSE,
#                           title = "Program Type")$res
selectedProgramText = "Fixed direct payment (contract payment)"

# # Dialogue boxes for user input
# selectedSupplyText = dlgList(supplyChoices, preselect = NULL, multiple = FALSE,
#                          title = "Supply Type")$res
selectedSupplyText = "Area of a Crop or Crops"

# Convert user selection to list key name for access by name
selectedProgram = switch(selectedProgramText, 
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

# Convert user selection to list key name for access by name
selectedSupply = switch(selectedSupplyText, 
                        "Area of a Crop or Crops" = c("NatureOfSupply_AreaOfACrop", 
                                                      "NatureOfSupply_AreaOfAllOrManyCrops"),
                        "Yield" = "NatureOfSupply_Yield",
                        "Production of a Crop or Crops" = c("NatureOfSupply_ProductionOfACrop", 
                                                            "NatureOfSupply_ProductionOfAllCrops"),
                        "All Supply Side Variables" = "NatureOfSupply_AllSupplysideVariables"
)

# Extract dummy variable for what type of program is used
programMaster = DcList[[selectedProgram]]
# Extract dummy variable for what type of supply is used
# If there is more than one type then we need to add those together
if (length(selectedSupply) > 1) {
  DcList[[selectedSupply[1]]] = replace_na(DcList[[selectedSupply[1]]], 0)
  DcList[[selectedSupply[2]]] = replace_na(DcList[[selectedSupply[2]]], 0)
  supplyMaster = DcList[[selectedSupply[1]]] + DcList[[selectedSupply[2]]]
  supplyMaster = as.numeric(gsub(0, NA, supplyMaster))
} else {
  supplyMaster = DcList[[selectedSupply]]
}

# Create sublists of the calculations
calcLists = function(subList, programId) {
  # Extra factors are other conditions to consider, such as studies covering all of the US
  if (programId == "all") {
    extraFactors = 1
  } else if (programId == "est") {
    extraFactors = DcList[["Method_Estimation"]]
  } else if (programId == "notEst") {
    notEstRelavancy = as.numeric(gsub(1, 5, DcList[["Method_Estimation"]]))
    notEstRelavancy = replace_na(notEstRelavancy, 1)
    notEstRelavancy = as.numeric(gsub(1, NA, DcList[["Method_Estimation"]]))
    extraFactors = notEstRelavancy
  } else if (programId == "usNat") {
    extraFactors = DcList[["Region_AllOfUS"]]
  } else if (programId == "other") {
    otherRelavancy = as.numeric(gsub(1, 5, DcList[["Region_AllOfUS"]]))
    otherRelavancy = replace_na(otherRelavancy, 1)
    otherRelavancy = as.numeric(gsub(5, NA, DcList[["Region_AllOfUS"]]))
    extraFactors = otherRelavancy
  }
  
  # Pull relavancy, calculate averages, and get a count of observations
  for (i in grep("Relavancy", names((subList)))) {
    avenue = which((sub("\\_.*", "", names(subList))) == sub("\\_.*", "", names((subList[i]))))
    
    Relavancy = grep("Relavancy", names(subList[avenue]))
    PCOPUP = grep("PCOPUP", names(subList[avenue]))
    PI2MI = grep("PI2MI", names(subList[avenue]))
    
    subList[[avenue[Relavancy]]][["data"]] = programMaster * supplyMaster * extraFactors * 
      DcList[[which((sub(".*_", "", names(DcList))) == sub("\\_.*", "", names((subList[i]))))]]
    subList[[avenue[PCOPUP]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["ImpactOfPayments_PctChangeOutputPerunitPmt"]]
    subList[[avenue[PI2MI]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["Ratio_RatioOfPmtImpToMarketImp"]]
  }
  return(subList)
}

# Run the function for all the sublists
all = calcLists(all, "all")
est = calcLists(est, "est")
notEst = calcLists(notEst, "notEst")
usNat = calcLists(usNat, "usNat")
other = calcLists(other, "other")

# Calculates the number of observations triggered and averages the values
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

# Run the function for all the sublists
all = calcStats(all)
est = calcStats(est)
notEst = calcStats(notEst)
usNat = calcStats(usNat)
other = calcStats(other)


###############################################################

# Template for loading data into the table
tableTemplate = read_excel("Decoupling/tableTemplate.xlsx", col_names = TRUE)
tableTemplate = as.data.frame(tableTemplate)

# Dynamic program identifier
tableTemplate[1, ] = selectedProgramText

# Create the flex table
myft = flextable(tableTemplate)

# Merge duplicate columns, row-wise
myft = merge_at(myft, i = 1)
myft = merge_at(myft, i = 2, j = 1:8)
myft = merge_at(myft, i = 14, j = 1:6)

# Rename columns
# These cannot be put into the template because the column names would not be unique
myft = set_header_labels(myft, values = list(V1 = " ",
                                             V2 = " ",
                                             V3 = " ",
                                             V4 = "Price Effect",
                                             V5 = "Price Effect",
                                             V6 = "Risk Aversion",
                                             V7 = "Risk Aversion",
                                             V8 = "Wealth",
                                             V9 = "Wealth",
                                             V10 = "Expectations",
                                             V11 = "Expectations",
                                             V12 = "Exclusions",
                                             V13 = "Exclusions",
                                             V14 = "Liquidity",
                                             V15 = "Liquidity",
                                             V16 = "Labor",
                                             V17 = "Labor",
                                             V18 = "Entry Or Exit",
                                             V19 = "Entry Or Exit",
                                             V20 = "Other",
                                             V21 = "Other",
                                             V22 = "All",
                                             V23 = "All"))

# Merge duplicate columns in the header
myft = merge_h(myft, part = "header")

# Align some rows to the left
myft = align(myft, i = c(1,2,14), align = "left")

# Fix the column widths
myft = width(myft, j = 1:3, width = 1.5)
myft = width(myft, j = 4:23, width = 0.5)

# Align title center
myft = align(myft, align = "center", part = "header")

# Align some data to the left
myft = align(myft, j = c(5,7,9,11,13,15,17,19,21,23), align = "left", part = "body")

# Text styling
myft = style(myft, i = c(1, 2, 14), pr_t = fp_text(color = "black", bold = TRUE), part = "body")
myft = style(myft, pr_t = fp_text(color = "black", bold = TRUE, font.size = 10), part = "header")

# Use to create dynamic header
set_caption(myft, paste0("Avenue of Payment Impact on ", 
                         selectedSupplyText, 
                         ", Number of Observations. and Simple Average"))

