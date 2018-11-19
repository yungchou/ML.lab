test.prep.org <- read.csv('data/test.csv', stringsAsFactors=FALSE)
names(test.prep.org)

# Dropped Features after initial EDA
test.prep <- test.prep.org[-c(

  #1, #Id,
  7, #Alley,
  8, #LotShape
  22, #RoofStyle,
  23, #RoofMatl,
  27, #MasVnrArea,
  30, #Foundation,
  43, #Electrical,
  48, #BsmtFullBath
  58, #FireplaceQu,
  73, #PoolQC,
  79, #SaleType
  80  #SaleCondition

)]

missing <- function(x) { round(( sum(is.na(x))/length(x) )*100, 2) }
apply(test.prep,2,missing)

#------------------
# data preparation
#------------------
# Ref: http://www-edlab.cs.umass.edu/~jianyang/data_description.pdf

#unique(test.prep$MSZoning)
test.prep$MSZoning[test.prep$MSZoning == "RH"] <- 4  # High - Density Residential
test.prep$MSZoning[test.prep$MSZoning == "RM"] <- 3  # Medium - Density Residential
test.prep$MSZoning[test.prep$MSZoning == "RL"] <- 2  # Low - Density Residential
test.prep$MSZoning[test.prep$MSZoning == "C (all)"] <- 1 # CM/CRC/CN/CT/CS - Commercial
test.prep$MSZoning[test.prep$MSZoning == "FV"] <- 0 # Floating Village Residential ??
test.prep$MSZoning[is.na(test.prep$MSZoning)] <- 3
test.prep$MSZoning <- as.integer(test.prep$MSZoning)

#unique(test.prep$Street)
test.prep$Street[test.prep$Street == "Pave"] <-1
test.prep$Street[test.prep$Street == "Grvl"] <-0
test.prep$Street[is.na(test.prep$Street)] <- 1
test.prep$Street <- as.integer(test.prep$Street)

#unique(test.prep$LandContour)
test.prep$LandContour[test.prep$LandContour == "HLS"] <- 3 # Hillside
test.prep$LandContour[test.prep$LandContour == "Lvl"] <- 2 # Near Flat/Level
test.prep$LandContour[test.prep$LandContour == "Bnk"] <- 1 # Banked
test.prep$LandContour[test.prep$LandContour == "Low"] <- 0 # Depression
test.prep$LandContour[is.na(test.prep$LandContour)] <- 2
test.prep$LandContour <- as.integer(test.prep$LandContour)

#unique(test.prep$Utilities)
test.prep$Utilities[test.prep$Utilities == "AllPub"] <- 3
test.prep$Utilities[test.prep$Utilities == "NoSewr"] <- 2
test.prep$Utilities[test.prep$Utilities == "NoSeWa"] <- 1
test.prep$Utilities[is.na(test.prep$Utilities)] <- 2
test.prep$Utilities <- as.integer(test.prep$Utilities)


#unique(test.prep$LandSlope)
test.prep$LandSlope[test.prep$LandSlope == "Gtl"] <- 2
test.prep$LandSlope[test.prep$LandSlope == "Mod"] <- 1
test.prep$LandSlope[test.prep$LandSlope == "Sev"] <- 0
test.prep$LandSlope[is.na(test.prep$LandSlope)] <- 2
test.prep$LandSlope <- as.integer(test.prep$LandSlope)

#unique(test.prep$LotConfig)
test.prep$LotConfig[test.prep$LotConfig == "CulDSac"] <- 4
test.prep$LotConfig[test.prep$LotConfig == "Inside"] <- 3
test.prep$LotConfig[test.prep$LotConfig == "Corner"] <- 2
test.prep$LotConfig[test.prep$LotConfig == "FR2"] <- 1 # Forntage on 3 sides of property
test.prep$LotConfig[test.prep$LotConfig == "FR3"] <- 0 # Forntage on 3 sides of property
test.prep$LotConfig[is.na(test.prep$LotConfig)] <- 3
test.prep$LotConfig <- as.integer(test.prep$LotConfig)

#unique(test.prep$Neighborhood) # TO BE VALIDATED
test.prep$Neighborhood[test.prep$Neighborhood %in% c(
  "CollgCr","Veenker","Crawfor","NoRidge","Mitchel","Somerst",
  "NWAmes","OldTown","BrkSide","Sawyer","NridgHt",
  "NAmes","SawyerW","IDOTRR","MeadowV","Edwards","Timber","Gilbert","StoneBr",
  "ClearCr","NPkVill","Blmngtn","BrDale","SWISU","Blueste")] <- 3
test.prep$Neighborhood [is.na(test.prep$Neighborhood)] <- 2
test.prep$Neighborhood <- as.integer(test.prep$Neighborhood)

#unique(test.prep$Condition1)
test.prep$Condition1[test.prep$Condition1 %in% c("PosA","PosN")] <- 3
test.prep$Condition1[test.prep$Condition1 %in% c("Norm")] <- 2
test.prep$Condition1[test.prep$Condition1 %in% c("Feedr","Artery")] <- 1
test.prep$Condition1[test.prep$Condition1 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
test.prep$Condition1[is.na(test.prep$Condition1)] <- 2
test.prep$Condition1 <- as.integer(test.prep$Condition1)

#unique(test.prep$Condition2)
test.prep$Condition2[test.prep$Condition2 %in% c("PosA","PosN")] <- 3
test.prep$Condition2[test.prep$Condition2 %in% c("Norm")] <- 2
test.prep$Condition2[test.prep$Condition2 %in% c("Feedr","Artery")] <- 1
test.prep$Condition2[test.prep$Condition2 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
test.prep$Condition2[is.na(test.prep$Condition2)] <- 2
test.prep$Condition2 <- as.integer(test.prep$Condition2)

#unique(test.prep$BldgType)
test.prep$BldgType[test.prep$BldgType == "1Fam" ] <- 2
test.prep$BldgType[test.prep$BldgType == "2fmCon" ] <- 1
test.prep$BldgType[test.prep$BldgType == "Duplex" ] <- 1
test.prep$BldgType[test.prep$BldgType == "TwnhsE" ] <- 2
test.prep$BldgType[test.prep$BldgType == "Twnhs" ] <- 1
test.prep$BldgType[is.na(test.prep$BldgType)] <- 1
test.prep$BldgType <- as.integer(test.prep$BldgType)

#unique(test.prep$HouseStyle)
test.prep$HouseStyle[test.prep$HouseStyle == "2.5Fin"] <- 7
test.prep$HouseStyle[test.prep$HouseStyle == "2.5Unf"] <- 5
test.prep$HouseStyle[test.prep$HouseStyle == "2Story"] <- 4
test.prep$HouseStyle[test.prep$HouseStyle == "1.5Fin"] <- 3
test.prep$HouseStyle[test.prep$HouseStyle == "1.5Unf"] <- 2
test.prep$HouseStyle[test.prep$HouseStyle == "1Story"] <- 1
test.prep$HouseStyle[test.prep$HouseStyle == "SFoyer"] <- 1
test.prep$HouseStyle[test.prep$HouseStyle == "SLvl"  ] <- 1
test.prep$HouseStyle[is.na(test.prep$HouseStyle)] <- 1
test.prep$HouseStyle<- as.integer(test.prep$HouseStyle)

#unique(test.prep$Exterior1st)
test.prep$Exterior1st[test.prep$Exterior1st %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 5
test.prep$Exterior1st[test.prep$Exterior1st %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 3
test.prep$Exterior1st[test.prep$Exterior1st %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 2
test.prep$Exterior1st[is.na(test.prep$Exterior1st)] <- 2
test.prep$Exterior1st <- as.integer(test.prep$Exterior1st)

#unique(test.prep$Exterior2nd)
test.prep$Exterior2nd[test.prep$Exterior2nd %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 5
test.prep$Exterior2nd[test.prep$Exterior2nd %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 3
test.prep$Exterior2nd[test.prep$Exterior2nd %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 2
test.prep$Exterior2nd[is.na(test.prep$Exterior2nd)] <- 2
test.prep$Exterior2nd <- as.integer(test.prep$Exterior2nd)

#unique(test.prep$MasVnrType)
test.prep$MasVnrType[test.prep$MasVnrType == "BrkFace"] <- 4
test.prep$MasVnrType[test.prep$MasVnrType == "Stone" ] <- 4
test.prep$MasVnrType[test.prep$MasVnrType == "BrkCmn"] <- 2
test.prep$MasVnrType[test.prep$MasVnrType == "None" ] <- 1
test.prep$MasVnrType[is.na(test.prep$MasVnrType)] <- 1
test.prep$MasVnrType <- as.integer(test.prep$MasVnrType)

#unique(test.prep$ExterQual)
test.prep$ExterQual[test.prep$ExterQual == "Ex"] <- 7
test.prep$ExterQual[test.prep$ExterQual == "Gd"] <- 5
test.prep$ExterQual[test.prep$ExterQual == "TA"] <- 4
test.prep$ExterQual[test.prep$ExterQual == "Fa"] <- 3
test.prep$ExterQual[test.prep$ExterQual == "Po"] <- 1
test.prep$ExterQual[is.na(test.prep$ExterQual)] <- 1
test.prep$ExterQual <- as.integer(test.prep$ExterQual)

#unique(test.prep$ExterCond)
test.prep$ExterCond[test.prep$ExterCond == "Ex"] <- 7
test.prep$ExterCond[test.prep$ExterCond == "Gd"] <- 5
test.prep$ExterCond[test.prep$ExterCond == "TA"] <- 4
test.prep$ExterCond[test.prep$ExterCond == "Fa"] <- 3
test.prep$ExterCond[test.prep$ExterCond == "Po"] <- 1
test.prep$ExterCond[is.na(test.prep$ExterCond)] <- 1
test.prep$ExterCond <- as.integer(test.prep$ExterCond)

#unique(test.prep$BsmtQual)
test.prep$BsmtQual[test.prep$BsmtQual == "Ex"] <- 5
test.prep$BsmtQual[test.prep$BsmtQual == "Gd"] <- 4
test.prep$BsmtQual[test.prep$BsmtQual == "TA"] <- 3
test.prep$BsmtQual[test.prep$BsmtQual == "Fa"] <- 2
test.prep$BsmtQual[test.prep$BsmtQual == "Po"] <- 1
test.prep$BsmtQual[is.na(test.prep$BsmtQual)] <- 1
test.prep$BsmtQual <- as.integer(test.prep$BsmtQual)

#unique(test.prep$BsmtCond)
test.prep$BsmtCond[test.prep$BsmtCond == "Ex"] <- 5
test.prep$BsmtCond[test.prep$BsmtCond == "Gd"] <- 4
test.prep$BsmtCond[test.prep$BsmtCond == "TA"] <- 3
test.prep$BsmtCond[test.prep$BsmtCond == "Fa"] <- 2
test.prep$BsmtCond[test.prep$BsmtCond == "Po"] <- 1
test.prep$BsmtCond[is.na(test.prep$BsmtCond)] <- 1
test.prep$BsmtCond <- as.integer(test.prep$BsmtCond)

#unique(test.prep$BsmtExposure)
test.prep$BsmtExposure[test.prep$BsmtExposure == "Gd"] <- 5
test.prep$BsmtExposure[test.prep$BsmtExposure == "Av"] <- 4
test.prep$BsmtExposure[test.prep$BsmtExposure == "Mn"] <- 3
test.prep$BsmtExposure[test.prep$BsmtExposure == "No"] <- 2
test.prep$BsmtExposure[test.prep$BsmtExposure == "NA"] <- 2
test.prep$BsmtExposure[is.na(test.prep$BsmtExposure)] <- 2
test.prep$BsmtExposure <- as.integer(test.prep$BsmtExposure)

#unique(test.prep$BsmtFinType1)
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "GLQ"] <- 6
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "ALQ"] <- 5
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "BLQ"] <- 3
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "Rec"] <- 2
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "LwQ"] <- 1
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "Unf"] <- 1
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "NA"] <- 1
test.prep$BsmtFinType1[is.na(test.prep$BsmtFinType1)] <- 1
test.prep$BsmtFinType1 <- as.integer(test.prep$BsmtFinType1)

#unique(test.prep$BsmtFinType2)
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "GLQ"] <- 6
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "ALQ"] <- 5
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "BLQ"] <- 3
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "Rec"] <- 2
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "LwQ"] <- 1
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "Unf"] <- 1
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "NA"] <- 1
test.prep$BsmtFinType2[is.na(test.prep$BsmtFinType2)] <- 1
test.prep$BsmtFinType2 <- as.integer(test.prep$BsmtFinType2)

#unique(test.prep$Heating)
test.prep$Heating[test.prep$Heating %in% c("Floor","GasA","GasW")] <- 2
test.prep$Heating[test.prep$Heating %in% c("Grav","Wall","OthW")] <- 1
test.prep$Heating[is.na(test.prep$Heating)] <- 0
test.prep$Heating <- as.integer(test.prep$Heating)

#unique(test.prep$HeatingQC)
test.prep$HeatingQC[test.prep$HeatingQC == "Ex"] <- 5
test.prep$HeatingQC[test.prep$HeatingQC == "Gd"] <- 4
test.prep$HeatingQC[test.prep$HeatingQC == "TA"] <- 3
test.prep$HeatingQC[test.prep$HeatingQC == "Fa"] <- 2
test.prep$HeatingQC[test.prep$HeatingQC == "Po"] <- 1
test.prep$HeatingQC[is.na(test.prep$HeatingQC)] <- 1
test.prep$HeatingQC <- as.integer(test.prep$HeatingQC)

#unique(test.prep$CentralAir)
test.prep$CentralAir[test.prep$CentralAir == "Y"] <- 5
test.prep$CentralAir[test.prep$CentralAir == "N"] <- 2
test.prep$CentralAir[is.na(test.prep$CentralAir)] <- 2
test.prep$CentralAir <- as.integer(test.prep$CentralAir)

#unique(test.prep$KitchenQual)
test.prep$KitchenQual[test.prep$KitchenQual == "Ex"] <- 5
test.prep$KitchenQual[test.prep$KitchenQual == "Gd"] <- 4
test.prep$KitchenQual[test.prep$KitchenQual == "TA"] <- 3
test.prep$KitchenQual[test.prep$KitchenQual == "Fa"] <- 2
test.prep$KitchenQual[test.prep$KitchenQual == "Po"] <- 1
test.prep$KitchenQual[is.na(test.prep$KitchenQual)] <- 1
test.prep$KitchenQual <- as.integer(test.prep$KitchenQual)

#unique(test.prep$Functional)
test.prep$Functional[test.prep$Functional %in% c("Typ","Mod")] <- 3
test.prep$Functional[test.prep$Functional %in% c("Min1","Min2")] <- 2
test.prep$Functional[test.prep$Functional %in% c("Maj1","Maj2")] <- 2
test.prep$Functional[test.prep$Functional == "Sev"] <- 1
test.prep$Functional[is.na(test.prep$Functional)] <- 1
test.prep$Functional <- as.integer(test.prep$Functional)

#unique(test.prep$GarageType)
test.prep$GarageType[test.prep$GarageType %in% c("Attchd","2Types","BuiltIn")] <- 5
test.prep$GarageType[test.prep$GarageType %in% c("Basment","Detchd")] <- 3
test.prep$GarageType[test.prep$GarageType == "CarPort"] <- 2
test.prep$GarageType[is.na(test.prep$GarageType)] <- 1
test.prep$GarageType <- as.integer(test.prep$GarageType)

#unique(test.prep$GarageFinish)
test.prep$GarageFinish[test.prep$GarageFinish == "Fin"] <- 4
test.prep$GarageFinish[test.prep$GarageFinish == "RFn"] <- 2
test.prep$GarageFinish[test.prep$GarageFinish == 'Unf'] <- 1
test.prep$GarageFinish[is.na(test.prep$GarageFinish)] <- 1
test.prep$GarageFinish <- as.integer(test.prep$GarageFinish)

#unique(test.prep$GarageQual)
test.prep$GarageQual[test.prep$GarageQual == "Ex"] <- 5
test.prep$GarageQual[test.prep$GarageQual == "Gd"] <- 4
test.prep$GarageQual[test.prep$GarageQual == 'TA'] <- 3
test.prep$GarageQual[test.prep$GarageQual == 'Fa'] <- 2
test.prep$GarageQual[test.prep$GarageQual == 'Po'] <- 1
test.prep$GarageQual[is.na(test.prep$GarageQual)] <- 1
test.prep$GarageQual <- as.integer(test.prep$GarageQual)

#unique(test.prep$GarageCond)
test.prep$GarageCond[test.prep$GarageCond == "Ex"] <- 5
test.prep$GarageCond[test.prep$GarageCond == "Gd"] <- 4
test.prep$GarageCond[test.prep$GarageCond == 'TA'] <- 3
test.prep$GarageCond[test.prep$GarageCond == 'Fa'] <- 2
test.prep$GarageCond[test.prep$GarageCond == 'Po'] <- 1
test.prep$GarageCond[is.na(test.prep$GarageCond)] <- 1
test.prep$GarageCond <- as.integer(test.prep$GarageCond)

#unique(test.prep$PavedDrive)
test.prep$PavedDrive[test.prep$PavedDrive == "Y"] <- 2
test.prep$PavedDrive[test.prep$PavedDrive == "P"] <- 1
test.prep$PavedDrive[test.prep$PavedDrive == "N"] <- 1
test.prep$PavedDrive[is.na(test.prep$PavedDrive)] <- 1
test.prep$PavedDrive <- as.integer(test.prep$PavedDrive)

#unique(test.prep$Fence)
test.prep$Fence[test.prep$Fence == "GdPrv"] <- 2
test.prep$Fence[test.prep$Fence == "MnPrv"] <- 1
test.prep$Fence[test.prep$Fence == 'GdWo'] <- 1
test.prep$Fence[test.prep$Fence == "MnWw"] <- 1
test.prep$Fence[is.na(test.prep$Fence)] <- 1
test.prep$Fence <- as.integer(test.prep$Fence)

#unique(test.prep$MiscFeature)
test.prep$MiscFeature[test.prep$MiscFeature == "Elev"] <- 3
test.prep$MiscFeature[test.prep$MiscFeature == "Gar2"] <- 3
test.prep$MiscFeature[test.prep$MiscFeature == 'Othr'] <- 1
test.prep$MiscFeature[test.prep$MiscFeature == "Shed"] <- 1
test.prep$MiscFeature[test.prep$MiscFeature == "TenC"] <- 3
test.prep$MiscFeature[is.na(test.prep$MiscFeature)] <- 1
test.prep$MiscFeature <- as.integer(test.prep$MiscFeature)

#unique(test.prep$MoSold)
test.prep$MoSold[test.prep$MoSold %in% c(10,11,12)] <- 4  # seasonality
test.prep$MoSold[test.prep$MoSold %in% c(7,8,9)] <- 3
test.prep$MoSold[test.prep$MoSold %in% c(4,5,6)] <- 2
test.prep$MoSold[test.prep$MoSold %in% c(1,2,3)] <- 1
test.prep$MoSold[is.na(test.prep$MoSold)] <- 1
test.prep$MoSold <- as.integer(test.prep$MoSold)

#sum(is.na(test.prep))
#test.prep <- na.omit(test.prep)
