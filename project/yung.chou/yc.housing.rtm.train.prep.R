train.prep.org <- read.csv('data/train.csv', stringsAsFactors=FALSE)
names(train.prep.org)

# Dropped Features after initial EDA
train.prep <- train.prep.org[-c(

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
apply(train.prep,2,missing)

#------------------
# data preparation
#------------------
# Ref: http://www-edlab.cs.umass.edu/~jianyang/data_description.pdf

#unique(train.prep$MSZoning)
train.prep$MSZoning[train.prep$MSZoning == "RH"] <- 4  # High - Density Residential
train.prep$MSZoning[train.prep$MSZoning == "RM"] <- 3  # Medium - Density Residential
train.prep$MSZoning[train.prep$MSZoning == "RL"] <- 2  # Low - Density Residential
train.prep$MSZoning[train.prep$MSZoning == "C (all)"] <- 1 # CM/CRC/CN/CT/CS - Commercial
train.prep$MSZoning[train.prep$MSZoning == "FV"] <- 0 # Floating Village Residential ??
train.prep$MSZoning[is.na(train.prep$MSZoning)] <- 3
train.prep$MSZoning <- as.integer(train.prep$MSZoning)

#unique(train.prep$Street)
train.prep$Street[train.prep$Street == "Pave"] <-1
train.prep$Street[train.prep$Street == "Grvl"] <-0
train.prep$Street[is.na(train.prep$Street)] <- 1
train.prep$Street <- as.integer(train.prep$Street)

#unique(train.prep$LandContour)
train.prep$LandContour[train.prep$LandContour == "HLS"] <- 3 # Hillside
train.prep$LandContour[train.prep$LandContour == "Lvl"] <- 2 # Near Flat/Level
train.prep$LandContour[train.prep$LandContour == "Bnk"] <- 1 # Banked
train.prep$LandContour[train.prep$LandContour == "Low"] <- 0 # Depression
train.prep$LandContour[is.na(train.prep$LandContour)] <- 2
train.prep$LandContour <- as.integer(train.prep$LandContour)

#unique(train.prep$Utilities)
train.prep$Utilities[train.prep$Utilities == "AllPub"] <- 3
train.prep$Utilities[train.prep$Utilities == "NoSewr"] <- 2
train.prep$Utilities[train.prep$Utilities == "NoSeWa"] <- 1
train.prep$Utilities[is.na(train.prep$Utilities)] <- 2
train.prep$Utilities <- as.integer(train.prep$Utilities)


#unique(train.prep$LandSlope)
train.prep$LandSlope[train.prep$LandSlope == "Gtl"] <- 2
train.prep$LandSlope[train.prep$LandSlope == "Mod"] <- 1
train.prep$LandSlope[train.prep$LandSlope == "Sev"] <- 0
train.prep$LandSlope[is.na(train.prep$LandSlope)] <- 2
train.prep$LandSlope <- as.integer(train.prep$LandSlope)

#unique(train.prep$LotConfig)
train.prep$LotConfig[train.prep$LotConfig == "CulDSac"] <- 4
train.prep$LotConfig[train.prep$LotConfig == "Inside"] <- 3
train.prep$LotConfig[train.prep$LotConfig == "Corner"] <- 2
train.prep$LotConfig[train.prep$LotConfig == "FR2"] <- 1 # Forntage on 3 sides of property
train.prep$LotConfig[train.prep$LotConfig == "FR3"] <- 0 # Forntage on 3 sides of property
train.prep$LotConfig[is.na(train.prep$LotConfig)] <- 3
train.prep$LotConfig <- as.integer(train.prep$LotConfig)

#unique(train.prep$Neighborhood) # TO BE VALIDATED
train.prep$Neighborhood[train.prep$Neighborhood %in% c(
  "CollgCr","Veenker","Crawfor","NoRidge","Mitchel","Somerst",
  "NWAmes","OldTown","BrkSide","Sawyer","NridgHt",
  "NAmes","SawyerW","IDOTRR","MeadowV","Edwards","Timber","Gilbert","StoneBr",
  "ClearCr","NPkVill","Blmngtn","BrDale","SWISU","Blueste")] <- 3
train.prep$Neighborhood [is.na(train.prep$Neighborhood)] <- 2
train.prep$Neighborhood <- as.integer(train.prep$Neighborhood)

#unique(train.prep$Condition1)
train.prep$Condition1[train.prep$Condition1 %in% c("PosA","PosN")] <- 3
train.prep$Condition1[train.prep$Condition1 %in% c("Norm")] <- 2
train.prep$Condition1[train.prep$Condition1 %in% c("Feedr","Artery")] <- 1
train.prep$Condition1[train.prep$Condition1 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
train.prep$Condition1[is.na(train.prep$Condition1)] <- 2
train.prep$Condition1 <- as.integer(train.prep$Condition1)

#unique(train.prep$Condition2)
train.prep$Condition2[train.prep$Condition2 %in% c("PosA","PosN")] <- 3
train.prep$Condition2[train.prep$Condition2 %in% c("Norm")] <- 2
train.prep$Condition2[train.prep$Condition2 %in% c("Feedr","Artery")] <- 1
train.prep$Condition2[train.prep$Condition2 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
train.prep$Condition2[is.na(train.prep$Condition2)] <- 2
train.prep$Condition2 <- as.integer(train.prep$Condition2)

#unique(train.prep$BldgType)
train.prep$BldgType[train.prep$BldgType == "1Fam" ] <- 2
train.prep$BldgType[train.prep$BldgType == "2fmCon" ] <- 1
train.prep$BldgType[train.prep$BldgType == "Duplex" ] <- 1
train.prep$BldgType[train.prep$BldgType == "TwnhsE" ] <- 2
train.prep$BldgType[train.prep$BldgType == "Twnhs" ] <- 1
train.prep$BldgType[is.na(train.prep$BldgType)] <- 1
train.prep$BldgType <- as.integer(train.prep$BldgType)

#unique(train.prep$HouseStyle)
train.prep$HouseStyle[train.prep$HouseStyle == "2.5Fin"] <- 7
train.prep$HouseStyle[train.prep$HouseStyle == "2.5Unf"] <- 5
train.prep$HouseStyle[train.prep$HouseStyle == "2Story"] <- 4
train.prep$HouseStyle[train.prep$HouseStyle == "1.5Fin"] <- 3
train.prep$HouseStyle[train.prep$HouseStyle == "1.5Unf"] <- 2
train.prep$HouseStyle[train.prep$HouseStyle == "1Story"] <- 1
train.prep$HouseStyle[train.prep$HouseStyle == "SFoyer"] <- 1
train.prep$HouseStyle[train.prep$HouseStyle == "SLvl"  ] <- 1
train.prep$HouseStyle[is.na(train.prep$HouseStyle)] <- 1
train.prep$HouseStyle<- as.integer(train.prep$HouseStyle)

#unique(train.prep$Exterior1st)
train.prep$Exterior1st[train.prep$Exterior1st %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 5
train.prep$Exterior1st[train.prep$Exterior1st %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 3
train.prep$Exterior1st[train.prep$Exterior1st %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 2
train.prep$Exterior1st[is.na(train.prep$Exterior1st)] <- 2
train.prep$Exterior1st <- as.integer(train.prep$Exterior1st)

#unique(train.prep$Exterior2nd)
train.prep$Exterior2nd[train.prep$Exterior2nd %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 5
train.prep$Exterior2nd[train.prep$Exterior2nd %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 3
train.prep$Exterior2nd[train.prep$Exterior2nd %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 2
train.prep$Exterior2nd[is.na(train.prep$Exterior2nd)] <- 2
train.prep$Exterior2nd <- as.integer(train.prep$Exterior2nd)

#unique(train.prep$MasVnrType)
train.prep$MasVnrType[train.prep$MasVnrType == "BrkFace"] <- 4
train.prep$MasVnrType[train.prep$MasVnrType == "Stone" ] <- 4
train.prep$MasVnrType[train.prep$MasVnrType == "BrkCmn"] <- 2
train.prep$MasVnrType[train.prep$MasVnrType == "None" ] <- 1
train.prep$MasVnrType[is.na(train.prep$MasVnrType)] <- 1
train.prep$MasVnrType <- as.integer(train.prep$MasVnrType)

#unique(train.prep$ExterQual)
train.prep$ExterQual[train.prep$ExterQual == "Ex"] <- 7
train.prep$ExterQual[train.prep$ExterQual == "Gd"] <- 5
train.prep$ExterQual[train.prep$ExterQual == "TA"] <- 4
train.prep$ExterQual[train.prep$ExterQual == "Fa"] <- 3
train.prep$ExterQual[train.prep$ExterQual == "Po"] <- 1
train.prep$ExterQual[is.na(train.prep$ExterQual)] <- 1
train.prep$ExterQual <- as.integer(train.prep$ExterQual)

#unique(train.prep$ExterCond)
train.prep$ExterCond[train.prep$ExterCond == "Ex"] <- 7
train.prep$ExterCond[train.prep$ExterCond == "Gd"] <- 5
train.prep$ExterCond[train.prep$ExterCond == "TA"] <- 4
train.prep$ExterCond[train.prep$ExterCond == "Fa"] <- 3
train.prep$ExterCond[train.prep$ExterCond == "Po"] <- 1
train.prep$ExterCond[is.na(train.prep$ExterCond)] <- 1
train.prep$ExterCond <- as.integer(train.prep$ExterCond)

#unique(train.prep$BsmtQual)
train.prep$BsmtQual[train.prep$BsmtQual == "Ex"] <- 5
train.prep$BsmtQual[train.prep$BsmtQual == "Gd"] <- 4
train.prep$BsmtQual[train.prep$BsmtQual == "TA"] <- 3
train.prep$BsmtQual[train.prep$BsmtQual == "Fa"] <- 2
train.prep$BsmtQual[train.prep$BsmtQual == "Po"] <- 1
train.prep$BsmtQual[is.na(train.prep$BsmtQual)] <- 1
train.prep$BsmtQual <- as.integer(train.prep$BsmtQual)

#unique(train.prep$BsmtCond)
train.prep$BsmtCond[train.prep$BsmtCond == "Ex"] <- 5
train.prep$BsmtCond[train.prep$BsmtCond == "Gd"] <- 4
train.prep$BsmtCond[train.prep$BsmtCond == "TA"] <- 3
train.prep$BsmtCond[train.prep$BsmtCond == "Fa"] <- 2
train.prep$BsmtCond[train.prep$BsmtCond == "Po"] <- 1
train.prep$BsmtCond[is.na(train.prep$BsmtCond)] <- 1
train.prep$BsmtCond <- as.integer(train.prep$BsmtCond)

#unique(train.prep$BsmtExposure)
train.prep$BsmtExposure[train.prep$BsmtExposure == "Gd"] <- 5
train.prep$BsmtExposure[train.prep$BsmtExposure == "Av"] <- 4
train.prep$BsmtExposure[train.prep$BsmtExposure == "Mn"] <- 3
train.prep$BsmtExposure[train.prep$BsmtExposure == "No"] <- 2
train.prep$BsmtExposure[train.prep$BsmtExposure == "NA"] <- 2
train.prep$BsmtExposure[is.na(train.prep$BsmtExposure)] <- 2
train.prep$BsmtExposure <- as.integer(train.prep$BsmtExposure)

#unique(train.prep$BsmtFinType1)
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "GLQ"] <- 6
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "ALQ"] <- 5
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "BLQ"] <- 3
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "Rec"] <- 2
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "LwQ"] <- 1
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "Unf"] <- 1
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "NA"] <- 1
train.prep$BsmtFinType1[is.na(train.prep$BsmtFinType1)] <- 1
train.prep$BsmtFinType1 <- as.integer(train.prep$BsmtFinType1)

#unique(train.prep$BsmtFinType2)
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "GLQ"] <- 6
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "ALQ"] <- 5
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "BLQ"] <- 3
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "Rec"] <- 2
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "LwQ"] <- 1
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "Unf"] <- 1
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "NA"] <- 1
train.prep$BsmtFinType2[is.na(train.prep$BsmtFinType2)] <- 1
train.prep$BsmtFinType2 <- as.integer(train.prep$BsmtFinType2)

#unique(train.prep$Heating)
train.prep$Heating[train.prep$Heating %in% c("Floor","GasA","GasW")] <- 2
train.prep$Heating[train.prep$Heating %in% c("Grav","Wall","OthW")] <- 1
train.prep$Heating[is.na(train.prep$Heating)] <- 0
train.prep$Heating <- as.integer(train.prep$Heating)

#unique(train.prep$HeatingQC)
train.prep$HeatingQC[train.prep$HeatingQC == "Ex"] <- 5
train.prep$HeatingQC[train.prep$HeatingQC == "Gd"] <- 4
train.prep$HeatingQC[train.prep$HeatingQC == "TA"] <- 3
train.prep$HeatingQC[train.prep$HeatingQC == "Fa"] <- 2
train.prep$HeatingQC[train.prep$HeatingQC == "Po"] <- 1
train.prep$HeatingQC[is.na(train.prep$HeatingQC)] <- 1
train.prep$HeatingQC <- as.integer(train.prep$HeatingQC)

#unique(train.prep$CentralAir)
train.prep$CentralAir[train.prep$CentralAir == "Y"] <- 5
train.prep$CentralAir[train.prep$CentralAir == "N"] <- 2
train.prep$CentralAir[is.na(train.prep$CentralAir)] <- 2
train.prep$CentralAir <- as.integer(train.prep$CentralAir)

#unique(train.prep$KitchenQual)
train.prep$KitchenQual[train.prep$KitchenQual == "Ex"] <- 5
train.prep$KitchenQual[train.prep$KitchenQual == "Gd"] <- 4
train.prep$KitchenQual[train.prep$KitchenQual == "TA"] <- 3
train.prep$KitchenQual[train.prep$KitchenQual == "Fa"] <- 2
train.prep$KitchenQual[train.prep$KitchenQual == "Po"] <- 1
train.prep$KitchenQual[is.na(train.prep$KitchenQual)] <- 1
train.prep$KitchenQual <- as.integer(train.prep$KitchenQual)

#unique(train.prep$Functional)
train.prep$Functional[train.prep$Functional %in% c("Typ","Mod")] <- 3
train.prep$Functional[train.prep$Functional %in% c("Min1","Min2")] <- 2
train.prep$Functional[train.prep$Functional %in% c("Maj1","Maj2")] <- 2
train.prep$Functional[train.prep$Functional == "Sev"] <- 1
train.prep$Functional[is.na(train.prep$Functional)] <- 1
train.prep$Functional <- as.integer(train.prep$Functional)

#unique(train.prep$GarageType)
train.prep$GarageType[train.prep$GarageType %in% c("Attchd","2Types","BuiltIn")] <- 5
train.prep$GarageType[train.prep$GarageType %in% c("Basment","Detchd")] <- 3
train.prep$GarageType[train.prep$GarageType == "CarPort"] <- 2
train.prep$GarageType[is.na(train.prep$GarageType)] <- 1
train.prep$GarageType <- as.integer(train.prep$GarageType)

#unique(train.prep$GarageFinish)
train.prep$GarageFinish[train.prep$GarageFinish == "Fin"] <- 4
train.prep$GarageFinish[train.prep$GarageFinish == "RFn"] <- 2
train.prep$GarageFinish[train.prep$GarageFinish == 'Unf'] <- 1
train.prep$GarageFinish[is.na(train.prep$GarageFinish)] <- 1
train.prep$GarageFinish <- as.integer(train.prep$GarageFinish)

#unique(train.prep$GarageQual)
train.prep$GarageQual[train.prep$GarageQual == "Ex"] <- 5
train.prep$GarageQual[train.prep$GarageQual == "Gd"] <- 4
train.prep$GarageQual[train.prep$GarageQual == 'TA'] <- 3
train.prep$GarageQual[train.prep$GarageQual == 'Fa'] <- 2
train.prep$GarageQual[train.prep$GarageQual == 'Po'] <- 1
train.prep$GarageQual[is.na(train.prep$GarageQual)] <- 1
train.prep$GarageQual <- as.integer(train.prep$GarageQual)

#unique(train.prep$GarageCond)
train.prep$GarageCond[train.prep$GarageCond == "Ex"] <- 5
train.prep$GarageCond[train.prep$GarageCond == "Gd"] <- 4
train.prep$GarageCond[train.prep$GarageCond == 'TA'] <- 3
train.prep$GarageCond[train.prep$GarageCond == 'Fa'] <- 2
train.prep$GarageCond[train.prep$GarageCond == 'Po'] <- 1
train.prep$GarageCond[is.na(train.prep$GarageCond)] <- 1
train.prep$GarageCond <- as.integer(train.prep$GarageCond)

#unique(train.prep$PavedDrive)
train.prep$PavedDrive[train.prep$PavedDrive == "Y"] <- 2
train.prep$PavedDrive[train.prep$PavedDrive == "P"] <- 1
train.prep$PavedDrive[train.prep$PavedDrive == "N"] <- 1
train.prep$PavedDrive[is.na(train.prep$PavedDrive)] <- 1
train.prep$PavedDrive <- as.integer(train.prep$PavedDrive)

#unique(train.prep$Fence)
train.prep$Fence[train.prep$Fence == "GdPrv"] <- 2
train.prep$Fence[train.prep$Fence == "MnPrv"] <- 1
train.prep$Fence[train.prep$Fence == 'GdWo'] <- 1
train.prep$Fence[train.prep$Fence == "MnWw"] <- 1
train.prep$Fence[is.na(train.prep$Fence)] <- 1
train.prep$Fence <- as.integer(train.prep$Fence)

#unique(train.prep$MiscFeature)
train.prep$MiscFeature[train.prep$MiscFeature == "Elev"] <- 3
train.prep$MiscFeature[train.prep$MiscFeature == "Gar2"] <- 3
train.prep$MiscFeature[train.prep$MiscFeature == 'Othr'] <- 1
train.prep$MiscFeature[train.prep$MiscFeature == "Shed"] <- 1
train.prep$MiscFeature[train.prep$MiscFeature == "TenC"] <- 3
train.prep$MiscFeature[is.na(train.prep$MiscFeature)] <- 1
train.prep$MiscFeature <- as.integer(train.prep$MiscFeature)

#unique(train.prep$MoSold)
train.prep$MoSold[train.prep$MoSold %in% c(10,11,12)] <- 4  # seasonality
train.prep$MoSold[train.prep$MoSold %in% c(7,8,9)] <- 3
train.prep$MoSold[train.prep$MoSold %in% c(4,5,6)] <- 2
train.prep$MoSold[train.prep$MoSold %in% c(1,2,3)] <- 1
train.prep$MoSold[is.na(train.prep$MoSold)] <- 1
train.prep$MoSold <- as.integer(train.prep$MoSold)

#sum(is.na(train.prep))
#train.prep <- na.omit(train.prep)
