library(mltools)
library(data.table)
library(caret)
data_frame_house_price <- read.csv("D:/AreeshThings/train.csv", header = TRUE)
View(data_frame_house_price)
str(data_frame_house_price)

#Finding NA values

is.na(data_frame_house_price)

#Replacing NA values

data_frame_house_price$MiscFeature[is.na(data_frame_house_price$MiscFeature)] <- "None"
data_frame_house_price$Alley[is.na(data_frame_house_price$Alley)] <- "No alley access"
data_frame_house_price$PoolQC[is.na(data_frame_house_price$PoolQC)] <- "No Pool"
data_frame_house_price$Fence[is.na(data_frame_house_price$Fence)] <- "No Fence"
data_frame_house_price$FireplaceQu[is.na(data_frame_house_price$FireplaceQu)] <- "No Fireplace"
data_frame_house_price$GarageFinish[is.na(data_frame_house_price$GarageFinish)] <- "No Garage"
data_frame_house_price$GarageQual[is.na(data_frame_house_price$GarageQual)] <- "No Garage"
data_frame_house_price$GarageType[is.na(data_frame_house_price$GarageType)] <- "No Garage"
data_frame_house_price$BsmtQual[is.na(data_frame_house_price$BsmtQual)] <- "No Basement"
data_frame_house_price$GarageCond[is.na(data_frame_house_price$GarageCond)] <- "No Garage"
data_frame_house_price$BsmtCond[is.na(data_frame_house_price$BsmtCond)] <- "No Basement"
data_frame_house_price$BsmtFinType2[is.na(data_frame_house_price$BsmtFinType2)] <- "No Basement"
data_frame_house_price$BsmtExposure[is.na(data_frame_house_price$BsmtExposure)] <- "No Basement"
data_frame_house_price$BsmtFinType1[is.na(data_frame_house_price$BsmtFinType1)] <- "No Basement"
data_frame_house_price$LotFrontage[is.na(data_frame_house_price$LotFrontage)] <- 0
view(data_frame_house_price)



#Checking to verify any na values
is.na(data_frame_house_price)

#Working on the data.

#Grouping for House Location & Size
lm_location <- lm(SalePrice ~ Utilities+Neighborhood+Street+LotArea+LotFrontage+Alley+LotShape+LotConfig+HouseStyle+LandSlope, data = data_frame_house_price)
summary(lm_location)

vif(lm_location)

lm_house_quality <- lm(SalePrice ~ LotArea + Neighborhood + LotFrontage + LotShape + HouseStyle, data = data_frame_house_price)
summary(lm_house_quality)

#Grouping for Overall Work & Year
lm_quality_year <- lm(SalePrice ~ OverallCond + OverallQual + YearBuilt + YearRemodAdd, data = data_frame_house_price)
summary(lm_quality_year)

vif(lm_quality_year)

lm_quality_year <- lm(SalePrice ~ OverallQual + YearBuilt, data = data_frame_house_price)
summary(lm_quality_year)

#Grouping for Basement & House Remodeling
lm_house_remodel <- lm(SalePrice ~ BsmtQual + BsmtCond + BsmtExposure + TotalBsmtSF + OverallQual + YearBuilt + YearRemodAdd, data = data_frame_house_price)
summary(lm_house_remodel)

vif(lm_house_remodel)

lm_house_remodel <- lm(SalePrice ~ BsmtExposure + TotalBsmtSF + YearRemodAdd, data = data_frame_house_price)
summary(lm_house_remodel)


#Grouping for House Information
lm_house <- lm(SalePrice ~ HouseStyle + YearBuilt + YearRemodAdd + RoofStyle + MasVnrArea + Foundation + TotalBsmtSF + Fireplaces + GarageArea + MiscFeature, data = data_frame_house_price)
summary(lm_house)

vif(lm_house)

lm_house<- lm(SalePrice ~  YrSold + Neighborhood + YearRemodAdd  + MasVnrArea + TotalBsmtSF + Fireplaces + GarageArea + OverallQual + ExterQual , data = data_frame_house_price )
summary(lm_house)

#Grouping with rooms and neighborhood

lm_rneighborhood <- lm(SalePrice ~ LowQualFinSF + GrLivArea  + FullBath + HalfBath + BsmtFullBath + BedroomAbvGr  + Functional + TotRmsAbvGrd + Neighborhood, data = data_frame_house_price)
summary(lm_rneighborhood)

vif(lm_rneighborhood)

lm_rneighborhood <- lm(SalePrice ~ GrLivArea + BsmtFullBath + BedroomAbvGr + TotRmsAbvGrd + Functional + Neighborhood, data = data_frame_house_price)
summary(lm_rneighborhood)

#Using previous variables.

lm_houseinfo <- lm(SalePrice ~  Neighborhood + YearRemodAdd + TotalBsmtSF + Fireplaces + OverallQual + ExterQual + GrLivArea   , data = data_frame_house_price )
summary(lm_houseinfo)

vif(lm_houseinfo)

#Now we got the r square value of 0.8149 & Now checking for outliers.
ols_plot_resid_fit(lm_houseinfo)
ols_plot_resid_stud(lm_houseinfo)

#Know we will remove the extreme outliers 524

m_property_info <- lm(SalePrice ~  Neighborhood + YearRemodAdd + TotalBsmtSF + Fireplaces + OverallQual + ExterQual + GrLivArea   , data = data_frame_house_price[-c(524), ] )
summary(lm_houseinfo)

#As removing the outlier doesn't change the r square value so we won't remove any outlier we will accept the r square.