## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

library(ggplot2)
library(scales)
library(reshape2)
library(DMwR)
library(dummies)
library(e1071) 
library(MASS)
library(forecast)

# get the most frequently category value
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

train = read.csv("../input/train.csv",header = T)
test = read.csv("../input/test.csv",header = T)

# Overview of the data base
#summary(train)
#summary(test)

# Creating que variable on test data base
test$SalePrice <- NA
train <- train[train$GrLivArea < 4000,]


sale.base <- rbind(train,test)
#summary(sale.base)
# MSSubClass -> categorical variable with numerical values, nedd to be converted
# LotFrontage -> 259 NA's
# Street -> 99,9% of the values are Pave
# Alley -> 1369 values NA's
# MasVnrType -> 8 values NA's
# MasVnrArea -> 8 values NA's
# BsmtQual -> 37 values NA's
# BsmtCond -> 37 values NA's
# BsmtExposure -> 38 values NA's
# BsmtFinType1 -> 37 values NA's
# BsmtFinType2 -> 38 values NA's
# Electrical -> 1 value NA
# FireplaceQu -> 690 NA's
# GarageType -> 81 values NA's
# GarageYrBlt -> 81 values NA's
# GarageFinish -> 81 values NA's
# GarageQual -> 81 values NA's
# GarageCond -> 81 values NA's
# PoolQC -> 1453 values NA's
# Fence -> 1179 values NA's
# MiscFeature -> 1406 values NA's

# MSZoning -> 4
# LotFrontage -> 486
# Alley -> 2721
# Utilities -> 2
# Exterior1st -> 1
# Exterior2nd -> 1
# MasVnrType -> 24
# MasVnrArea -> 23
# BsmtQual -> 81
# BsmtCond -> 82
# BsmtExposure -> 82
# BsmtFinType1 -> 79
# BsmtFinSF1 -> 1
# BsmtFinType2 -> 80
# BsmtFinSF2 -> 1
# BsmtUnfSF -> 1
# TotalBsmtSF -> 1
# BsmtFullBath -> 2
# BsmtHalfBath -> 2
# KitchenQual -> 1
# Functional -> 2
# FireplaceQu -> 1420
# GarageType -> 157
# GarageYrBlt -> 159
# GarageFinish -> 159
# GarageCars -> 1
# GarageArea -> 1
# GarageQual -> 159
# GarageCond -> 159
# PoolQC -> 2909
# Fence -> 2348
# MiscFeature -> 2814
# SaleType -> 1


# Converting the variable MSSubClass
sale.base$MSSubClass <- as.factor(sale.base$MSSubClass)

# Replacing the NA value on Electrical
sale.base$Electrical[is.na(sale.base$Electrical)] <- "SBrkr"
#summary(sale.base$Electrical)


# Replacing the values NA's by None

levels(sale.base$Alley) <- c(levels(sale.base$Alley),"None")
levels(sale.base$PoolQC) <- c(levels(sale.base$PoolQC),"None")
levels(sale.base$Fence) <- c(levels(sale.base$Fence),"None")
levels(sale.base$MiscFeature) <- c(levels(sale.base$MiscFeature),"None")
levels(sale.base$FireplaceQu) <- c(levels(sale.base$FireplaceQu),"None")
levels(sale.base$BsmtFinType2) <- c(levels(sale.base$BsmtFinType2),"None")
levels(sale.base$BsmtFinType1) <- c(levels(sale.base$BsmtFinType1),"None")
levels(sale.base$BsmtQual) <- c(levels(sale.base$BsmtQual),"None")
levels(sale.base$BsmtCond) <- c(levels(sale.base$BsmtCond),"None")
levels(sale.base$BsmtExposure) <- c(levels(sale.base$BsmtExposure),"None")
levels(sale.base$GarageType) <- c(levels(sale.base$GarageType),"None")
levels(sale.base$GarageFinish) <- c(levels(sale.base$GarageFinish),"None")
levels(sale.base$GarageQual) <- c(levels(sale.base$GarageQual),"None")
levels(sale.base$GarageCond) <- c(levels(sale.base$GarageCond),"None")


sale.base$Alley[is.na(sale.base$Alley)] <- "None"
sale.base$PoolQC[is.na(sale.base$PoolQC)] <- "None"
sale.base$Fence[is.na(sale.base$Fence)] <- "None"
sale.base$MiscFeature[is.na(sale.base$MiscFeature)] <- "None"
sale.base$FireplaceQu[is.na(sale.base$FireplaceQu)] <- "None"
sale.base$BsmtFinType2[is.na(sale.base$BsmtFinType2)] <- "None"
sale.base$BsmtFinType1[is.na(sale.base$BsmtFinType1)] <- "None"
sale.base$BsmtQual[is.na(sale.base$BsmtQual)] <- "None"
sale.base$BsmtCond[is.na(sale.base$BsmtCond)] <- "None"
sale.base$BsmtExposure[is.na(sale.base$BsmtExposure)] <- "None"
sale.base$GarageType[is.na(sale.base$GarageType)] <- "None"
sale.base$GarageFinish[is.na(sale.base$GarageFinish)] <- "None"
sale.base$GarageQual[is.na(sale.base$GarageQual)] <- "None"
sale.base$GarageCond[is.na(sale.base$GarageCond)] <- "None"

sale.base$MSZoning[is.na(sale.base$MSZoning)] <- "RL"
sale.base$Exterior1st[is.na(sale.base$Exterior1st)] <- "VinylSd"
sale.base$Exterior2nd[is.na(sale.base$Exterior2nd)] <- "VinylSd"
sale.base$BsmtFinSF1[is.na(sale.base$BsmtFinSF1)] <- mean(sale.base$BsmtFinSF1[!is.na(sale.base$BsmtFinSF1)])
sale.base$BsmtUnfSF[is.na(sale.base$BsmtUnfSF)] <- mean(sale.base$BsmtUnfSF[!is.na(sale.base$BsmtUnfSF)])
sale.base$TotalBsmtSF[is.na(sale.base$TotalBsmtSF)] <- mean(sale.base$TotalBsmtSF[!is.na(sale.base$TotalBsmtSF)])
sale.base$BsmtFullBath[is.na(sale.base$BsmtFullBath)] <- getmode(sale.base$BsmtFullBath)
sale.base$BsmtHalfBath[is.na(sale.base$BsmtHalfBath)] <- getmode(sale.base$BsmtHalfBath)
sale.base$KitchenQual[is.na(sale.base$KitchenQual)] <- getmode(sale.base$KitchenQual)
sale.base$Functional[is.na(sale.base$Functional)] <- getmode(sale.base$Functional)
sale.base$GarageCars[is.na(sale.base$GarageCars)] <- getmode(sale.base$GarageCars)
sale.base$GarageArea[is.na(sale.base$GarageArea)] <- mean(sale.base$GarageArea[!is.na(sale.base$GarageArea)])
sale.base$SaleType[is.na(sale.base$SaleType)] <- getmode(sale.base$SaleType)

#summary(sale.base)
# Exterior1st -> 1
# Exterior2nd -> 1
# BsmtFinSF1 -> 1
# BsmtUnfSF -> 1
# TotalBsmtSF -> 1
# BsmtFullBath -> 2
# BsmtHalfBath -> 2
# KitchenQual -> 1
# Functional -> 2
# GarageCars -> 1
# GarageArea -> 1
# SaleType -> 1

#summary(sale.base$LotFrontage)
#summary(sale.base$MasVnrArea)
#summary(sale.base$GarageYrBlt)

ttrain <- sale.base[!is.na(sale.base$LotFrontage),]
ttrain <- ttrain[!is.na(ttrain$MasVnrArea),]
ttrain <- ttrain[!is.na(ttrain$GarageYrBlt),]
ttrain <- ttrain[!is.na(ttrain$SalePrice),]
  
num.col.names <- names(sale.base)[which(sapply(sale.base, is.numeric))]

corr.train <- round(cor(ttrain[,num.col.names]),2)
#head(corr.train)

melt.corr.base <- melt(corr.train)
#head(melt.corr.base)
#ggplot(data = melt.corr.base, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25))+
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                       midpoint = 0, limit = c(-1,1), space = "Lab")

# Identificando os atributos numéricos com maior correlação com LotFrontage
# TotalBsmtSF,GrLivArea,LotArea,X1stFlrSF
temp <- melt.corr.base[melt.corr.base$Var1 == "LotFrontage",]
temp <- temp[order(abs(temp$value)),]


#ggplot(sale.base, aes(x=LotFrontage,y=TotalBsmtSF))+geom_point()
#ggplot(sale.base, aes(x=LotFrontage,y=GrLivArea))+geom_point()
#ggplot(sale.base, aes(x=LotFrontage,y=LotArea))+geom_point()
#ggplot(sale.base, aes(x=LotFrontage,y=X1stFlrSF))+geom_point()

# Linear Regression to predict the missing values for LotFrontage
linearmodel = lm(LotFrontage ~ TotalBsmtSF+GrLivArea+LotArea+X1stFlrSF, sale.base)
#summary(linearmodel)

predict.lotfrontage <- predict(linearmodel,sale.base[is.na(sale.base$LotFrontage),])
#summary(predict.lotfrontage)
#summary(train$LotFrontage)

na.lotfrontage <- sale.base[is.na(sale.base$LotFrontage),]
na.lotfrontage$LotFrontage <- predict.lotfrontage
#summary(sale.base$LotFrontage[is.na(train$LotFrontage)])
sale.base$LotFrontage[is.na(sale.base$LotFrontage)] <- predict.lotfrontage
#summary(sale.base$LotFrontage)

# Linear Regression to predict the missing values for MasVnrArea
temp <- melt.corr.base[melt.corr.base$Var1 == "MasVnrArea",]
temp <- temp[order(abs(temp$value)),]
linearmodel = lm(MasVnrArea ~ SalePrice+OverallQual+GrLivArea+GarageCars+TotalBsmtSF+X1stFlrSF, sale.base)

predict.MasVnrArea <- predict(linearmodel,sale.base[is.na(sale.base$MasVnrArea),])
sale.base$MasVnrArea[is.na(sale.base$MasVnrArea)] <- predict.MasVnrArea
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   108.5   170.0  1600.0
#summary(sale.base$MasVnrArea)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   104.1   166.0  1600.0 

# Linear Regression to predict the missing values for GarageYrBlt
temp <- melt.corr.base[melt.corr.base$Var1 == "GarageYrBlt",]
temp <- temp[order(abs(temp$value)),]
linearmodel = lm(GarageYrBlt ~ YearBuilt+YearRemodAdd+GarageCars+GarageArea+OverallQual+FullBath, sale.base)
predict.GarageYrBlt <- predict(linearmodel,sale.base[is.na(sale.base$GarageYrBlt),])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1900    1961    1980    1979    2002    2010      81
sale.base$GarageYrBlt[is.na(sale.base$GarageYrBlt)] <- predict.GarageYrBlt
#summary(sale.base$GarageYrBlt)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1900    1958    1978    1977    2001    2010 

corr.train <- round(cor(sale.base[!is.na(sale.base$SalePrice),num.col.names]),2)
#head(corr.train)

melt.corr.base <- melt(corr.train)
#head(melt.corr.base)
#ggplot(data = melt.corr.base, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25))+
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                       midpoint = 0, limit = c(-1,1), space = "Lab")


# Analysis the variable SalePrice
ggplot(data=train, aes(x=train$SalePrice)) + geom_histogram()+scale_x_continuous(labels=dollar)
# Need to be converted to normal distribution
sale.base$log.saleprice <- NA
sale.base$log.saleprice <- log(sale.base$SalePrice)
#ggplot(data=train, aes(x=sale.base$log.saleprice, fill = ..count..)) + 
#  geom_histogram() + scale_x_continuous(labels=dollar)

# Looking for Outliers as sugested bu author
# Remove the instances with GrLivArea > 4000
# Referência: http://jse.amstat.org/v19n3/decock.pdf
#ggplot(train, aes(x=GrLivArea, y=SalePrice)) +
#  geom_point(size=2, shape=23)

#ggplot(sale.base, aes(x=GrLivArea, y=log.saleprice)) +
#  geom_point(size=2, shape=23)

#sale.base <- sale.base[train$GrLivArea < 4000,]

#summary(sale.base)
# Separating categorial and numerical variables
num.col.names <- names(train)[which(sapply(train, is.numeric))]
num.col.names <- c(num.col.names,"log.saleprice")
cha.col.names <- colnames(train)[! colnames(train) %in% num.col.names]

# Creating a pdf to save grafics
# pdf("categ_saleprice.pdf",onefile = TRUE)

#for(i in cha.col.names){
#  if(i %in% colnames(train)){
#    
#    tplot <- ggplot(train, aes(x = train[,i], y = train$SalePrice)) + 
#      geom_bar(stat = "summary",fill="steelblue",fun.y = "mean")+
#      ggtitle(i) + # for the main title
#      scale_y_continuous(labels=dollar)+
#      ylab("")+
#      xlab("Classes")+ theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25))
#    print(tplot)
#  }
#}
# dev.off()

#pdf("numeric_saleprice.pdf",onefile = TRUE)
#
#for(i in num.col.names){
#  if(i %in% colnames(train)){
#    #i <- num.col.names[2]
#    tplot <- ggplot(train,aes_string(i,"SalePrice"))+geom_point()+geom_smooth(method = lm)+
#      ggtitle(i)+
#      scale_y_continuous(labels=dollar)+
#      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25))
#    print(tplot)
#  }
#}
#dev.off()

# Correlation  Matrix
ttrain <- sale.base[!is.na(sale.base$log.saleprice),]
num.col.names <- num.col.names[!num.col.names %in% c("MSSubClass")]
corr.train <- round(cor(ttrain[,num.col.names]),2)
#head(corr.train)

melt.corr.base <- melt(corr.train)
#head(melt.corr.base)
#ggplot(data = melt.corr.base, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25))+
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                       midpoint = 0, limit = c(-1,1), space = "Lab")


# Considering the correlations bigger the 0.5

colnames(melt.corr.base)
corr.subset <- subset(melt.corr.base, abs(value) > 0.5)
# ou 
# library(dplyr)
# filter(corr.subset, value > 0.5)

#summary(corr.subset)
corr.subset <- corr.subset[corr.subset$Var1 != corr.subset$Var2,]
uniq.subset <- c(as.character(corr.subset$Var1),as.character(corr.subset$Var2))

# Estes são os atributos numéricos que inicialmente irão fazer parte da modelagem
# Há possibilidade de mudanças dos atributos selecionados
uniq.num.subset <- unique(uniq.subset)



# Análise de atributos categóricos MSSubClass, BldgType e HouseStyle porque possuem categorias 
# com valores semelhantes
#summary(train)
#ggplot(train, aes(x=MSSubClass))+geom_bar(aes(fill=BldgType))
#ggplot(train, aes(x=BldgType))+geom_bar(aes(fill=MSSubClass))
#ggplot(train, aes(x=BldgType))+geom_bar(aes(fill=HouseStyle))
#ggplot(train, aes(x=HouseStyle))+geom_bar(aes(fill=BldgType))
#ggplot(train, aes(x=HouseStyle))+geom_bar(aes(fill=MSSubClass))
#ggplot(train, aes(x=MSSubClass))+geom_bar(aes(fill=HouseStyle))



dim(sale.base[sale.base$BldgType == '1Fam' & sale.base$MSSubClass == 120,])

# Converting the variable  MSSubClass, BldgType and HouseStyle into only variable  new.dwelling
sale.base$new.dwelling <- ifelse(sale.base$BldgType== "1Fam" & 
                                   sale.base$MSSubClass %in% c(20,30,40,45,50,60,70,75,80,85), "1Fam", 
                             ifelse(sale.base$BldgType== "Duplex", "Duplex", "Twnhs"))
sale.base$new.dwelling <- as.factor(sale.base$new.dwelling)
#summary(sale.base$new.dwelling)

# 3 categories for lotshape
sale.base$new.lotshape <- ifelse(sale.base$LotShape== "Reg","Reg","IR")
sale.base$new.lotshape <- as.factor(sale.base$new.lotshape)

# 3 categories for lotconfig
sale.base$new.lotconfig <- ifelse(sale.base$LotConfig == "Inside","Inside",
                              ifelse(sale.base$LotConfig == "Corner","Conner","FR"))
sale.base$new.lotconfig <- as.factor(sale.base$new.lotconfig)

# 2 categories for landslope
sale.base$new.landslope <- ifelse(sale.base$LandSlope == "Gtl","Gtl","Sev")
sale.base$new.landslope <- as.factor(sale.base$new.lotconfig)

# 3 categories for MasVnrType
sale.base$new.masvnrtype <- ifelse(sale.base$MasVnrType =="BrkFace","BrkFace",
                               ifelse(sale.base$MasVnrType == "Stone","Stone","None"))
sale.base$new.masvnrtype <- as.factor(sale.base$new.masvnrtype)

# 2 categories for Foundation
sale.base$new.foundation <- ifelse(sale.base$Foundation == "PConc","PConc","Other")
sale.base$new.foundation <- as.factor(sale.base$new.foundation)

# 2 categories for heating
sale.base$new.heating <- ifelse(sale.base$Heating == "GasA" | sale.base$Heating == "GasW","Gas","Other")
sale.base$new.heating <- as.factor(sale.base$new.heating)

# 2 categories for Electrical
sale.base$new.electrical <- ifelse(sale.base$Electrical == "SBrkr","SBrkr","Other")
sale.base$new.electrical <- as.factor(sale.base$new.electrical)


# 2 categories for Functional
sale.base$new.functional <- ifelse(sale.base$Functional == "Typ","Typ","Other")
sale.base$new.functional <- as.factor(sale.base$new.functional)

# 2 categories for garagetype
sale.base$new.garagetype <- ifelse(sale.base$GarageType == "Attchd","Attchd","Other")
sale.base$new.garagetype <- as.factor(sale.base$new.garagetype)

# 2 categories for garagetype
sale.base$new.garagefin <- ifelse(sale.base$GarageFinish == "Fin","Fin","Other")
sale.base$new.garagefin <- as.factor(sale.base$new.garagefin)

# 2 categories for garagequal
sale.base$new.garagequal <- ifelse(sale.base$GarageQual == "TA","TA","Other")
sale.base$new.garagequal <- as.factor(sale.base$new.garagequal)

# 2 categories for garagcond
sale.base$new.garagcond <- ifelse(sale.base$GarageCond == "TA","TA","Other")
sale.base$new.garagcond <- as.factor(sale.base$new.garagcond)


# 2 categories for pavedrive
sale.base$new.pavedrive<- ifelse(sale.base$PavedDrive == "Y","Y","Other")
sale.base$new.pavedrive <- as.factor(sale.base$new.pavedrive)

# Creating the Dummy variables

#Remove variables
rem.char <- c("MSSubClass","Street","BldgType","HouseStyle","Alley","Utilities","RoofMatl",
              "PoolQC","PoolArea","Fence","MiscFeature","BldgType","LotShape","LotConfig","LandSlope","MasVnrType","Foundation","Heating","Electrical","Functional",
              "GarageType","GarageFinish","GarageQual","GarageCond","PavedDrive")

cha.col.names <- colnames(train)[! colnames(train) %in% num.col.names]
cha.col.names <- cha.col.names[! cha.col.names %in% rem.char]

final.base <- data.frame()
final.base <- dummy(sale.base[,cha.col.names[1]], sep = paste(cha.col.names[1],"_"))
uniq.num.subset <- c(uniq.num.subset,"log.saleprice")
for(i in cha.col.names[2:length(cha.col.names)]){
  final.base <- cbind(final.base, dummy(sale.base[,i], sep = paste(i,"_")))
}
for(i in uniq.num.subset){
  final.base <- cbind(final.base, sale.base[,i])
}
dim(final.base)

colnames(final.base) <- c(colnames(final.base)[1:158],uniq.num.subset)
final.base <- data.frame(final.base)

test <- final.base[is.na(final.base$log.saleprice),]
final.base <- final.base[!is.na(final.base$log.saleprice),]
colnames(final.base)[colSums(is.na(final.base)) > 0]

set.seed(123)
samples <- sample(c(1:dim(final.base)[1]), dim(final.base)[1]*0.8)
final.base.train = final.base[samples,]
final.base.test <- final.base[-samples,]

# Training linear regression model
linearmodel <- lm(log.saleprice~.-SalePrice, data = final.base.train)
summary(linearmodel)
predicts1 <- predict(linearmodel,final.base.test,type = "response")
residuals <- final.base.test$log.saleprice - predicts1

linreg_pred <- data.frame("Predicted" = predicts1, "Actual" = final.base.test$log.saleprice, "Residual" = residuals)
accuracy(predicts1, final.base.test$log.saleprice)


# Test base to subimt on kaggle
predicts2 <- predict(linearmodel,test,type = "response")
test.saleprice <- exp(predicts2)


ttest = read.csv("../input/test.csv",header = T)

submit <- data.frame(Id=ttest$Id,SalePrice=test.saleprice)
#submit <- submit[,c("Id","SalePrice")]

# write.csv(submit, file = "../input/house_prices2.csv",row.names=FALSE)
# t <- read.csv("../input/house_prices2.csv",header = T)