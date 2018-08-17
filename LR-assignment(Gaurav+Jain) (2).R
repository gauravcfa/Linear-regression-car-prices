setwd("C:/Users/user/Desktop/Data Science/Module 3/Regression/case study")

#######-Packages-########
library(tidyr)
library(dplyr)
library(ggplot2)
library(car)
library(MASS)
library(dplyr)

#######-Download Data-#######
carprice<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)
View(carprice)
str(carprice)

#######-Data Prepration-#######
#seperate company and model name
#model name in rows 139 and 142 is missing but we don't need this columns so no further action was taken
carprice_1<-separate(carprice,CarName, into = c("car_company","car_model"),sep = " ",remove = FALSE)
carprice_1$car_company<-ifelse(carprice_1$car_company=="maxda","mazda",carprice_1$car_company)
carprice_1$car_company<-ifelse(carprice_1$car_company=="nissan","Nissan",carprice_1$car_company)
carprice_1$car_company<-ifelse(carprice_1$car_company=="porcshce","porsche",carprice_1$car_company)
carprice_1$car_company<-ifelse(carprice_1$car_company=="toyouta","toyota",carprice_1$car_company)
carprice_1$car_company<-ifelse(carprice_1$car_company=="vokswagen" |carprice_1$car_company=="volkswagen","vw",carprice_1$car_company)
uni<-unique(carprice_1$car_company)
#check for duplicate rows-no duplicate rows were spotted
carprice_2<-unique(carprice_1)
#check for null values
#only car_model has NA values. 
sapply(carprice_2, function(x) sum(is.na(x)))
#######-check dependent variable
#price
hist(carprice_2$price)
quantile(carprice_2$price, seq(0,1,0.01))
plot(quantile(carprice_2$price, seq(0,1,0.01)))

#######-check numerical values
#wheelbase
str(carprice_2)
hist(carprice_2$wheelbase)
quantile(carprice_2$wheelbase, seq(0,1,0.01))
plot(quantile(carprice_2$wheelbase, seq(0,1,0.01)))
carprice_2$wheelbase[which(carprice_2$wheelbase>115.544)]<-115.544 # remove outlier
#carlength
hist(carprice_2$carlength)
quantile(carprice_2$wheelbase, seq(0,1,0.01))
plot(quantile(carprice_2$wheelbase, seq(0,1,0.01))) # bump up around 96%th percentile but don't think that's an outlier
#carwidth
hist(carprice_2$carwidth)
quantile(carprice_2$carwidth, seq(0,1,0.01))
plot(quantile(carprice_2$carwidth, seq(0,1,0.01)))
carprice_2$carwidth[which(carprice_2$carwidth<62.536)]<-62.536 # remove outlier on lower end
#carheigth
hist(carprice_2$carheight)
quantile(carprice_2$carheight, seq(0,1,0.01))
plot(quantile(carprice_2$carheight, seq(0,1,0.01)))
carprice_2$carheight[which(carprice_2$carheight<48.824)]<-48.824 # remove outlier on lower end
carprice_2$carheight[which(carprice_2$carheight>57.500)]<-57.500 # remove outlier on higher end
#curbweight
hist(carprice_2$curbweight)
quantile(carprice_2$curbweight, seq(0,1,0.01))
plot(quantile(carprice_2$curbweight, seq(0,1,0.01)))
carprice_2$curbweight[which(carprice_2$curbweight<1819.72)]<-1819.72 # remove outlier on lower end
#enginesize
hist(carprice_2$enginesize)
quantile(carprice_2$enginesize, seq(0,1,0.01))
plot(quantile(carprice_2$enginesize, seq(0,1,0.01)))
carprice_2$enginesize[which(carprice_2$enginesize<79.08)]<-79.08 # remove outlier on lower end
carprice_2$enginesize[which(carprice_2$enginesize>209.00)]<-209.00 # remove outlier on higher end
#boreratio
hist(carprice_2$boreratio)
quantile(carprice_2$boreratio, seq(0,1,0.01))
plot(quantile(carprice_2$boreratio, seq(0,1,0.01)))
carprice_2$boreratio[which(carprice_2$boreratio<2.9100)]<-2.9100 # remove outlier on lower end
carprice_2$boreratio[which(carprice_2$boreratio>3.8000)]<-3.8000 # remove outlier on higher end
#stroke
hist(carprice_2$stroke)
quantile(carprice_2$stroke, seq(0,1,0.01))
plot(quantile(carprice_2$stroke, seq(0,1,0.01)))
carprice_2$stroke[which(carprice_2$stroke>3.9000)]<-3.9000 # remove outlier on higher end
#compression ratio
hist(carprice_2$compressionratio)
quantile(carprice_2$compressionratio, seq(0,1,0.01))
plot(quantile(carprice_2$compressionratio, seq(0,1,0.01)))
carprice_2$compressionratio[which(carprice_2$compressionratio>10.9400)]<-10.9400 # remove outlier on higher end
#horsepower
hist(carprice_2$horsepower)
quantile(carprice_2$horsepower, seq(0,1,0.01))
plot(quantile(carprice_2$horsepower, seq(0,1,0.01)))
carprice_2$horsepower[which(carprice_2$horsepower>207.00)]<-207.00 # remove outlier on higher end
#peakrpm
hist(carprice_2$peakrpm)
quantile(carprice_2$peakrpm, seq(0,1,0.01))
plot(quantile(carprice_2$peakrpm, seq(0,1,0.01)))
carprice_2$peakrpm[which(carprice_2$peakrpm>6000)]<-6000 # remove outlier on higher end
#citympg
hist(carprice_2$citympg)
quantile(carprice_2$citympg, seq(0,1,0.01))
plot(quantile(carprice_2$citympg, seq(0,1,0.01)))
carprice_2$citympg[which(carprice_2$citympg>38.00)]<-38.00 # remove outlier on higher end
#highwaympg
hist(carprice_2$highwaympg)
quantile(carprice_2$highwaympg, seq(0,1,0.01))
plot(quantile(carprice_2$highwaympg, seq(0,1,0.01)))
carprice_2$highwaympg[which(carprice_2$highwaympg>49.88)]<-49.88 # remove outlier on higher end

#######-dummy variables
#-symboling
hist(carprice_2$symboling)
carprice_2$symboling_f<-factor(carprice_2$symboling)
levels(carprice_2$symboling_f)
levels(carprice_2$symboling_f)[1:5]<-"moderate_risk_insurance"
levels(carprice_2$symboling_f)[2]<-"risky_insurance"
levels(carprice_2$symboling_f)<-c(0,1)
carprice_2$symboling_f<-as.numeric(levels(carprice_2$symboling_f))[carprice_2$symboling_f]
check<-data.frame(carprice_2$symboling,carprice_2$symboling_f) # check if dummy variable was created properly or not
#-car_company
ggplot(carprice_2,aes(carprice_2$car_company))+geom_bar()
table(carprice_2$car_company)
carprice_2$car_company<-factor(carprice_2$car_company)
dummy_car<-data.frame(model.matrix(~car_company,data = carprice_2))
dummy_car<-dummy_car[,-1]
carprice_2<-cbind(carprice_2[,-2],dummy_car)
#fuel type
ggplot(carprice_2,aes(carprice_2$fueltype))+geom_bar()
table(carprice_2$fueltype)
carprice_2$fueltype_f<-factor(carprice_2$fueltype)
levels(carprice_2$fueltype_f)<-c(0,1) # gas is 1 and Diesel is 0
carprice_2$fueltype_n<-as.numeric(levels(carprice_2$fueltype_f))[carprice_2$fueltype_f]
check_ft<-data.frame(carprice_2$fueltype,carprice_2$fueltype_n) # check if dummy variable was created properly or not
carprice_2<-carprice_2[,-c(5,50)]
#aspiration
ggplot(carprice_2,aes(carprice_2$aspiration))+geom_bar()
table(carprice_2$aspiration)
carprice_2$aspiration_f<-factor(carprice_2$aspiration)
levels(carprice_2$aspiration_f)<-c(0,1) # turbo is 1 and Std is 0
carprice_2$aspiration_n<-as.numeric(levels(carprice_2$aspiration_f))[carprice_2$aspiration_f]
check_asp<-data.frame(carprice_2$aspiration,carprice_2$aspiration_n) # check if dummy variable was created properly or not
table(carprice_2$aspiration,carprice_2$aspiration_n)
str(carprice_2)
carprice_2<-carprice_2[,-c(5,50)]
#door number
ggplot(carprice_2,aes(carprice_2$doornumber))+geom_bar()
table(carprice_2$doornumber)
carprice_2$doornumber_f<-factor(carprice_2$doornumber)
levels(carprice_2$doornumber_f)<-c(0,1) # two is 1 and four is 0
carprice_2$doornumber_n<-as.numeric(levels(carprice_2$doornumber_f))[carprice_2$doornumber_f]
check_door<-data.frame(carprice_2$doornumber,carprice_2$doornumber_n) # check if dummy variable was created properly or not
table(carprice_2$doornumber,carprice_2$doornumber_n)
str(carprice_2)
carprice_2<-carprice_2[,-c(5,50)]
#carbody-more than two categories
ggplot(carprice_2,aes(carprice_2$carbody))+geom_bar()
table(carprice_2$carbody) # five categories-convertible, hardtop,hatchback, sedan,wagon
carprice_2$carbody<-factor(carprice_2$carbody)
dummy_carbody<-data.frame(model.matrix(~carbody,data = carprice_2))
dummy_carbody<-dummy_carbody[,-1]
str(carprice_2)
carprice_2<-cbind(carprice_2[,-5],dummy_carbody)
#drivewheel-more than two categories
ggplot(carprice_2,aes(carprice_2$drivewheel))+geom_bar()
table(carprice_2$drivewheel) # three categories-4wd,fwd,rwd
carprice_2$drivewheel<-factor(carprice_2$drivewheel)
dummy_dw<-data.frame(model.matrix(~drivewheel,data = carprice_2))
dummy_dw<-dummy_dw[,-1]
str(carprice_2)
carprice_2<-cbind(carprice_2[,-5],dummy_dw)
#enginelocation-only two categories
ggplot(carprice_2,aes(carprice_2$enginelocation))+geom_bar()
table(carprice_2$enginelocation)
carprice_2$enginelocation_f<-factor(carprice_2$enginelocation)
levels(carprice_2$enginelocation_f)<-c(0,1) # rear is 1 and front is 0
carprice_2$enginelocation_n<-as.numeric(levels(carprice_2$enginelocation_f))[carprice_2$enginelocation_f]
check_el<-data.frame(carprice_2$enginelocation,carprice_2$enginelocation_n) # check if dummy variable was created properly or not
table(carprice_2$enginelocation,carprice_2$enginelocation_n)
str(carprice_2)
carprice_2<-carprice_2[,-c(5,54)]
#enginetype-more than two categories
ggplot(carprice_2,aes(carprice_2$enginetype))+geom_bar()
table(carprice_2$enginetype) # seven categories-dohc,dohcv,l,ohc,ohcv,ohcf,rotor
carprice_2$enginetype<-factor(carprice_2$enginetype)
dummy_et<-data.frame(model.matrix(~enginetype,data = carprice_2))
dummy_et<-dummy_et[,-1]
str(carprice_2)
carprice_2<-cbind(carprice_2[,-10],dummy_et)
#cylindernumber-more than two categories
ggplot(carprice_2,aes(carprice_2$cylindernumber))+geom_bar()
table(carprice_2$cylindernumber) # seven categories-8,5,4,6,3,12,2
carprice_2$cylindernumber<-factor(carprice_2$cylindernumber)
dummy_cn<-data.frame(model.matrix(~cylindernumber,data = carprice_2))
dummy_cn<-dummy_cn[,-1]
str(carprice_2)
carprice_2<-cbind(carprice_2[,-10],dummy_cn)
#fuelsystem-more than two categories
ggplot(carprice_2,aes(carprice_2$fuelsystem))+geom_bar()
table(carprice_2$fuelsystem) # eight categories-1bbl,2bbl,4bbl,idi,mfi,spdi,spfi
carprice_2$fuelsystem<-factor(carprice_2$fuelsystem)
dummy_fs<-data.frame(model.matrix(~fuelsystem,data = carprice_2))
dummy_fs<-dummy_fs[,-1]
str(carprice_2)
carprice_2<-cbind(carprice_2[,-11],dummy_fs)

#######-remove unwanted columns
carprice_2<-carprice_2[,-c(1:4)] # remove original symbolising variable because it can't be used in model
#######-adding new variable
#torque
carprice_2$torque<-(carprice_2$horsepower*5252)/carprice_2$peakrpm
hist(carprice_2$torque)
quantile(carprice_2$torque, seq(0,1,0.01))
plot(quantile(carprice_2$torque, seq(0,1,0.01)))

#######-check the final dataframe
str(carprice_2)
final_check<-data.frame(sapply(carprice_2,function(x) sum(is.na(x))))
#######-create training and test dataset
trainindices= sample(1:nrow(carprice_2), 0.7*nrow(carprice_2))
train = carprice_2[trainindices,]
test = carprice_2[-trainindices,]
#######-build model
set.seed(100)
model_1<-lm(price~.,data = train)
summary(model_1)
library(MASS)
step<-stepAIC(model_1,direction = "both")
step
model_2<-lm(formula = price ~ carlength + carwidth + carheight + curbweight + 
              stroke + car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymercury + car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault + 
              car_companysaab + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginelocation_n + 
              enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix + torque, data = train)
summary(model_2)
library(car)
vif(model_2)
#p<-cbind(summary(model_2)$coefficients,data.frame(summary(model_2)$coefficients[,4]))
v<-data.frame(vif(model_2))
##-torque has high VIF and is not significant so removing the same
model_3<-lm(formula = price ~ carlength + carwidth + carheight + curbweight + 
              stroke + car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymercury + car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault + 
              car_companysaab + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginelocation_n + 
              enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_3)
v<-data.frame(vif(model_3))
##-little impact or adjusted R square + carheight has high VIF and is not significant so removing the same
model_4<-lm(formula = price ~ carlength + carwidth + curbweight + 
              stroke + car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymercury + car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault + 
              car_companysaab + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginelocation_n + 
              enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_4)
v<-data.frame(vif(model_4))
##-little impact or adjusted R square + drivewheelrwd has high VIF and is not significant so removing the same
model_5<-lm(formula = price ~ carlength + carwidth + curbweight + 
              stroke + car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymercury + car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault + 
              car_companysaab + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginelocation_n + 
              enginetypeohc + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_5)
v<-data.frame(vif(model_5))

##-little impact or adjusted R square + enginetypeohc has high VIF and is not significant so removing the same
model_6<-lm(formula = price ~ carlength + carwidth + curbweight + 
              stroke + car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymercury + car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault + 
              car_companysaab + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginelocation_n + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_6)
v<-data.frame(vif(model_6))
##-little impact or adjusted R square + stroke has high VIF and is not significant so removing the same
model_7<-lm(formula = price ~ carlength + carwidth + curbweight + 
              car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymercury + car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault + 
              car_companysaab + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginelocation_n + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_7)
v<-data.frame(vif(model_7))

##-car_companysaab is not significant so removing the same
model_8<-lm(formula = price ~ carlength + carwidth + curbweight + 
              car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymercury + car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault 
              + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginelocation_n + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_8)
v<-data.frame(vif(model_8))
##-car_companymercury is not significant so removing the same
model_9<-lm(formula = price ~ carlength + carwidth + curbweight + 
              car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault 
            + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginelocation_n + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_9)
v<-data.frame(vif(model_9))
##-carbodyhartop has high VIF and relatvely not significant so removing the same
model_10<-lm(formula = price ~ carlength + carwidth + curbweight + 
              car_companybmw + car_companybuick + car_companychevrolet + 
              car_companydodge + car_companyjaguar + car_companymazda + 
              car_companymitsubishi + car_companyNissan + 
              car_companypeugeot + car_companyplymouth + car_companyrenault 
            + car_companysubaru + car_companytoyota + 
              car_companyvw + aspiration_n + carbodyhatchback + 
              carbodysedan + carbodywagon + enginelocation_n + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_10)
v<-data.frame(vif(model_10))
##-carlength has high VIF and relatvely not significant so removing the same
model_11<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyjaguar + car_companymazda + 
               car_companymitsubishi + car_companyNissan + 
               car_companypeugeot + car_companyplymouth + car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + aspiration_n + carbodyhatchback + 
               carbodysedan + carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_11)
v<-data.frame(vif(model_11))
##-carbodysedan has high VIF and relatvely not significant so removing the same
model_12<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyjaguar + car_companymazda + 
               car_companymitsubishi + car_companyNissan + 
               car_companypeugeot + car_companyplymouth + car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + aspiration_n + carbodyhatchback + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_12)
v<-data.frame(vif(model_12))
##-carbodyhatchback is not significant so removing the same
model_13<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyjaguar + car_companymazda + 
               car_companymitsubishi + car_companyNissan + 
               car_companypeugeot + car_companyplymouth + car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + aspiration_n + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_13)
v<-data.frame(vif(model_13))
##-car_companynissan is not significant so removing the same
model_14<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyjaguar + car_companymazda + 
               car_companymitsubishi + 
               car_companypeugeot + car_companyplymouth + car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + aspiration_n + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_14)
v<-data.frame(vif(model_14))
##-aspiration is not relatively significant so removing the same
model_15<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyjaguar + car_companymazda + 
               car_companymitsubishi + 
               car_companypeugeot + car_companyplymouth + car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_15)
v<-data.frame(vif(model_15))
##-dodge is not relatively significant so removing the same
model_16<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + car_companymazda + 
               car_companymitsubishi + 
               car_companypeugeot + car_companyplymouth + car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_16)
v<-data.frame(vif(model_16))
##-plymouth is not relatively significant so removing the same
model_17<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + car_companymazda + 
               car_companymitsubishi + 
               car_companypeugeot +car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_17)
v<-data.frame(vif(model_17))
##-mazda is not relatively significant so removing the same
model_18<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
               car_companymitsubishi + 
               car_companypeugeot +car_companyrenault 
             + car_companysubaru + car_companytoyota + 
               car_companyvw + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_18)
v<-data.frame(vif(model_18))
##-vw is not relatively significant so removing the same
model_19<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
               car_companymitsubishi + 
               car_companypeugeot +car_companyrenault 
             + car_companysubaru + car_companytoyota + 
                carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_19)
v<-data.frame(vif(model_19))
##-subaru is not relatively significant so removing the same
model_20<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
               car_companymitsubishi + 
               car_companypeugeot +car_companyrenault 
             + + car_companytoyota + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_20)
v<-data.frame(vif(model_20))
##-renault is not relatively significant so removing the same
model_21<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
               car_companymitsubishi + 
               car_companypeugeot 
             + car_companytoyota + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_21)
v<-data.frame(vif(model_21))
##-mitsubishi is not relatively significant so removing the same
model_22<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
                            car_companypeugeot 
             + car_companytoyota + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix, data = train)
summary(model_22)
v<-data.frame(vif(model_22))
##-cylindernumberfour has VIF so removing the same
model_23<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
               car_companypeugeot 
             + car_companytoyota + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive + 
               cylindernumbersix, data = train)
summary(model_23)
v<-data.frame(vif(model_23))
##-cylindernumbersix has VIF and is insignificant so removing the same
model_24<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
               car_companypeugeot 
             + car_companytoyota + 
               carbodywagon + enginelocation_n + 
               enginetyperotor + cylindernumberfive 
              , data = train)
summary(model_24)
v<-data.frame(vif(model_24))
##-enginetyperotor is insignificant so removing the same
model_25<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick + car_companychevrolet + 
               + car_companyjaguar + 
               car_companypeugeot 
             + car_companytoyota + 
               carbodywagon + enginelocation_n
               + cylindernumberfive 
             , data = train)
summary(model_25)
v<-data.frame(vif(model_25))
##-car_companychevrolet is insignificant so removing the same
model_26<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick
               + car_companyjaguar + 
               car_companypeugeot 
             + car_companytoyota + 
               carbodywagon + enginelocation_n
             + cylindernumberfive 
             , data = train)
summary(model_26)
v<-data.frame(vif(model_26))
##-car_companytoyota is insignificant so removing the same
model_27<-lm(formula = price ~ carwidth + curbweight + 
               car_companybmw + car_companybuick
             + car_companyjaguar + 
               car_companypeugeot 
             +                 carbodywagon + enginelocation_n
             + cylindernumberfive 
             , data = train)
summary(model_27)
v<-data.frame(vif(model_27))
##-curbweight has high VIF so removing the same
model_28<-lm(formula = price ~ carwidth + 
               car_companybmw + car_companybuick
             + car_companyjaguar + 
               car_companypeugeot 
             +                 carbodywagon + enginelocation_n
             + cylindernumberfive 
             , data = train)
summary(model_28)
v<-data.frame(vif(model_28))
##-carbodywagon is insignificant so removing the same
model_29<-lm(formula = price ~ carwidth + 
               car_companybmw + car_companybuick
             + car_companyjaguar + 
               car_companypeugeot 
             +  enginelocation_n
             + cylindernumberfive 
             , data = train)
summary(model_29)
v<-data.frame(vif(model_29))
##-car_companypeugeot is relatively insignificant so removing the same
model_30<-lm(formula = price ~ carwidth + 
               car_companybmw + car_companybuick
             + car_companyjaguar 
                            +  enginelocation_n
             + cylindernumberfive 
             , data = train)
summary(model_30)
v<-data.frame(vif(model_30))
##-cylindernumberfive is relatively insignificant so removing the same
model_31<-lm(formula = price ~ carwidth + 
               car_companybmw + car_companybuick
             + car_companyjaguar 
             +  enginelocation_n
                          , data = train)
summary(model_31)
v<-data.frame(vif(model_31))
### this is final model because all VIF are below 2, all factors are significant and adjusted rsquare is high

#######-run the model on test set
Predict_1 <- predict(model_31,test[,-14])
test$test_price <- Predict_1
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
plot(test$price,test$test_price)
plot(test$price-test$test_price) # plot the residuals
rsquared
### final model gives 82% R square on test data and residuals don't show any pattern

#######-Model no 2-using the original symbolising variable.Original variable was an integer 
#######-and was categorised into three levels of which data for one level was not available.
#######-so using original data to create a new model

carprice_3<-cbind(carprice_2[,-15],carprice_1$symboling)
carprice_3<-mutate(carprice_3,symbolising=carprice_1$symboling)
carprice_3<-carprice_3[,-66]
carprice_3$symbolising<-as.numeric(carprice_3$symbolising)
str(carprice_3)
#######-model building
trainindices_3= sample(1:nrow(carprice_3), 0.7*nrow(carprice_3))
train_3 = carprice_2[trainindices_3,]
test_3 = carprice_2[-trainindices_3,]
#######-build model
set.seed(200)
model_32<-lm(price~.,data = train_3)
summary(model_32)
library(MASS)
library(car)
step<-stepAIC(model_32,direction = "both")
step
model_33<-lm(formula = price ~ carwidth + carheight + curbweight + enginesize + 
               boreratio + horsepower + peakrpm + car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyNissan + car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companysubaru + car_companytoyota + 
               car_companyvolvo + car_companyvw + aspiration_n + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve + 
               torque, data = train_3)
summary(model_33)
v_3<-data.frame(vif(model_33))
##-removing peakrpm-high VIF and insignificant
model_34<-lm(formula = price ~ carwidth + carheight + curbweight + enginesize + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyNissan + car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companysubaru + car_companytoyota + 
               car_companyvolvo + car_companyvw + aspiration_n + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve + 
               torque, data = train_3)
summary(model_34)
v_3<-data.frame(vif(model_34))
##-removing enginesize-high VIF and insignificant
model_35<-lm(formula = price ~ carwidth + carheight + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyNissan + car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companysubaru + car_companytoyota + 
               car_companyvolvo + car_companyvw + aspiration_n + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve + 
               torque, data = train_3)
summary(model_35)
v_3<-data.frame(vif(model_35))
##-removing torque-high VIF and insignificant
model_36<-lm(formula = price ~ carwidth + carheight + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyNissan + car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companysubaru + car_companytoyota + 
               car_companyvolvo + car_companyvw + aspiration_n + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_36)
v_3<-data.frame(vif(model_36))
##-removing carheight-high VIF and insignificant
model_37<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyNissan + car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companysubaru + car_companytoyota + 
               car_companyvolvo + car_companyvw + aspiration_n + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_37)
v_3<-data.frame(vif(model_37))
##-removing car_companysubaru-high VIF and insignificant
model_38<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyNissan + car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companytoyota + 
               car_companyvolvo + car_companyvw + aspiration_n + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_38)
v_3<-data.frame(vif(model_38))
##-removing aspiration_n-high VIF and insignificant
model_39<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyNissan + car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companytoyota + 
               car_companyvolvo + car_companyvw + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_39)
v_3<-data.frame(vif(model_39))
##-removing car_companyNissan-insignificant
model_40<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymazda + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companytoyota + 
               car_companyvolvo + car_companyvw + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_40)
v_3<-data.frame(vif(model_40))
##-removing car_companymazda-insignificant
model_41<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companytoyota + 
               car_companyvolvo + car_companyvw + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_41)
v_3<-data.frame(vif(model_41))
##-removing drivewheelrwd-high VIF and insignificant
model_42<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + car_companyhonda + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companytoyota + 
               car_companyvolvo + car_companyvw + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_42)
v_3<-data.frame(vif(model_42))
##-removing car_companyhonda-insignificant
model_43<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companytoyota + 
               car_companyvolvo + car_companyvw + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_43)
v_3<-data.frame(vif(model_43))
##-removing car_companyvw-insignificant
model_44<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
               car_companyrenault + car_companytoyota + 
               car_companyvolvo + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_44)
v_3<-data.frame(vif(model_44))
##-removing car_companytoyota-insignificant
model_45<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
               car_companyrenault +  
               car_companyvolvo + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_45)
v_3<-data.frame(vif(model_45))
##-removing car_companyrenault-insignificant
model_46<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + car_companydodge + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
                           car_companyvolvo + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_46)
v_3<-data.frame(vif(model_46))
##-removing car_companydodge-insignificant
model_47<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyplymouth + car_companyporsche + 
               car_companyvolvo + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_47)
v_3<-data.frame(vif(model_47))
##-removing car_companyplymouth-insignificant
model_48<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
                car_companyporsche + 
               car_companyvolvo + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_48)
v_3<-data.frame(vif(model_48))
##-removing car_companyvolvo-relatively insignificant
model_49<-lm(formula = price ~ carwidth + curbweight + 
               boreratio + horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_49)
v_3<-data.frame(vif(model_49))
##-removing boreratio-high VIF abd relatively insignificant
model_50<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_50)
v_3<-data.frame(vif(model_50))
##-removing cylindernumberfour-high VIF 
model_51<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumbersix + cylindernumbertwelve, data = train_3)
summary(model_51)
v_3<-data.frame(vif(model_51))
##-removing cylindernumbertwelve-high VIF and insignificant
model_52<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n + enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumbersix , data = train_3)
summary(model_52)
v_3<-data.frame(vif(model_52))
##-removing enginetypedohcv-high VIF and insignificant
model_53<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n +  enginetyperotor + cylindernumberfive + 
               cylindernumbersix , data = train_3)
summary(model_53)
v_3<-data.frame(vif(model_53))
##-removing cylindernumbersix-insignificant
model_54<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n +  enginetyperotor + cylindernumberfive , data = train_3)
summary(model_54)
v_3<-data.frame(vif(model_54))
##-removing cylindernumberfive-insignificant
model_55<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companychevrolet + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n +  enginetyperotor, data = train_3)
summary(model_55)
v_3<-data.frame(vif(model_55))
##-removing car_companychevrolet-insignificant
model_56<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
                          car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n +  enginetyperotor, data = train_3)
summary(model_56)
v_3<-data.frame(vif(model_56))
##-removing enginetyperotor-relatively insignificant
model_57<-lm(formula = price ~ carwidth + curbweight + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n, data = train_3)
summary(model_57)
v_3<-data.frame(vif(model_57))
##-removing curbweight-high VIF and relatively insignificant
model_59<-lm(formula = price ~ carwidth + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
               car_companyporsche + 
               carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n, data = train_3)
summary(model_59)
v_3<-data.frame(vif(model_59))
##-removing car_companyporsche-high VIF and relatively insignificant
model_60<-lm(formula = price ~ carwidth + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
                              carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               enginelocation_n, data = train_3)
summary(model_60)
v_3<-data.frame(vif(model_60))
##-removing carbodysedan-high VIF 
model_61<-lm(formula = price ~ carwidth + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
               carbodyhardtop + 
               carbodyhatchback + carbodywagon + 
               enginelocation_n, data = train_3)
summary(model_61)
v_3<-data.frame(vif(model_61))
##-removing carbodywagon-insignificant
model_62<-lm(formula = price ~ carwidth + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
               carbodyhardtop + 
               carbodyhatchback + 
               enginelocation_n, data = train_3)
summary(model_62)
v_3<-data.frame(vif(model_62))
##-removing carbodyhardtop-insignificant
model_63<-lm(formula = price ~ carwidth + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
               carbodyhatchback + 
               enginelocation_n, data = train_3)
summary(model_63)
v_3<-data.frame(vif(model_63))
##-removing carbodyhatchback-insignificant
model_64<-lm(formula = price ~ carwidth + 
               horsepower +  car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
                              enginelocation_n, data = train_3)
summary(model_64)
v_3<-data.frame(vif(model_64))
##-removing horsepower-VIF>2
model_65<-lm(formula = price ~ carwidth + 
               car_companybmw + car_companybuick + 
               car_companyjaguar + car_companymitsubishi + 
               enginelocation_n, data = train_3)
summary(model_65)
v_3<-data.frame(vif(model_65))
##-removing car_companymitsubishi-insignificant
model_66<-lm(formula = price ~ carwidth + 
               car_companybmw + car_companybuick + 
               car_companyjaguar + 
               enginelocation_n, data = train_3)
summary(model_66)
v_3<-data.frame(vif(model_66))
### this is second final model because all VIF are below 2, all factors are significant and adjusted rsquare is high

#######-run the model on test set
Predict_3 <- predict(model_66,test_3[,-14])
test_3$test_price <- Predict_3
r_3 <- cor(test_3$price,test_3$test_price)
rsquared_3 <- cor(test_3$price,test_3$test_price)^2
plot(test_3$price,test_3$test_price)
plot(test_3$price-test_3$test_price) # plot the residuals
rsquared_3
plot(test_3$price,test_3$price-test_3$test_price)
### final model gives 87% R square on test data and residuals don't show any pattern
### both the models mentioned below have same variables, have high R square on test dataset
###, all variables are significant, VIF <2 for all variables, plot of residuals don't show any pattern
summary(model_31)
summary(model_66)
#######-Key observation: Car price is function of brand (BMW, Buick and Jaguar command price premium)
#######-width and engine location (cars with rear engines command premium)
#######-this model is inline with real life examples. Typically cars with same technical specifications
#######-but with better brand perception command very high premium
#######-so it's important for Geely Auto to build a strong brand amongst consumers or create a novelty quotient
#######-like Tesla