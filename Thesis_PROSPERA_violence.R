##Setting up----
library(tidyverse)
#library(magrittr) #Pipelines  
library(mice) #Imputation
library(lattice) #For density plot in imputation
library(pan) #Multiple imputation for multivariate panel
library(multilevel) #For ICC1 in imputation
library(gplots) #To run the plotmeans and test heterogeinity
library(plm) #To run the fixed effects models
library(lmtest) #For robust standard errors
library(stargazer) #Reports
library(pander)#For the Rmarkdown with nice tables
set.seed(777)
setwd("C:/Users/Externo/Documents/Master/Thesis/Datos")
violence <- read.csv(file = "homicides.csv", head = TRUE, sep=";", row.names = NULL)

##Imputations via linear regression----
##Duplicate the columns of the number of households and population
violence$Households_lr<- violence$Households_real
violence$Total_Population_lr<- violence$Total_Population_real
#Run regressions by municipality to predict households and total population
#Estimations for households
bymunH <- violence %>%
  group_by(ID_Municipality) %>%
  do(lm(Households_lr ~ Year,data = ., na.action=na.exclude) %>% 
       predict( .,data.frame(Year= 1998:2017), na.action=na.pass) %>% 
       data.frame(Year=1998:2017, Households_lr= .)) 
summary(bymunH)
negH <- subset(bymunH, Households_lr<0) #Analise negative cases
#Estimations for total population
bymunTP <- violence %>%
  group_by(ID_Municipality) %>%
  do(lm(Total_Population_lr ~ Year,data = ., na.action=na.exclude) %>% 
       predict( .,data.frame(Year= 1998:2017), na.action=na.pass) %>% 
       data.frame(Year=1998:2017, Total_Population_lr= .))
negTP <- subset(bymunTP, Total_Population_lr<0) #Analise negative cases
summary(bymunTP)
#Create ID_long to do the left join with the violence dataframe
bymunH$ID_long <- paste(bymunH$ID_Municipality,bymunH$Year)
bymunTP$ID_long <- paste(bymunTP$ID_Municipality,bymunTP$Year)
PredHH <- bymunH[,c("ID_long","Households_lr")]
PredTP <- bymunTP[,c("ID_long","Total_Population_lr")]
violence$ID_long <- (as.character(violence$ID_long))
summary(violence)
violence<-violence[,1:12] #Get rid of the variables we created for the imputation
violence <- left_join(violence, PredHH, by="ID_long")
violence <- left_join(violence, PredTP, by="ID_long")
summary(violence)
rm(bymunH)
rm(bymunTP)
rm(negH)
rm(negTP)
rm(PredHH)
rm(PredTP)


##Imputations via multiple imputation----
#Add control variables
expenditures <- read.csv(file = "Expenditures.csv", head = TRUE, sep=";", row.names = NULL)
vehicles <- read.csv(file = "Vehicles.csv", head = TRUE, sep=";", row.names = NULL)
births <- read.csv(file = "Births.csv", head = TRUE, sep=";", row.names = NULL)
inf_death <- read.csv(file = "InfantDeath.csv", head = TRUE, sep=";", row.names = NULL)
appbymed <- read.csv(file = "AppByMedic.csv", head = TRUE, sep=";", row.names = NULL)
appbyunit <- read.csv(file = "AppByUnit.csv", head = TRUE, sep=";", row.names = NULL)
expenditures$ID_long <- as.character(expenditures$ID_long)
vehicles$ID_long <- as.character(vehicles$ID_long)
births$ID_long <- as.character(births$ID_long)
inf_death$ID_long <- as.character(inf_death$ID_long)
appbymed$ID_long <- as.character(appbymed$ID_long)
appbyunit$ID_long <- as.character(appbyunit$ID_long)
violence <- left_join(violence, expenditures, by="ID_long")
violence <- left_join(violence, vehicles, by="ID_long")
violence <- left_join(violence, births, by="ID_long")
violence <- left_join(violence, inf_death, by="ID_long")
violence <- left_join(violence, appbymed, by="ID_long")
violence <- left_join(violence, appbyunit, by="ID_long")
rm(expenditures)
rm(vehicles)
rm(births)
rm(inf_death)
rm(appbymed)
rm(appbyunit)
##Create variables to run the imputation
violence$Households_imp<- violence$Households_real
violence$Total_Population_imp<- violence$Total_Population_real
##Explore
dim(violence)
summary(violence)
md.pattern(violence,rotate.names = TRUE) #Check matches in Nas accross variables
histogram(~ Year | is.na(Households_imp), data=violence)
histogram(~ Year | is.na(Total_Population_imp), data=violence)
ICC1(aov(Households_imp ~ as.factor(ID_Municipality), data = violence)) #Intraclass Correlation Coefficient, which the amount of individual-level variance that can be explained by group membership.
ICC1(aov(Homicides ~ ID_Municipality, data = violence))
ICC1(aov(Total_beneficiaries ~ ID_Municipality, data = violence))
#Drop some variables not useful for the analysis
names(violence)
viol2 <- violence[c(-1,-2,-4,-5,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-18,-19)]
md.pattern(viol2,rotate.names = TRUE)
names(viol2)
#Impute the dataset with mice
gc()#Garbage collection to have more memory
imp <- mice(viol2, 
            m=5,
            maxit = 10, 
            method = c("", #ID_Mun
                       "", #Year
                       "", #Births
                       "", #AbU
                       "pmm", #H_imp
                       "pmm" #TP_imp
            ))
rm(viol2)
head(complete(imp))
densityplot(imp, ~ Households_imp | .imp)
densityplot(imp, ~ Total_Population_imp | .imp)
#imp <- read.csv(file = "imp.csv", head = TRUE, sep=";", row.names = NULL) #To load the file instead of running again all the previous code
imp$ID_long <- paste(imp$ID_Municipality,imp$Year)
Imp_sh <- imp[,c("ID_long","Households_imp","Total_Population_imp")]
rm(imp)
violence <- violence[,1:20]
violence <- left_join(violence, Imp_sh, by="ID_long")
summary(violence)
rm(Imp_sh)


#Comparison between imputations----
cor(violence$Households_rp,violence$Households_lr) #0.996749
cor(violence$Households_rp,violence$Households_imp,use="complete.obs") #0.9748519
cor(violence$Households_lr,violence$Households_imp,use="complete.obs") #0.9835876
cor(violence$Total_Population_rp,violence$Total_Population_lr) #0.999
cor(violence$Total_Population_rp,violence$Total_Population_imp,use="complete.obs") #0.9905554
cor(violence$Total_Population_lr,violence$Total_Population_imp,use="complete.obs") #0.9922397

#Check if we have less cases over 100%
violence$benpH_o <- (violence$Total_beneficiaries/violence$Households_rp)*100
violence$benpH_p <- (violence$Total_beneficiaries/violence$Households_lr)*100
violence$benpH_i <- (violence$Total_beneficiaries/violence$Households_imp)*100
violence$hompTP_o <- (violence$Homicides/violence$Total_Population_rp)*100000
violence$hompTP_p <- (violence$Homicides/violence$Total_Population_lr)*100000
violence$hompTP_i <- (violence$Homicides/violence$Total_Population_imp)*100000
nrow(filter(violence, benpH_o < 0 | benpH_o > 100)) #2104 >100k
nrow(filter(violence, benpH_p < 0 | benpH_p > 100)) #1454 >100k + 4 <0
nrow(filter(violence, benpH_i < 0 | benpH_i > 100)) #6187 >100k
nrow(filter(violence, hompTP_o < 0 | hompTP_o > 100000)) #45 >100k
nrow(filter(violence, hompTP_p < 0 | hompTP_p > 100000)) #6 < 0
nrow(filter(violence, hompTP_i < 0 | hompTP_i > 100000)) #10 > 100k
#All imputations are comparable but I get less extreme values with the linear regression, which I will keep as main data for the standardisation in the next section.
names(violence)[names(violence) == "benpH_p"] <- "ben"
names(violence)[names(violence) == "hompTP_p"] <- "hom"


##Clean the data----
#Get rid of the cases with negative predictions or over 100%, as well as NAs
vf <- subset(violence, ben>=0 & ben<=100)
vf <- subset(vf, hom>=0 & hom<=100000) #Already included in those cases that I excluded
summary(vf)
rm(violence)


##Management of variables----
## create simple intervention variable where from 2007 onwards I assign 1 to each observation due to the start of the war on drugs.
vf$intervention_simple <- ifelse(vf$Year >= 2007, 1, 0)
table(vf$Year, vf$intervention_simple)
##Other control variables
schools <- read.csv(file = "Schools.csv", head = TRUE, sep=";", row.names = NULL)
retention <- read.csv(file = "Retention.csv", head = TRUE, sep=";", row.names = NULL)
approval <- read.csv(file = "Approval.csv", head = TRUE, sep=";", row.names = NULL)
cartels <- read.csv(file = "Cartels.csv", head = TRUE, sep=";", row.names = NULL)
medunits <- read.csv(file = "MedUnits.csv", head = TRUE, sep=";", row.names = NULL)
healthworkers <- read.csv(file = "HealthWorkers.csv", head = TRUE, sep=";", row.names = NULL)
schools$ID_long <- as.character(schools$ID_long)
retention$ID_long <- as.character(retention$ID_long)
approval$ID_long <- as.character(approval$ID_long)
cartels$ID_long <- as.character(cartels$ID_long)
medunits$ID_long <- as.character(medunits$ID_long)
healthworkers$ID_long <- as.character(healthworkers$ID_long)
vf <- left_join(vf, schools, by="ID_long")
vf <- left_join(vf, retention, by="ID_long")
vf <- left_join(vf, approval, by="ID_long")
vf <- left_join(vf, cartels, by="ID_long")
vf <- left_join(vf, medunits, by="ID_long")
vf <- left_join(vf, healthworkers, by="ID_long")
vf$MedUnits <-(vf$MedUnits/vf$Total_Population_lr)*100000
vf$HealthWorkers <-(vf$HealthWorkers/vf$Total_Population_lr)*100000
rm(schools)
rm(retention)
rm(approval)
rm(cartels)
rm(healthworkers)
rm(medunits)
summary(vf)

#IF I LOAD THE VF DATAFRAME, I CAN START HERE
vf <- read.csv(file = "vf.csv", head = TRUE, sep=";", row.names = NULL)
names(vf)[names(vf) == "benpH_p"] <- "ben"
names(vf)[names(vf) == "hompTP_p"] <- "hom"


##Exploring the data----
#subset the data into two types of municipalities
low_ben <- subset(vf,ben <= quantile(ben,0.25, na.rm=TRUE))#Lowest quartile of the proportion of beneficiaries
high_ben <- subset(vf,ben >= quantile(ben,0.75, na.rm=TRUE))#Highest quartile of the proportion of beneficiaries
pander(summary(low_ben))
pander(summary(high_ben))
#Compute the difference in means
mean(low_ben$hom) - mean(high_ben$hom) #0.9407518
#Plot
plot (vf$hom~vf$Year, ylab= "Homicides per 100,000", xlab = "Year", main ="Evolution of homicides in Mexico",las=2)
plot (vf$ben~vf$Year, ylab= "Proportion of beneficiaries", xlab = "Year", main ="Evolution of proportion of beneficiaries",las=2)
plot (vf$hom~vf$ben, ylab= "Homicides per 100,000", xlab = "Proportion of beneficiaries", main = "Homicides per proportion of beneficiaries")
cor(vf$hom,vf$ben) #-0.008086165
cor(log(vf$hom + 1),vf$ben) #-0.1318327
par(mfrow =c(1,2))
hist(vf$hom,breaks=200,xlim=c(0,200),main="Homicides",xlab="Homicides per 100,000 inhabitants")
abline(v=mean(vf$hom,na.rm=TRUE),col="orange",lwd=5)
abline(v=median(vf$hom,na.rm=TRUE),col="blue",lwd=5)
hist(vf$ben, breaks=50,xlim=c(0,100),main="Beneficiaries",xlab="Proportion of beneficiaries")
abline(v=mean(vf$hom,na.rm=TRUE),col="orange",lwd=5)
abline(v=median(vf$hom,na.rm=TRUE),col="blue",lwd=5)
par(mfrow =c(1,1))
plotmeans(hom ~ ID_Municipality,
          ylab= "Homicides per 100,000",
          xlab ="Municipalities",
          main="Heterogeneity across municipalities",
          n.label = FALSE,
          xaxt="n",
          data=vf)
plotmeans(hom ~ Year,
          ylab= "Homicides per 100,000",
          xlab ="Year",
          main="Heterogeneity across years",
          n.label = FALSE,
          las=2,
          data=vf)
stargazer(vf[c("Homicides","Total_beneficiaries","hom","ben","Vehicles","Births","InfDeath","HealthWorkers","MedUnits","Retention","Approval","Cartels")],
          type="html", title="Descriptive statistics", digits=1,out="Descriptive.htm" )


##Models----
vf$Year <- as.factor(vf$Year)
m1 <- plm(I(log(hom +1))~ben,
          data=vf,
          index = c("ID_Municipality","Year"),
          model = "within")
m1_rob <- coeftest(m1, vcov = vcovHC, type = "HC1")
m2 <- plm(I(log(hom+1))~ben + intervention_simple,
          data=vf,
          index = c("ID_Municipality","Year"),
          model = "within")
m2_rob <- coeftest(m2, vcov = vcovHC, type = "HC1")
m2.1 <- plm(I(log(hom+1))~ben*intervention_simple,
          data=vf,
          index = c("ID_Municipality","Year"),
          model = "within")
m2.1_rob <- coeftest(m2.1, vcov = vcovHC, type = "HC1")
m3.1 <- plm(I(log(hom+1))~ben*intervention_simple + HealthWorkers + MedUnits, 
            data=vf,
            index = c("ID_Municipality","Year"),
            model = "within")
m3.1_rob <- coeftest(m3.1, vcov = vcovHC, type = "HC1")
m3.2 <- plm(I(log(hom+1))~ben*intervention_simple + Vehicles + Births + InfDeath,
            data=vf,
            index = c("ID_Municipality","Year"),
            model = "within")
m3.2_rob <- coeftest(m3.2, vcov = vcovHC, type = "HC1")
m3.3 <- plm(I(log(hom+1))~ben*intervention_simple + Retention + Approval,
            data=vf,
            index = c("ID_Municipality","Year"),
            model = "within")
m3.3_rob <- coeftest(m3.3, vcov = vcovHC, type = "HC1")
m3 <- plm(I(log(hom+1))~ben*intervention_simple +  HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles, 
          data=vf,
          index = c("ID_Municipality","Year"),
          model = "within")
m3_rob <- coeftest(m3, vcov = vcovHC, type = "HC1")

stargazer(m1_rob, m2_rob, m2.1_rob, m3.1_rob, m3.2_rob, m3.3_rob, m3_rob, title="Results",
          column.labels=c("Beneficiaries","Military","Interaction","Health","Economic","Education","All"), align=TRUE, type="html", out="results_rob.htm")
stargazer(m1, m2, m2.1, m3.1, m3.2, m3.3, m3, title="Results",
          column.labels=c("Beneficiaries","Military","Interaction","Health","Economic","Education","All"), align=TRUE, type="html", out="results.htm")
#Models with cartels
m2c <- plm(I(log(hom+1))~ben*intervention_simple + Cartels,
           data=vf,
           index = c("ID_Municipality","Year"),
           model = "within")
m2c_rob <- coeftest(m2c, vcov = vcovHC, type = "HC1")
m3c.1 <- plm(I(log(hom+1))~ben*intervention_simple +  HealthWorkers + MedUnits + Cartels, 
             data=vf,
             index = c("ID_Municipality","Year"),
             model = "within")
m3c.1_rob <- coeftest(m3c.1, vcov = vcovHC, type = "HC1")
m3c.2 <- plm(I(log(hom+1))~ben*intervention_simple + Vehicles + Births + InfDeath + Cartels,
             data=vf,
             index = c("ID_Municipality","Year"),
             model = "within")
m3c.2_rob <- coeftest(m3c.2, vcov = vcovHC, type = "HC1")
m3c.3 <- plm(I(log(hom+1))~ben*intervention_simple + Retention + Approval + Cartels,
             data=vf,
             index = c("ID_Municipality","Year"),
             model = "within")
m3c.3_rob <- coeftest(m3c.3, vcov = vcovHC, type = "HC1")
m3c <- plm(I(log(hom+1))~ben*intervention_simple +  HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles + Cartels, 
           data=vf,
           index = c("ID_Municipality","Year"),
           model = "within")
m3c_rob <- coeftest(m3c, vcov = vcovHC, type = "HC1")
stargazer(m2c_rob, m3c.1_rob, m3c.2_rob, m3c_rob, m3c_rob, title="Results including cartel presence",
          column.labels=c("Interaction","Health","Economic","Education","All"), align=TRUE, type="html", out="resultscartels_rob.htm")
stargazer(m2c, m3c.1, m3c.2, m3c, m3c, title="Results including cartel presence",
          column.labels=c("Interaction","Health","Economic","Education","All"), align=TRUE, type="html", out="resultscartels.htm")
#Compare before and after the war on drugs
m4 <- plm(I(log(hom +1))~ben,
          data=subset(vf, intervention_simple==1),
          index = c("ID_Municipality","Year"),
          model = "within")
m4_rob <- coeftest(m4, vcov = vcovHC, type = "HC1")
m4.1 <- plm(I(log(hom +1))~ben,
          data=subset(vf, intervention_simple==0),
          index = c("ID_Municipality","Year"),
          model = "within")
m4.1_rob <- coeftest(m4.1, vcov = vcovHC, type = "HC1")
m5a <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits, 
           data=subset(vf, intervention_simple==1),
            index = c("ID_Municipality","Year"),
            model = "within")
m5a_rob <- coeftest(m5a, vcov = vcovHC, type = "HC1")
m5a.1 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits, 
          data=subset(vf, intervention_simple==0),
           index = c("ID_Municipality","Year"),
           model = "within")
m5a.1_rob <- coeftest(m5a.1, vcov = vcovHC, type = "HC1")
m5b <- plm(I(log(hom+1))~ben + Vehicles + Births + InfDeath,
            data=subset(vf, intervention_simple==1),
            index = c("ID_Municipality","Year"),
            model = "within")
m5b_rob <- coeftest(m5b, vcov = vcovHC, type = "HC1")
m5b.1 <- plm(I(log(hom+1))~ben + Vehicles + Births + InfDeath,
            data=subset(vf, intervention_simple==0),
            index = c("ID_Municipality","Year"),
            model = "within")
m5b.1_rob <- coeftest(m5b.1, vcov = vcovHC, type = "HC1")
m5c <- plm(I(log(hom+1))~ben + Retention + Approval,
            data=subset(vf, intervention_simple==1),
            index = c("ID_Municipality","Year"),
            model = "within")
m5c_rob <- coeftest(m5c, vcov = vcovHC, type = "HC1")
m5c.1 <- plm(I(log(hom+1))~ben + Retention + Approval,
            data=subset(vf, intervention_simple==0),
            index = c("ID_Municipality","Year"),
            model = "within")
m5c.1_rob <- coeftest(m5c.1, vcov = vcovHC, type = "HC1")
m6 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles, 
          data=subset(vf, intervention_simple==1),
          index = c("ID_Municipality","Year"),
          model = "within")
m6_rob <- coeftest(m6, vcov = vcovHC, type = "HC1")
m6.1 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles, 
          data=subset(vf, intervention_simple==0),
          index = c("ID_Municipality","Year"),
          model = "within")
m6.1_rob <- coeftest(m6.1, vcov = vcovHC, type = "HC1")
stargazer(m4_rob, m4.1_rob, m5a_rob, m5a.1_rob, m5b_rob, m5b.1_rob, m5c_rob, m5c.1_rob, m6_rob, m6.1_rob, title="Results comparing years with and without military intervention",
          column.labels=c("Beneficiaries(M)","Beneficiaries(nM)","Health(M)","Health(nM)","Economic(M)","Economic(nM)","Education(M)","Education(nM)","All(M)","All(nM)"), align=TRUE, type="html", out="resultsmilitary_rob.htm")
stargazer(m4, m4.1, m5a, m5a.1, m5b, m5b.1, m5c, m5c.1, m6, m6.1, title="Results comparing years with and without military intervention",
          column.labels=c("Beneficiaries(M)","Beneficiaries(nM)","Health(M)","Health(nM)","Economic(M)","Economic(nM)","Education(M)","Education(nM)","All(M)","All(nM)"), align=TRUE, type="html", out="resultsmilitary.htm")
#Including cartels
m4c <- plm(I(log(hom +1))~ben + Cartels,
          data=subset(vf, intervention_simple==1),
          index = c("ID_Municipality","Year"),
          model = "within")
m4c_rob <- coeftest(m4c, vcov = vcovHC, type = "HC1")
m4c.1 <- plm(I(log(hom +1))~ben + Cartels,
            data=subset(vf, intervention_simple==0),
            index = c("ID_Municipality","Year"),
            model = "within")
m4c.1_rob <- coeftest(m4c.1, vcov = vcovHC, type = "HC1")
m5ca <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Cartels, 
           data=subset(vf, intervention_simple==1),
           index = c("ID_Municipality","Year"),
           model = "within")
m5ca_rob <- coeftest(m5ca, vcov = vcovHC, type = "HC1")
m5ca.1 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Cartels, 
             data=subset(vf, intervention_simple==0),
             index = c("ID_Municipality","Year"),
             model = "within")
m5ca.1_rob <- coeftest(m5ca.1, vcov = vcovHC, type = "HC1")
m5cb <- plm(I(log(hom+1))~ben + Vehicles + Births + InfDeath + Cartels,
           data=subset(vf, intervention_simple==1),
           index = c("ID_Municipality","Year"),
           model = "within")
m5cb_rob <- coeftest(m5cb, vcov = vcovHC, type = "HC1")
m5cb.1 <- plm(I(log(hom+1))~ben + Vehicles + Births + InfDeath + Cartels,
             data=subset(vf, intervention_simple==0),
             index = c("ID_Municipality","Year"),
             model = "within")
m5cb.1_rob <- coeftest(m5cb.1, vcov = vcovHC, type = "HC1")
m5cc <- plm(I(log(hom+1))~ben + Retention + Approval + Cartels,
           data=subset(vf, intervention_simple==1),
           index = c("ID_Municipality","Year"),
           model = "within")
m5cc_rob <- coeftest(m5cc, vcov = vcovHC, type = "HC1")
m5cc.1 <- plm(I(log(hom+1))~ben + Retention + Approval + Cartels,
             data=subset(vf, intervention_simple==0),
             index = c("ID_Municipality","Year"),
             model = "within")
m5cc.1_rob <- coeftest(m5cc.1, vcov = vcovHC, type = "HC1")
m6c <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles + Cartels, 
          data=subset(vf, intervention_simple==1),
          index = c("ID_Municipality","Year"),
          model = "within")
m6c_rob <- coeftest(m6c, vcov = vcovHC, type = "HC1")
m6c.1 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles + Cartels, 
            data=subset(vf, intervention_simple==0),
            index = c("ID_Municipality","Year"),
            model = "within")
m6c.1_rob <- coeftest(m6c.1, vcov = vcovHC, type = "HC1")
stargazer(m4c_rob, m4c.1_rob, m5ca_rob, m5ca.1_rob, m5cb_rob, m5cb.1_rob, m5cc_rob, m5cc.1_rob, m6c_rob, m6c.1_rob, title="Results comparing years with and without military intervention including presence of Cartels",
          column.labels=c("Beneficiaries(M)","Beneficiaries(nM)","Health(M)","Health(nM)","Economic(M)","Economic(nM)","Education(M)","Education(nM)","All(M)","All(nM)"), align=TRUE, type="html", out="resultsmilitarycartels_rob.htm")
stargazer(m4c, m4c.1, m5ca, m5ca.1, m5cb, m5cb.1, m5cc, m5cc.1, m6c, m6c.1, title="Results comparing years with and without military intervention including presence of Cartels",
          column.labels=c("Beneficiaries(M)","Beneficiaries(nM)","Health(M)","Health(nM)","Economic(M)","Economic(nM)","Education(M)","Education(nM)","All(M)","All(nM)"), align=TRUE, type="html", out="resultsmilitarycartels.htm")
#DIfferent periods
t1<-vf[vf$Year=="1998"|vf$Year=="1999"|vf$Year=="2000"|vf$Year=="2001"|vf$Year=="2002"|vf$Year=="2003"|vf$Year=="2004"|vf$Year=="2005"|vf$Year=="2006",]
t2<-vf[vf$Year=="2007"|vf$Year=="2008"|vf$Year=="2009"|vf$Year=="2010"|vf$Year=="2011",]
t3<-vf[vf$Year=="2012"|vf$Year=="2013"|vf$Year=="2014",]
t4<-vf[vf$Year=="2015"|vf$Year=="2016"|vf$Year=="2017",]
mdp1 <- plm(I(log(hom +1))~ben,
          data=t1,
          index = c("ID_Municipality","Year"),
          model = "within")
mdp1_rob <- coeftest(mdp1, vcov = vcovHC, type = "HC1")
mdp2 <- plm(I(log(hom +1))~ben,
           data=t2,
           index = c("ID_Municipality","Year"),
           model = "within")
mdp2_rob <- coeftest(mdp2, vcov = vcovHC, type = "HC1")
mdp3 <- plm(I(log(hom +1))~ben,
            data=t3,
            index = c("ID_Municipality","Year"),
            model = "within")
mdp3_rob <- coeftest(mdp3, vcov = vcovHC, type = "HC1")
mdp4 <- plm(I(log(hom +1))~ben,
            data=t4,
            index = c("ID_Municipality","Year"),
            model = "within")
mdp4_rob <- coeftest(mdp4, vcov = vcovHC, type = "HC1")
stargazer(mdp1_rob, mdp2_rob, mdp3_rob, mdp4_rob, title="Results comparing different periods",
          column.labels=c("1998-2006","2007-2011","2012-2014","2015-2017"), align=TRUE, type="html", out="resultsperiods_rob.htm")
stargazer(mdp1, mdp2, mdp3, mdp4, title="Results comparing different periods", 
          column.labels=c("1998-2006","2007-2011","2012-2014","2015-2017"), align=TRUE, type="html", out="resultsperiods.htm")
#All variables
mdp5 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles,
               data=t1,
               index = c("ID_Municipality","Year"),
               model = "within")
mdp5_rob <- coeftest(mdp5, vcov = vcovHC, type = "HC1")
mdp6 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles,
            data=t2,
            index = c("ID_Municipality","Year"),
            model = "within")
mdp6_rob <- coeftest(mdp6, vcov = vcovHC, type = "HC1")
mdp7 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles,
            data=t3,
            index = c("ID_Municipality","Year"),
            model = "within")
mdp7_rob <- coeftest(mdp7, vcov = vcovHC, type = "HC1")
mdp8 <- plm(I(log(hom+1))~ben + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles,
            data=t4,
            index = c("ID_Municipality","Year"),
            model = "within")
mdp8_rob <- coeftest(mdp8, vcov = vcovHC, type = "HC1")
stargazer(mdp5_rob, mdp6_rob, mdp7_rob, mdp8_rob, title="Results comparing different periods, all variables", 
          column.labels=c("1998-2006","2007-2011","2012-2014","2015-2017"), align=TRUE, type="html", out="resultsperiodsall_rob.htm")
stargazer(mdp5, mdp6, mdp7, mdp8, title="Results comparing different periods, all variables", 
          column.labels=c("1998-2006","2007-2011","2012-2014","2015-2017"), align=TRUE, type="html", out="resultsperiodsall.htm")
##Robustness checks----
#Models with population and household repetiton
summary(vf$benpH_o)
is.na(vf$benpH_o) <- sapply(vf$benpH_o, is.infinite) #Replaces inf with NA.
summary(vf$hompTP_o)
is.na(vf$hompTP_o) <- sapply(vf$hompTP_o, is.infinite)
mr1 <- plm(I(log(hompTP_o +1))~benpH_o,
          data=vf,
          index = c("ID_Municipality","Year"),
          model = "within")
mr1_rob <- coeftest(mr1, vcov = vcovHC, type = "HC1")
mr2 <- plm(I(log(hompTP_o+1))~benpH_o + intervention_simple,
          data=vf,
          index = c("ID_Municipality","Year"),
          model = "within")
mr2_rob <- coeftest(mr2, vcov = vcovHC, type = "HC1")
mr2.1 <- plm(I(log(hompTP_o+1))~benpH_o*intervention_simple,
            data=vf,
            index = c("ID_Municipality","Year"),
            model = "within")
mr2.1_rob <- coeftest(mr2.1, vcov = vcovHC, type = "HC1")
mr3.1 <- plm(I(log(hompTP_o+1))~benpH_o*intervention_simple + HealthWorkers + MedUnits, 
            data=vf,
            index = c("ID_Municipality","Year"),
            model = "within")
mr3.1_rob <- coeftest(mr3.1, vcov = vcovHC, type = "HC1")
mr3.2 <- plm(I(log(hompTP_o+1))~benpH_o*intervention_simple + Vehicles + Births + InfDeath,
            data=vf,
            index = c("ID_Municipality","Year"),
            model = "within")
mr3.2_rob <- coeftest(mr3.2, vcov = vcovHC, type = "HC1")
mr3.3 <- plm(I(log(hompTP_o+1))~benpH_o*intervention_simple + Retention + Approval,
            data=vf,
            index = c("ID_Municipality","Year"),
            model = "within")
mr3.3_rob <- coeftest(mr3.3, vcov = vcovHC, type = "HC1")
mr3 <- plm(I(log(hompTP_o+1))~benpH_o*intervention_simple +  HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles, 
          data=vf,
          index = c("ID_Municipality","Year"),
          model = "within")
mr3_rob <- coeftest(mr3, vcov = vcovHC, type = "HC1")
stargazer(mr1_rob, mr2_rob, mr2.1_rob, mr3.1_rob, mr3.2_rob, mr3.3_rob, mr3_rob, title="Results with imputations with repetition", 
          column.labels=c("Beneficiaries","Military","Interaction","Health","Economic","Education","All"), align=TRUE, type="html", out="results_rp_rob.htm")
stargazer(mr1, mr2, mr2.1, mr3.1, mr3.2, mr3.3, mr3, title="Results with imputations with repetition",
          column.labels=c("Beneficiaries","Military","Interaction","Health","Economic","Education","All"), align=TRUE, type="html", out="results_rp.htm")
#Compare before and after the war on drugs
mr4 <- plm(I(log(hompTP_o +1))~benpH_o,
          data=subset(vf, intervention_simple==1),
          index = c("ID_Municipality","Year"),
          model = "within")
mr4_rob <- coeftest(mr4, vcov = vcovHC, type = "HC1")
mr4.1 <- plm(I(log(hompTP_o +1))~benpH_o,
            data=subset(vf, intervention_simple==0),
            index = c("ID_Municipality","Year"),
            model = "within")
mr4.1_rob <- coeftest(mr4.1, vcov = vcovHC, type = "HC1")
mr5a <- plm(I(log(hompTP_o+1))~benpH_o + HealthWorkers + MedUnits, 
           data=subset(vf, intervention_simple==1),
           index = c("ID_Municipality","Year"),
           model = "within")
mr5a_rob <- coeftest(mr5a, vcov = vcovHC, type = "HC1")
mr5a.1 <- plm(I(log(hompTP_o+1))~benpH_o + HealthWorkers + MedUnits, 
             data=subset(vf, intervention_simple==0),
             index = c("ID_Municipality","Year"),
             model = "within")
mr5a.1_rob <- coeftest(mr5a.1, vcov = vcovHC, type = "HC1")
mr5b <- plm(I(log(hompTP_o+1))~benpH_o + Vehicles + Births + InfDeath,
           data=subset(vf, intervention_simple==1),
           index = c("ID_Municipality","Year"),
           model = "within")
mr5b_rob <- coeftest(mr5b, vcov = vcovHC, type = "HC1")
mr5b.1 <- plm(I(log(hompTP_o+1))~benpH_o + Vehicles + Births + InfDeath,
             data=subset(vf, intervention_simple==0),
             index = c("ID_Municipality","Year"),
             model = "within")
mr5b.1_rob <- coeftest(mr5b.1, vcov = vcovHC, type = "HC1")
mr5c <- plm(I(log(hompTP_o+1))~benpH_o + Retention + Approval,
           data=subset(vf, intervention_simple==1),
           index = c("ID_Municipality","Year"),
           model = "within")
mr5c_rob <- coeftest(mr5c, vcov = vcovHC, type = "HC1")
mr5c.1 <- plm(I(log(hompTP_o+1))~benpH_o + Retention + Approval,
             data=subset(vf, intervention_simple==0),
             index = c("ID_Municipality","Year"),
             model = "within")
mr5c.1_rob <- coeftest(mr5c.1, vcov = vcovHC, type = "HC1")
mr6 <- plm(I(log(hompTP_o+1))~benpH_o + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles, 
          data=subset(vf, intervention_simple==1),
          index = c("ID_Municipality","Year"),
          model = "within")
mr6_rob <- coeftest(mr6, vcov = vcovHC, type = "HC1")
mr6.1 <- plm(I(log(hompTP_o+1))~benpH_o + HealthWorkers + MedUnits + Approval + Births + InfDeath + Retention + Vehicles, 
            data=subset(vf, intervention_simple==0),
            index = c("ID_Municipality","Year"),
            model = "within")
mr6.1_rob <- coeftest(mr6.1, vcov = vcovHC, type = "HC1")
stargazer(mr4_rob, mr4.1_rob, mr5a_rob, mr5a.1_rob, mr5b_rob, mr5b.1_rob, mr5c_rob, mr5c.1_rob, mr6_rob, mr6.1_rob, title="Results comparing years with and without military intervention (repetition)",
          column.labels=c("Beneficiaries(M)","Beneficiaries(nM)","Health(M)","Health(nM)","Economic(M)","Economic(nM)","Education(M)","Education(nM)","All(M)","All(nM)"), align=TRUE, type="html", out="resultsmilitary_rp_rob.htm")
stargazer(mr4, mr4.1, mr5a, mr5a.1, mr5b, mr5b.1, mr5c, mr5c.1, mr6, mr6.1, title="Results comparing years with and without military intervention (repetition)",
          column.labels=c("Beneficiaries(M)","Beneficiaries(nM)","Health(M)","Health(nM)","Economic(M)","Economic(nM)","Education(M)","Education(nM)","All(M)","All(nM)"), align=TRUE, type="html", out="resultsmilitary_rp.htm")
#R squared
sst <- with(vf, sum((log(hom+1) - mean(log(hom+1)))^2))
m1.sse <- t(residuals(m3)) %*% residuals(m3)
(sst - m1.sse) / sst #0.7007413
