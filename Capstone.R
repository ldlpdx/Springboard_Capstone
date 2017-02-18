library(ggplot2)
library(ggthemes)
library(psych)
library(GGally)
library(grDevices)
library(colorRamps)
library(dplyr)
library(maptools)
library(RColorBrewer)
library(blme)
library(lubridate)
library(reporttools)
library(stargazer)
library(fpp)
library(xtable)
library(plotly)
library(Cairo)
library(MASS)
library(car)
library(Amelia)
library(corrplot)
library(caret)
library(plm)
library(smbinning)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# READ IN DATA
elephants <- read.csv("elephant_master_reduced.csv", header = TRUE)
names(elephants)

# EXPLORE THE DATA PRIOR TO IMPUTATION OF MISSING DATA
summary(elephants)
sapply(elephants[,9:47], function(x) sum(is.na(x)))


par(mfrow=c(1,1))
boxplot(NGDP_RPCH ~ country, data = elephants, col="coral2", horizontal=TRUE)
boxplot(Definite.Possible ~ country, data = elephants, col="coral2", horizontal=TRUE)
boxplot(Illegal.Carcasses ~ country, data = elephants, col="coral2", horizontal=TRUE)

library(gplots)


# SEPARATE DATA INTO REGIONS AND PLOT GOVERNANCE INDICATORS
elephantsFC <- filter(elephants, subregionid=="FC")
elephantsFW <- filter(elephants, subregionid=="FW")
elephantsFE <- filter(elephants, subregionid=="FE")
elephantsFS <- filter(elephants, subregionid=="FS")

png("governanceFC.png", height = 1080, width = 680)
governanceFC <- ggplot(data=elephantsFC, aes(x=year))+
  geom_line(aes(y=Voice.Accountability), color="cadetblue3")+
  geom_line(aes(y=Political.Stability), color="coral")+
  geom_line(aes(y=Government.Effectiveness), color="aquamarine")+
  geom_line(aes(y=Rule.Law), color="gold")+
  geom_line(aes(y=Corruption.Control), color="palegreen")+
  geom_line(aes(y=Reg.Quality), color="darkorchid1")+
  ylim(-2, 3) + xlab(NULL) + ylab(NULL) +
  ggtitle("Governance indicators for Central Africa") + theme_bw()
governanceFC + facet_wrap(~country, ncol = 4)
dev.off()

png("governanceFW.png", height = 1080, width = 680)
governanceFW <- ggplot(data=elephantsFW, aes(x=year))+
  geom_line(aes(y=Voice.Accountability), color="cadetblue3")+
  geom_line(aes(y=Political.Stability), color="coral")+
  geom_line(aes(y=Government.Effectiveness), color="aquamarine")+
  geom_line(aes(y=Rule.Law), color="gold")+
  geom_line(aes(y=Corruption.Control), color="palegreen")+
  geom_line(aes(y=Reg.Quality), color="darkorchid1")+
  ylim(-2, 3) + xlab(NULL) + ylab(NULL) +
  ggtitle("Governance indicators for Western Africa")+ theme_bw()
governanceFW + facet_wrap(~country, ncol = 4)
dev.off()

png("governanceFE.png", height = 1080, width = 680)
governanceFE <- ggplot(data=elephantsFE, aes(x=year))+
  geom_line(aes(y=Voice.Accountability), color="cadetblue3")+
  geom_line(aes(y=Political.Stability), color="coral")+
  geom_line(aes(y=Government.Effectiveness), color="aquamarine")+
  geom_line(aes(y=Rule.Law), color="gold")+
  geom_line(aes(y=Corruption.Control), color="palegreen")+
  geom_line(aes(y=Reg.Quality), color="darkorchid1")+
  ylim(-2, 3) + xlab(NULL) + ylab(NULL) +
  ggtitle("Governance indicators for Eastern Africa")+ theme_bw()
governanceFE + facet_wrap(~country, ncol = 4)
dev.off()

png("governanceFS.png", height = 1080, width = 680)
governanceFS <- ggplot(data=elephantsFS, aes(x=year))+
  geom_line(aes(y=Voice.Accountability), color="cadetblue3")+
  geom_line(aes(y=Political.Stability), color="coral")+
  geom_line(aes(y=Government.Effectiveness), color="aquamarine")+
  geom_line(aes(y=Rule.Law), color="gold")+
  geom_line(aes(y=Corruption.Control), color="palegreen")+
  geom_line(aes(y=Reg.Quality), color="darkorchid1")+
  ylim(-2, 3) + xlab(NULL) + ylab(NULL) +
  ggtitle("Governance indicators for Southern Africa")+ theme_bw()
governanceFS + facet_wrap(~country, ncol = 4)
dev.off()

# CHECK FOR NORMALITY PRIOR TO IMPUTING MISSING VALUES
names(elephants)
str(elephants)
elephants$Percent.Illegal <- as.numeric(elephants$Percent.Illegal)

# TRANSFORM THE DATA TO MULTIVARIATE NORMAL DISTRIBUTION
centered.elephants <- data.frame(scale(elephants[,c(9:28,33:38,41:47)]))
centered.elephants <- cbind(elephants[,c(1:8, 29:32,39:40)], centered.elephants)
str(centered.elephants)
summary(centered.elephants)



# EXAMINE DATA FOR HIGHLY CORRELATED VARIABLES
clrs <- brewer.pal(10, "Spectral")
cors <- cor(centered.elephants[,9:49], use = "pairwise")
quartz()
corrplot <- corrplot.mixed(cors, col = clrs, number.cex=0.3,
                     tl.pos="lt", tl.cex=0.5, tl.col="black",
                     tl.srt=45)

# RUN MODELS TO DETERMINE WHICH OF THE CORRELATED VARIABLES TO KEEP
names(centered.elephants)
fit1 <- lm(Illegal.Carcasses~GNI, data=centered.elephants)
summary(fit1)
fit2 <- lm(Illegal.Carcasses~NGDPDPC, data=centered.elephants)
summary(fit2)

fit3 <- lm(Illegal.Carcasses~Mean.Schooling, data=centered.elephants)
summary(fit3)
fit4 <- lm(Illegal.Carcasses~HDI, data=centered.elephants)
summary(fit4)

fit5 <- lm(Illegal.Carcasses~Mean.Schooling, data=centered.elephants)
summary(fit5)
fit6 <- lm(Illegal.Carcasses~Adult.literacy, data=centered.elephants)
summary(fit6)

fit1 <- lm(Illegal.Carcasses~Total.pop, data=centered.elephants)
summary(fit1)
fit2 <- lm(Illegal.Carcasses~Pop.MultiDim.Povert, data=centered.elephants)
summary(fit2)

fit1 <- lm(Illegal.Carcasses~GNI, data=centered.elephants)
summary(fit1)
fit2 <- lm(Illegal.Carcasses~NGDPDPC, data=centered.elephants)
summary(fit2)

fit7 <- lm(Illegal.Carcasses~Government.Effectiveness, data=centered.elephants)
summary(fit7)
fit8 <- lm(Illegal.Carcasses~Rule.Law, data=centered.elephants)
summary(fit8)

fit9 <- lm(Illegal.Carcasses~Government.Effectiveness, data=centered.elephants)
summary(fit9)
fit10 <- lm(Illegal.Carcasses~Corruption.Control, data=centered.elephants)
summary(fit10)

fit11 <- lm(Illegal.Carcasses~Government.Effectiveness, data=centered.elephants)
summary(fit11)
fit12 <- lm(Illegal.Carcasses~Reg.Quality, data=centered.elephants)
summary(fit12)

fit11 <- lm(Illegal.Carcasses~Government.Effectiveness, data=centered.elephants)
summary(fit11)
fit12 <- lm(Illegal.Carcasses~Corruption.Perception.Index, data=centered.elephants)
summary(fit12)

fit13 <- lm(Illegal.Carcasses~Corruption.Perception.Index, data=centered.elephants)
summary(fit13)
fit14 <- lm(Illegal.Carcasses~Rule.Law, data=centered.elephants)
summary(fit14)

fit15 <- lm(Illegal.Carcasses~Corruption.Perception.Index, data=centered.elephants)
summary(fit15)
fit16 <- lm(Illegal.Carcasses~Corruption.Control, data=centered.elephants)
summary(fit16)

# SELECT THOSE VARIABLES THAT SHOULD REMAIN
pd.elephants <- subset(centered.elephants, subset=TRUE, select=c(1:10,12:23,26:40,42:49))

# CREATE DUMMY VARIABLE TO USE FOR BINOMIAL RESPONSE
pd.elephants$Percent.Illegal <- pd.elephants$Illegal.Carcasses / pd.elephants$Tot.Carcasses
pd.elephants$High.Illegal <- factor(with(pd.elephants, ifelse((Percent.Illegal > 0.5), 1, 0)))
table(pd.elephants$High.Illegal)

# SEPARATE DATA INTO TRAINING AND TESTING SETS
inVal <- createDataPartition(pd.elephants$High.Illegal, p = 0.7, list=FALSE)
eleph.train <- pd.elephants[inVal,]
eleph.test <- pd.elephants[-inVal,]



# IMPUTE MISSING VALUES USING MULTIPLE IMPUTATION
names(eleph.train)
library(snow)
set.seed(1357)
elephants.out.train <- amelia(eleph.train, m=5, frontend = FALSE, 
                              idvars = c("ISO2", "ISO3", "region", 
                                         "subregionid", "cap.lat", "cap.long"),
                              ts = "year", cs = "country", noms = "High.Illegal",
                              logs = c("Tot.Carcasses", "Illegal.Carcasses"),
                              polytime = 0, intercs = TRUE, p2s = 1, 
                              parallel="snow", ncpus = 3, 
                             empri = .01*nrow(centered.elephants))

# EXAMINE HIGH ILLEGAL VARIABLE
table(elephants.out.time1$imputations[[3]]$High.Illegal)

# SPOT CHECK IMPUTATIONS
quartz()
par(mfrow=c(3,2))
tscsPlot(elephants.out.train, cs = "Swaziland", main = "Swaziland",
          var = "Resource.Depletion", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Botswana", main = "Botswana",
         var = "Resource.Depletion", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Cameroon", main = "Cameroon",
         var = "Tot.Carcasses", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Central Africa Republic", main = "Central Africa Republic",
         var = "Tot.Carcasses", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Liberia", main = "Liberia",
         var = "Illegal.Carcasses", ylim = c(-2,2))
tscsPlot(elephants.out.train, cs = "Nigeria", main = "Nigeria",
         var = "Illegal.Carcasses", ylim = c(-2,2))
names(pd.elephants)
quartz()
par(mfrow=c(2,2))
plot(elephants.out.train, which.vars=c(20,42,45,46))





# RUN LINEAR REGRESSION ON THE ECONOMIC INDICATORS
names(eleph.train)
eleph.train.plm.econ <- lapply(elephants.out.train$imputations,
                           function(x){
  plm(Percent.Illegal ~ NGDP_RPCH + NGDPD + NGSD_NGDP + PCPI + 
       PCPIPCH + GGX_NGDP + GGXCNL_NGDP + GGXWDG_NGDP +
       BCA + GNI + International.Dev.Aid + Pop.MultiDim.Povert + 
       Deprivation.Intensity + Pop.Below.National.Poverty + PPP.125.day,
     index=c("country", "year"), model = "random",
     data = x)
})
summary(elep.train.lm$imp1)
summary(elep.train.lm$imp2)
summary(elep.train.lm$imp3)
summary(elep.train.lm$imp4)
summary(elep.train.lm$imp5)

eleph.train.plm.gov <- lapply(elephants.out.train$imputations,
                               function(x){
                                 plm(Percent.Illegal ~ HDI + Resource.Depletion +
                                       Adult.literacy + Primary.ed.enrollment + 
                                       Corruption.Perception.Index + Voice.Accountability +
                                       Political.Stability + Government.Effectiveness + 
                                       Rule.Law + factor(Armed.Conflict) +
                                       factor(Non.State.Conflict) + Non.State.Conflict.Deaths +
                                       Elephant.range,
                                     index=c("country", "year"), model = "between",
                                     data = x)
                               })

summary(eleph.train.plm.gov$imp1)
summary(eleph.train.plm.gov$imp2)
summary(eleph.train.plm.gov$imp3)
summary(eleph.train.plm.gov$imp4)
summary(eleph.train.plm.gov$imp5)

eleph.fit1 <- lapply(elephants.out.train$imputations,
                     function(x){
                       plm(Percent.Illegal ~ GGXWDG_NGDP + NGDP_RPCH +
                             Resource.Depletion + Political.Stability +
                             Government.Effectiveness,
                           index=c("country", "year"), model = "between",
                           data = x)
                     })


summary(eleph.fit1$imp1)
summary(eleph.fit1$imp2)
summary(eleph.fit1$imp3)
summary(eleph.fit1$imp4)
summary(eleph.fit1$imp5)

eleph.fit2 <- lapply(elephants.out.train$imputations,
                     function(x){
                       lm(Percent.Illegal ~ GGXWDG_NGDP + NGDP_RPCH,
                           data = x)
                     })

summary(eleph.fit2$imp1)
summary(eleph.fit2$imp2)
summary(eleph.fit2$imp3)
summary(eleph.fit2$imp4)
summary(eleph.fit2$imp5)

pFtest(eleph.fit1$imp2, eleph.fit2$imp1)


# CREATE GRAPHS OF INDICATOR MEANS
means <- read.csv("eleph_mean.csv", header = TRUE)
names(means)
mcols <- brewer.pal(4, "Spectral")
corrupt <- ggplot(means, aes(x=Country, y=Corruption.Control, color=Country))+
  geom_point(size=4) +
  ylim(-1, 1) +
  labs(y=NULL, x=NULL)
corrupt <- corrupt + theme_bw()
corrupt
svg("corrupt.svg")
corrupt
dev.off()
regq <- ggplot(means, aes(x=Country, y=Reg.Quality, color=Country))+
  geom_point(size=4) +
  ylim(-1, 1) +
  labs(y=NULL, x=NULL)
regq <- regq + theme_bw()
svg("regq.svg")
regq
dev.off()
illegal <- ggplot(means, aes(x=Country, y=Illegal, color=Country))+
  geom_point(size=4) +
  ylim(-1, 1) +
  labs(y=NULL, x=NULL)
illegal <- illegal + theme_bw()
svg("illegal.svg")
illegal
dev.off()
inflat <- ggplot(means, aes(x=Country, y=PCPI, color=Country))+
  geom_point(size=4) +
  ylim(-1, 1) +
  labs(y=NULL, x=NULL)
inflat <- inflat + theme_bw()
svg("inflat.svg")
inflat
dev.off()




