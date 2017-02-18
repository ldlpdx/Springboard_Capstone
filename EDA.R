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

# CREATE VARIABLES TO ASSESS LEVEL OF ILLEGAL KILLING OF ELEPHANTS
elephants$High.Illegal <- factor(with(elephants, ifelse((Percent.Illegal > 0.5), 1, 0)))
table(elephants$High.Illegal)


par(mfrow=c(1,1))
boxplot(NGDP_RPCH ~ country, data = elephants, col="coral2", horizontal=TRUE)
boxplot(Definite.Possible ~ country, data = elephants, col="coral2", horizontal=TRUE)
boxplot(Illegal.Carcasses ~ country, data = elephants, col="coral2", horizontal=TRUE)

p <- plot_ly(elephants, x = country, y = Resource.Depletion, color = region, type = "box") 
p
GGX_NGDP <- plot_ly(elephants, x = country, y = GGX_NGDP, color = region, type = "box") 
GGX_NGDP
GGXWDG_NGDP <- plot_ly(elephants, x = country, y = GGXWDG_NGDP, color = region, type = "box") 
GGXWDG_NGDP
GNI <- plot_ly(elephants, x = country, y = GNI, color = region, type = "box") 
GNI
HDI <- plot_ly(elephants, x = country, y = HDI, color = region, type = "box") 
HDI
International.Dev.Aid <- plot_ly(elephants, x = country, y = International.Dev.Aid, color = region, type = "box") 
International.Dev.Aid

pop.below.nat <- plot_ly(elephants, x = country, y = Pop.Below.National.Poverty, type = 'scatter', mode = 'markers', color = region, 
             marker = list(opacity = 0.8)) %>%
  layout(title = 'Percentage Below National Poverty Line')
pop.below.nat

pop.below.125 <- plot_ly(elephants, x = country, y = PPP.125.day, type = 'scatter', mode = 'markers', color = region, 
                         marker = list(opacity = 0.8)) %>%
  layout(title = 'Percentage Below National USD 1.25 per Day')
pop.below.125


library(gplots)


# SEPARATE DATA INTO REGIONS AND PLOT GOVERNANCE INDICATORS
elephantsFC <- filter(elephants, subregionid=="FC")
elephantsFW <- filter(elephants, subregionid=="FW")
elephantsFE <- filter(elephants, subregionid=="FE")
elephantsFS <- filter(elephants, subregionid=="FS")

round(mean(elephantsFC$PIKE.regional, na.rm = TRUE), digits = 2)
mean(elephantsFW$PIKE.regional, na.rm = TRUE)
mean(elephantsFE$PIKE.regional, na.rm = TRUE)
mean(elephantsFS$PIKE.regional, na.rm = TRUE)
round(mean(elephantsFC$Corruption.Control, na.rm = TRUE), digits = 2)
round(mean(elephantsFW$Corruption.Control, na.rm = TRUE), digits = 2)
round(mean(elephantsFE$Corruption.Control, na.rm = TRUE), digits = 2)
round(mean(elephantsFS$Corruption.Control, na.rm = TRUE), digits = 2)

png("governanceFC.png", height = 680, width = 680)
governanceFC <- ggplot(data=elephantsFC, aes(x=year))+
  geom_line(aes(y=Voice.Accountability), color="cadetblue3")+
  geom_line(aes(y=Political.Stability), color="coral")+
  geom_line(aes(y=Government.Effectiveness), color="aquamarine")+
  geom_line(aes(y=Rule.Law), color="gold")+
  geom_line(aes(y=Corruption.Control), color="palegreen")+
  geom_line(aes(y=Reg.Quality), color="darkorchid1")+
  ylim(-2, 0.5) + xlab(NULL) + ylab(NULL) +
  ggtitle("Governance indicators for Central Africa") + theme_bw()
governanceFC + facet_wrap(~country, ncol = 3)
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


# TRANSFORM THE DATA TO MULTIVARIATE NORMAL DISTRIBUTION
names(elephants)
centered.elephants <- data.frame(scale(elephants[,c(9:28,33:38,41:47)]))
centered.elephants <- cbind(elephants[,c(1:8, 29:32,39:40, 48)], centered.elephants)
str(centered.elephants)
names(centered.elephants)

centered.elephants$High.Illegal <- as.integer(centered.elephants$High.Illegal)



# EXAMINE DATA FOR HIGHLY CORRELATED VARIABLES
clrs <- brewer.pal(10, "Spectral")
cors <- cor(centered.elephants[,9:48], use = "pairwise")
quartz()
png("corrplot.png", height = 680, width = 680)
corr.plot <- corrplot.mixed(cors, col = clrs, number.cex=0.3,
                     tl.pos="lt", tl.cex=0.5, tl.col="black",
                     tl.srt=45)
dev.off()

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
names(centered.elephants)
pd.elephants <- subset(centered.elephants, subset=TRUE, select=c(1:22,24:28,30:34,36:48))
summary(pd.elephants)
# SEPARATE DATA INTO TRAINING AND TESTING SETS
inVal <- createDataPartition(pd.elephants$High.Illegal, p = 0.6, list=FALSE)
eleph.train <- pd.elephants[inVal,]
eleph.test <- pd.elephants[-inVal,]



# IMPUTE MISSING VALUES WITH A SINGLE IMPUTATION
names(eleph.train)
library(snow)
set.seed(1357)
elephants.out.train <- amelia(eleph.train, m=1, frontend = FALSE, 
                              idvars = c("ISO2", "ISO3", "region", 
                                         "subregionid", "cap.lat", "cap.long"),
                              ts = "year", cs = "country", 
                              polytime = 0, intercs = TRUE, p2s = 1, 
                              parallel="snow", ncpus = 3, 
                             empri = .01*nrow(centered.elephants))

elephants.out.test <- amelia(eleph.test, m=1, frontend = FALSE, 
                                                idvars = c("ISO2", "ISO3", "region", 
                                                           "subregionid", "cap.lat", "cap.long"),
                                                ts = "year", cs = "country", 
                                                polytime = 0, intercs = TRUE, p2s = 1, 
                                                parallel="snow", ncpus = 3, 
                                                empri = .15*nrow(centered.elephants))

# SPOT CHECK IMPUTATIONS
quartz()
par(mfrow=c(3,2))
tscsPlot(elephants.out.train, cs = "Kenya", main = "Kenya",
          var = "Resource.Depletion", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Botswana", main = "Botswana",
         var = "Resource.Depletion", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Cameroon", main = "Cameroon",
         var = "Definite.Possible", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Chad", main = "Chad",
         var = "Definite.Possible", ylim = c(-2,0))
tscsPlot(elephants.out.train, cs = "Zimbabwe", main = "zimbabwe",
         var = "Primary.ed.enrollment", ylim = c(-2,2))
tscsPlot(elephants.out.train, cs = "Congo Republic", main = "Congo Republic",
         var = "Primary.ed.enrollment", ylim = c(-2,2))
names(pd.elephants)
quartz()
par(mfrow=c(2,2))
plot(elephants.out.train, which.vars=c(20,42,45,46))





