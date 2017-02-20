library(reporttools)
library(stargazer)
library(fpp)
library(xtable)
library(partykit)
library(Rgraphviz)
library(plm)
library(RWeka)
library(rJava)
library(caret)
library(formattable)
str(eleph.train)
# CHANGE VARIABLES TO FACTOR
eleph.train$Reg.Market <- factor(eleph.train)

# RUN MULTIVARIATE LINEAR REGRESSION 
names(eleph.train)
eleph.train.plm <- plm(Percent.Illegal ~ Corruption.Control +
                                       PCPI + GGXWDG_NGDP,
                                     index=c("country", "year"), model = "between",
                                     data = outTrain)
                               
summary(eleph.train.plm)
rmsePLM <- sqrt(1-(0.54929 * sd(outTrain$Percent.Illegal)))
predPLM <- predict(eleph.train.plm, newdata = outTest)
measurePLM <- postResample(predPLM, outTest$Percent.Illegal)

# CREATE AND SUBSET DATASET FROM AMELIA OUTPUT
write.amelia(obj=elephants.out.train, file.stem = "outTrain")
outTrain <- read.csv("outTrain1.csv", header = TRUE)

write.amelia(obj=elephants.out.test, file.stem = "outTest")
outTest <- read.csv("outTest1.csv", header = TRUE)
names(outTrain)
str(outTrain)
outTrain$Reg.Market <- as.numeric(outTrain$Reg.Market)
outTestSub$Reg.Market <- as.numeric(outTestSub$Reg.Market)
outTrain$Unreg.Market <- as.numeric(outTrain$Unreg.Market)
outTrain$Reg.Market.Bordering <- as.numeric(outTrain$Reg.Market.Bordering)
outTrain$Unreg.Market.Bordering <- as.numeric(outTrain$Unreg.Market.Bordering)
outTrain$Armed.Conflict <- as.numeric(outTrain$Armed.Conflict)
outTrain$Non.State.Conflict <- as.numeric(outTrain$Non.State.Conflict)
outTrain$High.Illegal <- as.numeric(outTrain$High.Illegal)

outTrainSub <- subset(outTrain, subset=TRUE, select=c(10:15,17:35,37:40,42:43,46))

outTestSub <- subset(outTest, subset=TRUE, select=c(10:15,17:35,37:40,42:43,46))
str(outTestSub)



# RUN MODEL TREE ALGORITHM
library(doMC)
registerDoMC(cores = 3)

set.seed(100)
fitControl <- trainControl(method = "cv", number = 10,
                           returnData = TRUE,
                           savePredictions = TRUE)
library(RWeka)
m5train <- M5P(Percent.Illegal ~ ., data = outTrainSub)
m5rulesTrain <- M5Rules(Percent.Illegal ~ ., data = outTrainSub)

summary(m5train)
measure1M5 <- postResample(m5train$predictions, outTrainSub$Percent.Illegal)
predM5 <- predict(m5train, newdata = outTestSub)
measureM5 <- postResample(predM5, outTestSub$Percent.Illegal)



# RUN GBM TREE MODEL
set.seed(100)
library(gbm)
modelGBM <- gbm(Percent.Illegal ~ ., data = outTrainSub,
                distribution = "gaussian",
                n.trees = 1000,
                n.minobsinnode = 2,
                cv.folds = 10,
                shrinkage = 0.001)

measure1GBM <- postResample(modelGBM$fit, outTrainSub$Percent.Illegal)
predGBM <- predict.gbm(modelGBM, newdata = outTestSub,
                       n.trees = 1000)
measureGBM <- postResample(predGBM, outTestSub$Percent.Illegal)
modelGBM
summary(modelGBM)


# RUN RANDOM FOREST MODEL
set.seed(100)
modelRF <- train(Percent.Illegal ~ ., data=outTrainSub,
                 method = "rf",
                 trControl=fitControl,
                 ntrees=1000,
                 allowParallel=TRUE,
                 prox=TRUE,
                 importance=TRUE)
modelRF
predRF <- predict(modelRF, newdata = outTestSub)
corRF <- cor(predRF, outTestSub$Percent.Illegal, method = "spearman")
png("RFvarimp.png", height = 680, width = 680)
plot(varImp(modelRF))
dev.off()
measureRF <- postResample(predRF, outTestSub$Percent.Illegal)

# RUN NEURAL NETWORK MODEL
nnetGrid <- expand.grid(.decay = c(0, 0.01, 0.1),
                        .size = c(1:10),
                        .bag = FALSE)
set.seed(100)
modelNN <- train(Percent.Illegal ~ ., data = outTrainSub,
                 method = "avNNet",
                 tuneGrid = nnetGrid,
                 trControl = fitControl,
                 lineout = TRUE,
                 trace = FALSE,
                 MaxNWts = 10 + (ncol(outTrainSub) + 1) + 10 + 1,
                 maxit = 500,
                 importance=TRUE)

predNN <- predict(modelNN, newdata = outTestSub)
measureNN <- postResample(predNN, outTestSub$Percent.Illegal)
modelNN
plot(varImp(modelNN))

# RUN CUBIST MODEL
library(Cubist)
set.seed(100)
modelCubist <- train(Percent.Illegal ~ ., data = outTrainSub,
                     method = "cubist",
                     importance = TRUE, 
                     trControl = fitControl)

predCubist <- predict(modelCubist, newdata = outTestSub)
measureCubist <- postResample(predCubist, outTestSub$Percent.Illegal)
modelCubist
plot(varImp(modelCubist))



# RUN MARS MODEL
library(earth)
set.seed(100)
earthMars <- earth(Percent.Illegal ~ ., data = outTrainSub)

MarsRMSE <- sqrt(1 - (0.6888798 * sd(outTrainSub$Percent.Illegal)))
predMars <- predict(earthMars, newdata = outTestSub)
measureMars <- postResample(predMars, outTestSub$Percent.Illegal)
spCorMars <- cor(predMars, outTestSub$Percent.Illegal, method = "spearman")
earthMars
evimp(earthMars)

# CREATE TABLE OF RESULTS
ModelName <- c("Pooled LM", "Model Tree", "GBM", "Random Forest",
               "Neural Networks", "Cubist", "MARS")
ModelRMSE <- c(0.665, 0.785, 0.855, 0.780, 0.929, 0.881, 0.548)
ModelRSq <- c(0.549, 0.400, 0.423, 0.438, 0.293, 0.290, 0.689)
TestRMSE <- c(0.874, 0.945, 0.853, 0.745, 0.947, 0.856, 1.107)
TestRSq <- c(0.218, 0.206, 0.229, 0.411, 0.123, 0.268, 0.288)
Results <- cbind(ModelName, ModelRMSE, ModelRSq, TestRMSE, TestRSq)

