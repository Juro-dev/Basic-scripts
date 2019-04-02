#Julian
#version:1.0
#Date: 23. Jan. 2019

# Importing data ####
rm(list = ls())
setwd("~/UJIndoorLoc")
Daten <- read.csv("trainingData.csv", 
                  sep=",", dec = ".", stringsAsFactors=FALSE)

Valid <- read.csv("validationData.csv", 
                  sep=",", dec = ".", stringsAsFactors=FALSE)

# Inspecting data ####
str(Daten[,500:529])
summary(Daten[,500:529])
View(Daten[500:530])
is.na.data.frame(Daten)
sum(is.na.data.frame(Daten))
"%notin%" <- Negate("%in%")

# Cleaning data / Pre-processing ####
# Timestamp conversion
Daten$TIMESTAMP <- as.POSIXct(Daten$TIMESTAMP, origin="1970-01-01")

Daten1 <- Daten

# Removing duplicate rows
Daten2 <- unique(Daten1)

# Removing "dead" WAPs
allVar <- nearZeroVar(Daten2[,1:ncol(Daten2)], saveMetrics = TRUE)
Daten4 <- Daten2[, -which(allVar$zeroVar == TRUE)]

Daten6 <- Daten4[,1:(ncol(Daten4) - 9)]

# Converting "dead" and near-"dead" signals for better ordering
Daten6[Daten6 == 100] <- -105
Daten6[Daten6 <= -90] <- -105

# Removing "fishy" rows with -30 or below
Daten6 <- Daten6[which(apply(Daten6[1:nrow(Daten6),1:ncol(Daten6)], 1, function(x) (max(x, na.rm = TRUE)) <= -30)),]
Daten6 <- Daten6[,which(apply(Daten6[,1:ncol(Daten6)], 2, function(x) (max(x, na.rm = TRUE)) >= -90))]
Daten7 <- cbind(Daten6, Daten4[which(rownames(Daten4) %in% rownames(Daten6)),466:474])

# Unify Validation and Training set WAPs
allVar2 <- nearZeroVar(Valid[,1:ncol(Valid)], saveMetrics = TRUE)
Valid3 <- Valid[, -which(allVar2$zeroVar == TRUE)]

Valid4 <- Valid3[,1:(ncol(Valid3) -9)]
Valid4[Valid4 == 100] <- -105
Valid4[Valid4 <= -90] <- -105

Valid4 <- Valid4[which(apply(Valid4[1:nrow(Valid4),1:ncol(Valid4)], 1, function(x) (max(x, na.rm = TRUE)) <= -30)),]
Valid4 <- Valid4[,which(apply(Valid4[,ncol(Valid4)], 2, function(x) (max(x, na.rm = TRUE)) >= -90))]
Valid4 <- cbind(Valid4, Valid3[which(rownames(Valid3) %in% rownames(Valid4)),368:376])

Valid4 <- Valid4[,colnames(Valid4) %in% colnames(Daten7)]
Daten8 <- Daten7[,colnames(Daten7) %in% colnames(Valid4)]

# Factorization of BUILDINGID and FLOOR
source_gist("https://gist.github.com/mrdwab/6424112")
Daten8$BUILDINGID <- as.factor(Daten8$BUILDINGID)
Daten9 <- Daten8 %>% mutate(BUILDINGID = paste0("build", BUILDINGID))
Daten9$BUILDINGID <- as.factor(Daten9$BUILDINGID)
Daten9$FLOOR <- as.factor(Daten9$FLOOR)

Valid5 <- Valid4
Valid5$BUILDINGID <- as.factor(Valid5$BUILDINGID)
Valid5 <- Valid5 %>% mutate(BUILDINGID = paste0("build", BUILDINGID))
Valid5$BUILDINGID <- as.factor(Valid5$BUILDINGID)
Valid5$FLOOR <- as.factor(Valid5$FLOOR)

Daten11 <- Daten9
Valid7 <- Valid5

# Check maximum values for longitude and latitude in training and test sets
min(Daten11$LONGITUDE)
min(Valid7$LONGITUDE)
max(Daten11$LATITUDE)
max(Valid7$LATITUDE)

#remove all rows where LONGITUDE values from test set are higher than in train set
Valid7<-Valid7[!(Valid7$LONGITUDE < -7691.338), ] # avoid hard code!

# Modelling ####

# Cross-validation
ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  number = 10
)

                                                                                ### Predicting Buildings ###
trainSample <- stratified(Daten11, "BUILDINGID",  0.3)
knnFit <- train(
  BUILDINGID~ .,
  data = trainSample[,c(1:(ncol(trainSample) - 9),291)],
  method = "knn",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)

rf <- train(
  BUILDINGID ~ .,
  data = trainSample[,c(1:(ncol(trainSample) - 9),291)],
  method = "rf",
  preProc = c("center", "scale"),
  ntree = 350,
  trControl = ctrl,
  importance = TRUE
)
                                                                                ### Predicting Floors ###
trainSample <- stratified(Daten11, "FLOOR",  0.5)
knnFit1 <- train(
  FLOOR~ .,
  data = trainSample[,c(1:(ncol(trainSample) - 9),290)],
  method = "knn",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)

trainSample <- stratified(Daten11, "FLOOR",  0.05)
rfFit1 <- train(
  FLOOR ~ .,
  data = trainSample[,c(1:(ncol(trainSample) - 9),290)],
  method = "rf",
  preProc = c("center", "scale"),
  ntree = 350,
  trControl = ctrl,
  importance = TRUE
)

trainSample <- stratified(Daten11, "FLOOR",  0.3)
kknnFit1 <- train(
  FLOOR~ .,
  data = trainSample[,c(1:(ncol(trainSample) - 9),290)],
  method = "kknn",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)
                                                                                ### Predicting Latitude ###
inTrain <- caret::createDataPartition(
  y = Daten11$LATITUDE,
  p = .5,
  list = FALSE)
training <- Daten11[inTrain,]

knnFit2 <- train(
  LATITUDE~ .,
  data = training[,c(1:(ncol(trainSample) - 9),289)],
  method = "knn",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)

inTrain <- caret::createDataPartition(
  y = Daten11$LATITUDE,
  p = .3,
  list = FALSE)
training <- Daten11[inTrain,]

rfFit2 <- train(
  LATITUDE~ .,
  data = training[,c(1:(ncol(trainSample) - 9),289)],
  method = "rf",
  preProc = c("center", "scale"),
  trControl = ctrl
  metric = "Accuracy"
)

inTrain <- caret::createDataPartition(
  y = Daten11$LATITUDE,
  p = .3,
  list = FALSE)
training <- Daten11[inTrain,]

svmFit2 <- train(
  LATITUDE~ .,
  data = training[,c(1:(ncol(trainSample) - 9),289)],
  method = "svmLinear",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)

                                                                ### Predicting Longitude ###
inTrain <- caret::createDataPartition(
  y = Daten11$LONGITUDE,
  p = .5,
  list = FALSE)
training <- Daten11[inTrain,]

knnFit3 <- train(
  LONGITUDE~ .,
  data = training[,c(1:(ncol(trainSample) - 9),288)],
  method = "knn",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)

inTrain <- caret::createDataPartition(
  y = Daten11$LONGITUDE,
  p = .1,
  list = FALSE)
training <- Daten11[inTrain,]

kknnFit3 <- train(
  LONGITUDE~ .,
  data = training[,c(1:(ncol(trainSample) - 9),288)],
  method = "kknn",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)

inTrain <- caret::createDataPartition(
  y = Daten11$LONGITUDE,
  p = .1,
  list = FALSE)
training <- Daten11[inTrain,]

rfFit3 <- train(
  LONGITUDE~ .,
  data = training[,c(1:(ncol(trainSample) - 9),288)],
  method = "rf",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)

inTrain <- caret::createDataPartition(
  y = Daten11$LATITUDE,
  p = .3,
  list = FALSE)
training <- Daten11[inTrain,]

svmFit3 <- train(
  LATITUDE~ .,
  data = training[,c(1:(ncol(trainSample) - 9),288)],
  method = "svmLinear",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Accuracy"
)
                                                                      ### KNN Results ###
knnFit              #  k  Accuracy   Kappa 5  0.9930293  0.9891806
knnFit1             #  k  Accuracy   Kappa 5  0.9709903  0.9619154
knnFit2             #  k  RMSE      Rsquared   MAE  5   8.812292  0.9822420  3.844518
knnFit3             #  k  RMSE      Rsquared   MAE  5  12.80418  0.9891491  4.843861

                                                                      ### Making Predictions ###
# Predictions for BUILDINGID
knnPreds <- predict(knnFit, newdata = Valid7)

# Predictions for FLOOR
knnPreds1 <- predict(knnFit1, newdata = Valid7)
rfPreds1 <- predict(rf2, newdata = Valid7)
kknnPreds1 <- predict(kknnFit1, newdata = Valid7)

# Predictions for LATITUDE
knnPreds2 <- predict(knnFit2, newdata = Valid7)
rfPreds2 <- predict(rfFit2, newdata = Valid7)
svmPreds2 <- predict(svmFit2, newdata=Valid7)

# Predictions for LONGITUDE
knnPreds3 <- predict(knnFit3, newdata = Valid7)
rfPreds3 <- predict(rfFit3, newdata = Valid7)

# Deriving accuracy of predictions from validation set
confusionMatrix(data = knnPreds, Valid7$BUILDINGID) # Accuracy : 0.9432

confusionMatrix(data = knnPreds1, Valid7$FLOOR)     # Accuracy : 0.78  
confusionMatrix(data = rfPreds1, Valid7$FLOOR)      # Accuracy : 0.7737  
confusionMatrix(data = kknnPreds1, Valid7$FLOOR)

postResample(pred = knnPreds2, obs = Valid7$LATITUDE) # RMSE   Rsquared MAE 28.2291242  0.8424672 13.0312823 
postResample(pred = svmPreds2, obs = Valid7$LATITUDE)
postResample(pred = rfPreds2, obs = Valid7$LATITUDE)

postResample(pred = knnPreds3, obs = Valid7$LONGITUDE) # RMSE  Rsquared   MAE 41.155449  0.886864 17.532192 
postResample(pred = rfPreds3, obs = Valid7$LONGITUDE)

# Creating new datasets for plotting original and predicted positions
ValidPreds <- cbind(Valid7, knnPreds2, knnPreds3)
ValidPreds1 <- cbind(Valid7, knnPreds1, knnPreds2, knnPreds3)
ValidPreds1$FLOOR <- ValidPreds1$knnPreds1
ValidPredsrf <- cbind(Valid7, knnPreds, rfPreds1, rfPreds2, rfPreds3)
ValidPredssvm <- cbind(Valid7, knnPreds, svmPreds1, svmPreds2, svmPreds3)

# Plotting ####
p1 <- plot_ly(data = Valid7, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR)
p2 <- plot_ly(data = ValidPreds1, x = ~knnPreds3, y = ~knnPreds2, z = ~FLOOR)
subplot(p1, p2)

# Plot real and predicted results
ggplot() +
  geom_point(data = ValidPreds , aes(x = knnPreds3, y = knnPreds2, colour = "Predictions - KNN")) +
  geom_point(data = Valid7 , aes(x = LONGITUDE, y = LATITUDE, colour = "Real values")) +
  ggtitle("Log In Locations") +
  facet_wrap(~FLOOR)

# Distribution of distance error (in meters) ####
Error = sqrt((mean(ValidPredsrf$rfPreds3) - mean(Valid7$LONGITUDE))^2 + (mean(ValidPredsrf$rfPreds2) - mean(Valid7$LATITUDE))^2)
hist(Error, freq = T, xlab = " Absolute error (m)", col = "red", main = "Error distance in meters")