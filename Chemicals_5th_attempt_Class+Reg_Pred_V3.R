#0. Libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(party)
library(rpart)
library(rpart.plot)
library(fastDummies)
library(ggforce)
library(vcvComp)
library(ggfortify)
library(MASS)
library(klaR)
library(plyr)
library(e1071)
library(rgl)
library(summarytools)
library(psych)
library(caret)
library(car)
library(scales)  
library(reshape2)

########################
#1. Setting the workspace
#########################
setwd("C:/Users/Administrador/OneDrive/Documentos/Master degree/1. Statistical Machine Learning/Spring Semester/Group Project/Assesed Coursework/Chemicals/Data")

########################
#2. Making prediction on testing set
#########################

#2.1 Reading the data
training.data <- as.data.frame(read.csv("training_set.csv"))
validation.data <- as.data.frame(read.csv("test_set.csv"))

  ############################################################################
  #    APPLYING THE CLASIFICATION MODEL - Predicting Impurity Type           #
  ###########################################################################
  
  #######################
  #2.1 Cluster 1: Group A
  # Model: Linear SVM
  #Feature engineering
  Resp_variable <- factor(ifelse(training.data$Impurity.Type %in% c("A"),"A","O"))
  train.data <- cbind(training.data,Resp_variable)
  #Modeling
  weightss <- max(table(train.data$Resp_variable)) / table(train.data$Resp_variable)
  model.Imp.Type <- svm(Resp_variable ~ I + II, data = train.data, kernel = "linear", cost = 1, class.weights = weightss)
  #Predicting
  pred.Imp.Type <- predict(model.Imp.Type, validation.data)
  #Assigning the prediction
  index <- as.numeric(rownames(validation.data))
  val.data <- cbind(validation.data, index, pred.Imp.Type)
  val.data$pred.Imp.Type <- replace(val.data$pred.Imp.Type, val.data$pred.Imp.Type == "O", NA)
  names(val.data)[names(val.data) == 'pred.Imp.Type'] <- 'pred.Imp.Type.C1'
  
  
  #######################
  #2.3 Cluster 2: Group E
  # Model: Linear SVM
  #Feature engineering
  train.data <- filter(training.data, !(Impurity.Type %in% c("A")))
  train.data$Resp_variable <- factor(ifelse(train.data$Impurity.Type %in% c("E"),"E","O"))
  #Modeling
  weightss <- max(table(train.data$Resp_variable)) / table(train.data$Resp_variable)
  model.Imp.Type <- svm(Resp_variable ~ III + Temp, data = train.data, kernel = "linear", cost = 0.1, class.weights = weightss)
  #Predicting
  validation.data <- filter(val.data, !(pred.Imp.Type.C1 %in% c("A")))
  rownames(validation.data) <- rownames(val.data)[!(val.data$pred.Imp.Type.C1 %in% c("A"))]
  validation.data <- subset(validation.data, select= c("index","I","II","III","IV","V","Temp"))
  pred.Imp.Type <- data.frame(predict(model.Imp.Type, validation.data))
  #nrow(pred.Imp.Type)
  #Assigning the prediction
  pred.Imp.Type$predict.model.Imp.Type..validation.data. <- replace(pred.Imp.Type$predict.model.Imp.Type..validation.data., pred.Imp.Type$predict.model.Imp.Type..validation.data. == "O", NA)
  names(pred.Imp.Type)[names(pred.Imp.Type) == 'predict.model.Imp.Type..validation.data.'] <- 'pred.Imp.Type.C2'
  index <- as.numeric(rownames(pred.Imp.Type))
  pred.Imp.Type <- cbind(index,pred.Imp.Type)
  val.data <- merge(x=val.data, y=pred.Imp.Type[,c("index","pred.Imp.Type.C2")], by="index", all.x = T)
  rownames(val.data) <- val.data$index
  val.data$pred.Imp.Type <- ifelse(!is.na(val.data$pred.Imp.Type.C1),as.character(val.data$pred.Imp.Type.C1),as.character(val.data$pred.Imp.Type.C2))
  
  
  #######################
  #2.3 Cluster 3: Group B&H
  # Model: Linear SVM
  #Feature engineering
  train.data <- filter(training.data, !(Impurity.Type %in% c("A","E")))
  train.data$Resp_variable <- factor(ifelse(train.data$Impurity.Type %in% c("B","H"),"BH","O"))
  #Modeling
  weightss <- max(table(train.data$Resp_variable)) / table(train.data$Resp_variable)
  model.Imp.Type <- svm(Resp_variable ~ IV + II, data = train.data, kernel = "linear", cost = 0.1, class.weights = weightss) #Accuracy : 1; 95% CI : (0.9526, 1); Kappa : 1
  #Predicting
  validation.data <- filter(val.data, !(pred.Imp.Type %in% c("A","E")))
  #nrow(validation.data)
  rownames(validation.data) <- rownames(val.data)[!(val.data$pred.Imp.Type %in% c("A","E"))]
  validation.data <- subset(validation.data, select= c("index","I","II","III","IV","V","Temp"))
  pred.Imp.Type <- data.frame(predict(model.Imp.Type, validation.data))
  #Assigning the prediction
  pred.Imp.Type$predict.model.Imp.Type..validation.data. <- replace(pred.Imp.Type$predict.model.Imp.Type..validation.data., pred.Imp.Type$predict.model.Imp.Type..validation.data. == "O", NA)
  #nrow(pred.Imp.Type)
  names(pred.Imp.Type)[names(pred.Imp.Type) == 'predict.model.Imp.Type..validation.data.'] <- 'pred.Imp.Type.C3'
  index <- as.numeric(rownames(pred.Imp.Type))
  pred.Imp.Type <- cbind(index,pred.Imp.Type)
  val.data <- merge(x=val.data, y=pred.Imp.Type[,c("index","pred.Imp.Type.C3")], by="index", all.x = T)
  rownames(val.data) <- val.data$index
  #nrow(val.data)
  val.data$pred.Imp.Type <- ifelse(!is.na(val.data$pred.Imp.Type),as.character(val.data$pred.Imp.Type),as.character(val.data$pred.Imp.Type.C3))
  
  
  #######################
  #2.3.1 SubCluster 3.1: Split B from H
  # Model: Radial SVM
  #Feature engineering
  train.data <- filter(training.data, (Impurity.Type %in% c("B","H")))
  train.data$Resp_variable <- factor(train.data$Impurity.Type)
  #Modeling - No weights
  model.Imp.Type <- svm(Resp_variable ~ V + II, data = train.data, kernel = "radial", cost = 1, gamma = 0.1) 
  #Predicting
  validation.data <- filter(val.data, pred.Imp.Type %in% c("BH"))
  rownames(validation.data) <- rownames(val.data)[val.data$pred.Imp.Type %in% c("BH")]
  validation.data <- subset(validation.data, select= c("index","I","II","III","IV","V","Temp"))
  pred.Imp.Type <- data.frame(predict(model.Imp.Type, validation.data))
  #Assigning the prediction
  pred.Imp.Type$predict.model.Imp.Type..validation.data. <- replace(pred.Imp.Type$predict.model.Imp.Type..validation.data., pred.Imp.Type$predict.model.Imp.Type..validation.data. == "O", NA)
  names(pred.Imp.Type)[names(pred.Imp.Type) == 'predict.model.Imp.Type..validation.data.'] <- 'pred.Imp.Type.C4'
  index <- as.numeric(rownames(pred.Imp.Type))
  pred.Imp.Type <- cbind(index,pred.Imp.Type)
  #nrow(pred.Imp.Type)
  val.data <- merge(x=val.data, y=pred.Imp.Type[,c("index","pred.Imp.Type.C4")], by="index", all.x = T)
  rownames(val.data) <- val.data$index
  val.data$pred.Imp.Type <- replace(val.data$pred.Imp.Type, val.data$pred.Imp.Type == "BH", NA)
  #nrow(val.data)
  val.data$pred.Imp.Type <- ifelse(!is.na(val.data$pred.Imp.Type),as.character(val.data$pred.Imp.Type),as.character(val.data$pred.Imp.Type.C4))
  
  
  #######################
  #2.4 Cluster 4: Group c("C","D","F","G","J","K","L","M","N")
  # Model: Regularized Linear Discriminant (RDA)
  #Feature engineering
  train.data <- filter(training.data, (Impurity.Type %in% c("C","D","F","G","J","K","L","M","N")))
  train.data$Resp_variable <- factor(train.data$Impurity.Type)
  #Modeling
  model.Imp.Type <- rda(Resp_variable ~ I + II + III + IV + V, train.data, gamma = 0.03448276, lambda = 0.03448276) 
  #Predicting
  validation.data <- filter(val.data, !(pred.Imp.Type %in% c("A","E","B","H")))
  #nrow(validation.data)
  validation.data <- subset(validation.data, select= c("index","I","II","III","IV","V","Temp"))
  pred.Imp.Type <- predict(model.Imp.Type, validation.data)
  pred.Imp.Type <- as.data.frame(pred.Imp.Type$class)
  #Assigning the prediction
  rownames(pred.Imp.Type) <- validation.data$index
  names(pred.Imp.Type)[names(pred.Imp.Type) == 'pred.Imp.Type$class'] <- 'pred.Imp.Type.C5'
  index <- as.numeric(rownames(pred.Imp.Type))
  pred.Imp.Type <- cbind(pred.Imp.Type,index)
  #nrow(pred.Imp.Type)
  val.data <- merge(x=val.data, y=pred.Imp.Type[,c("index","pred.Imp.Type.C5")], by="index", all.x = T)
  rownames(val.data) <- val.data$index
  #nrow(val.data)
  val.data$pred.Imp.Type <- ifelse(!is.na(val.data$pred.Imp.Type),as.character(val.data$pred.Imp.Type),as.character(val.data$pred.Imp.Type.C5))
  
  
  ############################################################################
  #              REGRESSION MODEL- Impurity Percent                         #
  ###########################################################################
  
  #Cluster 1 - MD
  subset <- filter(training.data, Impurity.Type %in% c("M","D"))
  model.Imp.Perc <- lm(Impurity.Percent ~ II, data = subset)
  pred.Imp.Perc.MD <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("M","D"),
                             predict(model.Imp.Perc,val.data),
                             NA))
  
  #Cluster 2 - BH
  subset <- filter(training.data, Impurity.Type %in% c("B","H"))
  model.Imp.Perc <- lm(Impurity.Percent ~ IV, data = subset)
  pred.Imp.Perc.BH <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("B","H"),
                             predict(model.Imp.Perc,val.data),
                             NA))
  
  #Cluster 3 - A
  subset <- filter(training.data, Impurity.Type %in% c("A"))
  model.Imp.Perc <- lm(Impurity.Percent ~ II, data = subset)
  pred.Imp.Perc.A <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("A"),
                             predict(model.Imp.Perc,val.data),
                             NA))
  
  #Cluster 4 - GF
  subset <- filter(training.data, Impurity.Type %in% c("G","F"))
  model.Imp.Perc <- lm(Impurity.Percent ~ IV, data = subset)
  pred.Imp.Perc.GF <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("G","F"),
                            predict(model.Imp.Perc,val.data),
                            NA))
  
  #Cluster 5 - K
  subset <- filter(training.data, Impurity.Type %in% c("K"))
  model.Imp.Perc <- lm(Impurity.Percent ~ V, data = subset)
  pred.Imp.Perc.K <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("K"),
                                                        predict(model.Imp.Perc,val.data),
                                                        NA))

  #Cluster 6 - CE
  subset <- filter(training.data, Impurity.Type %in% c("C","E"))
  model.Imp.Perc <- lm(Impurity.Percent ~ V, data = subset)
  pred.Imp.Perc.CE <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("C","E"),
                                                        predict(model.Imp.Perc,val.data),
                                                        NA))
  
  #Cluster 7 - NJ
  subset <- filter(training.data, Impurity.Type %in% c("N","J"))
  model.Imp.Perc <- lm(Impurity.Percent ~ I, data = subset)
  pred.Imp.Perc.NJ <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("N","J"),
                                                        predict(model.Imp.Perc,val.data),
                                                        NA))
  
  #Cluster 8 - L
  subset <- filter(training.data, Impurity.Type %in% c("L"))
  model.Imp.Perc <- lm(Impurity.Percent ~ II, data = subset)
  pred.Imp.Perc.L <- data.frame(pred.Imp.Perc = ifelse(val.data$pred.Imp.Type %in% c("L"),
                                                       predict(model.Imp.Perc,val.data),
                                                       NA))
  pred.Imp.Perc <- cbind(pred.Imp.Perc.MD,
                         pred.Imp.Perc.BH,
                         pred.Imp.Perc.A,
                         pred.Imp.Perc.GF,
                         pred.Imp.Perc.K,
                         pred.Imp.Perc.CE,
                         pred.Imp.Perc.NJ,
                         pred.Imp.Perc.L)
  colnames(pred.Imp.Perc) <- make.names(colnames(pred.Imp.Perc), unique = TRUE)
  
  pred.Imp.Perc <- pred.Imp.Perc %>%
    mutate(pred.Imp.Percent = do.call(coalesce, .))
  
  #We need to replace for Zero if the Impurity is greater than 1.8 percent
  pred.Imp.Perc$y.hat <- ifelse(pred.Imp.Perc$pred.Imp.Percent < 1.8, 0, pred.Imp.Perc$pred.Imp.Percent)
  
  rownames(pred.Imp.Perc) <- val.data$index
  pred.Imp.Perc$index <- as.numeric(rownames(pred.Imp.Perc))
  
  
  ############################################################################
  #    MODELS INTEGRATION - Impurity Percent and Impurity Type             #
  ###########################################################################
  
  val.data <- merge(x=val.data, y=pred.Imp.Perc[,c("index","y.hat")], by="index", all.x = T)

  #We need to replace with "X" if the Impurity Type is pure (Impurity.Percent < 1.8)
  val.data$g.hat <- ifelse(val.data$y.hat == 0, "X", val.data$pred.Imp.Type)
  
  #Importing the test data to upload it to Moodle
  predictions <- subset(val.data, select= c("g.hat","y.hat"))
  write.csv(predictions, file = "chemical_predictions_group_C_week_5_v2.csv", row.names=FALSE)
  
