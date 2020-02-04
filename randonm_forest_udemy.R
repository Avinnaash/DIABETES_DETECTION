                        
                        #INSTALL THE PACKAGE

installed.packages('randomForest')
installed.packages('randomForeest')
library('randomForest')
library(caret)

                         #load the dataset

data<-read.csv('C:/Users/91977/Desktop/diabetes.csv')
data
set.seed(2)
#rnorm(3)

               #DATA SLICING INTO TRAING AND TESTING PARTS

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
ind
data_train<-data[ind==1,]
data_test<-data[ind==2,]

                #CONVERTING THIS TARGET VARIABLE IN FACTOR

data$Outcome<-as.factor(data$Outcome)
data_train$Outcome<-as.factor(data_train$Outcome)
data$Outcome
                          
                      #ALL TO TRAIN THE MODEL

#Starting with the default value of mtry, search for the optimal value 

#(with respect to Out-of-Bag error estimate) of mtry for randomForest.

#TUNERF<-This function is a specific utility to tune the mtry parameter based on OOB error, 

#which is helpful when you want a quick & easy way to tune your modeL.

bestmtry<-tuneRF(data_train,data_train$Outcome,stepFactor = 1.2,improve = .01,trace = T,plot = T)
#OOB is the error which shows the how much different are your actual and the predicted values.


library(randomForest)
data_forest<-randomForest(Outcome~.,data=data_train)
data_forest


                          #to check the importance

importance(data_forest)

varImpPlot(data_forest)

                            #READY FOR PREDICTION


pred_diabetic<-predict(data_forest,newdata = data_test,type = 'class')
pred_diabetic

                                 #VALIDATE


confusionMatrix(table(pred_diabetic,data_test$Outcome))
