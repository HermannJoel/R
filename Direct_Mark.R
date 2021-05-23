#Project 3 Direct Marketing
#Problem statement: Targeting customers requires resources and cost. the company want 
#only those who are most likely to be interested in a offer. Targeting only the potential customers 
#will protect the brand image and save resources and cost by not spamming those who doesn't whant to be contacted
#Goal: The goil is to predict if a given client will subscribe to a term deposit 
#Data: This data is related wit direct marketing campaigns of a portuguese institution
library(dplyr)
library(skimr)
library(mice)
library(caret)
bank_data <- read.csv(file.choose(), sep = ',')
str(bank_data)
summary(bank_data)
head(bank_data)
dim(bank_data)
#Rename y target
bank_data %>% 
  rename(
    subscribed=y)#with dplyr
table(bank_data$y)
colnames(bank_data)[colnames(bank_data) == 'y'] <- 'suscribed'
#EDA
#check na
skimmed <- skim(bank_data)
View(skimmed)

mice::md.pattern(bank_data)

table(bank_data$suscribed)
table(bank_data$suscribed)/NROW(bank_data$suscribed)*100
#Almost 89 percent of the data are did not churn, So the data set is quite imbalanced
sapply(bank_data, function(x){unique(x) %>% NROW()})#To se unique values

#Distribution of other variables
table(bank_data$housing)#most of clients own they house
table(bank_data$contact)
#chi square test for categorical variables
chisq.test(y=bank_data$suscribed, x=bank_data$housing)
chisq.test(y=bank_data$suscribed, x=bank_data$contract)
#Anova for cat vs num
summary(aov(duration~suscribed, data=bank_data))
summary(aov(age~suscribed, data=bank_data))

#Boxplot
featurePlot(x=bank_data[, c(1, 11:12, 16, 19)], y=factor(bank_data$suscribed), 
            plot='box',
            strip=strip.custom(par.strip.text=list(cex=.6)),
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))

#Density plot
featurePlot(x=bank_data[, c(1, 11:12, 16, 19)], y=factor(bank_data$suscribed), 
            plot='density',
            strip=strip.custom(par.strip.text=list(cex=.6)),
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))

#Scatter plot
featurePlot(x=bank_data[, c(1, 11:12 , 16, 19)], 
            y=as.numeric(bank_data$suscribed), 
            plot='scatter',
            strip=strip.custom(par.strip.text=list(cex=.6)))


#-------------------------------Split into training and test
#set.seed(88)
#trainRowNums <- createDataPartition(bank_data$suscribed, p=0.8, list=FALSE)
#train <- bank_data[trainRowNums, ]
#test <- bank_data[-trainRowNums,]
#dim(train)
#dim(test)
anyNA(bank_data)
set.seed(88)
data_split <- initial_split(bank_data, prop = .80)
train <- training(data_split)
test  <- testing(data_split)
dim(train)
dim(test)
#---------------------------------------------------Export the data as csv file
write.csv(train, 'D:\\DataBases\\DataSets\\R_programming\\bank_data_train.csv', row.names = F)
write.csv(test, 'D:\\DataBases\\DataSets\\R_programming\\bank_data_test.csv', row.names = F)

#5.------------------------BUILDING ML
#To reduce training time
train.temp <- train[1:10000, ]
#train the model
set.seed(88)
rf <- caret::train(subscribed ~ ., data=train, method = 'rf', ntree=700)
rf
fitted <- predict(rf)
plot(rf, main='Model accuracy with random forest')
#var importance
var.imp <- varImp(rf)
var.imp
#plot var imp
plot(var.imp, main='Feature importance with rf')
#rf$finalModel
#Make prediction
#finalModel:No preprocesing steps are applied before prediction 
#model:all preprocessed step done on the training set are applied on the test before making prediction
preds <- predict(rf$finalModel, test)
head(preds)
#class scores
prob <- predict(rf, test, type='prob')
prob
preds.prob <- predict(rf, test, type='prob')[, 1]
preds.prob

#Deciles tables
actuals <- ifelse(test$Revenue =='true', 1, 0)
df.probs <- data.frame(Act          =actuals,
                       prob         =preds.prob) %>% arrange(desc(prob))
df.probs$pred.decile <- c(rep(10:1, each=NROW(df.probs)%/%10), 
                          rep(1, NROW(df.probs)%%10))

outpout <- df.probs %>% group_by(pred.decile) %>%
  summarise(n_obs=length(Act), n_pos=sum(Act))%>%
  arrange(desc(pred.decile)) %>%
  mutate(capture_rate = round(cumsum(n_pos)/sum(n_pos), 3)*100)
outpout
#--------------------------------2nd Method
# Prepare % of Responders by Deciles:
bins <- quantile(preds.prob, probs = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
bins
binned <- cut(preds.prob, breaks = quantile(preds.prob, probs=seq(0, 1, .1), na.rm = T), 
              include.lowest = T, labels = 1:10)
actuals <- ifelse(test$Revenue == 'true', 1, 0)

df_prob <- data.frame(Act            = actuals, 
                       Prob           = preds.prob, 
                       Equal_Decile   = factor(binned), 
                       Natural_Decile = ceiling(preds.prob*10))

df_prob %>% group_by(Natural_Decile) %>% 
  summarise( n_obs=length(Act), n_pos = sum(Act)) %>% 
  arrange(-Natural_Decile) %>% 
  mutate(perc_cover = round(cumsum(n_pos)/sum(n_pos), 3) *100)
table(test$Revenue)
#If the company target only (246*3)= 738 observations of the entire dataset,
#They will be able to capture 89.9% of all the peoples who made a purchase(Revenue=1)
#cm
confusionMatrix(reference=test$Revenue, data=preds, mode='everything', positive='true')

#---------------------------------------------Hyper tuning parameters
modelLookup("earth")
#1.define the grid
marsGrid <- expand.grid(nprune=c(2, 3, 5, 8, 10, 12),
                        degree=c(1, 2, 3))
 
#Define the train control function
#2.Tune hyper parameters by setting tunegrid
fitControl <- trainControl(
  method = 'cv',#repeatedcv, oob, LOOCV, LGOCV
  number = 5,
  savePredictions = 'final',#save preds for optimal tuning param
  classProbs = T,#should class prob be returned
  summaryFunction =twoClassSummary#result summary function
)

set.seed(88)
TunedmarsGridModel <- train(Revenue ~., data=train, method="earth", metric="ROC",
                            tuneGrid=marsGrid, trControl=fitControl)

TunedmarsGridModel
#3.Predict and compute cm
predsGrid <- predict(TunedmarsGridModel, test)
confusionMatrix(reference=test$Revenue, data=predsGrid, mode='everything', positive='true')
#------------------------------------Evaluating Multiple ML Algorithms
#1
fitControl <- trainControl(
  method = 'cv',#k-fold cv.boot,boot632,optimism_boot,cv,repeatedcv,oob,LOOCV, LGOCV
  number = 5,                     
  savePredictions = 'final',
  classProbs = T,                 
  summaryFunction=twoClassSummary
) 

set.seed(88)
# Train the model using adaboost
adaboost = train(Revenue ~ ., data=train, method='adaboost', tuneLength=10, 
                 trControl = fitControl)
set.seed(88)
# Train the model using rf
rf = train(Revenue ~ ., data=train, method='rf', tuneLength=10, trControl = fitControl)
set.seed(88)
startTime <- Sys.time()
# Train the model using XGboost
xgbDART = train(Revenue ~ ., data=train, method='xgbDART', tuneLength=10, 
                trControl = fitControl, verbose=F)
endTime <- Sys.time()
timeTaken <- endTime - startTime
timeTaken

set.seed(88)
# Train the model using MARS
svmRadial = train(Revenue ~ ., data=train, method='svmRadial', tuneLength=15, trControl = fitControl)
# Compare the models with resamples
modelsCompare <- resamples(list(ADABOOST=adaboost, RF=rf, XGboost=xgbDART, SVM= svmRadial))
models_compare

# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)
#--------------------------------------Ensemble of Ensembles
#Ensembling the predictions
library(caretEnsemble)
# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

algorithmList <- c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial')
set.seed(88)
models <- caretList(Purchase ~ ., data=train, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
results

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
# Create the trainControl
set.seed(101)
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
# Predict on df_test
stack_preds <- predict(stack.glm, newdata=test)
head(stack_preds)