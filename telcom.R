#Project 2/ Telcom customers churn prediction: Project overview
#Probleme statement:Customer churn, also known as customer retention, 
# customer turnover, or customer defection, is the loss of clients or customers
# Telephone service companies, Internet service providers, pay TV companies, insurance firms, 
# and alarm monitoring services, often use customer attrition analysis and customer attrition 
# rates as one of their key business metrics because the cost of retaining an existing customer 
# is far less than acquiring a new one

#Goal: Predict behavior to retain customers. 
# The business can analyze all relevant customer data and develop focused customer retention programs.
#Data:Each row represents a customer, each column contains customer’s attributes described on the column Metadata.
#-Customers who left within the last month – the column is called Churn
#The raw data contains 7043 rows (customers) and 21 columns (features).
#The “Churn” column is our target.
options(scipen = 999)#Enable any decimal point being rounded off as scientific notation to be schown as decimal point itself 
telcom <- read.csv(file.choose())
str(telcom)
summary(telcom)
sapply(telcom, function(x){unique(x) %>% NROW()})
#check the class distribution of target
#To look for a dominant category in any of the vaiables, If a category fill almost the entire
#variables, the chances of that variables to be usefull are slim. because far less 
#variation in that variable
table(telcom$gender)
table(telcom$MultipleLines)
table(telcom$OnlineSecurity)
table(telcom$Churn)
#Chi square test
#To determine if each of the x variables is going to be usefull in predicting the target
#Decision Rule: p-value<alpha -> statistically sig and likely be usefull in predicting the target
chisq.test(y=telcom$Churn, x=telcom$gender)#Not sig
chisq.test(y=telcom$Churn, x=telcom$MultipleLines)
chisq.test(y=telcom$Churn, x=telcom$OnlineSecurity)
chisq.test(y=telcom$Churn, x=telcom$PhoneService)#Not sig
chisq.test(y=telcom$Churn, x=telcom$Partner)#sig p-value<0.01

#ANOVA(for target(cat) vs num)
#To determine if continuous variables will be usefull in predicting target
#ANOVA gives a sense of whether the two category of the target have a #ce mean in the inputs var or not
#If there is a sig #ce of mean, the given input var is likely to be statistically sig 
summary(aov(MonthlyCharges  ~ Churn, data=telcom))
summary(aov(TotalCharges  ~ Churn, data=telcom))
summary(aov(tenure  ~ Churn, data=telcom))
#
boxplot(MonthlyCharges ~ Churn, data=telcom, main='MonthlyCharges~Churn')
boxplot(TotalCharges~ Churn, data=telcom, main='TotalCharges ~ Churn')
boxplot(tenure ~ Churn, data=telcom, main='tenure ~ Churn')
#Data preprocessing
num_vars <- c("MonthlyCharges", "TotalCharges", "tenure ")

for(column in colnames(telco.prep)){
  feature.col <- telco.prep[, column]
  if(!column %in% num.vars){
    levels.cat <- sort(unique(feature.col))
    telco.prep[, column] <- factor(tolower(as.character(telco.prep[, column])), 
                             levels = tolower(as.character(levels.cat)))
  }else{
    telco.prep[, column] <- as.numeric(telco.prep[, column])
  }
}
#To Drop variables
#drop.var <- c("ExtraCharges")
#telco.prep <- telco.prep [,!(names(telco.prep) %in% drop.var)]

head(telco.prep, n=2)
#To change levels of a category
telco.prep$Churn <- relevel(telco.prep$Churn, "No")#Specify no as second parameter
#
telco.prep$SeniorCitizen <- as.factor(telco.prep$SeniorCitizen)
str(telco.prep)
#
#Split data into training and test
set.seed(88)
trainRowNums <- createDataPartition(telco.prep$Churn, p=0.8, list=FALSE)
train <- telco.prep[trainRowNums,]
test <- telco.prep[-trainRowNums,]
dim(train)
dim(test)
#Export the data as csv file
write.csv(train, 'telco_train.csv', row.names = F)
write.csv(test, 'telco_test.csv', row.names = F)

#Modeling
log.reg <- glm(Churn ~ ., data = train, family=binomial("logit"))
summary(log.reg)
#----------------------------SECOND MODEL
#dummy.trap.var <- c("StreamingMovies","StreamingTV", "TechSupport", "DeviceProtection", 
                    #"OnlineBackup", "OnlineSecurity", "MultipleLines")
#To drop dummy trap variables
#telco.prep <- telco.prep [,!(names(telco.prep) %in% dummy.trap.var)]

formula <- Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+ 
InternetService+Contract+PaperlessBilling+MonthlyCharges + TotalCharges
log_reg <- glm(formula, data = train, family=binomial("logit"))
summary(log_reg)

#Mc fadden pseudo R-square
null_dev <- log_reg$null.deviance #deviance of intercept only model (-loglos)
dev <- log_reg$deviance
1 - (dev/null_dev)
#Prediction
pred_prob <- predict(log_reg, test, type='response')
pred_label <- factor(ifelse(pred_prob > 0.5, 'Yes', 'No'), levels = c('No', 'Yes'))
#Confusion Matrix
confusionMatrix(reference=test$Churn, data=pred_prob, mode='everything', positive='Yes')
confusionMatrix(pred_prob, test$Churn, positive = "Yes")

#Precision recall curve
#For a good model as recall increases(x-axis), the precision should not fall back
#the closest the value to zero, the better
log_reg_roc <- prediction(pred_prob, test$Churn)
plot(performance(log_reg_roc, "tpr", "fpr"), main='precision Recall Chart')
#AUC
log_reg_auc <- performance(log_reg_roc, "auc")
log_reg_auc@y.values[[1]]
#Exproting the model
saveRDS(log_reg, file = "telcom-log_reg-RDS")
#Importing Model
log_reg <- readRDS("telcom-log_reg-RDS")

#FEATURES IMPORTANCE
varImp(log.reg)

#Cumulative Recall Chart
DMwR::CRchart(pred.prob, test$Churn, main=)
#ROC curve(Receiver Operating Characteristic Curve)
library(ROCit)
RoCit.obj <- rocit(score=pred.prob, class=test$Churn)
plt <- plot(RoCit.obj)
plt + theme(legend.position=c(1, 0),
            legend.justification=c(1, 0))
#ROC AUC 
RoCit.obj$AUC

#youden's index =[sensitivity+specificity-1]
#KS table function
#KS Statistic and Gain curve(Marketing, buyer prop model)
#help to determine who are the of the population that should be target in order to get
#the max return
#total_pop:Total nber of P,non_responders:TN(No),responders: TP(Yes)
#cum_perc_responders:6(60%):if we target 60% of the cust pop, we will capture 92% of Churn
#cost saving.n=7043 -> target sample=7043*60%. with a sample of this size, we are able to capture 92% of all the peaople who 
#are expected to respond(TP)
#KS-stat= the maximum #ce between cum_perc_responders and cum_perc_non_responders:
#The widder the #ce, the better the model being able to capture the responders(TP)
library(InformationValue)
InformationValue:::ks_table(test$Churn, pred.prob)
InformationValue:::ks_stat(test$Churn, pred.prob)
#----------------KS plot Gain chart
ks_table <- InformationValue:::ks_table
ks_plot <- function (actuals, predictedScores) {
  rank <- 0:10
  ks_table_out <- ks_table(actuals = actuals, predictedScores = predictedScores)
  perc_events <- c(0, ks_table_out$cum_perc_responders) * 100
  perc_nonevents <- c(0, ks_table_out$cum_perc_non_responders) * 100
  random_prediction <- seq(0, 100, 10)
  df <- data.frame(rank, random_prediction, perc_events, perc_nonevents)
  df_stack <- stack(df, c(random_prediction, perc_events, perc_nonevents))
  df_stack$rank <- rep(rank, 3)
  df_stack$delta <- df_stack$values[12:22] - df_stack$values[1:11]
  values <- df_stack$values
  ind <- df_stack$ind
  
  rowmax <- which.max(ks_table_out$difference)
  l_start <- ks_table_out[rowmax, "cum_perc_non_responders"]
  l_end <- ks_table_out[rowmax, "cum_perc_responders"]
  
  print(ggplot2::ggplot(df_stack, aes(x = rank, y = values, 
                                      colour = ind, label = paste0(round(values, 2), "%"))) + 
          geom_line(size = 1.00) + 
          labs(x = "rank", y = "Percentage events (1's) & non-events (0's) Captured", 
               title = "KS Plot", subtitle=paste("KS Statistic: ", InformationValue::ks_stat(actuals, predictedScores))) + 
          theme(plot.title = element_text(size = 20, 
                                          face = "bold")) + 
          geom_text(aes(y = values + 4), size=2) + 
          scale_x_continuous(breaks=0:10, labels=0:10) + 
          geom_segment(x = rowmax, y = l_start*100, xend = rowmax, yend = l_end*100, col="red", arrow = arrow(length = unit(0.05, "npc"), ends="both"), linetype = "dashed", lwd=.5)) + 
    scale_color_discrete(name="")  + theme(legend.justification=c(.99,0.01), legend.position=c(.99,0.01))
}

ks_plot(test$Churn, pred.prob)
#Gain Chart
#eg.If we were to randomly pick pick people for our campaign let say 40% of the entire pop sample, 
#The percentage of respondent that we would achieve is 40-42%. But with our current model by targeting the top
#40% of the entire population sample recommended by our model, we would achieve 80%.
#Cost saving equals 80%-40%. can be converted into dollar term if we are able to attach a dollar value per convertible.
#Ks-sata = the dotted red line.

#Concordant and discordant pairs
#concordant pair: predited prob score positive(pred, y.hat) > predicted prob score negative
#Discordant pair: predited prob score positive < predicted prob score negative
#e.g:Model1:TP=600, TN=400, nber of pair=600*400=24000 with 90% concordant and 10% discordant
#Model2:88% concordant and 12% discordant. The model 1 perform better than model 2
preds <- predict(log.reg, test)
actuals <- as.numeric(test$Churn)-1
out <- Concordance(actuals, preds)
View(actuals)
table(actuals)
table(preds)
out$Concordance
out$Discordance

#------------------------CLASS IMBALANCE
#Sensitivity:(Recall) TP/(TP+TN)
#F-1score: Recall and precision(TP/(TP-TP)) 
#Capture rate:What % of all positive cases are being capture by 10% decile. the entire dataset is splited into 
#buckets of 10 percentile. Under each bucket, what % of the positive cases are being captured 

#A good model with imbalanced class will have high ROC score, High KS-stat, high concordance ratio, larger AUC,
#large cohen's kappa, high balance accuracy

#1.Cost sensitive learning (FP will be attributed more weight that FN)
#Build the model
log.reg <- glm(formula, data = train, family=binomial("logit"))
summary(log.reg)
#Mc Fadden's Pseudo R-square
null.dev <- log.reg$null.deviance
dev <- log.reg$deviance
(null.dev-dev)/null.dev
#Predictions
pred <- predict(log.reg, test, type='response')
pred.label <- factor(ifelse(pred > 0.5, 'Yes', 'No'), levels = c('No', 'Yes'))
#Confusion Matrix
caret::confusionMatrix(pred.label, test$Churn, positive = "Yes")
caret::confusionMatrix(reference = test$Churn, data = pred.label, mode='everything', positive='Yes')
(A + C) / (A + B + C + D)#Prevalence
#(he proportion of actual events in data. As the name suggests it 
#reflects the general prevalence of the event)
(A + B)/(A + B + C + D)# Detection Prevalence : The proportion of predicted events in data
#Balanced Accuracy = (sensitivity + specificity)/2
#kappa=(observed accuracy - expected accuracy)/(1 - expected accuracy)

remove(pred)
# Capture Rates
actuals <- ifelse(test$Churn == 'Yes', 1, 0)

df_probs <- data.frame(Act            = actuals, 
                       Prob           = pred, 
                       Pred_Decile    = cut(pred, 
                                            breaks = quantile(pred, probs = seq(0, 1, .1), na.rm=T), 
                                            include.lowest = T, labels = 1:10),
                       Natural_Decile = ceiling(pred*10))

output <- df_probs %>% group_by(Pred_Decile) %>% 
  summarise(n_obs=length(Act), n_pos = sum(Act)) %>% 
  arrange(desc(Pred_Decile)) %>% 
  mutate(capture_rate = round(cumsum(n_pos)/sum(n_pos), 3) *100)

output
head(df_probs)
caret::confusionMatrix(reference = test$Churn, data =pred.label, mode='everything', positive='Yes')
#Precision:0.6392
#F1 score:0.5602
#Recall:0.4987
#-------------------------------COST SENSITIVE LEARNING IMPLEMENTATION 
#Define the weight that will be used by the logistic model to penalize any miss classification happening in the Positive class
# No=0.73, Yes=0.26-> Yes minority class. we are going to inverse the class -> values with Yes in the training data set will be 
#remplaced by 0.73 and NO by 0.26(giving Yes the weight of No(0.73) and NO the weight of Yes(0.26))
# Build Logit Model
table(train$Churn)/NROW(train$Churn)#To determine the weight of each classes in the target
wts_map <- as.numeric(table(train$Churn)/NROW(train$Churn))#Weight map
wts <- ifelse(train$Churn == 'Yes', wts_map[1], wts_map[2])
wts
levels(wts)
log.reg <- glm(formula, weights = wts, data=train, family=binomial("logit"))
summary(log.reg)

# McFadden's Pseudo R-square
null_dev <- log.reg$null.deviance #deviance of intercept only model
dev      <- log.reg$deviance   
(null_dev - dev)/null_dev #:0.288/R-Sq How much improvement?

# Predict
pred <- predict(log.reg, test, type = 'response')
pred.label <- factor(ifelse(pred > 0.5, 'Yes', 'No'), levels = c('No', 'Yes'))

# Capture Rates
actuals <- ifelse(test$Churn == 'Yes', 1, 0)
df_probs <- data.frame(Act            = actuals, 
                       Prob           = pred, 
                       Pred_Decile    = cut(pred, 
                                            breaks = quantile(pred, probs = seq(0, 1, .1), na.rm=T), 
                                            include.lowest = T, labels = 1:10),
                       Natural_Decile = ceiling(pred*10))


out <- df_probs %>% group_by(Pred_Decile) %>% 
  summarise(n_obs=length(Act), n_pos = sum(Act)) %>% 
  arrange(desc(Pred_Decile)) %>% 
  mutate(capture_rate = round(cumsum(n_pos)/sum(n_pos), 3) *100)
out
caret::confusionMatrix(reference = test$Churn, data = pred.label, mode='everything', positive='Yes')
#Precision=0.5206
#Recall=0.8123
#F1=0.6346
#Sensitivity=0.8123
#---------------------------------------------------------------------
#-------------------------------------2.Over Sampling
# Freq Distribution
table(train$Churn)
# Oversample the minority Class
df_y <- train[train$Churn == 'Yes', ]
df_n <- train[train$Churn != 'Yes', ]
set.seed(88)
df_y <- df_y[sample(1:NROW(df_y), size = NROW(df_n), replace=T), ]
train <- rbind(df_y, df_n)
table(train$Churn)  # equal number of Yes's and No's
#Build the model
log.reg <- glm(formula, data = train, family=binomial("logit"))
summary(log.reg)
#Mc Fadden's Pseudo R-square
null.dev <- log.reg$null.deviance
dev <- log.reg$deviance
(null.dev-dev)/null.dev#0.2922
#Predictions
remove(pred)
pred <- predict(log.reg, test, type='response')
pred.label <- factor(ifelse(pred > 0.5, 'Yes', 'No'), levels = c('No', 'Yes'))
#Confusion Matrix
caret::confusionMatrix(reference = test$Churn, data = pred.label, mode='everything', positive='Yes')

#3.Down Sampling
#4.Hybrid sampling
train_new <- DMwR::SMOTE(Churn ~ . ,data = train, perc.over = 400, perc.under = 100)
table(train_new$Churn)
#Building model
log.reg <- glm(formula, data=train_new, family=binomial("logit"))
summary(log.reg)
# McFadden's Pseudo R-square
null_dev <- log.reg$null.deviance  # deviance of intercept only model
dev      <- log.reg$deviance   
(null_dev - dev)/null_dev #:0.1725/R-Sq-> 17% of the variance of target is explained by the model 
#Predict
pred <- predict(log.reg, test, type = 'response')
pred.label <- factor(ifelse(pred > 0.5, 'Yes', 'No'), levels = c('No', 'Yes'))
# Capture Rates
actuals <- ifelse(test$Churn == 'Yes', 1, 0)
df_probs <- data.frame(Act            = actuals, 
                       Prob           = pred, 
                       Pred_Decile    = cut(pred, 
                                            breaks = quantile(pred, probs = seq(0, 1, .1), na.rm=T), 
                                            include.lowest = T, labels = 1:10),
                       Natural_Decile = ceiling(pred*10))


out <- df_probs %>% group_by(Pred_Decile) %>% 
  summarise(n_obs=length(Act), n_pos = sum(Act)) %>% 
  arrange(desc(Pred_Decile)) %>% 
  mutate(capture_rate = round(cumsum(n_pos)/sum(n_pos), 3) *100)
out
caret::confusionMatrix(reference = test$Churn, data = pred.label, mode='everything', positive='Yes')
#Precision=0.5225        
#Recall=0.8097        
#F1=0.6351  
#Sensitivity=0.8097
#Concordance
InformationValue::Concordance(as.numeric(test$Churn)-1, pred)