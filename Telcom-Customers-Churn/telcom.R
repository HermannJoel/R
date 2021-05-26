#1.
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
#
num_vars <- c("MonthlyCharges", "TotalCharges", "tenure ")