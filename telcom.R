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
#
num_vars <- c("MonthlyCharges", "TotalCharges", "tenure ")