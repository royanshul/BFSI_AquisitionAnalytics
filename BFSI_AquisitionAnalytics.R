# Loading bank marketing data in the working directory. 
setwd("C:/Users/ansroy/Desktop/Personal/IIIT B/Assignment & Case Study/Portugese Data")
bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------
# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response
#Implying only 11% responded.
# Checking missing values

sum(is.na(bank_data))  #No NA Values


############################################################################
############EDA#############################################################
############################################################################
# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80))) #Good to learn this step.

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------


#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 
#Not capping the duration Variable. this may affect the total cost.
#bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

#ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")



# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")



#-------------------------------------------------------


#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)


##################################################################################################
#Creating ProspectID variable, a Unique Number for each record.
# x<-as.data.frame(paste(c("Prospect"),seq(1:41188),sep="-"))
# names(x) <- c("ProspectID")
# bank_data <- cbind(bank_data,x)
# str(bank_data)

#Creating the cost based on Formula (0.033*duration +0.8)

bank_data$orig_cost_of_call <- (0.033*bank_data$duration + 0.8)



bank <- bank_data
remove(Bank_data_age20,agg_age,Avg_duration)
#bank dataframe has the converted data now. We will use bank dataframe going further, The source dataframe is preserved safely in bank_data.

#Dummy Dataframe creation for categorical/Factor variables.
# Required Packages

library(caret)
library(caTools)
library(dummies)
bank$ProspectID <- NULL
bank$age <- NULL

bank <- dummy.data.frame(bank)

bank$response <- as.integer(bank$response)

bank$response <- as.factor(ifelse(bank$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank$response, SplitRatio = 0.70)

train <- bank[split_indices, ]

test <- bank[!split_indices, ]

nrow(train)/nrow(bank)

nrow(test)/nrow(bank)


#Removing the unwanted variables not required for model creation from train dataset. We need to calculate things in test data.

train$orig_cost_of_call <- NULL
train$duration <- NULL

#---------------------------------------------------------    

### Model 1: Logistic Regression


library(MASS)

library(car)

logistic_1 <- glm(response ~ ., family = "binomial", data = train)



summary(logistic_1)
#NA simply mean that these variables has no contribution in model.
#---------------------------------------------------------    
# Using stepwise algorithm for removing insignificant variables 

# logistic_2 <- stepAIC(logistic_1, direction = "both")

logistic_2 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed + `binning.age(30,40]` + `binning.age(40,50]` + 
                    day_of_weekwed, family = "binomial", data = train)

summary(logistic_2)
vif(logistic_2)


#--------------------Logistic_3
#VIF for emp.var.rate, nr.employed,cons.price.idx are huge. They can be collienar. lets see the impact on AIC & Residual by removing them from the model 1 by 1.
#Removing + cons.price.idx

logistic_3 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate  + cons.conf.idx + 
                    nr.employed + `binning.age(30,40]` + `binning.age(40,50]` + 
                    day_of_weekwed, family = "binomial", data = train)

summary(logistic_3)
vif(logistic_3)

#The AIC came to 16094, Residual Deviance to 16042 with 28806 df & Null Deviance to 20299. But VIF values reduced for other variables.

#--------------------Logistic_4 being created on top of Logistic_2 checking thechange in collinearity.
#VIF for emp.var.rate, nr.employed,cons.price.idx are huge. They can be collienar. lets see the impact on AIC & Residual by removing them from the model 1 by 1.
#Removing + emp.var.rate

logistic_4 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure  + cons.price.idx + cons.conf.idx + 
                    nr.employed + `binning.age(30,40]` + `binning.age(40,50]` + 
                    day_of_weekwed, family = "binomial", data = train)

summary(logistic_4)
vif(logistic_4)

#The AIC came to 16105, Residual Deviance to 16053 with 28806 df & Null Deviance to 20299. But VIF values reduced for other variables.


#--------------------Logistic_5 being created on top of Logistic_2 checking thechange in collinearity.
#VIF for emp.var.rate, nr.employed,cons.price.idx are huge. They can be collienar. lets see the impact on AIC & Residual by removing them from the model 1 by 1.
#Removing nr.employed 

logistic_5 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     `binning.age(30,40]` + `binning.age(40,50]` + 
                    day_of_weekwed, family = "binomial", data = train)

summary(logistic_5)
vif(logistic_5)

#The AIC came to 16027, Residual Deviance to 15975 with 28806 df & Null Deviance to 20299. But VIF values reduced for other variables.

####_-------------------------Among Logistic_3,Logistic_4 & logistic_5, the one with removing nr.employed has low vif and aic. We will remove this and proceed.

#--------------------Logistic_5 being created on top of Logistic_2 checking thechange in collinearity.
#VIF for emp.var.rate, nr.employed,cons.price.idx are huge. They can be collienar. lets see the impact on AIC & Residual by removing them from the model 1 by 1.
#Removing nr.employed 



####Logistic_6 on top of Logistic_5.Removing the Less Significant variables.
#Removing monthjul
logistic_6 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr  + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    `binning.age(30,40]` + `binning.age(40,50]` + 
                    day_of_weekwed, family = "binomial", data = train)

summary(logistic_6)
vif(logistic_6)



####Logistic_7 on top of Logistic_6.Removing the Less Significant variables.
#Removing day_of_weekwed
logistic_7 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr  + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    `binning.age(30,40]` + `binning.age(40,50]` , family = "binomial", data = train)

summary(logistic_7)


####Logistic_8 .Removing the Less Significant variables.
#Removing `binning.age(30,40]`
logistic_8 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr  + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx  + `binning.age(40,50]` , family = "binomial", data = train)

summary(logistic_8)


####Logistic_9 .Removing the Less Significant variables.
#Removing + `binning.age(40,50]`
logistic_9 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr  + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_9)

####Logistic_10 .Removing the Less Significant variables.
#Removing monthoct
logistic_10 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr  + monthjun + monthmar + 
                    monthmay + monthnov +  day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_10)


####Logistic_11 .Removing the Less Significant variables.
#Removing educationPrimary_Education
logistic_11 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                      educationTertiary_Education + 
                     contactcellular + monthapr  + monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekfri + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_11)


####Logistic_12 .Removing the Less Significant variables.
#Removing monthapr
logistic_12 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                     educationTertiary_Education + 
                     contactcellular +  monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekfri + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_12)


####Logistic_13 .Removing the Less Significant variables.
#Removing day_of_weekfri
logistic_13 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                     educationTertiary_Education + 
                     contactcellular +  monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_13)



####Logistic_14 .Removing the Less Significant variables.
#Removing jobtechnician
logistic_14 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationTertiary_Education + 
                     contactcellular +  monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_14)


####Logistic_15 .Removing the Less Significant variables.
#Removing jobretired
logistic_15 <- glm(formula = response ~  jobstudent + 
                     educationTertiary_Education + 
                     contactcellular +  monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_15)



####Logistic_16 .Removing the Less Significant variables.
#Removing educationTertiary_Education
logistic_16 <- glm(formula = response ~  jobstudent + contactcellular +  monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx   , family = "binomial", data = train)

summary(logistic_16)


#Logistic_final <- logistic_16
logistic_final <- logistic_16

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test[, -59], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf


# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.5,.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    


#---------------------------------------------------------    

#Requirement is, that we need to contact the prospects with a minimum cost, implying, we need to know for sure the prospect who will respond.
#This means, that the sensitivity of the model should be good. Additionally, the model should also find the specificity with good accuracy, the 
#reson being we should not end up contacting prspoects who wont turn up. And the accuracy should also be good. Hence, lets target to get a good accuracy
#with good specificity & sensitivity. Lets pick the point where all the three meets in the plot.


# Let's choose a cutoff value of 7.3% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.073, "yes", "no"))

conf_final <-   confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

# Appending the probabilities and response variables to the test data
predicted_response <- as.data.frame(predicted_response)
predictions_logit <- as.data.frame(predictions_logit)
test <- cbind(test,predictions_logit,predicted_response)


#---------------------------------------------------------    
# ------Model Evaluation----------------------------------

# Creating new dataframe "test_predictions"
x<-as.data.frame(paste(c("Prospect"),seq(1:12356),sep="-"))
names(x) <- c("ProspectID")
test <- cbind(test,x)
str(test)

test_predictions <- test[, c("ProspectID","response", "predictions_logit", "predicted_response","duration","orig_cost_of_call")]


summary(test_predictions$response)
summary(test_predictions$predicted_response)


response_rate <- table(test$response)[2]/(table(test$response)[1] + table(test$response)[2])

# sorting the probabilities in decreasing order 
test_predictions <- test_predictions[order(test_predictions$predictions_logit, decreasing = T), ]

#Downloading the data 
write.csv(test_predictions,"test_prediction.csv")

summary(test_predictions$response)
summary(test_predictions$predicted_response)



# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
   if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)),
           cumtotal = cumsum(total)
           )
  #Summarise based on prediction values. This will be used for ratio calculation.
  gaintable1 = helper %>% group_by(bucket)  %>%
    summarise_at(vars(predicted_prob ), funs(totalresp_pred=sum(., na.rm = TRUE))) %>% mutate(totalcumresp_pred = cumsum(totalresp_pred))
    
  gaintable<-cbind(gaintable,gaintable1[,-1])
  ratio_pred_actual <- gaintable[,9]/gaintable[,4]
  gaintable <- cbind(gaintable,ratio_pred_actual)
  return(gaintable)
 
}

# Create a Table of cumulative gain and lift 

test_predictions$response <- as.factor(ifelse(test_predictions$response=="yes",1,0))

LG = lift(test_predictions$response, test_predictions$predictions_logit, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

#plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")
#Checkpoint 5:
plot(LG$cumtotal,LG$ratio_pred_actual,col="red",type="l",main="Lift Chart",xlab="Number Of Prospect Contacted",ylab = "Ratio of response rate using the model and without using the model")
# Total Cost incur throught direct telemarketing 

# Let's say if you have spent 1Re for each customer
View(LG)


##For 80%+ prospect, we need to hit in 5th decile.CHeck LG$Gain for 5th decile. It is 81
#Model_cost => sum of cost of first 1235*5+3 =6178 records.
#However, after model building, the first 1128 prospect will only give 80% success hit.
total_original_cost <- sum(test_predictions$orig_cost_of_call)
model_cost <- sum(test_predictions$orig_cost_of_call[1:6178])

#Avg Call duration as per the model
avg_call_duration <-sum(test_predictions$duration[1:6178])/6178
#avg call duration is 271 secs. Total Cost as per the model ->60325.41
#Total cost without the model -> 113846.7







# The Cumulative Lift of 1.6 for top 5 deciles,
# means that when selecting 50% of the records based on the model, 
# one can expect 1.6 times the total number of targets (events) found by randomly 
# selecting 50%-of-records without a model. 

### Analyzing the Charts: Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers to contact. The lift chart shows how much more likely we are to receive
# respondents than if we contact a random sample of customers. 



