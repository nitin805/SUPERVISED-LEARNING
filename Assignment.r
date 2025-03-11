#clear environment
rm(list = ls())

#clear plots
dev.off() #But only if there is a plot

#clear console
cat("\014") #Ctrl + L

getwd()

##Packages for this script
pacman::p_load(psych,corrplot,patchwork,caret,vcd,gridExtra,knitr,scales,ROCR,lme4,InformationValue,MASS,ggmosaic,e1071,ggcorrplot,caTools,dplyr,mlbench,rpart,rpart.plot,stats,huxtable,ggthemes)
###########################################################
##load packages
pacman::p_load(pacman,tidyverse,gmodels,caret,ROCR)

##Read and format data##########################
bank_data <- read.csv("train.csv")
#View Bank Data
str(bank_data)

bank_data <- na.omit(bank_data)
bank_data <- subset(bank_data, select = -c(previous))

##Declare factors and remove unwanted columns
bank_data <- bank_data  %>% mutate(job = as.factor(job),
                                   marital = as.factor(marital),
                                   education = as.factor(education),
                                   default = as.factor(default),
                                   loan = as.factor(loan),
                                   housing = as.factor(housing),
                                   contact = as.factor(contact),
                                   month = as.factor(month),
                                   poutcome = as.factor(poutcome),
                                   y = as.factor(y))

##Thus we have no missing data
#Summary statistics
summary(bank_data)

##xtabs/crosstabs to check whether any relationship exists between categorical independent variables and the dependent variable

## To check existence of relationship between y and age
table_1 <-xtabs(~ y + age, data = bank_data)
ftable(table_1)
summary(table_1)

## To check existence of relationship between y and education
table_2 <-xtabs(~ y + education, data = bank_data)
ftable(table_2)
summary(table_2)

## To check existence of relationship between y and job
table_3 <-xtabs(~ y + job, data = bank_data)
ftable(table_3)
summary(table_3)

## To check existence of relationship between y and marital
table_4 <-xtabs(~ y + marital, data = bank_data)
ftable(table_4)
summary(table_4)

## To check existence of relationship between y and default
table_5 <-xtabs(~ y + default, data = bank_data)
ftable(table_5)
summary(table_5)

## To check existence of relationship between y and housing
table_6 <-xtabs(~ y + housing, data = bank_data)
ftable(table_6)
summary(table_6)


#Descriptive statistics
describe(bank_data)

##Bar plots and Histograms
#The dependent variable is Housing

########################## Exploratory Data Analysis #####################################

#Response variable
# Housing Data
ggplot(bank_data,aes(housing,fill=housing)) + 
  geom_bar() + 
  theme(legend.position = 'none')
table(bank_data$housing)
round(prop.table(table(bank_data$housing))) #rounding values of the table to 3 decimal places

# Comparison of data between Term Deposit and Martial Status
graph_1 <- table(bank_data$y, bank_data$marital)
barplot(graph_1, main = "Marital Status of Term Deposit", xlab = "Term Deposit",
        ylab="Martial", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )

# Comparison of data between Term Deposit and Job
graph_2 <- table(bank_data$y, bank_data$job)
barplot(graph_2, main = "Job of Term Deposit", xlab = "Term Deposit",
        ylab="Job", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )

# Comparison of data between Term Deposit and Education
graph_3 <- table(bank_data$y, bank_data$education)
barplot(graph_3, main = "education of Term Deposit", xlab = "Term Deposit",
        ylab="education", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )

# Comparison of data between Term Deposit and Loan
graph_4 <- table(bank_data$y, bank_data$loan)
barplot(graph_4, main = "Loan of Term Deposit", xlab = "Term Deposit",
        ylab="loan", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )

# Comparison of data between Term Deposit and Poutcome
graph_5 <- table(bank_data$y, bank_data$poutcome)
barplot(graph_5, main = "poutcome of Term Deposit", xlab = "Term Deposit",
        ylab="poutcome", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )



##Overall distribution for all features
bank_data %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')


##Correlation matrix
numericVarName <- names(which(sapply(bank_data, is.numeric)))
corr <- cor(bank_data[,numericVarName], use = 'pairwise.complete.obs')
ggcorrplot(corr, lab = TRUE)


##Categorical Variable Distribution
bank_data %>%
  dplyr::select(-housing) %>% 
  keep(is.factor) %>%
  gather() %>%
  group_by(key, value) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_bar(mapping=aes(x = value, y = n, fill=key), color="black", stat='identity') + 
  coord_flip() +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

##Continuous Variable Exploration
##Age
age_hist <- ggplot(bank_data, aes(x = age, fill = y)) +
  geom_histogram(binwidth = 5) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = comma)

age_boxplot <- ggplot(bank_data, aes(x = y, y = age, fill = y)) +
  geom_boxplot() + 
  theme_minimal() +
  theme(legend.position = 'none')

age_hist | age_boxplot

##Categorical Variable Exploration
marital_graph <- bank_data %>%
  dplyr::select(marital, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(marital), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'marital')

education_graph <- bank_data %>%
  dplyr::select(education, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(education), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'Education')

######################### T Test for Independent variable means w.r.t. Dependent###################################################
## If a numreical variable is to be significant in the logistic regression, you would expect that it's mean is different for 
## the different outcomes of the dependent variable

## Test for difference in means for independent samples 
## use t.test function
t.test(bank_data$age~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
t.test(bank_data$balance~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
t.test(bank_data$day~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
t.test(bank_data$campaign~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95, var.equal = TRUE)


###################################################################################
##            SPLIT DATA                                                        ###
##                                                                              ###
###################################################################################        
# create training and testing datasets
## set the seed to make your partition reproducible
set.seed(111)
train_ind <- bank_data$y %>%
  createDataPartition(p = 0.75, list = FALSE) #this is splitting the churn data in 75% estimation and 25% testing/holdout sample
bank_train <- bank_data[train_ind,] #new data frame of 7501 observations in the training set
bank_test <- bank_data[-train_ind,] #-train_ind means "not in the index" which will give a new data frame i.e test set of 2409 obs
print(table(bank_train$y))
print(table(bank_test$y))
str(bank_data)
##END SPLITTING DATA

# Logistic Regression
#Model1
model_1 <-glm(y~job+marital+education,family = "binomial", data = bank_train)

summary(model_1)


bank_predict_1 <- predict(object = model_1,newdata = bank_test,type = "response")


predict_1 <- as.factor(ifelse(bank_predict_1>0.5,"yes","no"))

caret :: confusionMatrix(predict_1, bank_test$y, positive ="yes")

model_1[["aic"]]

#Model2
model_2 <-glm(y~job+marital+education+loan,family = "binomial", data = bank_train)

summary(model_2)


bank_predict_2 <- predict(object = model_2,newdata = bank_test,type = "response")


predict_2 <- as.factor(ifelse(bank_predict_2>0.5,"yes","no"))

caret :: confusionMatrix(predict_2, bank_test$y, positive ="yes")

model_2[["aic"]]

#Model3
model_3 <-glm(y~job+marital+education+loan+month+poutcome,family = "binomial", data = bank_train)

summary(model_3)


bank_predict_3 <- predict(object = model_3,newdata = bank_test,type = "response")


predict_3 <- as.factor(ifelse(bank_predict_3>0.5,"yes","no"))

caret :: confusionMatrix(predict_3, bank_test$y, positive ="yes")

model_3[["aic"]]


#Decision Tree

dmodel_1 <- rpart(formula = y~.,
                  data = bank_train,
                  method = "class")
summary(dmodel_1)
rpart.plot(x = dmodel_1, yesno = 2, type = 0, extra = 0)# Display the results

ChurnPredict_model1 <- predict(object = dmodel_1,  
                               newdata = bank_test,   
                               type = "class")

caret::confusionMatrix(ChurnPredict_model1,bank_test$y,positive ="yes")


ChurnPredict_DT1 <- predict(object = dmodel_1,  
                            newdata = bank_test,   
                            type = "prob")
predROCR <- prediction(ChurnPredict_DT1[,2], bank_test$y)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE) + abline(0,1)
performance(predROCR, "auc")@y.values
