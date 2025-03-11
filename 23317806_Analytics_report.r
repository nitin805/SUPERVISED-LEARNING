# Clear environment
rm(list = ls()) 
# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

getwd()
## packages for this script
## Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

## load packages (including pacman) with pacman
pacman::p_load(psych,corrplot,patchwork,caret,vcd,gridExtra,knitr,scales,ROCR,lme4,InformationValue,MASS,ggmosaic,e1071,ggcorrplot,caTools,dplyr,mlbench,rpart,rpart.plot,stats,huxtable,ggthemes)


############################Read and format data##########################
bank_data <- read.csv("train.csv")
#View Bank Data
str(bank_data)

bank_data <- na.omit(bank_data)
bank_data <- subset(bank_data, select = -c(previous))

##Declare factors 
MyFactors <- c("job", "marital","education","default","loan","housing","contact","poutcome","y") #create the name list
bank_data[MyFactors] <- lapply(bank_data[MyFactors], as.factor) #declare factors



#Summary statistics
summary(bank_data)

##xtabs/crosstabs to check whether any relationship exists between categorical independent variables and the dependent variable

## To check existence of relationship between housing and term deposit(y)
table_1 <-xtabs(~ y + housing, data = bank_data)
ftable(table_1)
summary(table_1)

## To check existence of relationship between  education and term deposit(y)
table_2 <-xtabs(~ y + education, data = bank_data)
ftable(table_2)
summary(table_2)

## To check existence of relationship between  job and term deposit(y)
table_3 <-xtabs(~ y + job, data = bank_data)
ftable(table_3)
summary(table_3)

## To check existence of relationship between  marital and term deposit(y)
table_4 <-xtabs(~ y + marital, data = bank_data)
ftable(table_4)
summary(table_4)

## To check existence of relationship between loan  and term deposit(y)
table_5 <-xtabs(~ y + loan, data = bank_data)
ftable(table_5)
summary(table_5)


## To check existence of relationship between contact  and term deposit(y)
table_6 <-xtabs(~ y + contact, data = bank_data)
ftable(table_6)
summary(table_6)




#Descriptive statistics
describe(bank_data)

##Bar plots and Histograms


########################## Exploratory Data Analysis #####################################

#Response variable
# term deposit
ggplot(bank_data,aes(y,fill=y)) + 
  geom_bar() + 
  theme(legend.position = 'none')
table(bank_data$y)
round(prop.table(table(bank_data$y)))

ggplot(bank_data, aes(factor(y), age, fill=y)) + geom_boxplot() +
  labs(x="subscribe to a term deposit ", y="Age", title="Boxplot for 'subscribe to a term deposit' with respect to age", fill="stroke")

# Comparison of data between term deposit(y) and Martial Status
graph_1 <- table(bank_data$housing, bank_data$marital)
barplot(graph_1, main = "Marital Status of term deposit(y)", xlab = "Term Deposit",
        ylab="Martial", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )

# Comparison of data between term deposit(y) and Job
graph_2 <- table(bank_data$housing, bank_data$job)
barplot(graph_2, main = "Job of term deposit(y)", xlab = "Term Deposit",
        ylab="Job", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )

# Comparison of data between term deposit(y) and Education
graph_3 <- table(bank_data$housing, bank_data$education)
barplot(graph_3, main = "education of term deposit(y)", xlab = "Term Deposit",
        ylab="education", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )

# Comparison of data between term deposit(y) and Loan
graph_4 <- table(bank_data$housing, bank_data$loan)
barplot(graph_4, main = "Loan of term deposit(y)", xlab = "Term Deposit",
        ylab="loan", col = c("yellow","orange"),
        legend = rownames(graph_1), beside = TRUE )



####Correlation matrix
numericVarName <- names(which(sapply(bank_data, is.numeric)))
corr <- cor(bank_data[,numericVarName], use = 'pairwise.complete.obs')
ggcorrplot(corr, lab = TRUE)



#########################Continuous Variable Exploration########################

###################distribution for all continuous variables####################
bank_data %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

##Age

ggplot(bank_data, aes(factor(y), age, fill=y)) + geom_boxplot() +
  labs(x="subscribe to a term deposit ", y="Age", title="Boxplot for 'subscribe to a term deposit' with respect to age", fill="subscribe to a term deposit")

##Balance
ggplot(bank_data, aes(factor(y), balance, fill=y)) + geom_boxplot() +
  labs(x="subscribe to a term deposit ", y="Balance", title="Boxplot for 'subscribe to a term deposit' with respect to Balance", fill="subscribe to a term deposit")

##Day
ggplot(bank_data, aes(factor(y), day, fill=y)) + geom_boxplot() +
  labs(x="subscribe to a term deposit ", y="day", title="Boxplot for 'subscribe to a term deposit' with respect to day", fill="subscribe to a term deposit")


##Duration
ggplot(bank_data, aes(factor(y), duration, fill=y)) + geom_boxplot() +
  labs(x="subscribe to a term deposit ", y="Duration", title="Boxplot for 'subscribe to a term deposit' with respect to Duration", fill="subscribe to a term deposit")

##Pdays
ggplot(bank_data, aes(factor(y), pdays, fill=y)) + geom_boxplot() +
  labs(x="subscribe to a term deposit ", y="pdays", title="Boxplot for 'subscribe to a term deposit' with respect to pdays", fill="subscribe to a term deposit")




#####################Categorical Variable Exploration###########################

###################Distribution for all Categorical Variable####################

bank_data %>%
  dplyr::select(-y) %>% 
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

##subscribe to a term deposit vs marital
bank_data %>%
  dplyr::select(marital, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(marital), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'marital',y='subscribe to a term deposit')

##subscribe to a term deposit vs education
bank_data %>%
  dplyr::select(education, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(education), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'education',y='subscribe to a term deposit')

##subscribe to a term deposit vs housing
bank_data %>%
  dplyr::select(housing, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(housing), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'housing',y='subscribe to a term deposit')

##subscribe to a term deposit vs contact
bank_data %>%
  dplyr::select(contact, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(contact), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'contact',y='subscribe to a term deposit')

##subscribe to a term deposit vs default
bank_data %>%
  dplyr::select(default, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(default), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'default',y='subscribe to a term deposit')

##subscribe to a term deposit vs job
bank_data %>%
  dplyr::select(job, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(job), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'job',y='subscribe to a term deposit')

##subscribe to a term deposit vs loan
bank_data %>%
  dplyr::select(loan, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(loan), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'loan',y='subscribe to a term deposit')

##subscribe to a term deposit vs poutcome
bank_data %>%
  dplyr::select(poutcome, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(poutcome), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'poutcome',y='subscribe to a term deposit')

##subscribe to a term deposit vs month
bank_data %>%
  dplyr::select(month, y) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(month), fill = y)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'month',y='subscribe to a term deposit')



######################### T Test for Independent variable means w.r.t. Dependent###################################################
## If a numerical variable is to be significant in the logistic regression, you would expect that it's mean is different for 
## the different outcomes of the dependent variable

## Test for difference in means for independent samples 
## use t.test function
t.test(bank_data$age~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
t.test(bank_data$balance~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
t.test(bank_data$day~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
t.test(bank_data$campaign~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95, var.equal = TRUE)
t.test(bank_data$pdays~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
t.test(bank_data$duration~bank_data$y,mu=0,alternative = "two.sided",conf.level = 0.95, var.equal = TRUE)


###################################################################################
##            SPLIT DATA                                                        ###
##                                                                              ###
###################################################################################        
# create training and testing data sets
## set the seed to make your partition reproducible
set.seed(999)
train_ind <- bank_data$y %>%
  createDataPartition(p = 0.80, list = FALSE) #this is splitting the banking data in 80% estimation and 20% testing/training sample
bank_train <- bank_data[train_ind,] #new data frame of 36170 observations in the training set
bank_test <- bank_data[-train_ind,] #-train_ind means "not in the index" which will give a new data frame i.e test set of 9041 obs
print(table(bank_train$y))
print(table(bank_test$y))
str(bank_data)
##END SPLITTING DATA

# Logistic Regression


#Model1
model_1 <-glm(y~job+marital+education+age+housing+contact+default+loan+month+poutcome+day*campaign,family = "binomial", data = bank_train)
summary(model_1)
model_1[["aic"]]
anova(model_1, test = "LRT") 
logLik(model_1)
deviance(model_1)


#Model2
model_2 <-glm(y~job+marital+education+loan,family = "binomial", data = bank_train)
summary(model_2)
model_2[["aic"]]
anova(model_2, test = "LRT") 
logLik(model_2)
deviance(model_2)


#Model3
model_3 <-glm(y~poutcome+loan+housing+pdays+duration+age,family = "binomial", data = bank_train)
summary(model_3)
model_3[["aic"]]
anova(model_3, test = "LRT") 
logLik(model_3)
deviance(model_3)


################## Prediction and model lift ############################################

# Make and save predictions to the test data
prediction_test <-predict(model_3, bank_test, type="response")
bank_test <- mutate(bank_test, predictions = prediction_test)

# confusion tables 
# compare actual results with a probability threshold
table(bank_test$y, prediction_test > 0.5)
bank_test$predicted <- as.factor(ifelse(bank_test$predictions>0.5,"yes","no"))
caret :: confusionMatrix(bank_test$predicted, bank_test$y, positive ="yes")

### ROC-AUC diagram
predROCR <- prediction(prediction_test, bank_test$y)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values




###################  Decision Tree   ################## 

dmodel_1 <- rpart(formula = y~.,
                  data = bank_train,
                  method = "class")
summary(dmodel_1)
rpart.plot(x = dmodel_1, yesno = 2, type = 0, extra = 0)# Display the results

BankData_model1 <- predict(object = dmodel_1,  
                               newdata = bank_test,   
                               type = "class")

caret::confusionMatrix(BankData_model1,bank_test$y,positive ="yes")


BankPredict_DT1 <- predict(object = dmodel_1,  
                            newdata = bank_test,   
                            type = "prob")
predROCR <- prediction(BankPredict_DT1[,2], bank_test$y)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE) + abline(0,1)
performance(predROCR, "auc")@y.values
