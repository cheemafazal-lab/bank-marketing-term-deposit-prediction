######## Term Deposit Data ###############################

######## Data loaded using the Global Environment #################

### set working directory ######################
getwd()

### Installing the appropriate packages ##########
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("caret")

### Initial look at the data #########
summary(termsv)
str(termsv)

### DATA CLEANING #####
### Checking for missing values ######
sum(is.na(termsv))

### 25 NAs found ##### Since number is low will be omitting rows with NAs ####
termsv <- na.omit(termsv)

sum(is.na(termsv))

### Checking and fixing data types ##########
str(termsv)

#### Trimming white space in all character columns #####
#### This code snippet is taken from ChatGPT ##########
#### It uses sapply and lapply to apply trimws on the columns #####
char_cols <- sapply(termsv, is.character)
termsv[char_cols] <- lapply(termsv[char_cols], trimws)

### There are inconsistent names in the month and day columns ######
if ("month" %in% names(termsv)) {termsv$month[termsv$month == "july"] <- "jul"}
if ("day" %in% names(termsv)) {termsv$day[termsv$day == "friday"] <- "fri"}

#### Now to select the column types manually for each column ######
termsv$ID <- as.integer(termsv$ID)
termsv$age <- as.integer(termsv$age)
termsv$contact_duration <- as.integer(termsv$contact_duration)
termsv$number_contacts <- as.integer(termsv$number_contacts)
termsv$last_contacted <- as.integer(termsv$last_contacted)
termsv$prev_contacts <- as.integer(termsv$prev_contacts)
termsv$empl_var_rate <- as.numeric(termsv$empl_var_rate)
termsv$cons_price_index <- as.numeric(termsv$cons_price_index)
termsv$cons_conf_index <- as.numeric(termsv$cons_conf_index)
termsv$euribor_three_mth <- as.numeric(termsv$euribor_three_mth)
termsv$nr_employed <- as.numeric(termsv$nr_employed)
termsv$subscribed <- as.factor(termsv$subscribed)
termsv$gender <- as.factor(termsv$gender)
termsv$occupation <- as.factor(termsv$occupation)
termsv$salary_level <- as.factor(termsv$salary_level)
termsv$marital_status <- as.factor(termsv$marital_status)
termsv$education_level <- as.factor(termsv$education_level)
termsv$credit_default <- as.factor(termsv$credit_default)
termsv$has_mortgage <- as.factor(termsv$has_mortgage)
termsv$has_personal_loan <- as.factor(termsv$has_personal_loan)
termsv$has_car_insurance <- as.factor(termsv$has_car_insurance)
termsv$has_life_insurance <- as.factor(termsv$has_life_insurance)
termsv$has_savings_account <- as.factor(termsv$has_savings_account)
termsv$has_current_account <- as.factor(termsv$has_current_account)
termsv$has_credit_card <- as.factor(termsv$has_credit_card)
termsv$contact_method <- as.factor(termsv$contact_method)
termsv$month <- as.factor(termsv$month)
termsv$day <- as.factor(termsv$day)
termsv$prev_campaign_outcome <- as.factor(termsv$prev_campaign_outcome)

str(termsv)


######### DESCRIPTIVE STATISTICS #############################
summary(termsv)

######### Initial descriptive plots. #######################

install.packages("ggplot2")
library(ggplot2)

### First we plot the target variable subscribed #########
#### This allows us to see the distribution of the target variable #########
ggplot(termsv, aes(x = subscribed)) +
  geom_bar() +
  labs(title = "Subscription Outcome Distribution",
       x = "Subscribed", y = "Count")

#### Now we plot the numeric variables of interest to have an initial view ####

ggplot(termsv, aes(x = age)) +
  geom_histogram(bins = 30) +
  labs(title = "Age Distribution", x = "Age")

ggplot(termsv, aes(x = contact_duration)) +
  geom_histogram(bins = 30) +
  labs(title = "Contact Duration Distribution", x = "Duration (seconds)")

ggplot(termsv, aes(x = contact_method)) +
  geom_bar() +
  labs(title = "Contact Method Frequency", x = "Contact Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

termsv$month <- factor(
  termsv$month,
  levels = c("jan","feb","mar","apr","may","jun",
             "jul","aug","sep","oct","nov","dec")
)


ggplot(termsv, aes(x = month)) +
  geom_bar() +
  labs(title = "Contacts by Month", x = "Month")

ggplot(termsv, aes(x = subscribed, y = age)) +
  geom_boxplot() +
  labs(title = "Age vs Subscription", x = "Subscribed", y = "Age")

ggplot(termsv, aes(x = subscribed, y = contact_duration)) +
  geom_boxplot() +
  labs(title = "Contact Duration vs Subscription",
       x = "Subscribed", y = "Duration (seconds)")

### CHECKING BIVARIATE RELATIONSHIPS #################################

#### THE FOUR RELATIONSHIPS TO BE TESTED ARE #######################
### AGE VS SUBSCRIPTION########
### CONTACT DURATION VS SUBSCRIPTION #######################
### CONTACT METHOD VS SUBSCRIPTION #########################
### PREVIOUS CAMPAIGN OUTCOME VS SUBSCRIPTION ##############
### MONTH VS SBSCRIPTION ###################################

### BOXPLOT FOR AGE VS SUBSCRIPTION ########################
### CORRELATION NOT APPROPRIATE BECAUSE IT THE DEPENDANT VARIABLE IS BIANRY ####
boxplot(age ~ subscribed,
        data = termsv,
        main = "Age by Subscription Outcome",
        xlab = "Subscribed",
        ylab = "Age")

### BOXPLOT FOR CONTACT DURATION VS SUBSCRIPTION ############
boxplot(contact_duration ~ subscribed,
        data = termsv,
        main = "Contact Duration by Subscription Outcome",
        xlab = "Subscribed",
        ylab = "Contact Duration (seconds)")


### CHISQUARED TEST FOR CONTACT METHOD VS SUBSCRIPTION ######
chisq.test(table(termsv$contact_method, termsv$subscribed))

### CHISQUARED TEST FOR CAMPAIGN OUTCOME VS SUBSCRIPTION ####
chisq.test(table(termsv$prev_campaign_outcome, termsv$subscribed))

### CHISQUARED TEST FOR MONTH VS SUBSCRIPTION ###############
chisq.test(table(termsv$month, termsv$subscribed))


#### BASELINE LOGISTICAL REGRESSION MODEL ####################

### SPLITTING DATA ###########################################
set.seed(40465466)
library(caret)

partition <- createDataPartition(termsv$subscribed, p = 0.8, list = FALSE)
train <- termsv[partition,]
test <- termsv[-partition,]

model <- glm(subscribed ~ age + contact_duration + contact_method + prev_campaign_outcome, data = train, family = binomial)

summary(model)

##### checking model accuracy and assumption testing ##########

### Interpreting coefficients using odds ratios ##########
exp(coef(model))

### Confidence intervals for odds ratios ################
exp(confint(model))

### Assessing overall model fit using PseudoR2 ####
### using this function from the class handout on Logistical Regression ####

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  
  R.l  <- 1 - dev / nullDev
  R.cs <- 1 - exp(-(nullDev - dev) / modelN)
  R.n  <- R.cs / (1 - exp(-(nullDev / modelN)))
  
  cat("Pseudo R^2\n",
      "Hosmer–Lemeshow:", round(R.l, 3), "\n",
      "Cox & Snell:", round(R.cs, 3), "\n",
      "Nagelkerke:", round(R.n, 3), "\n")
}

logisticPseudoR2s(model)

###### Checking for multicolinearity ###########

library(car)
vif(model)

####### Using Stargazer toget a table for the first model #####

install.packages("stargazer")
library(stargazer)

stargazer(
  model,
  type = "text",
  title = "Logistic Regression Results: Subscription Model",
  dep.var.labels = "Subscribed (Yes = 1)",
  covariate.labels = c("Age", "Contact Duration", "Contact Method", "Previous Campaign Outcome"),
  digits = 3
)

##### Odds ratio in stargazer ############
stargazer(
  model,
  type = "text",
  apply.coef = exp,
  apply.se = exp,
  title = "Logistic Regression (Odds Ratios)"
)



########## BUILDING A MODEL USING MULTIPLE VARIABLES ######

model2 <- glm(
  subscribed ~ age + contact_duration + contact_method + prev_campaign_outcome +
  month + number_contacts,
  data = train,
  family = binomial )

summary(model2)

###### Comparing model 2 with model 1 ############################

AIC(model2)

logisticPseudoR2s(model2)

library(car)
vif(model2)

train$pred_prob_m2 <- fitted(model2)
head(train[, c("pred_prob_m2", "subscribed")])

pred_prob_m2 <- predict(model2, test, type = "response")
class_pred_m2 <- as.factor(ifelse(pred_prob_m2 > 0.5, "yes", "no"))

library(caret)
confusionMatrix(class_pred_m2, test$subscribed)

train$std_resid_m2 <- rstandard(model2)
sum(abs(train$std_resid_m2) > 1.96)

sum(cooks.distance(model2) > 1)


############# Model 3 (this one excludes contact_duration) ############

####### Model 3: Realistic / deployable (excludes contact_duration)####
model3 <- glm(
  subscribed ~ age + contact_method + prev_campaign_outcome +
  month + number_contacts,
  data = train,
  family = binomial
)

summary(model3)

##### Results (same set as model2) #########
AIC(model3)
logisticPseudoR2s(model3)

library(car)
vif(model3)

pred_prob_m3 <- predict(model3, test, type = "response")
class_pred_m3 <- as.factor(ifelse(pred_prob_m3 > 0.5, "yes", "no"))

library(caret)
confusionMatrix(class_pred_m3, test$subscribed)

train$std_resid_m3 <- rstandard(model3)
sum(abs(train$std_resid_m3) > 1.96)

sum(cooks.distance(model3) > 1)


###############################################################################
###############################################################################













