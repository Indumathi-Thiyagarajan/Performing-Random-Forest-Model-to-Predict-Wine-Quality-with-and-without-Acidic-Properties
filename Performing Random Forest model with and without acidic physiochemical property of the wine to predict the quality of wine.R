
install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
library(dplyr)
library(readr)
library(tidyverse)
wine <- read.csv("E:/MSITM/ISM 645/Assignment/assignment 4/wine_quality.csv", stringsAsFactors = TRUE)
glimpse(wine)
head(wine)




#===================================================================
#Define a binary variable of "alcohol_level" defined as "Strong" if alcohol > 10 and "Weak" otherwise.

library(dplyr)
wine_alc_level <- read.csv("E:/MSITM/ISM 645/Assignment/assignment 4/wine_quality.csv")%>%
  mutate(alcohol_level = ifelse(alcohol > 10, "Strong","Week"))

glimpse(wine_alc_level)

wine_alc_avg <- wine_alc_level %>% select(type, quality, alcohol_level)%>%
  group_by(type, alcohol_level) %>%
  mutate(mean_quality = mean(quality))

wine_alc_avg

install.packages("ggplot2")
library(ggplot2)

ggplot(wine_alc_avg, mapping = aes(x=type, y = mean_quality, fill = alcohol_level)) +
  geom_col(position = "dodge")+
  labs(y= "Average Quality(Sensoy Preference)", x= "type")

#Make a histogram of wine quality by wine types with binwidth = 1
ggplot(data = wine_alc_avg, mapping = aes(x= quality, fill = type)) +
  geom_histogram(binwidth = 1) +
  facet_grid(~type)+
  labs(x= "Quality(Sensoy Preference)", y= "Frequency (Wine samples)")
#===================================================================

#Split data into 70% train and 30% test data.

install.packages("caTools")
library("caTools")
set.seed(123)
split <- sample.split(wine_alc_level, SplitRatio = 0.7)
train <- subset(wine_alc_level, split == TRUE)
test <- subset(wine_alc_level, split == FALSE)

#Based on train data, make "two" linear regression models to predict wine quality using (i) all available predictor variables and (ii) all but wine type.

library(dplyr)

#(i) all available predictor variables

wine_lr_train_all <- lm(quality~ type + fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                    total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train )


#(ii) all but wine type.
wine_lr_train_no_type <- lm(quality~ fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                          total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train )


#Based on test data, evaluate two predictive models based on RMSE (Root Mean Squared Error). Is it important to consider wine type differently in predicting quality (sensory preference)?


install.packages("broom")
library(broom)
library(dplyr)

wine_lr_test_all <- lm(quality~ type + fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                         total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train )%>%
  augment(newdata = test)%>%
  mutate(residual=quality - .fitted)%>%
  summarize(rmse2= sqrt(mean(residual^2)))


wine_lr_test_no_type <- lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                         total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train )%>%
  augment(newdata = test)%>%
  mutate(residual=quality -.fitted)%>%
  summarize(rmse2= sqrt(mean(residual^2)))

wine_lr_test_all # = 0.742
wine_lr_test_no_type #=0.745

# As lower RMSE indicates better fit model, Here RMSE value increases without including type value, So I predict, it is necessary to consider wine type in predicting quality.



#===================================================================


#Based on "original" data frame, replace the target variable "quality" with a binary variable (factor type) defined as 1 if quality > 6 and 0 otherwise.

library(dplyr)
wine_lg <-wine %>% mutate(quality = factor(ifelse(quality > 6, 1,0)))%>%
  mutate(alcohol_level = ifelse(alcohol > 10, "Strong","Week"))%>%
  mutate_if(is.character, as.factor)

str(wine_lg)


#Based on "original" data frame, split data again into 70% train and 30% test data.


library(caTools)
set.seed(123)
split <- sample.split(wine_lg, SplitRatio = 0.7)
train_lg <- subset(wine_lg, split== TRUE)
test_lg <- subset(wine_lg, split == FALSE)

#Based on train data, make a logistic regression model to predict wine quality using all available predictor variables.


wine_lg_all_train <- glm(quality ~ type + fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                     total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train_lg,  family = "binomial")


#Based on test data, make a ROC curve and calculate AUC.


library(broom)
wine_lg_all_test <- glm(quality ~ type + fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                     total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train_lg,  family = "binomial")%>%
                    augment(type.predict ="response", newdata = test_lg)


install.packages("pROC")
library("pROC")

Roc <-roc(test_lg $quality , wine_lg_all_test$.fitted)
plot(Roc)
auc(Roc)

#0.8028
#===================================================================

#Based on train data, build a random forest model to predict quality using all available predictor variables.


install.packages("randomForest")
library("randomForest")

random_forest <- randomForest(quality ~ type + fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                                total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train_lg, ntree=500, importance = TRUE)

#Based on importance measure from the random forest, which factors do you think are important in predicting wine quality?



importance(random_forest)
varImpPlot(random_forest)

#Based on importance measure from the random forest, alcohol, volatile.acidity and residual.sugar are important in predicting wine quality


#Based on test data, make a ROC curve and calculate AUC. Which model do you think is better, logistic regression (Q3) or random forest (Q4)?


library(dplyr)
predict_rf <- random_forest %>% predict(type = "prob", newdata = test_lg)

predict_rf

library(pROC)
ROC_RF <- roc(test_lg$quality, predict_rf[,2])
plot(ROC_RF)
auc(ROC_RF)

#Based on calculated AUC, random forest (Q4) is found to be better model



#===================================================================

# Many wine experts argue that acid is a vital component of wine.
#Build and evaluate "two" random forest models to predict wine quality using (i) all available predictor variables and (ii) all but acidity measures (fixed.acidity, volatile.acidity, citric.acid, and pH).
#Based on your analysis, do you agree that acid is a significant predictor of wine quality (sensory preference)?

library("randomForest")

random_forest <- randomForest(quality ~ type + fixed.acidity+volatile.acidity+citric.acid+ residual.sugar +chlorides+free.sulfur.dioxide +
                                total.sulfur.dioxide +density+pH+sulphates+alcohol+alcohol_level, data = train_lg, ntree=500, importance = TRUE)
importance(random_forest)
varImpPlot(random_forest)

library(dplyr)
predict_rf <- random_forest %>% predict(type = "prob", newdata = test_lg)

predict_rf

library(pROC)
ROC_RF <- roc(test_lg$quality, predict_rf[,2])
plot(ROC_RF)
auc(ROC_RF)


library(randomForest)

random_forest_2 <- randomForest(quality ~type+ residual.sugar +chlorides+free.sulfur.dioxide +
                                total.sulfur.dioxide +density+sulphates+alcohol+alcohol_level, data = train_lg, ntree=500, importance = TRUE)
importance(random_forest_2)
varImpPlot(random_forest_2)


library(dplyr)
predict_rf_2 <- random_forest_2 %>% predict(type = "prob", newdata = test_lg)

predict_rf_2

library(pROC)
ROC_RF_2 <- roc(test_lg$quality, predict_rf_2[,2])
plot(ROC_RF_2)
auc(ROC_RF_2)


#As AUC value is more in random forest model with all predictors, I agree that Acid is a significant predictor of wine quality (sensory preference)

#===================================================================
