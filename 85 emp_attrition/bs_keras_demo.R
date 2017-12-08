# http://www.business-science.io/business/2017/11/28/customer_churn_analysis_keras.html

# Load libraries
library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

# Install Keras if you have not installed before
# install_keras()

# Import data
churn_data_raw <- read_csv("./data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

glimpse(churn_data_raw)

# Remove unnecessary data
churn_data_tbl <- churn_data_raw %>%
  select(-customerID) %>%
  drop_na() %>%
  select(Churn, everything())

glimpse(churn_data_tbl)

# Split test/training sets
set.seed(100)
train_test_split <- initial_split(churn_data_tbl, prop = 0.8)
train_test_split

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split) 

# Determine if log transformation improves correlation 
# between TotalCharges and Churn
train_tbl %>%
  select(Churn, TotalCharges) %>%
  mutate(
    Churn = Churn %>% as.factor() %>% as.numeric(),
    LogTotalCharges = log(TotalCharges)
  ) %>%
  correlate() %>%
  focus(Churn) %>%
  fashion()

# Create recipe
rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
  step_discretize(tenure, options = list(cuts = 6)) %>%
  step_log(TotalCharges) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_tbl)

# Predictors
x_train_tbl <- bake(rec_obj, newdata = train_tbl)
x_test_tbl  <- bake(rec_obj, newdata = test_tbl)

glimpse(x_train_tbl)

# Response variables for training and testing sets
y_train_vec <- ifelse(pull(train_tbl, Churn) == "Yes", 1, 0)
y_test_vec  <- ifelse(pull(test_tbl, Churn) == "Yes", 1, 0)
