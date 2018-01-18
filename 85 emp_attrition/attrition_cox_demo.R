# http://www.business-science.io/business/2017/09/18/hr_employee_attrition.html
# Watson Analytics Use Case for HR: Retaining valuable employees
# https://www.ibm.com/communities/analytics/watson-analytics-blog/watson-analytics-use-case-for-hr-retaining-valuable-employees/
# Point your browser to http://localhost:54321

library(tidyverse)
library(readxl)     # Super simple excel reader
library(survival)
library(survminer)
library(broom)


# Read excel data
hr_data_raw <- read_excel(path="./data/WA_Fn-UseC_-HR-Employee-Attrition.xlsx")

glimpse(hr_data_raw)

# Делаем разметку данных для событийного анализа. Используем YearsAtCompany как полный стаж в компании
hr_data <- hr_data_raw %>%
  mutate_if(is.character, as.factor) %>%
  mutate(event=if_else(Attrition=="Yes", 1, 0)) %>%
  mutate(tenure=YearsAtCompany) %>%
  select(tenure, event, everything())

summary(hr_data$tenure)

cfit <- coxph(Surv(tenure, event) ~ as.factor(Education), data=hr_data)
sfit <- survfit(cfit)
class(sfit)

head(tidy(sfit))
glance(sfit)

# https://github.com/kassambara/survminer/issues/67
# без нового data.frame ggsurvplot работать не будет
ggsurvplot(survfit(cfit), palette="#2E9FDF", ggtheme=theme_minimal(),
           title="График дожития (анализ увольнений)",
           xlab="Кол-во лет до увольнения")

stop()

hr_data <- hr_data_raw %>%
  mutate_if(is.character, as.factor) %>%
  select(Attrition, everything())

# https://github.com/thomasp85/lime/issues/56
recipe_obj <- hr_data %>%
  recipe(formula = ~ .) %>%
  step_rm(EmployeeNumber) %>%
  step_zv(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep(data = hr_data)
recipe_obj

hr_data <- bake(recipe_obj, newdata=hr_data)
glimpse(hr_data)

# Initialize H2O JVM
h2o.init()
h2o.no_progress() # Turn off output of progress bars

# Split data into Train/Validation/Test Sets
hr_data_h2o <- as.h2o(hr_data)

split_h2o <- h2o.splitFrame(hr_data_h2o, c(0.7, 0.15), seed = 1234 )

train_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 70%
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 15%
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 15%


# Set names for h2o
y <- "Attrition"
x <- setdiff(names(train_h2o), y)

# Run the automated machine learning 
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = valid_h2o,
  max_runtime_secs  = 30
)

# Extract leader model
automl_leader <- automl_models_h2o@leader

# Predict on hold-out set, test_h2o
pred_h2o <- h2o.predict(object=automl_leader, newdata=test_h2o)

# Prep for performance assessment
test_performance <- test_h2o %>%
  tibble::as_tibble() %>%
  select(Attrition) %>%
  add_column(pred = as.vector(pred_h2o$predict)) %>%
  mutate_if(is.character, as.factor)
test_performance

# Confusion table counts
confusion_matrix <- test_performance %>%
  table() 
confusion_matrix

# Performance analysis
tn <- confusion_matrix[1]
tp <- confusion_matrix[4]
fp <- confusion_matrix[3]
fn <- confusion_matrix[2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
misclassification_rate <- 1 - accuracy
recall <- tp / (tp + fn)
precision <- tp / (tp + fp)
null_error_rate <- tn / (tp + tn + fp + fn)

tibble(
  accuracy,
  misclassification_rate,
  recall,
  precision,
  null_error_rate
) %>% 
  transpose() 

class(automl_leader)

# Setup lime::model_type() function for h2o
model_type.H2OBinomialModel <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  #
  # x is our h2o model
  
  return("classification")
}

# Setup lime::predict_model() function for h2o
predict_model.H2OBinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return probs
  return(as.data.frame(pred[,-1]))
}

# Test our predict_model() function
predict_model(x=automl_leader, newdata=as.data.frame(test_h2o[,-1]), type='raw') %>%
  tibble::as_tibble()

# Run lime() on training set
explainer <- lime::lime(
  as.data.frame(train_h2o[,-1]), 
  model          = automl_leader, 
  bin_continuous = FALSE)

# Run explain() on explainer
explanation <- lime::explain(
  as.data.frame(test_h2o[1:10,-1]), 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  kernel_width = 0.5)

plot_features(explanation) +
  labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

# Focus on critical features of attrition
attrition_critical_features <- hr_data %>%
  tibble::as_tibble() %>%
  select(Attrition, TrainingTimesLastYear, JobRole, OverTime) %>%
  rowid_to_column(var = "Case")
attrition_critical_features
