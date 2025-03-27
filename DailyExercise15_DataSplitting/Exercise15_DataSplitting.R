###### Daily Assignment 15: Data Splitting
# Author: Clara Jordan
# Date Created: Mar 24, 2025
# Date Last Edited: Mar 27, 2025

###### Day 1 Instructions
# 1. Open an R script (extension .R)
# 2. Load the tidymodels package and the penguins dataset

library(tidymodels)

penguins <- palmerpenguins::penguins

# 3. Set a seed (set.seed())

set.seed(123456)

# 4. Split the data into training/testing sets with a proportion of 70%/30% split

resample_split <- initial_split(penguins, prop = 0.70)

# 5. Extract the training and test tibbles into unique objects

penguins_train <- training(resample_split)
glimpse(penguins_train)

penguins_test <- testing(resample_split)
glimpse(penguins_test)

# 6. Create a 10 fold cross validation dataset based on the training data

cv_folds <- vfold_cv(penguins_train, v = 10)

cv_folds

###### Day 2 Instructions
# 1. Open your R script from the last daily assignment
# 2. Add a new section for the model fitting and workflow
# 3. Define a logistic regression model and a rand_forest model 
#    rand_forest is new, so, think about the engine (use the default) and mode 
#    needed for our problem

    # Creating my logistic regression model
log_reg_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm")

    # Creating my random forest model 
rand_forest_model <- rand_forest(mode = "classification") %>%
  set_engine("ranger")

# 4. Set up a workflow_set() to compare the logistic regression model 
#    (the winner of lecture here) to the rand_forest model you create. 
#    Use accuracy as you primary metric to rank the models.

    # Creating my recipe to feed into my workflow 
penguin_recipe <- recipe(species ~ ., data = penguins_train) %>%
  step_naomit(all_predictors())

    # Create the workflows
workflow_set <- workflow_set(
  preproc = list(penguin_recipe),
  models = list(
    log_reg = log_reg_model,
    rand_forest = rand_forest_model
  ),
  cross = TRUE
)

    # Create a workflow set to compare the models
workflow_set <- workflow_set(
  preproc = list(recipe),
  models = list(log_reg_model = log_reg_workflow, rand_forest_model = rand_forest_workflow),
  cross = TRUE, 
  case_weights = NULL)

# Fit the models with cross-validation
cv_folds <- vfold_cv(penguins_train, v = 5)

results <- workflow_map(
  workflow_set,
  resamples = cv_folds,
  metrics = metric_set(accuracy),
  verbose = TRUE
)

# 5. As a comment, write a sentence about what model you think is best!

    # ANSWER: I think that the logistic regression model is best because it is 
    #         more accurate than the random forest model with accuracy estimates
    #         of 0.9 or higher (unlike the random forest models of accuracy 
    #         estimates of 0.5 to 0.8). 

# 6. Submit your R script to Canvas





