# Load required packages
library(tidyverse)
library(tidymodels)

# Read and rename dataset
heart <- read_csv("data/heart.csv") %>%
  rename(
    Age = age,
    Sex = sex,
    ChestPainType = cp,
    RestingBP = trestbps,
    Cholesterol = chol,
    FastingBS = fbs,
    RestingECG = restecg,
    MaxHR = thalach,
    ExerciseAngina = exang,
    Oldpeak = oldpeak,
    Slope = slope,
    NumMajorVessels = ca,
    Thalassemia = thal,
    HeartDisease = target
  ) %>%
  mutate(
    Sex = as.factor(Sex),
    ChestPainType = as.factor(ChestPainType),
    RestingECG = as.factor(RestingECG),
    ExerciseAngina = as.factor(ExerciseAngina),
    Thalassemia = as.factor(Thalassemia),
    HeartDisease = as.factor(HeartDisease)
  )

# Split into training and test sets
set.seed(123)
split <- initial_split(heart, prop = 0.7, strata = HeartDisease)
heart_train <- training(split)
heart_test <- testing(split)

# Take 5 test cases for LIME/IML
sample_cases <- heart_test %>% slice_sample(n = 5)

# Preprocessing recipe for all models
heart_recipe <- recipe(HeartDisease ~ ., data = heart_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())
