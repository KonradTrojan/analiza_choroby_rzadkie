library(tidyverse)
library(tidymodels)
set.seed(123)


# wczytanie danych

data <- read_excel("data.xlsx")
data <- data[-c(1)]



# tworzenie setów treningowego i testowego

proportion = 3/4

splitted_data <- initial_split(prepared_data, prop = proportion)

data_train <- training(splitted_data)
data_train_ = data_train

data_test <- testing(splitted_data)
data_test_ = data_test


# tworzenie modelu regresji logistycznej

fitted_logistic_model <- logistic_reg() %>%
  set_engine("glm")  %>%
  set_mode("classification") %>%
  fit(Response ~ ., data = data_train)

tidy(fitted_logistic_model)

pred_class <- predict(fitted_logistic_model,
                      new_data = data_test,
                      type = "class")

pred_proba <- predict(fitted_logistic_model,
                      new_data = data_test,
                      type = "prob")

pred_class[1:5,]
pred_proba[1:5,]

data_results <- data_test %>%
  select(Response) %>%
  bind_cols(pred_class, pred_proba)

data_results[1:5,]


conf_mat(data_results, truth = Response,
         estimate = .pred_class)

accuracy(data_results, truth = Response,
         estimate = .pred_class)

f_meas(data_results, truth = Response,
       estimate = .pred_class)
