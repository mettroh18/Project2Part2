#' Random Forest Cross Validation on Penguins Data
#'
#' Designed specifically for the Penguins Data in R, this function will
#'   predict output body_mass_g using covariates bill_length_mm,
#'   bill_depth_mm, and flipper_length_mm.
#'
#' @param k Numeric input data representing the number of folds within data.
#'
#' @keywords cross-validation, forest, prediction
#' @import dplyr
#' @import randomForest
#'
#' @return Numeric value of the cross validation error.
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(4)
#' my_rf_cv(3)
#'
#' @export

my_rf_cv <- function(data, k) {
  n <- nrow(data)
  inds <- sample(rep(1:k, length = n))
  data$fold <- inds

  #setting aside the true values of body_mass_g
  true_mass <- data$body_mass_g

  #for each fold 'k', create a model using the randomForest() algorithm
  #using this model, we predict the data and record the mean squared error
  MSE <- c()
  for (i in 1:k) {
    data_train <- data %>% filter(fold != i) %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
    data_test <- data %>% filter(fold == i) %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm)

    test_values <- data %>% filter(fold == i) %>% select(body_mass_g)
    test_values <- test_values[,1, drop = TRUE]

    MODEL <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)
    predictions <- predict(MODEL, data_test)

    differences <- predictions - test_values
    mse <- mse <- sum(differences * differences) / length(predictions)
    MSE <- append(MSE, mse)
  }
  return (mean(MSE))
}
