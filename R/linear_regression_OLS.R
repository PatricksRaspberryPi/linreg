linreg <- function(formula, data) {

  X <- model.matrix(formula, data)
  #convert y to matrix, otherwise the computations won't work.
  y <- as.matrix(data[all.vars(formula)[1]])

  beta <- solve((t(X) %*% X)) %*% t(X) %*% y
  predictions <- X %*% beta
  residuals <- y - predictions
  df <- nrow(X) - ncol(X)
  res_var <- (t(residuals) %*% residuals)/df
  beta_var <- res_var[[1,1]] * solve(t(X) %*% X)
  t_value <- beta/sqrt(var(beta))[1]
  p_value <- pt(q=t_value, df=df)

  result <- linreg_class()

  result$formula <- formula
  result$data <- data
  result$beta <- beta
  result$predictions <- predictions
  result$residuals <- residuals
  result$df <- df
  result$res_var <- res_var
  result$beta_var <- beta_var
  result$t_value <- t_value
  result$p_value <- p_value

  return(result)
}



