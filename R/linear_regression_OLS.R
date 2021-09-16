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

  res <- list("beta"=beta, "predictions"=predictions, "residuals"=residuals,
              "degrees of freedom"=df, "residual variance"=res_var, "beta variance"=beta_var,
              "t_value"=t_value, "p-value"=p_value)
  print(res)
}


formula <- Petal.Length ~ Species
data(iris)
linreg(formula, iris)
