#'
#' @description Definition of the Reference Class linreg_class for the linear regression model
#' in Lab 4.
#'
#' @title Linear Regression Reference Class
#'
#' @usage  linreg_class(formula, data, beta, predictions, residuals, df, res_var, beta_var, t_value, p_value)
#'
#' @field formula formula for building the model
#' @field data dataset that contains feature and target values
#' @field beta values of the model's coefficients
#' @field predictions values of the dependent variable predicted by the model
#' @field residuals values of the residuals
#' @field df degrees of freedom
#' @field res_var variance of the residuals
#' @field beta_var variance of the regression coefficients
#' @field t_value t-values for each coefficient
#' @field p_value p-values for each coefficient
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export linreg
#' @exportClass  linreg

linreg_class <- setRefClass(Class = "linreg",
                      fields = list(formula = "formula",
                                    data = "data.frame",
                                    beta = "matrix",
                                    predictions = "matrix",
                                    residuals = "matrix",
                                    df = "numeric",
                                    res_var = "matrix",
                                    beta_var = "matrix",
                                    t_value = "matrix",
                                    p_value = "matrix"))




#'
#' @description print out the coefficients and coefficient names
#'
#' @title customized print function
#' @usage linreg_class$plot()
#' @export

linreg_class$methods(show = function(){
  print("Call: ")
  print(sprintf("linreg(formula = %s, data = %s)", format(.self$formula), deparse(substitute(.self$data))))
  print("")
  print("Coefficients:")
  print(t(.self$beta))
})






#' @description Return the vector of the residuals
#'
#' @title Resid
#'
#' @usage linreg_class$resid()
#' @export

linreg_class$methods(resid = function(){
  return(.self$residuals)
})




#'
#' @description Return the vector of the predicted values
#'
#' @title Pred
#'
#' @usage linreg_class$pred()
#' @export

linreg_class$methods(pred = function(){
  return(.self$predictions)
})



#'
#' @description Return the vector of the coefficients
#'
#' @title Coef
#'
#' @usage linreg_class$coef()
#' @export

linreg_class$methods(coef = function(){
  return(.self$beta)
})



#'
#' @description Print out a summary for the linear regression
#'
#' @title Summary
#'
#' @usage linreg_class$summary()
#' @export

linreg_class$methods(summary = function(){

  coef_df <- data.frame(.self$beta, sqrt(.self$res_var), .self$t_value, .self$p_value)
  colnames(coef_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  print("Call: ")
  print(sprintf("linreg(formula = %s, data = %s)", format(.self$formula), deparse(substitute(.self$data))))
  print("")
  print("Residuals:")
  print(c("Min"=min(.self$residuals),
          "1Q"=quantile(.self$residuals)[[2]],
          "Median"=median(.self$residuals),
          "3Q"=quantile(.self$residuals)[[4]],
          "Max"=max(.self$residuals)))
  print("")
  print("Coefficients:")
  print(coef_df)
  print("")
  print(sprintf("Residual standard error: %f on %i degrees of freedom", sqrt(.self$res_var), .self$df))

})


