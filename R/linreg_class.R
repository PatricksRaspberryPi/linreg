#'
#' @description Definition of the Reference Class linreg for the linear regression model
#' in Lab 4.
#'
#' @title Linear Regression Reference Class
#'
#' @usage  linreg(formula, data, beta, predictions, residuals, df, res_var, beta_var, t_value, p_value)
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
#' @export linreg
#' @exportClass linreg

linreg <- setRefClass(Class = "linreg",
                      fields = list(formula = "formula",
                                    data = "data.frame",
                                    beta = "matrix",
                                    predictions = "matrix",
                                    residuals = "matrix",
                                    df = "numeric",
                                    res_var = "matrix",
                                    beta_var = "matrix",
                                    t_value = "matrix",
                                    p_value = "matrix",
                                    name_dataset = "character",
                                    col_names = "character"),

                      methods = list(

                        initialize = function(formula, data){


                        if (!(inherits(formula, "formula") & is.data.frame(data))) {
                          stop("Wrong input!")
                        }

                        stopifnot("invalid input" = all(all.vars(formula) %in% colnames(data)))


                        X <- model.matrix(formula, data)
                        #convert y to matrix, otherwise the computations won't work.
                        y <- as.matrix(data[all.vars(formula)[1]])

                        .self$col_names <<- colnames(X)
                        .self$formula <<- formula
                        .self$data <<- data
                        .self$name_dataset <<- deparse(substitute(data))
                        .self$beta <<- solve((t(X) %*% X)) %*% t(X) %*% y
                        .self$predictions <<- X %*% beta
                        .self$residuals <<- y - predictions
                        .self$df <<- nrow(X) - ncol(X)
                        .self$res_var <<- (t(residuals) %*% residuals)/df
                        .self$beta_var <<- res_var[[1,1]] * solve(t(X) %*% X)
                        .self$t_value <<- beta/sqrt(var(beta))[1]
                        .self$p_value <<- pt(q=t_value, df=df)

                        return(.self)
                      }))




#'
#' @description print out the coefficients and coefficient names
#'
#' @title customized print function
#' @usage linreg$plot()
#' @export

linreg$methods(print = function(){

  to_print <- sprintf("linreg(formula = %s, data = iris)", format(.self$formula))
  coef_output <- beta
  rownames(coef_output) <- .self$col_names

  cat("Call:",
      toString(to_print),
      "Coefficients:",
      t(coef_output))
})






#' @description Return the vector of the residuals
#'
#' @title Resid
#'
#' @usage linreg$resid()
#' @export

linreg$methods(resid = function(){
  return(.self$residuals)
})




#'
#' @description Return the vector of the predicted values
#'
#' @title Pred
#'
#' @usage linreg$pred()
#' @export

linreg$methods(pred = function(){
  return(.self$predictions)
})



#'
#' @description Return the vector of the coefficients
#'
#' @title Coef
#'
#' @usage linreg$coef()
#' @export

linreg$methods(coef = function(){
  return(.self$beta)
})



#'
#' @description Print out a summary for the linear regression
#'
#' @title Summary
#'
#' @usage linreg$summary()
#' @export

linreg$methods(summary = function(){

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
