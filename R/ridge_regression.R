#' Ridge Regression Class
#'
#' Definition of the Reference Class ridgereg for the ridge regression model in
#' the Bonus Lab.
#'
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
#' @field lambda hyperparameter to tune models
#'
#'
#'
#' @export ridgereg
#' @exportClass ridgereg

ridgereg <- setRefClass(Class = "ridgereg",

                      fields = list(formula = "formula",
                                    data = "data.frame",
                                    beta = "matrix",
                                    predictions = "matrix",
                                    lambda = "numeric",
                                    col_names = "character"),


                      methods = list(

                        initialize = function(formula, data, lambda){

                          if (!(inherits(formula, "formula") & is.data.frame(data))) {
                            stop("Wrong input!")
                          }

                          stopifnot("invalid input" = all(all.vars(formula) %in% colnames(data)))


                          X <- model.matrix(formula, data)

                          X_norm <- cbind(X[, 1], scale(X[,2:ncol(X)]))
                          ext_print(X_norm)
                          #convert y to matrix, otherwise the computations won't work.
                          y <- as.matrix(data[all.vars(formula)[1]])

                          .self$lambda <<- lambda
                          .self$col_names <<- colnames(X)
                          .self$formula <<- formula
                          .self$data <<- data
                          .self$beta <<- solve((t(X_norm) %*% X_norm) + lambda*diag(ncol(X_norm))) %*% (t(X_norm) %*% y)
                          .self$predictions <<- X %*% beta


                          return(.self)
                        },

                        # print function: object$print()
                        print = function(){

                          to_print <- sprintf("linreg(formula = %s, data = iris)", format(.self$formula))
                          coef_output <- beta
                          rownames(coef_output) <- .self$col_names

                          cat("Call:",
                              toString(to_print),
                              "\nCoefficients:\n")

                          ext_print(t(coef_output))
                        },


                        # show function is used to change what is shown when print(object) is called
                        show = function(){

                          to_print <- sprintf("linreg(formula = %s, data = iris)", format(.self$formula))
                          coef_output <- beta
                          rownames(coef_output) <- .self$col_names

                          cat("Call:",
                              toString(to_print),
                              "\nCoefficients:\n")

                          ext_print(t(coef_output))
                        },



                        predict = function() {
                          return(.self$predictions)
                        },


                        coef = function(){
                          return(.self$beta)
                        }
                      )
)

# library("MASS")
# a <- matrix(c(1,1,1,1,5,6,20,30,40,50,70,30), nrow=4)
# a
# (t(a) %*% a)
# data("iris")
#
# c <- scale(a)
#
#
#
# iris.scaled <- iris
# iris.scaled$Sepal.Width <- scale(iris.scaled$Sepal.Width)
# iris.scaled$Sepal.Length <- scale(iris.scaled$Sepal.Length)
# ridgereg_MASS <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris.scaled, lambda = 2)
# try_1 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris.scaled, lambda = 2)
# ridgereg_MASS
# try_1$coef()
