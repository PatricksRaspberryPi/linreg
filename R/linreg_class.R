#' @title Linear Regression Reference Class
#'
#' Definition of the Reference Class for the linear regression model
#' in Lab 4.
#'
#' @usage  a A number
#' @param b A number
#' @return The greatest common divisor
#' @examples
#' euclidean(100, 1000)
#' @export

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






linreg_class$methods(show = function(){
  print("Call: ")
  print(sprintf("linreg(formula = %s, data = %s)", format(.self$formula), deparse(substitute(.self$data))))
  print("")
  print("Coefficients:")
  print(t(beta))
})


#' @import ggplot2
#' @import dplyr
linreg_class$methods(plot = function() {
  rel_data <- data.frame(.self$predictions, .self$residuals, sqrt(abs(.self$residuals/sd(.self$residuals))))
  colnames(rel_data) <- c("predictions", "residuals", "standardized_res")

  points_to_print <- rel_data[c(99, 118, 119), ]

  #subtract a little amount from all predictions, in order for the label to be left from the point
  points_to_print$predictions <- points_to_print$predictions - 0.25

  #compute the medians for the plot
  grouped_median_residuals <- rel_data %>% group_by(predictions) %>% summarise(median=median(residuals))
  grouped_median_residuals_stand <- rel_data %>% group_by(predictions) %>% summarise(median=median(standardized_res))

  called_formula <- sprintf("linreg(%s)", format(.self$formula))

  #call ggplot to define what data should be plotted, + kind of plot
  first_plot <- ggplot(rel_data, aes(x=predictions, y=residuals)) +
  geom_point(shape="circle open") +
  scale_y_continuous(name = "Residuals", breaks = seq(-1.5,1.5, 0.5), limits = c(-1.6, 1.6)) +
  scale_x_continuous(name = paste("Fitted values \n", called_formula, sep = ""))  +
  ggtitle("Residuals vs Fitted") +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_line(data = grouped_median_residuals, aes(x=predictions, y=median), color = "red") +
  geom_text(data = points_to_print, aes(x=predictions, y=residuals), label = rownames(points_to_print)) +
  coord_fixed(ratio = 0.8) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

  print(rel_data)

  second_plot <- ggplot(rel_data, aes(x=predictions, y=standardized_res)) +
    geom_point(shape="circle open") +
    scale_y_continuous(expression(sqrt("|Standardized residuals|")), breaks = seq(0,1.5, 0.5), limits = c(0, 1.8)) +
    scale_x_continuous(name = paste("Fitted values \n", called_formula, sep = "")) +
    ggtitle("Scale-Location") +
    geom_line(data = grouped_median_residuals_stand, aes(x=predictions, y=median), color = "red") +
    geom_text(data = points_to_print, aes(x=predictions, y=standardized_res), label = rownames(points_to_print)) +
    coord_fixed(ratio = 1.4) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

  grid.arrange(first_plot, second_plot, ncol=1, nrow=2, heights=c(5,5))

  return(rel_data)
})

data(iris)
formula <- Petal.Length ~ Species
c <- linreg(formula=formula, data=iris)
k <- c$plot()

rownames(k[k$standardized_res > 1.4, ])

data.frame(c$predictions, c$residuals)

d <- rbind(iris[order(iris$Sepal.Length), c("Sepal.Length", "Sepal.Width")][1, ], tail(iris[order(iris$Sepal.Length), c("Sepal.Length", "Sepal.Width")], 2))
d$Sepal.Length <- d$Sepal.Length - 1


hallo <- iris[1:10, ]

hallo[["probe"]] <- 21:30

summarise(grouped, median)
