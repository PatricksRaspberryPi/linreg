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



linreg_class$methods(plot = function() {
  rel_data <- data.frame(.self$predictions, .self$residuals)
  colnames(rel_data) <- c("predictions", "residuals")

  points_to_print <- rbind(rel_data[order(rel_data$residuals), ][1, ],
                           tail(rel_data[order(rel_data$residuals), ], 2))
  #subtract a little amount from all predictions, in order for the label to be left from the point
  points_to_print$predictions <- points_to_print$predictions - 0.25


  print(rel_data)

  #call ggplot to define what data should be plotted, + kind of plot
  ggplot(rel_data, aes(x=predictions, y=residuals)) +
  geom_point(shape="circle open") +
  scale_y_continuous(name = "Residuals", breaks = seq(-1.5,1.5, 0.5), limits = c(-1.6, 1.6)) +
  scale_x_continuous(name = "Fitted values") +
  ggtitle("Residuals vs Fitted") +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_text(data = points_to_print, label = points_to_print$residuals) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
})

library(ggplot2)
data(iris)
formula <- Petal.Length ~ Species
c <- linreg(formula=formula, data=iris)
c$plot()


data.frame(c$predictions, c$residuals)

d <- rbind(iris[order(iris$Sepal.Length), c("Sepal.Length", "Sepal.Width")][1, ], tail(iris[order(iris$Sepal.Length), c("Sepal.Length", "Sepal.Width")], 2))
d$Sepal.Length <- d$Sepal.Length - 1

unique(iris["Species"])

c
