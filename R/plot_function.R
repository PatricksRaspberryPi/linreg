#' @import ggplot2
#' @importFrom dplyr %>%
#' @import gridExtra
#'
#' @description Class method to generate two plots. First plot plots residuals vs predictions and second
#' plot shows the square root of the standardized residuals vs the predictions
#'
#' @title customized plot function
#'
#' @export
linreg$methods(plot = function() {
  rel_data <- data.frame(.self$predictions, .self$residuals, sqrt(abs(.self$residuals/sd(.self$residuals))))
  colnames(rel_data) <- c("predictions", "residuals", "standardized_res")

  points_to_print <- rel_data[c(99, 118, 119), ]

  #subtract a little amount from all predictions, in order for the label to be left from the point
  points_to_print$predictions <- points_to_print$predictions - 0.25

  #compute the medians for the plot
  grouped_median_residuals <- rel_data %>% group_by(predictions) %>% summarise(median=median(residuals))
  grouped_median_residuals_stand <- rel_data %>% group_by(predictions) %>% summarise(mean=mean(standardized_res))

  called_formula <- sprintf("linreg(%s)", format(.self$formula))

  #call ggplot to define what data should be plotted, + kind of plot
  #first plot
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


  #second plot
  second_plot <- ggplot(rel_data, aes(x=predictions, y=standardized_res)) +
    geom_point(shape="circle open") +
    scale_y_continuous(expression(sqrt("|Standardized residuals|")), breaks = seq(0,1.5, 0.5), limits = c(0, 1.8)) +
    scale_x_continuous(name = paste("Fitted values \n", called_formula, sep = "")) +
    ggtitle("Scale-Location") +
    geom_line(data = grouped_median_residuals_stand, aes(x=predictions, y=mean), color = "red") +
    geom_text(data = points_to_print, aes(x=predictions, y=standardized_res), label = rownames(points_to_print)) +
    coord_fixed(ratio = 1.4) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

  #arrange both plots in a grid
  grid.arrange(first_plot, second_plot, ncol=1, nrow=2, heights=c(5,5))
})

