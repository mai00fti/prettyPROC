#' Pretty Precision-Recall curves
#'
#' This function plots a pretty PR (precision-recall curve) using the values from the [tidyverse::tibble()] that
#' can be generated with the package function [get_threshold_data()].
#' It colors the curve according to the value of the threshold.
#'
#' @param df - The tibble that can be generated with the package function [get_threshold_data()]
#' @param plot_title - A title for your plot
#' @param x_col - The column name of the values 2B plotted on the x-axis. Default = "Recall".
#' @param y_col - The column name of the values 2B plotted on the x-axis. Default = "Precision".
#' @param col_col - The column name of the values for coloring the points. Default = "threshold".
#' @param auc_col - The column name of the values that contain the AUC. Default = "roc-auc". Set to `NA` if you do
#'                  not want to show the AUC line
#' @param colors - A vector of colors from which a gradient will be generated.
#'                 Default: c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
#'
#' @return The plot as ggplot2.
#'
#' @examples
#' y_true <- sample(c(0,1), replace = TRUE, size = 1000)
#' y_predicted <- runif(1000)
#' data <- get_threshold_data(truth = y_true, prediction = y_predicted)
#' pr <- pretty_pr_curve(df = data,
#'                       plot_title = "Precision-recall curve")
#' show(pr$plot)
#'
#' @export
pretty_pr_curve <- function(df, plot_title = "Precision-recall curve",
                            x_col = "Recall", y_col = "Precision", col_col = "threshold", f1_col = "F1",
                            colors = c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                       "yellow", "#FF7F00", "red", "#7F0000")) {
  baseline <- df %>%
    filter(Metric == "pr_baseline") %>%
    head(1) %>%
    pull(Value)
  df_wide <- df %>%
    dplyr::filter(Metric %in% c(x_col, y_col, col_col, f1_col, "Recall", "Specificity")) %>%
    unique() %>%
    tidyr::pivot_wider(names_from = Metric, values_from = Value)
  p <- ggplot(data = df_wide) +
    geom_hline(aes(yintercept = baseline), linetype = "dashed", color = "grey") +
    lims(x = c(0, 1), y = c(0, 1)) +
    geom_path(aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[col_col]]),
              size=2, linejoin = "round", lineend = "round")
  if (length(colors) > 0) {
    p <- p +
      scale_color_gradientn(colors = colors, space = "Lab")
  }
  p <- p +
    labs(title = plot_title,
         subtitle = "Thresholds are sampled from the predicted values") +
    theme_classic() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(face = "italic")
    )
  return(p)
}
