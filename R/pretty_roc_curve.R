#' Plot pretty ROC curve
#'
#' This function plots a pretty ROC (receiver-operator curve) using the values from the [tidyverse::tibble()] that
#' can be generated with the package function [get_threshold_data()].
#' It colors the curve according to the value of the threshold, and annotates and labels the threshold with maximal AUC.
#'
#' @param df - The tibble that can be generated with the package function [get_threshold_data()]
#' @param plot_title - A title for your plot
#' @param x_col - The column name of the values 2B plotted on the x-axis. Default = "fpr".
#' @param y_col - The column name of the values 2B plotted on the x-axis. Default = "tpr".
#' @param col_col - The column name of the values for coloring the points. Default = "threshold".
#' @param auc_col - The column name of the values that contain the AUC. Default = "roc-auc". Set to `NA` if you do
#'                  not want to show the AUC line
#' @param colors - A vector of colors from which a gradient will be generated.
#'                 Default: c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
#'
#' @return A list containing the plot as ggplot2, the maximal AUC (or [auc_col]), the index of this max value in the
#'         provided [tibble()], the threshold that corresponds to this max value.
#'
#' @examples
#' data <- get_threshold_data(truth = y_true, prediction = y_predicted)
#' roc <- pretty_roc_curve(df = data,
#'                         plot_title = "ROC curve")
#' show(roc$plot)
#'
#' @export
pretty_roc_curve <- function(df, plot_title, x_col = "fpr", y_col = "tpr", col_col = "threshold", auc_col = "roc_auc",
                             colors = c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                        "yellow", "#FF7F00", "red", "#7F0000")) {
  max_auc <- df[[auc_col]] %>% max()
  idx_max_auc <- which(df[[auc_col]] == max_auc)
  tr_max_auc <- df[[col_col]][idx_max_auc] %>% round(digits = 2)
  max_auc <- max_auc %>% round(digits = 2)
  p <- ggplot2::ggplot(data = df) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")
  if (!is.na(auc_col)) {
    p <- p + ggplot2::geom_line(aes(x = .data[[x_col]], y = .data[[auc_col]]), color = "black")
  }
  p <- p +
    ggplot2::geom_point(aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[col_col]])) +
    ggplot2::geom_point(x = df[[x_col]][idx_max_auc], y = df[[y_col]][idx_max_auc],
                        pch = 21, color = "black", size = 5) +
    ggplot2::geom_text(x = df[[x_col]][idx_max_auc], y = df[[y_col]][idx_max_auc],
                       label = paste0("AUC = ", max_auc), vjust = -2, hjust = 1.5, size = 5, fontface = "plain") +
    ggplot2::geom_text(x = df[[x_col]][idx_max_auc], y = df[[y_col]][idx_max_auc],
                       label = paste0("threshold = ", tr_max_auc), vjust = 0, hjust = 1.5, size = 5, fontface = "plain") +
    ggplot2::scale_color_gradientn(colors = jet_colors, space = "Lab") +
    ggplot2::labs(title = plot_title,
                  subtitle = "Thresholds are sampled from the predicted values",
                  caption = paste(sep = "\n",
                                  "The Black line indicates AUC in dependency of the threshold.",
                                  "The selected point on the ROC curve is the threshold with maximal AUC."),
                  x = "FPR", y = "TPR") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(face = "italic")
    )
  return(list("plot" = p,
              "max_auc" = max_auc,
              "max_auc_idx" = idx_max_auc,
              "max_auc_threshold" = tr_max_auc))
}
