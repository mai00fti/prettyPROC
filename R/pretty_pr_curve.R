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
#' @param annotate A threshold between 0 and 1 (digits = 2) that should be annotated at the curve. You may select one
#'                 of the thresholds that you can get using the package function [select_threshold].
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
#' @importFrom magrittr %>%
#'
#' @export
pretty_pr_curve <- function(df, plot_title = "Precision-recall curve",
                            x_col = "Recall", y_col = "Precision", col_col = "threshold", f1_col = "F1",
                            colors = c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                       "yellow", "#FF7F00", "red", "#7F0000"),
                            annotate = NULL) {
  baseline <- df %>%
    dplyr::filter(Metric == "pr_baseline") %>%
    head(1) %>%
    dplyr::pull(Value)
  df_wide <- df %>%
    dplyr::filter(Metric %in% c(x_col, y_col, col_col, f1_col, "Recall", "Specificity")) %>%
    unique() %>%
    tidyr::pivot_wider(names_from = Metric, values_from = Value)
  p <- ggplot(data = df_wide) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = baseline), linetype = "dashed", color = "grey") +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::geom_path(ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[col_col]]),
                       size = 2, linejoin = "round", lineend = "round")
  if (length(colors) > 0) {
    p <- p +
      scale_color_gradientn(colors = colors, space = "Lab")
  }
  if (!is.null(annotate) & is.numeric(annotate)) {
    tmp <- df_wide %>% dplyr::filter(threshold == annotate)
    p <- p +
      ggplot2::geom_point(data = tmp, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]]), size = 5) +
      ggplot2::geom_text(data = tmp, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]]),
                         size = 5, label = annotate, hjust = -0.5, vjust = 0.1)
  }
  p <- p +
    ggplot2::labs(title = plot_title,
         subtitle = "100 thresholds selected, evenly distributed between 0 and 1") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = c(0.9, 0.9),
      legend.box.background = ggplot2::element_rect(color = "grey"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(face = "italic")
    )
  return(p)
}
