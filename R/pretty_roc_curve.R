#' Plot pretty ROC curve
#'
#' This function plots a pretty ROC (receiver-operator curve) using the values from the ['tibble::tibble()] that
#' can be generated with the package function [get_threshold_data()].
#' G-mean as the unbiased evaluation metric for imbalanced classification is calculated from [sqrt(Recall*Specificity)].
#' This function colors the curve according to the value of the threshold.
#'
#' @param df - The tibble that can be generated with the package function [get_threshold_data()]
#' @param plot_title - A title for your plot
#' @param x_col - The column name of the values 2B plotted on the x-axis. Default = "fpr".
#' @param y_col - The column name of the values 2B plotted on the x-axis. Default = "tpr".
#' @param col_col - The column name of the values for coloring the points. Default = "threshold".
#' @param auc_col - The column name of the values that contain the AUC. Default = "roc-auc". Set to `NA` if you do
#'                  not want to show the AUC line
#' @param colors - A vector of colors from which a gradient will be generated.
#'                 An empty list will produce a single black line instead of a colored one.
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
#' roc <- pretty_roc_curve(df = data,
#'                         plot_title = "ROC curve")
#' show(roc$plot)
#'
#' @importFrom magrittr %>%
#'
#' @export
pretty_roc_curve <- function(df, plot_title = "Receiver-operator curve",
                             x_col = "fpr", y_col = "tpr", col_col = "threshold",
                             auc_col = "roc_auc_tr", colors = c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                                                "yellow", "#FF7F00", "red", "#7F0000"),
                             annotate = NULL) {

  df_wide <- df %>%
    dplyr::filter(Metric %in% c(x_col, y_col, col_col, auc_col, "Recall", "Specificity")) %>%
    unique() %>%
    tidyr::pivot_wider(names_from = Metric, values_from = Value)
  abline_data <- tibble::tibble(x = c(0, 1), y = c(0, 1))
  p <- ggplot2::ggplot(data = df_wide) +
    ggplot2::geom_line(data = abline_data, aes(x = x, y = y), linetype = "dashed", color = "grey") +
    ggplot2::geom_path(aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[col_col]]),
                       size = 2, linejoin = "round", lineend = "round") +
    lims(x = c(0, 1), y = c(0, 1))
  if (length(colors) > 0) {
    p <- p +
      ggplot2::scale_color_gradientn(colors = colors, space = "Lab")
  }
  if(!is.null(annotate) & is.numeric(annotate)) {
    tmp <- df_wide %>% dplyr::filter(threshold == annotate)
    p <- p +
      ggplot2::geom_point(data = tmp, aes(x = .data[[x_col]], y = .data[[y_col]]), size = 5) +
      ggplot2::geom_text(data = tmp, aes(x = .data[[x_col]], y = .data[[y_col]]),
                         size = 5, label = annotate, hjust = -0.5, vjust = 0.1)
  }
  p <- p +
    ggplot2::labs(title = plot_title,
                  subtitle = "Thresholds are sampled from the predicted values",
                  x = "FPR", y = "TPR") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(face = "italic")
    )
  return(p)
}
