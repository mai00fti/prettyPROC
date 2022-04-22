#' Plot model metrics across classification threshold
#'
#' This function uses the tibble that can be generated with the package function [get_threshold_data()] and plots
#' the values of selected metrics in dependency of the threshold.
#' This can be inspiring to evaluate the selection of a threshold for different learning aims.
#' A selected threshold value can be provided to set a vertical mark in the plot
#'
#' @param df - The tibble that can be generated with the package function [get_threshold_data()]
#' @param metrics - A character vectore of metrics that will be plotted. Note, that some of the returned metrics mean
#'                  principally the same: Recall = Sensitivity, Precision = PPV, BalancedAccuracy = roc_auc.
#'                  The available metrics are: "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value",
#'                                             "Precision", "Recall", "F1", "Prevalence", "Detection Rate",
#'                                             "Detection Prevalence", "Balanced Accuracy", "TP", "FN", "FP",
#'                                             "TN", "P", "N", "N_samples", "pr_baseline", "P_pred", "N_pred", "fpr",
#'                                             "tpr", "tnr", "fnr", "roc_auc"
#'
#' @param plot_title - A title for your plot
#' @param annotate - The threshold that should be marked with a vertical dashed line. Optional.
#'
#' @return A ggplot2 object is returned which can be extended by further ggplot functions, be showed and saved. NULL is
#'         returned, if none of the requested metrics is available
#'
#' @examples
#' y_true <- sample(c(0,1), replace = TRUE, size = 1000)
#' y_predicted <- runif(1000)
#' data <- get_threshold_data(truth = y_true, prediction = y_predicted)
#' plot <- model_metrics_curves(df = data)
#' show(plot)
#'
#' @export
model_metrics_curves <- function(df,
                                 metrics = c("Sensitivity", "F1", "Balanced Accuracy"),
                                 plot_title = "Metrics development across threshold",
                                 annotate = 0.5) {
  available_metrics <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall",
                         "F1", "Prevalence", "Detection Rate", "Detection Prevalence", "Balanced Accuracy", "TP",
                         "FN", "FP", "TN", "P", "N", "N_samples", "pr_baseline", "P_pred", "N_pred", "fpr", "tpr",
                         "tnr", "fnr", "roc_auc")

  metrics_not_available <- metrics[which(!metrics %in% available_metrics)]
  if (length(metrics_not_available > 0)) {
    write(paste0("The following metric is not available and is discarded: '", metrics_not_available, "'", stderr()))
  }
  metrics <- metrics[which(metrics %in% available_metrics)]
  if (length(metrics) > 0) {
    p <- ggplot2::ggplot(df %>% filter(Metric %in% metrics),
                         ggplot2::aes(x = threshold, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(color = Metric))
    if (length(annotate) == 0 | !is.na(annotate)) {
      p <- p +
        ggplot2::geom_vline(xintercept = annotate, linetype = "dashed", color = "grey")
    }
    p <- p +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
      ggplot2::labs(title = plot_title,
                    subtitle = "100 thresholds selected, evenly distributed between 0 and 1",
                    x = "Threshold") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = "top",
        plot.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(face = "italic")
      )
    return(p)
  } else {
    write(paste0("None of your metrics is available. Returning NULL.", stderr()))
    return(NULL)
  }
}
