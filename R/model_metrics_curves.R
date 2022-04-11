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
#' @param threshold - The threshold that should be marked with a vertical dashed line. Optional.
#'
#' @return A ggplot2 object is returned which can be extended by further ggplot functions, be showed and saved. NULL is
#'         returned, if none of the requested metrics is available
#'
#' @examples
#' y_true <- sample(c(0,1), replace = TRUE, size = 1000)
#' y_predicted <- runif(1000)
#' data <- get_threshold_data(truth = y_true, prediction = y_predicted)
#' plot <- model_metrics_curves(df = data,
#'                              metrics = c("Sensitivity", "F1", "Balanced Accuracy"),
#'                              plot_title = "SENS, F1 and balanced Accuracy across classification threshold",
#'                              threshold = 0.5)
#' show(plot)
#'
#' @export
model_metrics_curves <- function(df, metrics, plot_title, threshold = NA) {
  available_metrics <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall",
                         "F1", "Prevalence", "Detection Rate", "Detection Prevalence", "Balanced Accuracy", "TP",
                         "FN", "FP", "TN", "P", "N", "N_samples", "pr_baseline", "P_pred", "N_pred", "fpr", "tpr",
                         "tnr", "fnr", "roc_auc")

  metrics_not_available <- metrics[which(!metrics %in% available_metrics)]
  if(length(metrics_not_available > 0)) {
    write(paste0("The following metric is not available and is discarded: '", metrics_not_available, "'", stderr()))
  }
  metrics <- metrics[which(metrics %in% available_metrics)]
  if(length(metrics) > 0) {
  p <- ggplot(df %>% filter(Metric %in% metrics),
              aes(x = threshold, y = Value)) +
    geom_line(aes(color = Metric))
  if (length(threshold) == 0 | !is.na(threshold)) {
    p <- p +
      geom_vline(xintercept = threshold, linetype = "dashed", color = "grey")
  }
  p <- p +
    guides(color = guide_legend(nrow = 1)) +
    labs(title = plot_title,
         subtitle = "Thresholds are sampled from the predicted values",
         caption = paste(sep = "\n",
                         # " Some metrics mean the same:",
                         "Recall = Sensitivity, Precision = PPV, BalancedAccuracy = roc_auc")) +
    theme_classic() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(face = "italic")
    )
    return(p)
  } else {
    write(paste0("None of your metrics is available. Returning NULL.", stderr()))
    return(NULL)
  }
}
