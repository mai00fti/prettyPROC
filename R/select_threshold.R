#' Select the best classification threshold
#'
#' This function calculates the best threshold for a set of metrics. The metric names must be present in the `Metric`
#' column of the data frame. The data frame can be generated with the package function [get_threshold_data()].
#' The metric value and the selected threshold that maximizes or minizmises the metric is returned.
#'
#' `TP, FN, FP, TN` - the confusion matrix values;
#' `P_pred, N_pred` - the number of positive and negative predictions
#' `Sensitivity, Specificity, Pos Pred Value, Neg Pred Value,`
#' `Precision, Recall, F1,`
#' `Prevalence, Detection Rate, Detection Prevalence, Balanced Accuracy,`
#' `fpr, tpr, tnr, fnr`
#'
#' And the values that are not dependent on the threshold:
#'
#' `roc_auc` - the area under the receiver-operator curve;
#' `P, N, N_samples` - The numbers of positive, negative and total samples (extracted from the truth vector);
#' `pr_baseline` - The baseline for a precision-recall curve. `pr_baseline = P / N_samples`
#'
#' All values are returned in a [tibble::tibble()] with the columns
#' `Metric` - containing the name of the metric;
#' `Value` - containing the value of the metric;
#' `threshold` - containing the threshold, for threshold independet metrics, the metric value is the same for all
#'               thresholds
#'
#' You may plot these metrics along the different thresholds with the package function [model_metrics_curves()].
#'
#' @param truth Vector of true values
#' @param prediction Vector of predicted values
#'
#' @return A tibble containing all metrics with their values for each threshold
#'
#' @examples
#' y_true <- sample(c(0,1), replace = TRUE, size = 1000)
#' y_predicted <- runif(1000)
#' data <- get_threshold_data(truth = y_true, prediction = y_predicted)
#' data %>% head()
#' data %>% colnames()
#'
#' data_thresholds <- select_threshold(df = data, metrics = c("mcc_tr"), optimize = c("max"))
#'
#' @importFrom magrittr %>%
#'
#' @export
select_threshold <- function(df,
                             metrics = c("mcc_tr", "Gmean", "F1", "Balanced Accuracy", "Precision", "Recall"),
                             optimize = c("max", "max", "max", "max", "max", "max")) {

  names(optimize) <- metrics

  # compute Gmean
  tmp <- df %>% dplyr::filter(Metric %in% c("Recall", "Specificity"))
  df <- rbind(df,
              do.call(rbind, lapply(tmp$threshold %>% unique(), function(t) {
                row <- tmp %>%
                  dplyr::filter(threshold == t) %>%
                  tidyr::pivot_wider(names_from = Metric, values_from = Value)
                tibble::tibble_row(Metric = "Gmean", Value = sqrt(row$Recall * row$Specificity),
                                   threshold = t, roc_auc = tmp$roc_auc[1])
              })))

  # select all best thresholds
  return(do.call(rbind, lapply(metrics, function(m) {
    tmp <- df %>% dplyr::filter(Metric == m)
    opt_metric <- tmp %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::pull(Value)
    opt_metric <- ifelse(optimize[[m]] == "max", max(opt_metric), min(opt_metric))
    opt_metric_value <- tmp %>%
      dplyr::filter(Value >= opt_metric) %>%
      dplyr::pull(Value) %>%
      min()
    opt_metric_value_idx <- which(tmp$Value == opt_metric_value)[1]
    dplyr::slice(tmp, opt_metric_value_idx) %>% dplyr::select(Metric, Value, threshold)
  })))
}
