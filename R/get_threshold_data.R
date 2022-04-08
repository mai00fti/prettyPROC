#' Calculate model metrics
#'
#' This function calculates a bunch of model metrics that can be plotted in dependency of the threshold that separates
#' classes 0 and 1. The provided vectors must be of the same length and the indices correspond to the same sample. The
#' function iterates over all predicted values and computes the following metrics for each threshold:
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
#' All values are returned in a [tidyverse::tibble()] with the columns
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
#' @export
get_threshold_data <- function(truth, prediction) {
  truth_f <- factor(round(truth), levels = c(1, 0))
  LEN <- length(prediction)
  P <- sum(truth)
  N <- LEN - P
  AUC <- ModelMetrics::auc(actual = truth, predicted = prediction)
  thresholds <- 1:1000 /1000
  # loop through all unique prediction values and use these as thresholds
  return(do.call(rbind, lapply(thresholds, function(tr) {
    prediction_tr <- ifelse(prediction >= tr, 1, 0)
    prediction_tr_f <- factor(prediction_tr, levels = c(1, 0))
    cfm <- caret::confusionMatrix(data = prediction_tr_f, reference = truth_f)
    rbind(tibble::enframe(cfm$byClass, name = "Metric", value = "Value"),
          cfm$table %>%
            tibble::as_tibble() %>%
            dplyr::mutate("Metric" = c("TP", "FN", "FP", "TN")) %>%
            dplyr::select(Metric, n) %>%
            dplyr::rename("Value" = "n"),
          tibble::tibble_row(Metric = "P", Value = P),
          tibble::tibble_row(Metric = "N", Value = N),
          tibble::tibble_row(Metric = "N_samples", Value = LEN),
          tibble::tibble_row(Metric = "pr_baseline", Value = P / LEN),
          tibble::tibble_row(Metric = "P_pred", Value = sum(prediction)),
          tibble::tibble_row(Metric = "N_pred", Value = LEN - sum(prediction)),
          tibble::tibble_row(Metric = "fpr", Value = fpr(tn = cfm$table[1], fp = cfm$table[2])),
          tibble::tibble_row(Metric = "tpr", Value = tpr(tp = cfm$table[4], fn = cfm$table[3])),
          tibble::tibble_row(Metric = "tnr", Value = tnr(tn = cfm$table[1], fp = cfm$table[2])),
          tibble::tibble_row(Metric = "fnr", Value = fnr(tp = cfm$table[4], fn = cfm$table[3])),
          tibble::tibble_row(Metric = "roc_auc_tr", Value = ModelMetrics::auc(truth_f, prediction_tr_f))) %>%
      dplyr::mutate("threshold" = tr) %>%
      dplyr::mutate("roc_auc" = AUC)
  }))
  )
}
