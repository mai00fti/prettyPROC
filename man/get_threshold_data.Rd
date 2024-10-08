% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_threshold_data.R
\name{get_threshold_data}
\alias{get_threshold_data}
\title{Calculate model metrics}
\usage{
get_threshold_data(truth, prediction)
}
\arguments{
\item{truth}{Vector of true values}

\item{prediction}{Vector of predicted values}
}
\value{
A tibble containing all metrics with their values for each threshold
}
\description{
This function calculates a bunch of model metrics that can be plotted in dependency of the threshold that separates
classes 0 and 1. The provided vectors must be of the same length and the indices correspond to the same sample. The
function iterates over all predicted values and computes the following metrics for each threshold:
}
\details{
`TP, FN, FP, TN` - the confusion matrix values;
`P_pred, N_pred` - the number of positive and negative predictions
`Sensitivity, Specificity, Pos Pred Value, Neg Pred Value,`
`Precision, Recall, F1,`
`Prevalence, Detection Rate, Detection Prevalence, Balanced Accuracy,`
`fpr, tpr, tnr, fnr`

And the values that are not dependent on the threshold:

`roc_auc` - the area under the receiver-operator curve;
`P, N, N_samples` - The numbers of positive, negative and total samples (extracted from the truth vector);
`pr_baseline` - The baseline for a precision-recall curve. `pr_baseline = P / N_samples`

All values are returned in a [tibble::tibble()] with the columns
`Metric` - containing the name of the metric;
`Value` - containing the value of the metric;
`threshold` - containing the threshold, for threshold independet metrics, the metric value is the same for all
              thresholds

You may plot these metrics along the different thresholds with the package function [model_metrics_curves()].
}
\examples{
y_true <- sample(c(0,1), replace = TRUE, size = 1000)
y_predicted <- runif(1000)
data <- get_threshold_data(truth = y_true, prediction = y_predicted)
data \%>\% head()
data \%>\% colnames()

}
