#' True positive rate
#'
#' This function returns the true positive rate given the number of true-positives and false-negatives.
#' The respective formula is tpr = tp / (tp + fn)
#'
#' @param tp Number of true-positives
#' @param fn Number of false-neagtives
#' @return The value of the true positive rate
#'
#' @examples
#' #          Reference
#' # Prediction    1    0
#' #          1  892  398
#' #          0  446 2278
#' tpr(tp = 892, fn = 446)
#'
tpr <- function(tp, fn) {
  return(tp / (tp + fn))
}

#' Fales positive rate
#'
#' This function returns the false positive rate given the number of true-negatives and false-positives.
#' The respective formula is fpr = fp / (fp + tn)
#'
#' @param tn Number of true-negatives
#' @param fp Number of false-positives
#' @return The value of the false positive rate
#'
#' @examples
#' #          Reference
#' # Prediction    1    0
#' #          1  892  398
#' #          0  446 2278
#' tpr(tn = 2278, fp = 398)
#'
fpr <- function(tn, fp) {
  return(fp / (fp + tn))
}

#' True negative rate
#'
#' This function returns the true negative rate given the number of true-negatives and false-positives.
#' The respective formula is tnr = tn / (tn + fp)
#'
#' @param tn Number of true-negatives
#' @param fp Number of false-positives
#' @return The value of the false positive rate
#'
#' @examples
#' #          Reference
#' # Prediction    1    0
#' #          1  892  398
#' #          0  446 2278
#' tpr(tn = 2278, fp = 398)
#'
tnr <- function(tn, fp) {
  return(tn / (tn + fp))
}

#' False negative rate
#'
#' This function returns the false negative rate given the number of true-positives and false-negatives.
#' The respective formula is fnr = fn / (fn + tp)
#'
#' @param tp Number of true-positives
#' @param fn Number of false-neagtives
#' @return The value of the true positive rate
#'
#' @examples
#' #          Reference
#' # Prediction    1    0
#' #          1  892  398
#' #          0  446 2278
#' tpr(tp = 892, fn = 446)
#'
fnr <- function(tp, fn) {
  return(fn / (fn + tp))
}

#' Computate Matthews Correlation Coefficient (MCC) from a confusion matrix table
#'
#' @param table The table object produced by [caret::confusionMatrix()]
#'
#' @return The value for the MCC. It is in the range of [-1, 1] with values above 0 indicating a good correlation
#'
mcc <- function(table) {
  d <- list("tp" = table[1], "fn" = table[2], "fp" = table[3], "tn" = table[4])
  n <- d$tp * d$tn - d$fp * d$fn
  a <- (d$tp + d$fp) * (d$tp + d$fn)
  b <- (d$tn + d$fp) * (d$tn + d$fn)
  return(n / (sqrt(a) * sqrt(b)))
}
