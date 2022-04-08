# The `prettyPROC` `R` package

Print pretty PR and ROC curves that also allow for threshold selection in ML classification problems.

## Author

Jana Schor [schor.jana@gmail.com](schor.jana@gmail.com)

## Introduction

In machine learning (ML), the classification problem is a predictive modeling problem that associates a class label with
a given input data sample. To evaluate the predictive power of an ML model, unseen but labeled test data is predicted,
and the predictions are compared to the known truth.

Often this process must happen multiple times during model training and optimization process of the model parameters. If
the model returns a probability of the class association, a value above a certain threshold indicates one class and a
value below the other class. However, this threshold is problem-dependent and not always at 0.5.

With the `prettyPROC` `R` package, I provide functions that calculate all model metrics that might depend on a certain
threshold and generates pretty plots of the results.

* Precision-recall curves
* Receiver-operator curves
* Model metrics vs threshold
* Any model metric against any other model metric w.r.t. to the classifiation threshold.

Within these plots, the best threshold is annotated, labeled, and returned:

|                  Precision-Recall curve                  |            Receiver operator curv            |
|:--------------------------------------------------------:|:--------------------------------------------:|
| ![Precision-recall curve](./01_precision_recall.png =x400) | ![Receiver-operator curve](./02_roc.png =x400) |

|                  Metrics vs. threshold                   |
|:--------------------------------------------------------:|
| ![Metrics vs. threshold](./03_metrics_threshold.png =x800) |


## Installation

Install `R` in at least version 4.0 - or use a respective environment (e.g. `conda` or `mamba`). Start `R` and then you
can install the most up to date version (development) easily with
[devtools](https://github.com/hadley/devtools):

```R
install.packages("devtools")
devtools::install_github("https://github.com/mai00fti/prettyPROC")
```

Once installed, just load the `prettyPROC` package with:

```R
library("prettyPROC")
```

# License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
version. Details see [LICENCSE](https://github.com/mai00fti/prettyPROC/LICENSE).
