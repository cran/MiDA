% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MiBiClassGBODT.R
\name{MiBiClassGBODT}
\alias{MiBiClassGBODT}
\title{Binary classification using gradient boosting over desicion trees}
\usage{
MiBiClassGBODT(Matrix, specimens, n.crossval = 5, ntrees = 10000,
  shrinkage = 0.1, intdepth = 2, n.terminal = 10, bag.frac = 0.5)
}
\arguments{
\item{Matrix}{numeric matrix of expression data where each row corresponds to a probe (gene, transcript),
and each column correspondes to a specimen (patient).}

\item{specimens}{factor vector with two levels specifying specimens in the columns of the \code{Matrix}.}

\item{n.crossval}{integer specifying number of cross-validation folds.}

\item{ntrees}{integer specifying the total number of decision trees (boosting iterations).}

\item{shrinkage}{numeric specifying the learning rate. Scales the step size in the gradient descent procedure.}

\item{intdepth}{integer specifying the maximum depth of each tree.}

\item{n.terminal}{integer specifying the actual minimum number of observations in the terminal nodes of the trees.}

\item{bag.frac}{the fraction of the training set observations randomly selected to propose the next tree in the expansion.}
}
\value{
list of 2:
\cr
\code{QC} - matrix containing quality measures for each fitted model and their summary.
Accur - accuracy (percentage of correct predictions),
AUC - area under ROC curve (see \code{\link{roc}}),
MCC - Mattew's correlation coefficient
\cr
formula ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)),
\cr
F1sc - F1 score
\cr
formula 2xPresxRec/(Pres+Rec).
\cr
If all the data points from one class are misclassified into other, MCC and F1 score may get NaN values.
\cr
\cr
\code{Importance} - list of data frames containing for each fitted model:
\code{var} - probe ID and \code{rel.inf} - its feature importance for classification (relative influence).
\cr
Feature importance (relative influence) graphs are also produced.
}
\description{
This function conducts a binary classification of specimens based on microarray gene (transcript) expression data.
Gradient boosting over desicion trees algorithm is used.
Several generalized boosted regression models are fitted during cross-validation, for each model
measurements of classification quality and feature importance are returned.
}
\details{
\code{Matrix} must contain specimens from two classification groups only. To sample expression matrix
use \code{\link{MiDataSample}}.
\cr
The order of the variables in \code{specimens} and the columns of \code{Matrix} must be the same. Levels of
\code{specimens} are two classification groups. To sample specimens use \code{\link{MiSpecimenSample}}.
\cr
Number of cross-validation folders defines number of models to be fitted. For example,
if n.crossval=5 then all specimens are divided into 5 folders, each of them is later used for model testing,
so 5 models are fitted. See \code{\link{createFolds}} for details.
\cr
While boosting, basis functions are iteratively adding in a greedy fashion
so that each additional basis function further reduces the selected loss function.
Gaussian distribution (squared error) is used.
\code{ntrees}, \code{shrinkage}, \code{intdepth} are parameters for model tuning.
\code{bag.frac} introduces randomnesses into the model fit.
If \code{bag.frac} < 1 then running the same model twice will result in similar but different fits.
Number of specimens in train sample must be enough to provide the minimum number of observations in terminal nodes.I.e.
\cr(1-1/\code{n.crossval})*\code{bag.frac} > \code{n.terminal}.
\cr
See \code{\link{gbm}} for details.
}
\examples{

#get gene expression and specimen data
data("IMexpression");data("IMspecimen")
#sample expression matrix and specimen data for binary classification,
#only "NORM" and "EBV" specimens are left
SampleMatrix<-MiDataSample(IMexpression, IMspecimen$diagnosis,"norm", "ebv")
SampleSpecimen<-MiSpecimenSample(IMspecimen$diagnosis, "norm", "ebv")
#Fitting, low tuning for faster running
BoostRes<-MiBiClassGBODT(SampleMatrix, SampleSpecimen, n.crossval = 3,
                       ntrees = 10, shrinkage = 1, intdepth = 2)
BoostRes[[1]] # QC values for n.crossval = 3 models and its summary
length(BoostRes[[2]]) # n.crossval = 3 data frames of probes feature importance for classification
head(BoostRes[[2]][[1]])

}
\seealso{
\code{\link{createFolds}}, \code{\link{gbm}}, \code{\link{MiSpecimenSample}}, \code{\link{MiDataSample}},
\code{\link{roc}}
}
\author{
Elena N. Filatova
}
