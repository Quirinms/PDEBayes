\name{ApplyBayesTheorem4Likelihoods}
\alias{ApplyBayesTheorem4Likelihoods}
\title{ApplyBayesTheorem4Likelihoods}
\description{
ApplyBayesTheorem4Likelihoods
}
\usage{
ApplyBayesTheorem4Likelihoods(ListOfLikelihoods, Priors, TrainX = NULL,
TrainY = NULL, TestX = NULL, Model = NULL, Plausible = TRUE, threshold = 0.00001)
}
\arguments{
\item{ListOfLikelihoods}{List of d numeric matrices, one per feature, each matrix with 1:k columns
containing the distribution of class 1:k}
\item{Priors}{[1:k] Numeric vector with prior probability for each class.}
\item{TrainX}{Data from training, if the plausible Bayes is computed, in order to determine closest assignment to a cluster if evidence is very low.}
\item{TrainY}{Numeric vector with classification vector required for determining cluster centres. Currently assuming Gaussian structure for plausible Bayes computation.}
\item{TestX}{Data from test which is required, if the plausible Bayes is computed, in order to determine closest assignment to a cluster if evidence is very low.}
\item{Model}{(Optional: Model for the computation of the class assignment according to a plausible Bayes.}
\item{Plausible}{(Optional: If TRUE, then plausible Bayes is computed. Otherwise, classic Bayes classification is computed.}
\item{threshold}{(Optional: Default=0.00001).}
}
\value{
\item{Posteriors}{[1:n, 1:d] Numeric matrix with posterior probability according to the
bayes theorem.}
}
\author{
Michael Thrun
}
\examples{
if(requireNamespace("FCPS")){
data(Hepta)
Data=Hepta$Data
Cls=Hepta$Cls
#parametric
V=Train_naivBayes(Data,Cls,Gaussian=TRUE)
ClsTrain=V$Modelfit
table(Cls,ClsTrain)

#non-parametric
V=Train_naivBayes(Data,Cls,Gaussian=FALSE)
ClsTrain=V$Modelfit
table(Cls,ClsTrain)
}
}
\keyword{Classification}
\keyword{Bayes}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}

