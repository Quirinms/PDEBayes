\name{Predict_naivBayes}
\alias{Predict_naivBayes}
\title{Apply_naivBayes}
\description{
Predict classification with naive bayes model.
}
\usage{
Predict_naivBayes(TestX, TrainingModel)
}
\arguments{
\item{TestX}{[1:n,1:d] matrix of test data. It consists of n cases of
d-dimensional data points. Every case has d attributes, variables or features.}
\item{TrainingModel}{List of a trained naive bayes model containing Priors, information about the fitted model.}
}
\value{
\item{ClsTest}{[1:n]  numerical vector with n numbers defining the
classification. It has k unique numbers representing the arbitrary labels of the
classification.}
\item{Posteriors}{[1:n, 1:l] Numeric matrices with posterior probabilities.}
\item{ListOfLikelihoods}{List of m numeric matrices with l columns containing
the distribution of class i in 1:l.}
}
\author{
Michael Thrun
}
\seealso{
\code{\link{Train_naivBayes}}
}
\examples{
if(requireNamespace("FCPS")){
V=FCPS::ClusterChallenge("Hepta",1000)
Data=V$Hepta
Cls=V$Cls
ind=1:length(Cls)
indtrain=sample(ind,800)
indtest=setdiff(ind,indtrain)
#parametric
model=Train_naivBayes(Data[indtrain,],Cls[indtrain],Gaussian=TRUE)
ClsTrain=model$Modelfit
table(Cls[indtrain],ClsTrain)

res=Predict_naivBayes(Data[indtest,], model)
table(Cls[indtest],res$Prediction)

#PDEbayes
model=Train_naivBayes(Data[indtrain,],Cls[indtrain],Gaussian=FALSE)
ClsTrain=model$Modelfit
table(Cls[indtrain],ClsTrain)

res=Predict_naivBayes(Data[indtest,],model)
table(Cls[indtest],res$Prediction)
}
}
\keyword{Classification}
\keyword{Bayes}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}
