\name{lmmR2LR}
\alias{lmmR2LR}
\title{
R squared coefficient based on likelihood ratio for mixed models
}
\description{
Function lmmR2LR() can be evaluated only for models class lme, because it uses methods for this class.
}
\usage{
lmmR2LR(model, type = "marginal", adjust = "none")
}
\arguments{
  \item{model}{object - one-level-grouped linear mixed model fitted with lme().}
  \item{type}{changing this parameter won't change anything. It is only used for structStepR2.}
  \item{adjust}{changing this parameter won't change anything. It is only used for structStepR2.}
}
\details{
The R squared coefficient based on likelihood ratio (of the model and the Intercept-model) can be used as a measure of goodness-of-fit of mixed model.
}
\value{
lmmR2LR() returns R squared coefficient based on likelihood ratio (of the model and the Intercept-model) for mixed model, this value lies between 0 and 1.
}
\references{
Magee L., R2 measures based on Wald and likelihood ratio joint significance tests, The American Statistician 44, 250-253, 1990. 

Vonesh E.F., Chinchili V.M., Pu K., Goodness-of-fit in generalized nonlinear mixed-effect models, Biometrics, 52, 1996.
}
\author{
Aleksandra Maj
}
\seealso{
\code{\link{lmmCCC}}, \code{\link{lmmR2W}}, \code{\link{lmmR2}}
}
\examples{
  fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
  lmmR2LR(fm1)
  }