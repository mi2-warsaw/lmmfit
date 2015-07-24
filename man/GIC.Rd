\name{GIC}
\alias{GIC}
\title{
General Information Criterion (GIC) for linear mixed model
}
\description{
Function GIC() evaluates General Information Criterion (GIC) for linear mixed models. The function is usable only for models evaluated by lme() function from nlme package. 
}
\usage{
GIC(model, k, type)
}
\arguments{
  \item{model}{
object - one-level-grouped linear mixed model fitted with lme().
}
  \item{k}{
numeric or "PRESS" - if numeric: penalty for number of model parameters, if "PRESS": PRESS coefficient for the model is evaluated 
}
\item{type}{
character - type for PRESS: "marginal" (default) or "conditional"
}
}
\details{
k can be any of real numeric, but if k = 2 (default) GIC is equivalent of AIC, if k = log(nrow(data)) GIC is equivalent of BIC. If k is "PRESS", then PRESS coefficient for the model is 
evalauted. This option is useful in structStep().
}
\value{
GIC() returns general information criterion for mixed model (numeric) or PRESS coefficient for the model (numeric).
}
\author{
Aleksandra Maj
}
\seealso{
\code{\link{lmmPRESS}}, \code{\link{lmmR2LR}}, \code{\link{lmmCCC}}, \code{\link{lmmR2W}}, \code{\link{lmmR2}}
}
\examples{
fm1 <- lme(distance ~ age, data = Orthodont, random = pdDiag(~age)) 
GIC(fm1)
GIC(fm1, k = log(nrow(getData(fm1))))
GIC(fm1, k = "PRESS")
}
