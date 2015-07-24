\name{structStep}
\alias{structStep}

\title{
Selecting the best structures for random factors of linear mixed model with one-level-grouping using GIC (General Information Criterion) or PRESS coefficient
}
\description{
Function structStep() selects the best structures (for random effects and for model errors for linear mixed model with one-level-grouping using GIC or PRESS coefficient) from: 

random effects available structures: pdIdent, pdDiag, pdCompSymm, pdSymm, pdLogChol, pdNatural, 

model errors available structures: NULL, corCompSymm, corARMA(p,q), corAR1.
}
\usage{
structStep(model, k = 2, p = 1, q = 1, structChange = "both", trace = TRUE, type = "marginal")
}
\arguments{
  \item{model}{
object - one-level-grouped linear mixed model fitted with lme().
}
  \item{k}{
numeric - penalty for number of model parameters (parameter for GIC() function). For example: if k = 2 (default) model will be selected by AIC, if k = log(nrow(data)) model will be selected by BIC. Or character "PRESS", then 
criterion for minimization is PRESS coefficient
}
  \item{p}{
integer - parameter for ARMA(p,q) correlation structure, default 1.
}
  \item{q}{
integer - parameter for ARMA(p,q) correlation structure, default 1.
}
  \item{structChange}{
character - specifies way to choose models. If "both" (default) both: random errors and random effects structures are selected, 
if "correlation" only correlation of random errors is chosen, if "random" only random effects structure is selected.
}
  \item{trace}{
logical - if TRUE all fitted models will be printed, if FALSE nothing is printed
}
  \item{type}{
character - type for PRESS residuals: "marginal" (default) or "conditional"
}
}
\details{
ARMA(p,q) correlation structure is default to ARMA(1,1) it can be changed, the best change may be if you know something about structure of your data. 

Returned model is selected by minimizing the given criterion.
}
\value{
Model of class lme().
}
\author{
Aleksandra Maj
}
\seealso{
\code{\link{GIC}}, \code{\link{lmmPRESS}}
}
\examples{
fm1 <- lme(distance ~ age, data = Orthodont, random = pdDiag(~age)) 
out <- structStep(fm1)
summary(out)
}
