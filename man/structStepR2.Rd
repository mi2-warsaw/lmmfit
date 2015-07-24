\name{structStepR2}
\alias{structStepR2}
\title{
Selecting the best structures for random factors of linear mixed model with one-level-grouping using R^2 measures of goodness-of-fit
}
\description{
Function structStepR2() selects the best structures (for random effects and for model errors for linear mixed model with one-level-grouping using R^2 measures of goodness-of-fit) from: 

random effects available structures: pdIdent, pdDiag, pdCompSymm, pdSymm, pdLogChol, pdNatural, 

model errors available structures: NULL, corCompSymm, corARMA(p,q), corAR1.

Available R^2 measures of goodness-of-fit: lmmCCC, lmmR2, lmmR2W, lmmR2LR.
}
\usage{
structStepR2(model, crit = lmmCCC, type = "marginal", adjust = "none", p = 1, q = 1, structChange = "both", trace = TRUE)
}
\arguments{
  \item{model}{
object - one-level-grouped linear mixed model fitted with lme().
}
  \item{crit}{
function - one of: lmmCCC, lmmR2, lmmR2W, lmmR2LR. 
}
  \item{type}{
character - defining type of fitted values. There are two types available: "marginal" (default) and "conditional".
}
  \item{adjust}{
character defining adjustment for number of model parameters. Three types available: "none" (default, no adjustment), "fixed" (adjustment for number of fixed effects) and "both" 
  (adjustment for number of fixed effects and number of correlation structures parameters). Available only for: lmmCCC, lmmR2.
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
}
\details{
ARMA(p,q) correlation structure is default to ARMA(1,1) it can be changed, the best change may be if you know something about structure of your data. 

Returned model is selected by maximizing the given criterion.
}
\value{
Model of class lme().
}
\author{
Aleksandra Maj
}
\seealso{
\code{\link{lmmR2W}}, \code{\link{lmmR2LR}}, \code{\link{lmmR2}}, \code{\link{lmmCCC}}, \code{\link{structStep}}
}
\examples{
fm1 <- lme(distance ~ age, data = Orthodont, random = pdDiag(~age)) 
out <- structStep(fm1)
summary(out)
}