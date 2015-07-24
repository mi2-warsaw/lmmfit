\name{lmmR2}
\alias{lmmR2}
\title{
R squared coefficient for linear mixed models with one-level-grouping
}
\description{
Function lmmR2() can be evaluated only for models class lme, because it uses methods for this class.
}
\usage{
lmmR2(model, type = "marginal", adjust = "none")
}
\arguments{
  \item{model}{
object - one-level-grouped linear mixed model fitted with lme(). 
}
  \item{type}{
character defining type of fitted values. There are two types available: "marginal" (default) and "conditional".
}
  \item{adjust}{
character defining adjustment for number of model parameters. Three types available: "none" (default, no adjustment), "fixed" (adjustment for number of fixed effects) and "both" (adjustment for number of fixed effects and 
number of correlation structures parameters).
}
}
\details{
R2(adjust = "fixed") = 1 - k*(1 - R2), where k = nrow(data)/(nrow(data) - rank(X)).

R2(adjust = "both") = 1 - k*(1 - R2), where k = nrow(data)/(nrow(data) - (rank(X) + length(theta))), where theta is a vector of parameters of correlation structures of model.
}
\value{
Numeric, between 0 and 1.
}
\references{
Gurka M., Selecting the best linear mixed model under REML, The American Statistician, 60(1), 19-26, 2006. 

Vonesh E.F., Chinchili V.M., Pu K., Goodness-of-fit in generalized nonlinear mixed-effect models, Biometrics, 52, 1996.
}
\author{
Aleksandra Maj
}
\seealso{
\code{\link{lmmCCC}}, \code{\link{lmmR2LR}}, \code{\link{lmmR2W}}
}
\examples{
 fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
  lmmR2(fm1)
  lmmR2(fm1, type = "conditional")
  lmmR2(fm1, type = "conditional", adjust = "both")
}