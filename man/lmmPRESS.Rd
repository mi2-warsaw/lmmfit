\name{lmmPRESS}
\alias{lmmPRESS}
\title{
Predicted Residual Sum of Squares for linear mixed models with one-level-grouping
}
\description{
Function lmmPRESS() evaluates predicted residual sum of squares (PRESS) for linear mixed models with one-level-grouping. The function is usable only for models evaluated by lme() function from nlme package. 
Two types of PRESS are available: marginal (default) and conditional.
}
\usage{
lmmPRESS(model, type = "marginal")
}
\arguments{
  \item{model}{
object - one-level-grouped linear mixed model fitted with lme(). 
}
  \item{type}{
character defining type of fitted values. There are two types available: "marginal" (default) and "conditional".
}
}
\details{
PRESS is evaluated using leave-one-out-group method. In order not to evaluate model too many times, the model hat matrix is used. For linear mixed model this matrix is: H = X(X'V^{-1}X)^(-1)X'V^{-1}, where X is fixed
effects model matrix and V is correlation matrix of model.
}
\value{
Numeric.
}
\references{
Gurka M., Selecting the best linear mixed model under REML, The American Statistician, 60(1), 19-26, 2006. 

Vonesh E.F., Chinchili V.M., Pu K., Goodness-of-fit in generalized nonlinear mixed-effect models, Biometrics, 52, 1996.

Liu H., Weiss R.E., Jennrich I., Wenger N.S., PRESS model selection in repeated measures data, Elsevier, 1999.
}
\author{
Aleksandra Maj
}

\seealso{
\code{\link{lmmCCC}}, \code{\link{lmmR2LR}}, \code{\link{lmmR2W}}, \code{\link{lmmR2}}
}
\examples{
  fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
  lmmPRESS(fm1)
  lmmPRESS(fm1, type = "conditional")
}