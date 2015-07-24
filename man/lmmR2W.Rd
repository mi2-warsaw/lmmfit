\name{lmmR2W}
\alias{lmmR2W}
\title{
R squared coefficient based on Wald's statistics for linear mixed models with one-level-grouping
}
\description{
Function lmmR2W() evaluates an R squared coefficient based on Wald's statistics for linear mixed models with one-level-grouping. The function is usable only for models evaluated by lme() function from nlme package. 
Two types of R squared coefficients based on Wald's statistics are available: marginal (default) and conditional.
}
\usage{
lmmR2W(model, type = "marginal", adjust = "none")
}
\arguments{
  \item{model}{a one-level-grouping linear mixed model fitted with lme()
}
  \item{type}{character defining type of fitted values. There are two types available: "marginal" (default) and "conditional".
} 
  \item{adjust}{changing this parameter won't change anything. It is only used for structStepR2.}
}
\details{The R squared coefficient based on Wald's statistics can be used as a measure of goodness-of-fit of linear mixed model with one-level-grouping.
}
\value{
lmmR2W() returns R squared coefficient based on Wald's statistics for linear mixed model with one-level-grouping, this value lies between 0 and 1.
}
\references{
Gurka M., Selecting the best linear mixed model under REML, The American Statistician, 60(1), 19-26, 2006. 

Magee L., R2 measures based on Wald and likelihood ratio joint significance tests, The American Statistician 44, 250-253, 1990. 

Vonesh E.F., Chinchili V.M., Pu K., Goodness-of-fit in generalized nonlinear mixed-effect models, Biometrics, 52, 1996.
}
\author{
Aleksandra Maj
}
\seealso{
\code{\link{lmmCCC}}, \code{\link{lmmR2LR}}, \code{\link{lmmR2}}
}
\examples{
  fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
  lmmR2W(fm1)
  lmmR2W(fm1, type = "conditional")
}