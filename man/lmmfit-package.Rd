\name{lmmfit-package}
\alias{lmmfit-package}
\alias{lmmfit}
\docType{package}
\title{
Goodness-of-fit measures for linear mixed models with one-level-grouping
}
\description{
Package contains four different functions for evaluating goodness-of-fit measures for linear mixed models with one-level-grouping, 
such as concordance correlation coefficient (CCC), R^2 based on Wald's statistic and R^2 based on likelihood ratio and ordinary R^2.
It also contains two functions for evaluating some criterions, which can be helpful in selecting linear mixed models: predicted residual sum of squares (PRESS) 
and general information criterion (GIC). 
There are also a few step-function for linear mixed models with one-level-grouping, which can be useful for selecting the best correlation
structures for given/known fixed and random effects. They find the model through minimiznig GIC criterion or maximizing measures of fit 
listed above.  
}
\details{
\tabular{ll}{
Package: \tab lmmfit\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-04-28\cr
License: \tab GPL-2\cr
}
}
\author{
Aleksandra Maj

Maintainer: Aleksandra Maj <am248424@students.mimuw.edu.pl>
}
\references{
Gurka M., Selecting the best linear mixed model under REML, The American Statistician, 60(1), 19-26, 2006. 

Magee L., R2 measures based on Wald and likelihood ratio joint significance tests, The American Statistician 44, 250-253, 1990. 

Vonesh E.F., Chinchili V.M., Pu K., Goodness-of-fit in generalized nonlinear mixed-effect models, Biometrics, 52, 1996.
}
\seealso{
\code{\link{lme}}
}
\keyword{ package }
\examples{
fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
  lmmR2W(fm1)
  lmmCCC(fm1)
  lmmR2LR(fm1)
  GIC(fm1)
  structStep(fm1)
  structStepR2(fm1)
}
