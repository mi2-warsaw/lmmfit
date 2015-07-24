\name{lmmCCC}
\alias{lmmCCC}
\title{
Concordance correlation coefficient for linear mixed models with one-level-grouping
}
\description{
Function lmmCCC() evaluates concordance correlation coefficient (CCC) for linear mixed models with one-level-grouping. The function is usable only for models evaluated by lme() function from nlme package. 
Two types of CCC are available: marginal (default) and conditional.
}
\usage{
lmmCCC(model, type = "marginal", adjust = "none")
}
\arguments{
  \item{model}{a one-level-grouping linear mixed model fitted with lme()
}
  \item{type}{character defining type of fitted values. There are two types available: "marginal" (default) and "conditional".
}
  \item{adjust}{character defining adjustment for number of model parameters. Three types available: "none" (default, no adjustment), "fixed" (adjustment for number of fixed effects) and "both" 
  (adjustment for number of fixed effects and number of correlation structures parameters).
}
}
\details{The CCC can be used as a measure of goodness-of-fit of linear mixed model with one-level-grouping.
}
\value{
lmmCCC() returns concordance correlation coefficient for linear mixed model with one-level-grouping, this value lies between -1 and 1.
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
\code{\link{lmmR2W}}, \code{\link{lmmR2LR}}, \code{\link{lmmR2}}
}
\examples{
  fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
  lmmCCC(fm1)
  lmmCCC(fm1, type = "conditional")
}

