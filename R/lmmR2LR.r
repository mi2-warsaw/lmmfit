lmmR2LR <- function(model, type = "marginal", adjust = "none"){  
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 } 
  M0 <- lm(getResponse(model) ~ 1, data = getData(model))
  R2 <- 1 - exp((-2/nrow(getData(model)))*(abs(logLik(model)[1] - logLik(M0)[1])))
 return(R2)
}
