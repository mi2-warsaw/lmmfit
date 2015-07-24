GIC <- function(model, k = 2, type = "marginal"){  
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 } 
 if (k == "PRESS"){
  GIC <- lmmPRESS(model, type = type)
  return(GIC)
 } else {
  npar <- length(fixef(model)) + length(model$modelS$cor) + length(model$modelS$reS[[1]]) + 1
  GIC <- -2*logLik(model)[[1]] + k*npar
  return(GIC)
 }
}
