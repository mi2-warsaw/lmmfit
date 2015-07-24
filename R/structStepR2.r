structStepR2 <- function(model, crit = lmmCCC, type = "marginal", adjust = "none", p = 1, q = 1, structChange = "both", trace = TRUE){    
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 } 
 if (structChange != "both" & structChange != "correlation" & structChange != "random"){
  stop("Worong structChange. Should be one of: both, correlation, random.")
 }
 if (trace != FALSE & trace != TRUE){
  stop("Worong trace. Should be one of: TRUE, FALSE.")
 }
  actCrit <- crit(model, type = type, adjust = adjust)
  visited <- t(matrix(visitedNames(model)))
  visited <- cbind(visited, actCrit)
  return(structStepHelperR2(model = model, crit = crit, type = type, p = p, q = q, visited = visited, actCrit = actCrit, structChange = structChange, trace = trace))
}
