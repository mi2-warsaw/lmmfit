structStep <- function(model, k = 2, p = 1, q = 1, structChange = "both", trace = TRUE, type = "marginal"){
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 } 
 if (structChange != "both" & structChange != "correlation" & structChange != "random"){
  stop("Worong structChange. Should be one of: both, correlation, random.")
 } 
 if (trace != FALSE & trace != TRUE){
  stop("Worong trace. Should be one of: TRUE, FALSE.") 
 }
 if (type != "marginal" & type != "conditional"){
  stop("Worong type. Should be one of: marginal, conditional.")
 } 
  actCrit <- GIC(model, k = k, type = type)
  visited <- t(matrix(visitedNames(model)))
  visited <- cbind(visited, actCrit)
  return(structStepHelper(model = model, k = k, p = p, q = q, visited = visited, actCrit = actCrit, structChange = structChange, trace = trace, type = type))
}
