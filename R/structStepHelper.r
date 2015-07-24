structStepHelper <- function(model, k = 2, p = 1, q = 1, visited, actCrit, structChange = "both", trace, type){
 if (structChange == "both"){
  neighbours <- bothstruct(model, p = p, q = q, visited = visited)
 } else {
  if (structChange == "correlation"){
   neighbours <- corstruct(model, p = p, q = q, visited = visited)
  } else {
   neighbours <- randstruct(model, visited = visited)
  }
 }
 if (length(neighbours) > 0){
  crits <- sapply(neighbours, fun, k, visited, actCrit, type)
  if (actCrit <= min(crits)){
   if (trace == TRUE){
    newVisited <- t(sapply(neighbours, visitedNames))
    newVisited <- cbind(newVisited, crits)
    visited <- rbind(visited, newVisited)
    visited[,3] <- round(as.numeric(visited[,3]), digits = 5)
    colnames(visited) <- c("correlation", "random", "criterion")
    visited <- as.data.frame(visited)
    print(visited)
   }
   print(paste("The best model is:", strand <- class(summary(model)$modelStruct$reStruct[[1]])[1], stcor <- class(summary(model)$modelStruct$corStruct)[1], "its criterion:", round(actCrit, digits = 5)))
   return(model)
  } else {
   newVisited <- t(sapply(neighbours, visitedNames))
   newVisited <- cbind(newVisited, crits)
   visited <- rbind(visited, newVisited)
   tmp <- neighbours[[which.min(crits)]]
   return(structStepHelper(tmp, k = k, p = p, q = q, visited = visited, actCrit = min(crits), structChange = structChange, trace = trace, type = type))
  }
 } else {
   if (trace == TRUE){
    visited[,3] <- round(as.numeric(visited[,3]), digits = 5)
    colnames(visited) <- c("correlation", "random", "criterion")
    visited <- as.data.frame(visited)
    print(visited)
   } 
  print(paste("The best model is:", strand <- class(summary(model)$modelStruct$reStruct[[1]])[1], stcor <- class(summary(model)$modelStruct$corStruct)[1], "its criterion:", round(actCrit, digits = 5)))
  return(model)
 }
}
