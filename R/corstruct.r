corstruct <- function(model, p = 1, q = 1, visited){
 stcor <- class(summary(model)$modelStruct$corStruct)[1]
 if (stcor == "corSymm"){
  return(prvcor(model, p = p, q = q, visited = visited))
 } else {
  if (stcor == "NULL"){
   return(nxtcor(model, p = p, q = q, visited = visited))
  } else {
   tmp1 <- nxtcor(model, p = p, q = q, visited = visited)
   tmp2 <- prvcor(model, p = p, q = q, visited = visited)
   return(c(tmp1, tmp2))
  }
 }
}
