prvstruct <- function(model, p = 1, q = 1, visited){
 modelstr <- summary(model)$modelStruct
 strand <- class(modelstr$reStruct[[1]])[1]
 stcor <- class(modelstr$corStruct)[1]
 if (strand == "pdIdent"){
  return(prvcor(model, p = p, q = q, visited = visited))
 }
 if (stcor == "NULL"){
  return(prvrand(model, visited = visited))
 } else {
  tmp1 <- prvcor(model, p = p, q = q, visited = visited)
  tmp2 <- prvrand(model, visited = visited)
  prv <- c(tmp1, tmp2)
  return(prv)
 }
}
