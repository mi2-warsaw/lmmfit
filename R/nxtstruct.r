nxtstruct <- function(model, p = 1, q = 1, visited){
 modelstr <- summary(model)$modelStruct
 strand <- class(modelstr$reStruct[[1]])[1]
 stcor <- class(modelstr$corStruct)[1]
 if (strand == "pdSymm" | strand == "pdLogChol" | strand == "pdNatural"){
  return(nxtcor(model, p = p, q = q, visited = visited))
 }
 if (stcor == "corSymm"){
  return(nxtrand(model, visited = visited))
 } else {
  tmp1 <- nxtcor(model, p = p, q = q, visited = visited)
  tmp2 <- nxtrand(model, visited = visited)
  nxt <- c(tmp1, tmp2) 
  return(nxt)
  }
}
