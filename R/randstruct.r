randstruct <- function(model, visited){
 modelstr <- summary(model)$modelStruct
 form <- formula(modelstr$reStruct)[[1]]
 struct <- class(modelstr$reStruct[[1]])[1]
 if (struct == "pdSymm"){
  return(prvrand(model, visited = visited))
 } else {
  if (struct == "pdIdent"){
   return(nxtrand(model, visited = visited))
  } else {
   tmp1 <- nxtrand(model, visited = visited)
   tmp2 <- prvrand(model, visited = visited)
   return(c(tmp1, tmp2))
  }
 }
}
