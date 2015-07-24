bothstruct <- function(model, p = 1, q = 1, visited){
 struct <- summary(model)$modelStruct
 strand <- class(struct$reStruct[[1]])[1]
 stcor <- class(struct$corStruct)[1]

 if ((strand == "pdSymm" & stcor == "corSymm") | (strand == "pdNatural" & stcor == "corSymm") | (strand == "pdLogChol" & stcor == "corSymm")){
  return(prvstruct(model, p = p, q = q, visited = visited))
 } else {
  if (strand == "pdIdent" & stcor == "NULL"){
   return(nxtstruct(model, p = p, q = q, visited = visited))
  } else {
   tmp1 <- nxtstruct(model, p = p, q = q, visited = visited)
   tmp2 <- prvstruct(model, p = p, q = q, visited = visited)
   tmp <- c(tmp1, tmp2)
   return(tmp)
  }
 }
}
