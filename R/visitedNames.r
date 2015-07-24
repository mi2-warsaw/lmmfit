visitedNames <- function(model){
 struct <- summary(model)$modelStruct
 modcor <- class(struct$corStruct)[1]
 modrand <- class(struct$reStruct[[1]])[1]
 c(modcor,modrand)
}
