fun <- function(model, k = 2, vis, actmin, type){
 struct <- summary(model)$modelStruct
 modcor <- class(struct$corStruct)[1] 
 modrand <- class(struct$reStruct[[1]])[1] 
 mat <- vis[vis[,1] == modcor,]
 if (class(mat) != "matrix"){
  mat <- mat[which(mat == modrand)]
 } else {
  mat <- mat[mat[,2] == modrand,]
 }
 if (length(mat) > 0){
  return(actmin + 100)
 } else {
  return(GIC(model, k = k, type = type))
 }
}
