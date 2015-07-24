funR2 <- function(model, crit = lmmCCC, type = "marginal", adjust = "none", vis, actmax){
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
  return(actmax - 100)
 } else {
  return(crit(model, type = type, adjust = adjust))
 }
}
