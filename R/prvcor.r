prvcor <- function(model, p = 1, q = 1, visited){
 modelstr <- summary(model)$modelStruct
  structcor <- class(modelstr$corStruct)[1]
  structrand <- class(modelstr$reStruct[[1]])[1]
  dat <- getData(model)
  prv <- list()
  if (structcor == "corSymm"){
   mat <- visited[visited[,1] == "corCompSymm",]
   if (class(mat) != "matrix"){
    mat <- mat[which(mat == structrand)]
   } else {
    mat <- mat[mat[,2] == structrand,]
   } 
   if (length(mat) == 0){
     tmp1 <- try(update(model, correlation = corCompSymm(), data = dat), silent = TRUE)
     if (class(tmp1) != "try-error"){
      prv <- c(prv, list(tmp1))
     }
   }       
   mat <- visited[visited[,1] == "corARMA",] 
   if (class(mat) != "matrix"){
    mat <- mat[which(mat == structrand)]
   } else {
    mat <- mat[mat[,2] == structrand,]
   } 
   if (length(mat) == 0){
     tmp2 <- try(update(model, correlation = corARMA(p = p, q = q), data = dat), silent = TRUE)
     if (class(tmp2) != "try-error"){
      prv <- c(prv, list(tmp2))
     }
   }
   mat <- visited[visited[,1] == "corAR1",]
   if (class(mat) != "matrix"){
    mat <- mat[which(mat == structrand)]
   } else {
    mat <- mat[mat[,2] == structrand,]
   } 
   if (length(mat) == 0){
     tmp3 <- try(update(model, correlation = corAR1(), data = dat), silent = TRUE)
     if (class(tmp3) != "try-error"){
      prv <- c(prv, list(tmp3))
     }
   }
   if (length(prv) == 0){
     return(NULL)
   } else {
     return(prv)
   }
  } else {
   if (structcor == "NULL"){
    return(NULL)
   } else {      
    mat <- visited[visited[,1] == "NULL",] 
   if (class(mat) != "matrix"){
    mat <- mat[which(mat == structrand)]
   } else {
    mat <- mat[mat[,2] == structrand,]
   } 
   if (length(mat) == 0){
      tmp <- try(update(model, correlation = NULL, data = dat), silent = TRUE)
      if (class(tmp) != "try-error"){
       prv <- c(prv, list(tmp))
      } 
    }
    if (length(prv) == 0){
      return(NULL)
    } else {
      return(prv)
    }
   }
  }
}
