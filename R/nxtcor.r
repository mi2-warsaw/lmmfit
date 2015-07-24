nxtcor <- function(model, p = 1, q = 1, visited){
 modelstr <- summary(model)$modelStruct
  structcor <- class(modelstr$corStruct)[1]
  structrand <- class(modelstr$reStruct[[1]])[1]
  dat <- getData(model)
  nxt <- list()
  if (structcor == "NULL"){
   mat <- visited[visited[,1] == "corCompSymm",]
   if (class(mat) != "matrix"){
    mat <- mat[which(mat == structrand)]
   } else {
    mat <- mat[mat[,2] == structrand,]
   } 
   if (length(mat) == 0){
     tmp1 <- try(update(model, correlation = corCompSymm(), data = dat), silent = TRUE)
     if (class(tmp1) != "try-error"){
      nxt <- c(nxt, list(tmp1))
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
      nxt <- c(nxt, list(tmp2))
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
      nxt <- c(nxt, list(tmp3))
     }
   }
   if (length(nxt) == 0){
     return(NULL)
   } else {
     return(nxt)
   }
  } else {
   if (structcor == "corSymm"){
    return(NULL)
   } else {      
    mat <- visited[visited[,1] == "corSymm",]
   if (class(mat) != "matrix"){
    mat <- mat[which(mat == structrand)]
   } else {
    mat <- mat[mat[,2] == structrand,]
   } 
   if (length(mat) == 0){
      tmp <- try(update(model, correlation = corSymm(), data = dat), silent = TRUE)
      if (class(tmp) != "try-error"){
       nxt <- c(nxt, list(tmp))
      } 
    }
    if (length(nxt) == 0){
      return(NULL)
    } else {
      return(nxt)
    }
   }
  }
}
