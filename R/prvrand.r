prvrand <- function(model, visited){
 modelstr <- summary(model)$modelStruct
 form <- formula(modelstr$reStruct)[[1]]
 structcor <- class(modelstr$corStruct)[1]
 structrand <- class(modelstr$reStruct[[1]])[1]
 prv <- list()
 if (structrand == "pdIdent"){
  return(NULL)
 } else {
  dat <- getData(model)
  if (sum(class(dat) == "groupedData") == 0){
  group <- attr(getGroups(model),"label")
  yname <- as.character(formula(model))[2]
  fgr <- as.formula(paste(yname, "~", paste(colnames(dat)[-which((colnames(dat) == yname) | (colnames(dat) == group))], collapse = "+"), "|", group))
  dat1 <- groupedData(fgr, data = as.data.frame(dat))
  } else {
   dat1 <- dat
  }
  if (structrand == "pdSymm" | structrand == "pdLogChol" | structrand == "pdNatural"){
    mat <- visited[visited[,1] == structcor,]
    if (class(mat) != "matrix"){
     mat <- mat[which(mat == "pdDiag")]
    } else {
     mat <- mat[mat[,2] == "pdDiag",]
    } 
    if (length(mat) == 0){
     tmp1 <- try(update(model, random = pdDiag(form), data = dat1), silent = TRUE)
     if (class(tmp1) != "try-error"){
       prv <- c(prv, list(tmp1))
     }
    }
    mat <- visited[visited[,1] == structcor,] 
    if (class(mat) != "matrix"){
     mat <- mat[which(mat == "pdCompSymm")]
    } else {
     mat <- mat[mat[,2] == "pdCompSymm",]
    } 
    if (length(mat) == 0){
     tmp2 <- try(update(model, random = pdCompSymm(form), data = dat1), silent = TRUE)
     if (class(tmp2) != "try-error"){
       prv <- c(prv, list(tmp2))
     }
    }
    if (length(prv) == 0){
      return(NULL)
    } else {
      return(prv)
    }
  } else {
    mat <- visited[visited[,1] == structcor,]
    if (class(mat) != "matrix"){
     mat <- mat[which(mat == "pdIdent")]
    } else {
     mat <- mat[mat[,2] == "pdIdent",]
    } 
    if (length(mat) == 0){
     tmp <- try(update(model, random = pdIdent(form), data = dat1), silent = TRUE)
     if (class(tmp) != "try-error"){
      return(list(tmp))
     } else {
      return(NULL)
     }
    }
  }
 }
}
