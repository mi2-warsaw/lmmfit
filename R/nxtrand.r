nxtrand <- function(model, visited){
 modelstr <- summary(model)$modelStruct
 form <- formula(modelstr$reStruct)[[1]]
 structcor <- class(modelstr$corStruct)[1]
 structrand <- class(modelstr$reStruct[[1]])[1]
 nxt <- list()
 if (structrand == "pdSymm" | structrand == "pdLogChol" | structrand == "pdNatural"){
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
  if (structrand == "pdIdent"){
    mat <- visited[visited[,1] == structcor,] 
    if (class(mat) != "matrix"){
     mat <- mat[which(mat == "pdDiag")]
    } else {
     mat <- mat[mat[,2] == "pdDiag",]
    } 
    if (length(mat) == 0){
     tmp1 <- try(update(model, random = pdDiag(form), data = dat1), silent = TRUE)
     if (class(tmp1) != "try-error"){
       nxt <- c(nxt, list(tmp1))
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
       nxt <- c(nxt, list(tmp2))
     }
    }
    if (length(nxt) == 0){
      return(NULL)
    } else {
      return(nxt)
    }
  } else {
    mat <- visited[visited[,1] == structcor,] 
    if (class(mat) != "matrix"){
     mat <- mat[which(mat == "pdSymm")]
    } else {
     mat <- mat[mat[,2] == "pdSymm",]
    } 
    if (length(mat) == 0){
     tmp <- try(update(model, random = pdSymm(form), data = dat1), silent = TRUE)
     if (class(tmp) != "try-error"){
      return(list(tmp))
     } else {
      return(NULL)
     }
    }
  }
 }
}
