lmmR2W <- function(model, type = "marginal", adjust = "none"){ 
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 } 
 if (class(model) != "lme" | (type != "conditional" & type != "marginal")){
  stop("Class of your model should be lme or argument type should be one of: conditional, marginal.")
 } 
 if (formula(model)[[3]] != 0){
  group <- getGroups(model)
  lev <- seq_along(levels(group))
  
  y <- getResponse(model)
  names(y) <- NULL
  y <- cbind(y, group)
  y <- as.data.frame(y, row.names = NULL)
  
  ord <- tapply(y$group, y$group, length)[unique(group)]
  gr1 <- list()
  for (i in seq_along(lev)){
   gr1[[i]] <- rep(i, ord[i])
  }
  gr1 <- unlist(gr1)
  
  y$group <- gr1

  out <- matrix(0,2,length(lev))
  if (type == "marginal"){
  
   r <- residuals(model, type = "response", level = 0)
   names(r) <- NULL
   r <- cbind(r, group)
   r <- as.data.frame(r, row.names = NULL)

   r$group <- gr1
 
   for (i in seq_along(lev)){
    V <- getVarCov(model, type = "marginal", individual = lev[i])
    V1 <- ginv(V[[1]])
    out[1,i] <- t(r[which(r$group == lev[i]), 1])%*%V1%*%r[which(r$group == lev[i]), 1]
    out[2,i] <- t(y[which(y$group == lev[i]), 1] - mean(y[, 1]))%*%V1%*%(y[which(y$group == lev[i]), 1] - mean(y[, 1]))
   }
  }
  if (type == "conditional"){
  
   r <- residuals(model, type = "response", level = 1)
   names(r) <- NULL
   r <- cbind(r, group)
   r <- as.data.frame(r, row.names = NULL)
   
   r$group <- gr1

   for (i in seq_along(lev)){
    V <- getVarCov(model, type = "conditional", individual = lev[i])
    V1 <- ginv(V[[1]])
    out[1,i] <- t(r[which(r$group == lev[i]), 1])%*%V1%*%r[which(r$group == lev[i]), 1]
    out[2,i] <- t(y[which(y$group == lev[i]), 1] - mean(y[, 1]))%*%V1%*%(y[which(y$group == lev[i]), 1] - mean(y[, 1]))
   }
  }
  sums <- rowSums(out)
  R2 <- 1 - (sums[1])/(sums[2])
  return(R2)
  } else {
   if (type == "marginal"){
    stop("For models with no fixed effects available type is only conditional")
   } else {
    group <- getGroups(model)
    lev <- seq_along(levels(group))
   
    y <- getResponse(model)
    names(y) <- NULL
    y <- cbind(y, group)
    y <- as.data.frame(y, row.names = NULL)

    r <- list()
    g <- ranef(model)
    for (i in seq_along(lev)){
     r[[i]] = y[which(y$gr == lev[i]),"y"] - g[[1]][i]
    }
    out <- matrix(0, 2, length(lev))
    for (i in seq_along(lev)){
     V <- getVarCov(model, type = "conditional", individual = lev[i])
     V1 <- ginv(V[[1]])
     out[1,i] <- t(r[[i]])%*%V1%*%(r[[i]])
     out[2,i] <- t(y[which(y$group == lev[i]), 1])%*%V1%*%(y[which(y$group == lev[i]), 1])
    }
    sums <- rowSums(out)
    R2 <- 1 - (sums[1])/(sums[2])
    return(R2)
   }
  }
}
