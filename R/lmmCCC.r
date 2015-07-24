lmmCCC <- function(model, type = "marginal", adjust = "none"){  
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 } 
 if ((adjust != "none") & (adjust != "fixed") & (adjust != "both")){
  stop("Wrong adjust parameter. Should be one of: none, fixed, both")
 } 
 if (class(model) != "lme" | (type != "conditional" & type != "marginal")){
  stop("Class of your model should be lme or argument type should be one of: conditional, marginal.")
 }
 if (formula(model)[[3]] != 0){
  y <- getResponse(model)
  group <- getGroups(model)
  lev <- levels(group)
  out <- matrix(0, 3, length(lev))
   if (type == "marginal"){
    haty <- fitted(model, level = 0)
    Y <- data.frame(y, haty, group)
    for (i in seq_along(lev)){
     out[1,i] <- t(Y[which(Y[, "group"] == lev[i]), "y"] - Y[which(Y[, "group"] == lev[i]), "haty"])%*%(Y[which(Y[, "group"] == lev[i]), "y"] - Y[which(Y[, "group"] == lev[i]), "haty"])
     out[2,i] <- t(Y[which(Y[, "group"] == lev[i]), "y"] - mean(Y[, "y"]))%*%(Y[which(Y[, "group"] == lev[i]), "y"] - mean(Y[, "y"]))
     out[3,i] <- t(Y[which(Y[, "group"] == lev[i]), "haty"] - mean(Y[, "haty"]))%*%(Y[which(Y[, "group"] == lev[i]), "haty"] - mean(Y[, "haty"]))
    }
    out <- rowSums(out)                                                                   
    sred <- nrow(Y)*(mean(Y$y) - mean(Y$haty))^2
    ccc <- (out[1])/(out[2] + out[3] + sred)
   } 
   if (type == "conditional"){
    haty <- fitted(model)
    Y <- data.frame(y, haty, group)
    for (i in seq_along(lev)){
     out[1,i] <- t(Y[which(Y[, "group"] == lev[i]), "y"] - Y[which(Y[, "group"] == lev[i]), "haty"])%*%(Y[which(Y[, "group"] == lev[i]), "y"] - Y[which(Y[, "group"] == lev[i]), "haty"])
     out[2,i] <- t(Y[which(Y[, "group"] == lev[i]), "y"] - mean(Y[, "y"]))%*%(Y[which(Y[, "group"] == lev[i]), "y"] - mean(Y[, "y"]))
     out[3,i] <- t(Y[which(Y[, "group"] == lev[i]), "haty"] - mean(Y[, "haty"]))%*%(Y[which(Y[, "group"] == lev[i]), "haty"] - mean(Y[, "haty"]))
    }
    out <- rowSums(out)                                                                   
    sred <- nrow(Y)*(mean(Y$y) - mean(Y$haty))^2
    ccc <- (out[1])/(out[2] + out[3] + sred)
   } 
  if (adjust == "none"){
   return(ccc)
  } else {
   if (adjust == "fixed"){
    p <- length(fixef(model))
    stark <- nrow(getData(model))/(nrow(getData(model)) - p)
    ccc <- 1 - stark*(1 - ccc)
    return(ccc)
   } else {
    p <- length(fixef(model))
    k <- length(model$modelS$cor) + length(model$modelS$reS[[1]])
    stark <- nrow(getData(model))/(nrow(getData(model)) - (p + k))
    ccc <- 1 - stark*(1 - ccc)
    return(ccc)
   }
  }
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
     out[1,i] <- t(r[[i]])%*%r[[i]]
     out[2,i] <- t(y[which(y[, "group"] == lev[i]), "y"])%*%(y[which(y[, "group"] == lev[i]), "y"])
    }
    out <- rowSums(out)
    ccc <- (out[1])/(2*out[2])
    if (adjust == "both"){
     k <- length(model$modelS$cor) + length(model$modelS$reS[[1]])
     stark <- nrow(getData(model))/(nrow(getData(model)) - k)
     ccc <- 1 - stark*(1 - ccc)
     return(ccc)
    } else {
     return(ccc)
    }
   }
 }  
} 
 