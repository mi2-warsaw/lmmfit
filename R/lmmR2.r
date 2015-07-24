lmmR2 <- function(model, type = "marginal", adjust = "none"){ 
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 } 
 if ((adjust != "none") & (adjust != "fixed") & (adjust != "both")){
  stop("Wrong adjust parameter. Should be one of: none, fixed, both")
 }
 if (class(model) != "lme" | (type != "conditional" & type != "marginal")){
  stop("Class of your model should be lme or argument type should be one of: conditional, marginal.")
 }
 y <- getResponse(model)
 bary <- mean(y)
 if (formula(model)[[3]] != 0){
  if (type == "marginal"){
   res <- residuals(model, level = 0)
   R2 <- 1 - (sum(res^2))/(sum((y - bary)^2))
  } else {
   res <- residuals(model, level = 1)
   R2 <- 1 - (sum(res^2))/(sum((y - bary)^2))
  }
  if (adjust == "none"){
   return(R2)
  } else {
   if (adjust == "fixed"){
    p <- length(fixef(model))
    stark <- nrow(getData(model))/(nrow(getData(model)) - p)
    R2 <- 1 - stark*(1 - R2)
    return(R2)
   } else {
    p <- length(fixef(model))
    k <- length(model$modelS$cor) + length(model$modelS$reS[[1]])
    stark <- nrow(getData(model))/(nrow(getData(model)) - (p + k))
    R2 <- 1 - stark*(1 - R2)
    return(R2)
   }
  }
 } else {
  if (type == "marginal"){
   stop("For models with no fixed effects available type is only conditional")
  } else {
   group <- getGroups(model)
   lev <- seq_along(levels(group))
   
   names(y) <- NULL
   y <- cbind(y, group)
   y <- as.data.frame(y, row.names = NULL)
   
   r <- list()
   out <- c()
   g <- ranef(model)
   for (i in seq_along(lev)){
    r[[i]] = y[which(y$gr == lev[i]),"y"] - g[[1]][i]
    out[i] <- t(r[[i]])%*%(r[[i]])
   }
   R2 <- 1 - (sum(out))/(sum(y^2))
   if (adjust == "none"){
    return(R2)
   } else {
    if (adjust == "fixed"){
     p <- length(fixef(model))
     stark <- nrow(getData(model))/(nrow(getData(model)) - p)
     R2 <- 1 - stark*(1 - R2)
     return(R2)
    } else {
     p <- length(fixef(model))
     k <- length(model$modelS$cor) + length(model$modelS$reS[[1]])
     stark <- nrow(getData(model))/(nrow(getData(model)) - (p + k))
     R2 <- 1 - stark*(1 - R2)
     return(R2)
    }
   }
  }
 }
}
