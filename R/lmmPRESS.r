lmmPRESS <- function(model, type = "marginal"){
 if (length(getGroupsFormula(model, asList = TRUE)) > 1){
  stop("This function works only with one-level-grouping models.")
 }
 if (class(model) != "lme" | (type != "conditional" & type != "marginal")){
  stop("Class of your model should be lme or argument type should be one of: conditional, marginal.")
 }
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

  out <- matrix(0, 3, length(lev))
  X <- model.matrix(formula(model), data = getData(model))
  X <- cbind(X, group)
  X <- as.data.frame(X)

  X$group <- gr1

   if (type == "marginal"){
    A1 <- list()
    B1 <- list()
    for (i in seq_along(lev)){
     Xi <- X[which(X$group == i) , -ncol(X)]
     Xi <- as.matrix(Xi)
     yi <- y[which(y$group == i), 1]
     V <- getVarCov(model, type = "marginal", individual = i)
     V1 <- ginv(V[[1]])
     A1[[i]] <- t(Xi)%*%V1%*%Xi
     B1[[i]] <- t(Xi)%*%V1%*%yi
    }

    A <- matrix(0, dim(A1[[1]]), dim(A1[[1]]))
    for (i in seq_along(lev)){
     A <- A1[[i]] + A
    }
    B <- matrix(0, dim(B1[[1]]))
    for (i in seq_along(lev)){
     B <- B1[[i]] + B
    }

    Ai <- lapply(A1, FUN = function(x) A - x)
    Bi <- lapply(B1, FUN = function(x) B - x)
    alfai <- list()
    for (i in seq_along(lev)){
     alfai[[i]] <- ginv(Ai[[i]])%*%Bi[[i]]
    }
    r <- list()
    for (i in seq_along(lev)){
     Xi <- X[which(X$group == i) , -ncol(X)]
     Xi <- as.matrix(Xi)
     yi <- y[which(y$group == i), 1]
     r[[i]] <- yi - Xi%*%alfai[[i]]
    }
    press <- sum(unlist(r)^2)
    return(press)
   } else {
    A1 <- list()
    B1 <- list()
    for (i in seq_along(lev)){
     Xi <- X[which(X$group == i) , -ncol(X)]
     Xi <- as.matrix(Xi)
     yi <- y[which(y$group == i), 1]
     V <- getVarCov(model, type = "conditional", individual = i)
     V1 <- ginv(V[[1]])
     A1[[i]] <- t(Xi)%*%V1%*%Xi
     B1[[i]] <- t(Xi)%*%V1%*%yi
    }

    A <- matrix(0, dim(A1[[1]]), dim(A1[[1]]))
    for (i in seq_along(lev)){
     A <- A1[[i]] + A
    }
    B <- matrix(0, dim(B1[[1]]))
    for (i in seq_along(lev)){
     B <- B1[[i]] + B
    }

    Ai <- lapply(A1, FUN = function(x) A - x)
    Bi <- lapply(B1, FUN = function(x) B - x)
    alfai <- list()
    for (i in seq_along(lev)){
     alfai[[i]] <- ginv(Ai[[i]])%*%Bi[[i]]
    }
    r <- list()
    for (i in seq_along(lev)){
     Xi <- X[which(X$group == i) , -ncol(X)]
     Xi <- as.matrix(Xi)
     yi <- y[which(y$group == i), 1]
     r[[i]] <- yi - Xi%*%alfai[[i]]
    }
    press <- sum(unlist(r)^2)
    return(press)
   }
}
