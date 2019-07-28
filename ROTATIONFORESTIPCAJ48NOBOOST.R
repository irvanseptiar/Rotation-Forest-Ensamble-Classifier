rotationForestIPCAj48nb <- function (x, y, K = round(ncol(x)/3, 0), L = 10, verbose = FALSE, ...) 
{
  library(rpart)
  library(mixOmics)
  if (K >= ncol(x)) 
    stop("K should not be greater than or equal to the number of columns in x")
  if (all(sapply(x, is.numeric)) == FALSE) 
    stop("All features in x need to be of type numeric")
  if (K > floor(ncol(x)/2)) 
    stop(cat("The maximum K for your data is", floor(ncol(x)/2)))
  if (K == 0) 
    stop("The minimum K is 1")
  sets <- rep(floor(ncol(x)/K), K)
  overflow <- rep(1, ncol(x) - sum(sets))
  overflow <- c(overflow, rep(0, K - length(overflow)))
  overflow <- overflow[sample(1:length(overflow), length(overflow), FALSE)]
  subsetsizes <- sets + overflow
  if (verbose) 
    cat("Number of subsets: ", K, ". Number of variables per respective subset:", sep = "")
  if (verbose) 
    cat(subsetsizes, "\n", sep = " ")
  fact                            <- factor(rep(1:K, subsetsizes))
  predicted                       <- list()
  fit                             <- numeric()
  Ri                              <- list()
  Ria                             <- list()
  fit                             <- list()
  predicted                       <- matrix(NA, nrow = nrow(x), ncol = L)
  subsets                         <- list()
  SelectedClass                   <- list()
  IndependentsClassSubset         <- list()
  IndependentsClassSubsetBoot     <- list()
  pcdata                          <- list()
  loadings                        <- list()
  for (i in 1:L) {
    subsets[[i]]                <- list()
    SelectedClass[[i]]          <- list()
    IndependentsClassSubset[[i]] <- list()
    IndependentsClassSubsetBoot[[i]] <- list()
    pcdata[[i]] <- list()
    loadings[[i]] <- list()
    varIndPerSubset <- split(sample(1:ncol(x), ncol(x), FALSE),fact)
    for (j in seq(1, K)) {
      subsets[[i]][[j]] <- data.frame(x[, varIndPerSubset[[j]]], y)
      SelectedClass[[i]][[j]] <- as.integer(sample(levels(as.factor(y)),1))
      IndependentsClassSubset[[i]][[j]] <- subsets[[i]][[j]][subsets[[i]][[j]]$y == SelectedClass[[i]][[j]], ]
      #IndependentsClassSubsetBoot[[i]][[j]] <- IndependentsClassSubset[[i]][[j]][sample(1:dim(IndependentsClassSubset[[i]][[j]])[1], 
                                                                                        #round(0.75 * nrow(IndependentsClassSubset[[i]][[j]])), 
                                                                                        #replace = TRUE), ]
      IndependentsClassSubset[[i]][[j]] = data.matrix(as.data.frame(IndependentsClassSubset[[i]][[j]]))
      pcdata[[i]][[j]] <- ipca(IndependentsClassSubset[[i]][[j]][,!colnames(IndependentsClassSubset[[i]][[j]]) %in% "y"],
                               ncomp = ncol(IndependentsClassSubset[[i]][[j]][,!colnames(IndependentsClassSubset[[i]][[j]]) %in% "y"]),
                               mode = "deflation",fun = "logcosh", scale = FALSE,
                               w.init = NULL, max.iter = 200, tol = 1e-04)
      
      loadings[[i]][[j]] <- pcdata[[i]][[j]]$rotation
      colnames(loadings[[i]][[j]]) <- dimnames(loadings[[i]][[j]])[[1]]
      loadings[[i]][[j]] <- data.frame(dimnames(loadings[[i]][[j]])[[1]], 
                                       loadings[[i]][[j]])
      colnames(loadings[[i]][[j]])[1] <- "rowID"
    }
    Ri[[i]] <- Reduce(function(x, y) merge(x, y, by = "rowID",all = TRUE), loadings[[i]])
    Ri[[i]][is.na(Ri[[i]])] <- 0
    Ria[[i]] <- Ri[[i]][order(match(Ri[[i]]$rowID, colnames(x))),order(match(colnames(Ri[[i]]), colnames(x)))]
    rownames(Ria[[i]]) <- Ria[[i]]$rowID
    Ria[[i]]$rowID <- NULL
    final <- data.frame(as.matrix(x) %*% as.matrix(Ria[[i]]), y)
    fit[[i]] <- J48(y ~ ., data = final, ...)
  }
  res <- list(models = fit, 
              columnnames = colnames(x),  
              loadings = Ria,
              pcdata = pcdata)
  class(res) <- "rotationForestIPCAj48nb"
  res
}

predictipcaj48nb <- function (object, newdata, all = FALSE, ...) 
{
  newdata <- data.frame(sapply(newdata, as.numeric, simplify = FALSE))
  if (!identical(colnames(newdata), object$columnnames)) 
    stop("Variable names and/or order of variables in newdata is not identical to training set. Please check if variables are exactly the same in both sets.")
  predicted <- matrix(NA, nrow = nrow(newdata), ncol = length(object$models))
  for (i in 1:length(object$models)) {
    final <- data.frame(as.matrix(newdata) %*% as.matrix(object$loadings[[i]]))
    predicted[, i] <- predict(object$models[[i]], final,type = "prob")[, 2]
  }
  if (all) {
    predicted
  }
  else {
    rowMeans(predicted)
  }
}