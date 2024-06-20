#Modifications of some functions of piecewiseSEM package
#see https://github.com/jslefche/piecewiseSEM/issues/284

basisSet2 <- function (modelList.with.data, direction = NULL, interactions = FALSE) {
  amat <- getDAG(modelList.with.data)
  b <- lapply(1:nrow(amat), function(i) {
    lapply(i:ncol(amat), function(j) {
      if (amat[i, j] != 0 | i == j)
        NULL
      else {
        cvars <- c(rownames(amat)[amat[, rownames(amat)[i],
                                       drop = FALSE] == 1], rownames(amat)[amat[,
                                                                                rownames(amat)[j], drop = FALSE] == 1])
        cvars <- cvars[!duplicated(cvars)]
        c(rownames(amat)[i], rownames(amat)[j], cvars)
      }
    })
  })
  b <- unlist(b, recursive = FALSE)
  b <- b[!sapply(b, is.null)]
  if (length(b) > 0) {
    b <- piecewiseSEM:::filterExogenous(b, modelList.with.data, amat)
    b <- piecewiseSEM:::filterSmoothed(b, modelList.with.data)
    b <- piecewiseSEM:::filterExisting(b, modelList.with.data)
    b <- piecewiseSEM:::filterInteractions(b, interactions)
    b <- piecewiseSEM:::removeCerror(b, modelList.with.data)
    b <- piecewiseSEM:::reverseAddVars(b, modelList.with.data, amat)
    b <- piecewiseSEM:::reverseNonLin(b, modelList.with.data, amat)
    b <- piecewiseSEM:::fixCatDir(b, modelList.with.data) # Here is the cause of the problem!
    if (!is.null(direction))
      b <- piecewiseSEM:::specifyDir(b, direction)
  }
  class(b) <- "basisSet"
  return(b)
}




multigroup2 <- function(
    modelList,
    group,
    standardize = "scale",
    standardize.type = "latent.linear",
    test.type = "III") {
  name <- deparse(match.call()$modelList)
  data <- modelList$data
  
  modelList.with.data <- modelList # My solution
  modelList <- piecewiseSEM:::removeData(modelList, formulas = 1) # piecewiseSEM:::fixCatDir(b, modelList) caused the issue
  
  intModelList <- lapply(modelList, function(i) {
    rhs2 <-
      paste(paste(piecewiseSEM:::all.vars_trans(i)[-1], "*", group),
            collapse = " + "
      )
    i <- update(i, formula(paste(". ~ ", rhs2)))
    return(i)
  })
  newModelList <- lapply(unique(data[, group]), function(i) {
    update(as.psem(modelList),
           data = data[data[, group] == i, ]
    )
  })
  names(newModelList) <- unique(data[, group])
  coefsList <- lapply(
    newModelList,
    coefs,
    standardize,
    standardize.type,
    test.type
  )
  names(coefsList) <- unique(data[, group])
  coefTable <- coefs(
    modelList, standardize, standardize.type,
    test.type
  )
  anovaTable <- anova(as.psem(intModelList))[[1]]
  anovaInts <- anovaTable[grepl(":", anovaTable$Predictor), ]
  global <- anovaInts[anovaInts$P.Value >= 0.05, c(
    "Response",
    "Predictor"
  )]
  global$Predictor <-
    sub(":", "\1", sub(group, "\1", global$Predictor))
  if (nrow(global) == nrow(anovaInts)) {
    newCoefsList <- list(global = coefTable)
  } else {
    newCoefsList <- lapply(names(coefsList), function(i) {
      ct <- as.matrix(coefsList[[i]])
      idx <- which(
        apply(ct[, 1:2], 1, paste, collapse = "") %in%
          apply(global[, 1:2], 1, paste, collapse = "")
      )
      ct[idx, ] <- as.matrix(coefTable[idx, ])
      ct <- cbind(ct, ifelse(1:nrow(ct) %in% idx, "c",
                             ""
      ))
      for (j in 1:nrow(ct)) {
        if (ct[j, ncol(ct)] == "c") {
          model <- modelList[[which(sapply(
            piecewiseSEM:::listFormula(modelList),
            function(x) {
              piecewiseSEM:::all.vars.merMod(x)[1] == ct[
                j,
                "Response"
              ]
            }
          ))]]
          data. <- data[data[, group] == i, ]
          sd.x <-
            piecewiseSEM:::GetSDx(model, modelList, data., standardize)
          sd.x <- sd.x[which(names(sd.x) == ct[j, "Predictor"])]
          sd.y <- piecewiseSEM:::GetSDy(
            model, data., standardize,
            standardize.type
          )
          new.coef <- as.numeric(ct[j, "Estimate"]) *
            (sd.x / sd.y)
          ct[j, "Std.Estimate"] <- ifelse(length(new.coef) >
                                            0, round(as.numeric(new.coef), 4), "-")
        }
      }
      ct <- as.data.frame(ct)
      ct[is.na(ct)] <- "-"
      names(ct)[(ncol(ct) - 1):ncol(ct)] <- ""
      return(ct)
    })
    names(newCoefsList) <- names(coefsList)
  }
  
  if (nrow(global) == nrow(anovaInts)) {
    gof <- piecewiseSEM::fisherC(modelList)
  } else {
    # b <- piecewiseSEM:::basisSet(modelList) # Here is the cause of the problem!
    b <- basisSet2(modelList.with.data)
    cf <- coefTable[coefTable$Response %in% global$Response & coefTable$Predictor %in% global$Predictor, ]
    b <- lapply(
      X = b,
      FUN = function(i) {
        for (j in 3:length(i)) {
          value <- cf[cf$Response == i[2] & cf$Predictor == i[j], "Estimate"]
          if (length(value) != 0) {
            i[j] <- paste0("offset(", value, "*", i[j], ")")
          }
        }
        return(i)
      }
    )
    if (length(b) == 0) {
      b <- NULL
    }
    gof <- fisherC(modelList, basis.set = b)
  }
  
  ret <- list(
    name = name,
    group = group,
    Cstat = gof,
    global = global,
    anovaInts = anovaInts,
    group.coefs = newCoefsList
  )
  class(ret) <- "multigroup.psem"
  return(ret)
}