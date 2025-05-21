lassoDIF.ABWIC <- function(Data, group, type="AIC", N=NULL, lambda=NULL, ...){
  
  data <- LassoData(Data, group)
  out <- lassoDIF(data, lambda, ...)
  if(is.null(N)){N <- nrow(Data)}
  
  J <- (nrow(out$beta)-1)/2
  
  
  if (type == "AIC" | type == "BIC"){
    CRIT <- switch(type, AIC = deviance(out)+2*out$df, BIC = deviance(out)+log(J*N)*out$df)
    l.opt <- out$lambda[CRIT == min(CRIT)]
    nr.opt <- (1:length(out$lambda))[abs(out$lambda-l.opt) == min(abs(out$lambda-l.opt))]
    pr <- out$beta[, nr.opt]
  }
  
  if (type == "WIC"){
    
    CRIT <- NULL
    ppAIC <- deviance(out) + 2*out$df
    ppBIC <- deviance(out) + log(J*N)*out$df
    nr.w <- 100    
    s <- seq(from = 0, to = 1, length = nr.w)
    l.seq <- NULL
    
    for (i in 1:length(s)){
      f <- s[i]*ppAIC + (1-s[i])*ppBIC
      l.seq[i] <- out$lambda[f == min(f)]
    }
    
    l.opt <- median(unique(l.seq))
    nr1 <- nrow(coef(out, s = 0))-J+1
    nr2 <- nrow(coef(out, s = 0))
    pr <- coef(out, s = l.opt)[nr1:nr2, 1]
  }
  
  IND <- (length(pr)-J+1):(length(pr))
  RES <- NULL
  if (max(abs(pr[IND])) > 0) RES <- (1:J)[abs(pr[IND]) > 0]
  mat <- cbind(pr[IND])
  mat.names <- "Item1"
  for (i in 2:J) mat.names <- c(mat.names, paste("Item", i, sep = ""))
  rownames(mat) <- mat.names

  return(list(DIFitems = RES, DIFpars = mat, crit.value = CRIT, crit.type = type, lambda = out$lambda, opt.lambda = l.opt,
              glmnet.fit = out))
}

#######################################################

#' Plot LASSO coefficient paths
#'
#' This function displays the coefficient trajectories for DIF items detected via LASSO regularization.
#'
#' @param out An object returned by \code{lassoDIF.ABWIC}.
#' @param nr.lambda Number of lambda values to display (default is 100).
#' @param highlight Items to highlight in the plot.
#' @param title Title of the plot.
#' @param ... Additional arguments passed to \code{plot()}.
#'
#' @return A plot showing coefficient paths.
#' @export
plot_lasso_paths <- function(lasso_object, main = "Coefficient Paths", xlab = "Log Lambda", ylab = "Coefficients", ...) {
  ...
}

plot_lasso_paths <- function(out, nr.lambda = 100, highlight = NULL, title = "Regularization Paths of DIF Effects",...) {
  coef_list <- lassoDIF.coef(out, nr.lambda = nr.lambda)
  DIF_coefs <- coef_list$pars
  log_lambda <- log(coef_list$lambda)

  item_names <- rownames(DIF_coefs)
  matplot(log_lambda, t(DIF_coefs), type = "l", lty = 1,
          xlab = expression(log(lambda)),
          ylab = "Estimated DIF Effects",
          main = title,
          col = if (is.null(highlight)) 1:nrow(DIF_coefs) else ifelse(1:nrow(DIF_coefs) %in% highlight, 2, "grey"))
  legend("topright", legend = item_names, col = 1:nrow(DIF_coefs), lty = 1, cex = 0.6)
}

#' @export
plot.lassoDIF <- function(x, ...) {
  plot_lasso_paths(x, ...)
}