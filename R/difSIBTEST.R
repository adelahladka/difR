##' SIBTEST and Crossing-SIBTEST DIF method
##'
##' Performs DIF detection using SIBTEST (Shealy & Stout, 1993) or the
##' modified Crossing-SIBTEST method (Chalmers, 2018).
##'
##' The SIBTEST method (Shealy & Stout, 1993) allows for detecting uniform
##' differential item functioning without requiring an item response model
##' approach. Its modified version, the Crossing-SIBTEST (Chalmers, 2018; Li
##' & Stout, 1996), focuses on nonuniform DIF instead. This function provides
##' a wrapper to the \code{\link[mirt]{SIBTEST}} function from the \bold{mirt}
##' package (Chalmers, 2012) to fit within the \code{difR} framework (Magis et
##' al., 2010). Therefore, if you are using this function for publication
##' purposes please cite Chalmers (2018; 2012) and Magis et al. (2010).
##'
##' The \code{Data} is a matrix whose rows correspond to the subjects and
##' columns to the items. In addition, \code{Data} can hold the vector of group
##' membership. If so, \code{group} indicates the column of \code{Data} which
##' corresponds to the group membership, either by specifying its name or by
##' giving the column number. Otherwise, \code{group} must be a vector of same
##' length as \code{nrow(Data)}.
##'
##' The vector of group membership must hold only two different values, either
##' as numeric or character. The focal group is defined by the value of the
##' argument \code{focal.name}.
##'
##' The type of DIF effect, uniform through SIBTEST or nonuniform through
##' Crossing-SIBTEST, is determined by the \code{type} argument. By default it
##' is \code{"udif"} for uniform DIF, and may take the value \code{"nudif"} for
##' nonuniform DIF.
##'
##' The threshold (or cut-score) for classifying items as DIF is computed as
##' the quantile of the chi-square distribution with lower-tail probability of
##' one minus \code{alpha} and with one degree of freedom. Note that the
##' degrees of freedom are also returned by the \code{df} argument.
##'
##' Item purification can be performed by setting \code{purify} to \code{TRUE}.
##' Purification works as follows: if at least one item was detected as
##' functioning differently at some step of the process, then the data set of
##' the next step consists in all items that are currently anchor (DIF free)
##' items, plus the tested item (if necessary). The process stops when either
##' two successive applications of the method yield the same classifications of
##' the items (Clauser & Mazor, 1998), or when \code{nrIter} iterations are
##' run without obtaining two successive identical classifications. In the
##' latter case a warning message is printed.
##'
##' Adjustment for multiple comparisons is possible with the argument
##' \code{p.adjust.method}. The latter must be an acronym of one of the
##' available adjustment methods of the \code{\link{p.adjust}} function.
##' According to Kim and Oshima (2013), Holm and Benjamini-Hochberg adjustments
##' (set respectively by \code{"Holm"} and \code{"BH"}) perform best for DIF
##' purposes. See \code{\link{p.adjust}} function for further details. Note
##' that item purification is performed on original statistics and p-values; in
##' case of adjustment for multiple comparisons this is performed \emph{after}
##' item purification.
##'
##' A pre-specified set of anchor items can be provided through the
##' \code{anchor} argument. It must be a vector of either item names (which
##' must match exactly the column names of \code{Data} argument) or integer
##' values (specifying the column numbers for item identification). In case
##' anchor items are provided, they are used to compute the test score
##' (matching criterion), including also the tested item. None of the anchor
##' items are tested for DIF: the output separates anchor items and tested
##' items and DIF results are returned only for the latter. Note also that item
##' purification is not activated when anchor items are provided (even if
##' \code{purify} is set to \code{TRUE}). By default it is \code{NULL} so that
##' no anchor item is specified.
##'
##' The output of the \code{difSIBTEST}, as displayed by the
##' \code{print.SIBTEST} function, can be stored in a text file provided that
##' \code{save.output} is set to \code{TRUE} (the default value \code{FALSE}
##' does not execute the storage). In this case, the name of the text file must
##' be given as a character string into the first component of the
##' \code{output} argument (default name is \code{"out"}), and the path for
##' saving the text file can be given through the second component of
##' \code{output}. The default value is \code{"default"}, meaning that the file
##' will be saved in the current working directory. Any other path can be
##' specified as a character string: see the \bold{Examples} section for an
##' illustration.
##'
##' The \code{plot.SIBTEST} function displays the DIF statistics in a plot,
##' with each item on the X axis. The type of point and the color are fixed by
##' the usual \code{pch} and \code{col} arguments. Option \code{number} permits
##' to display the item numbers instead. Also, the plot can be stored in a
##' figure file, either in PDF or JPEG format. Fixing \code{save.plot} to
##' \code{TRUE} allows this process. The figure is defined through the
##' components of \code{save.options}. The first two components perform
##' similarly as those of the \code{output} argument. The third component is
##' the figure format, with allowed values \code{"pdf"} (default) for PDF file
##' and \code{"jpeg"} for JPEG file. Note that no plot is returned for exact
##' inference.
##'
##' @aliases difSIBTEST print.SIBTEST plot.SIBTEST
##'
##' @param Data numeric: either the data matrix only, or the data matrix plus
##' the vector of group membership. See \bold{Details}.
##' @param group numeric or character: either the vector of group membership or
##' the column indicator (within \code{Data}) of group membership. See
##' \bold{Details}.
##' @param focal.name numeric or character indicating the level of \code{group}
##' which corresponds to the focal group.
##' @param type character: the type of DIF effect to test. Possible values are
##' \code{"udif"} (default) for uniform DIF using SIBTEST, or \code{"nudif"}
##' for nonuniform DIF using Crossing-SIBTEST (CSIBTEST).
##' @param anchor either \code{NULL} (default) or a vector of item names (or
##' identifiers) to specify the anchor items. See \bold{Details}.
##' @param alpha numeric: significance level (default is 0.05).
##' @param purify logical: should the method be used iteratively to purify the
##' set of anchor items? (default is \code{FALSE}).
##' @param nrIter numeric: the maximal number of iterations in the item
##' purification process (default is 10).
##' @param p.adjust.method either \code{NULL} (default) or the acronym of the
##' method for p-value adjustment for multiple comparisons. See \bold{Details}.
##' @param puriadjType character: type of combination of the item purification
##'   and the method for p-value adjustment for multiple comparisons. Either
##'   \code{"simple"} for the item purification followed by the selected
##'   p-adjustment method (default) or \code{"combined"} for the p-adjustment
##'   method applied in each iteration of item purification. For details, see
##'   Hladká, Martinková, and Magis (2024). Argument is ignored when
##'   \code{purify} is \code{FALSE} or \code{p.adjust.method} is \code{NULL}.
##' @param save.output logical: should the output be saved into a text file?
##' (default is \code{FALSE}).
##' @param output character: a vector of two components. The first component is
##' the name of the output file, the second component is either the file path
##' or \code{"default"} (default value). See \bold{Details}.
##' @param x the result from a \code{SIBTEST} class object.
##' @param pch,col type of usual \code{pch} and \code{col} graphical options.
##' @param number logical: should the item number identification be printed
##' (default is \code{TRUE}).
##' @param save.plot logical: should the plot be saved into a separate file?
##' (default is \code{FALSE}).
##' @param save.options character: a vector of three components. The first
##' component is the name of the output file, the second component is either
##' the file path or \code{"default"} (default value), and the third component
##' is the file extension, either \code{"pdf"} (default) or \code{"jpeg"}. See
##' \bold{Details}.
##' @param ... other generic parameters for the \code{plot} or the \code{print}
##' functions.
##'
##' @return
##' A list of class \code{"SIBTEST"} with the following arguments:
##' \describe{
##'   \item{Beta}{the values of the SIBTEST Beta values.}
##'   \item{SE}{the standard errors of the Beta values.}
##'   \item{X2}{the values of the SIBTEST or Crossing-SITBTEST chi-square statistics.}
##'   \item{df}{the degrees of freedom for \code{X2} statistics.}
##'   \item{p.value}{the p-values for the SIBTEST or Crossing-SIBTEST statistics.}
##'   \item{type}{the value of the \code{type} argument.}
##'   \item{alpha}{the value of \code{alpha} argument.}
##'   \item{DIFitems}{either the column indicators of the items which were detected as DIF items, or \code{"No DIF item detected"}.}
##'   \item{p.adjust.method}{the value of the \code{p.adjust.method} argument.}
##'   \item{adjusted.p}{either \code{NULL} or the vector of adjusted p-values for multiple comparisons.}
##'   \item{purification}{the value of \code{purify} option.}
##'   \item{nrPur}{the number of iterations in the item purification process. Returned only if \code{purify} is \code{TRUE}.}
##'   \item{difPur}{a binary matrix with one row per iteration in the item purification process and one column per item. Zeros and ones in the \emph{i}-th row refer to items which were classified respectively as non-DIF and DIF items at the (\emph{i}-1)-th step. The first row corresponds to the initial classification of the items. Returned only if \code{purify} is \code{TRUE}.}
##'   \item{convergence}{logical indicating whether the iterative item purification process stopped before the maximal number \code{nrIter} of allowed iterations. Returned only if \code{purify} is \code{TRUE}.}
##'   \item{puriadjType}{the value of \code{puriadjType} option. Returned only when \code{purify} is \code{TRUE}.}
##'   \item{names}{the names of the items or \code{NULL} if the items have no name.}
##'   \item{anchor.names}{the value of the \code{anchor} argument.}
##'   \item{save.output}{the value of the \code{save.output} argument.}
##'   \item{output}{the value of the \code{output} argument.}
##' }
##'
##' @author
##' David Magis \cr
##' Data science consultant at IQVIA Belux \cr
##' Brussels, Belgium\cr
##'
##' @seealso \code{\link{sibTest}}, \code{\link{dichoDif}}, \code{\link{p.adjust}}
##'
##' @references
##' Chalmers, R. P. (2012). mirt: A Multidimensional item response theory
##' package for the R environment. \emph{Journal of Statistical Software,
##' 48(6)}, 1--29, \doi{10.18637/jss.v048.i06}
##'
##' Chalmers, R. P. (2018). Improving the Crossing-SIBTEST statistic for
##' detecting non-uniform DIF. \emph{Psychometrika, 83}(2), 376--386,
##' \doi{10.1007/s11336-017-9583-8}
##'
##' Hladká, A., Martinková, P., and Magis, D. (2023). Combining item purification
##' and multiple comparison adjustment methods in detection of differential item
##' functioning. \emph{Multivariate Behavioral Research, 59}(1), 46--61,
##' \doi{10.1080/00273171.2023.2205393}
##'
##' Kim, J., and Oshima, T. C. (2013). Effect of multiple testing adjustment in
##' differential item functioning detection. \emph{Educational and
##' Psychological Measurement, 73}, 458--470, \doi{10.1177/0013164412467033}
##'
##' Li, H.-H., and Stout, W. (1996). A new procedure for detection of crossing
##' DIF. \emph{Psychometrika, 61}, 647--677, \doi{10.1007/BF02294041}
##'
##' Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general
##' framework and an R package for the detection of dichotomous differential
##' item functioning. \emph{Behavior Research Methods, 42}, 847--862,
##' \doi{10.3758/BRM.42.3.847}
##'
##' Shealy, R. and Stout, W. (1993). A model-based standardization approach
##' that separates true bias/DIF from group ability differences and detect test
##' bias/DTF as well as item bias/DIF. \emph{Psychometrika, 58}, 159--194,
##' \doi{10.1007/BF02294572}
##'
##' @examples
##'
##' \dontrun{
##'
##'  # Loading of the verbal data
##'  data(verbal)
##'
##'  # Excluding the "Anger" variable
##'  verbal <- verbal[colnames(verbal) != "Anger"]
##'
##'  # Three equivalent settings of the data matrix and the group membership
##'  r <- difSIBTEST(verbal, group = 25, focal.name = 1)
##'  difSIBTEST(verbal, group = "Gender", focal.name = 1)
##'  difSIBTEST(verbal[, 1:24], group = verbal[, 25], focal.name = 1)
##'
##'  # Test for nonuniform DIF
##'  difSIBTEST(verbal, group = 25, focal.name = 1, type = "nudif")
##'
##'  # Multiple comparisons adjustment using Benjamini-Hochberg method
##'  difSIBTEST(verbal, group = 25, focal.name = 1, p.adjust.method = "BH")
##'
##'  # With item purification
##'  difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE)
##'  r2 <- difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE, nrIter = 5)
##'
##'  # With combination of item purification and multiple comparisons adjustment
##'  difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "simple")
##'  difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "combined")
##'
##'  # With items 1 to 5 set as anchor items
##'  difSIBTEST(verbal, group = "Gender", focal.name = 1, anchor = 1:5)
##'  difSIBTEST(verbal, group = "Gender", focal.name = 1, anchor = 1:5, purify = TRUE)
##'
##'  # Saving the output into the "SIBresults.txt" file (and default path)
##'  r <- difSIBTEST(verbal, group = 25, focal.name = 1, save.output = TRUE,
##'                  output = c("SIBresults","default"))
##'
##'  # Graphical devices
##'  plot(r)
##'  plot(r2)
##'
##'  # Plotting results and saving it in a PDF figure
##'  plot(r, save.plot = TRUE, save.options = c("plot", "default", "pdf"))
##'
##'  # Changing the path, JPEG figure
##'  path <- "c:/Program Files/"
##'  plot(r, save.plot = TRUE, save.options = c("plot", path, "jpeg"))
##' }
##' @export difSIBTEST
difSIBTEST <- function(
    Data, group, focal.name, type = "udif", anchor = NULL,
    alpha = 0.05, purify = FALSE, nrIter = 10, p.adjust.method = NULL, puriadjType = "simple",
    save.output = FALSE, output = c("out", "default")) {
  if (!is.character(puriadjType) || !puriadjType %in% c("simple", "combined")) {
    stop("'puriadjType' can be either 'simple' or 'combined'.",
         call. = FALSE
    )
  }
  internalSIBTEST <- function() {
    if (length(group) == 1) {
      if (is.numeric(group)) {
        gr <- Data[, group]
        DATA <- Data[, (1:ncol(Data)) != group]
        colnames(DATA) <- colnames(Data)[(1:ncol(Data)) != group]
      } else {
        gr <- Data[, colnames(Data) == group]
        DATA <- Data[, colnames(Data) != group]
        colnames(DATA) <- colnames(Data)[colnames(Data) != group]
      }
    } else {
      gr <- group
      DATA <- Data
    }
    Group <- rep(0, nrow(DATA))
    Group[gr == focal.name] <- 1
    if (is.null(anchor)) {
      ANCHOR <- 1:ncol(DATA)
      anchor.names <- NULL
    } else {
      if (is.numeric(anchor)) {
        ANCHOR <- anchor
        anchor.names <- anchor
      } else {
        ANCHOR <- NULL
        for (i in anchor) {
          ANCHOR <- c(ANCHOR, which(colnames(Data) == i))
        }
        anchor.names <- anchor
      }
    }

    if (purify) {
      if (is.null(p.adjust.method)) {
        puri.adj.method <- "none"
        adj.method <- "none"
      } else {
        if (puriadjType == "simple") {
          puri.adj.method <- "none"
          adj.method <- p.adjust.method
        } else {
          puri.adj.method <- p.adjust.method
          adj.method <- p.adjust.method
        }
      }
    } else {
      adj.method <- ifelse(is.null(p.adjust.method), "none", p.adjust.method)
    }

    if (!purify | !is.null(anchor)) {
      PROV <- sibTest(DATA, Group, type = type, anchor = ANCHOR)
      PVAL <- PROV$p.value
      P.ADJUST <- p.adjust(PVAL, method = adj.method)
      if (min(P.ADJUST, na.rm = TRUE) >= alpha) {
        DIFitems <- "No DIF item detected"
      } else {
        DIFitems <- which(!is.na(PVAL) & P.ADJUST < alpha)
      }

      if (is.null(p.adjust.method)) {
        adjusted.p <- NULL
      } else {
        adjusted.p <- P.ADJUST
      }

      RES <- list(
        Beta = PROV$Beta, SE = PROV$SE, X2 = PROV$X2,
        df = PROV$df, p.value = PROV$p.value, type = type,
        alpha = alpha, DIFitems = DIFitems, p.adjust.method = p.adjust.method,
        adjusted.p = adjusted.p, purification = purify, names = colnames(DATA),
        anchor.names = anchor.names, save.output = save.output,
        output = output
      )
      if (!is.null(anchor)) {
        RES$Beta[ANCHOR] <- NA
        RES$SE[ANCHOR] <- NA
        RES$X2[ANCHOR] <- NA
        RES$df[ANCHOR] <- NA
        RES$p.value[ANCHOR] <- NA
        for (i in 1:length(RES$DIFitems)) {
          if (sum(RES$DIFitems[i] == ANCHOR) == 1) {
            RES$DIFitems[i] <- NA
          }
        }
        RES$DIFitems <- RES$DIFitems[!is.na(RES$DIFitems)]
      }
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- sibTest(DATA, Group, type = type)
      pval1 <- prov1$p.value
      p.adjust1 <- p.adjust(pval1, method = puri.adj.method)
      if (min(p.adjust1, na.rm = TRUE) >= alpha) {
        DIFitems <- "No DIF item detected"
        noLoop <- TRUE
      } else {
        dif <- which(!is.na(pval1) & p.adjust1 < alpha)
        difPur <- rep(0, length(pval1))
        difPur[dif] <- 1
        repeat {
          if (nrPur >= nrIter) {
            break
          } else {
            nrPur <- nrPur + 1
            nodif <- NULL
            if (is.null(dif)) {
              nodif <- 1:ncol(DATA)
            } else {
              for (i in 1:ncol(DATA)) {
                if (sum(i == dif) == 0) {
                  nodif <- c(nodif, i)
                }
              }
            }
            prov2 <- sibTest(DATA, Group, type = type, anchor = nodif)
            pval2 <- prov2$p.value
            p.adjust2 <- p.adjust(pval2, method = puri.adj.method)

            if (min(p.adjust2, na.rm = TRUE) >= alpha) {
              dif2 <- NULL
            } else {
              dif2 <- which(!is.na(pval2) & p.adjust2 < alpha)
            }
            difPur <- rbind(difPur, rep(0, ncol(DATA)))
            difPur[nrPur + 1, dif2] <- 1
            if (length(dif) != length(dif2)) {
              dif <- dif2
            } else {
              dif <- sort(dif)
              dif2 <- sort(dif2)
              if (sum(dif == dif2) == length(dif)) {
                noLoop <- TRUE
                break
              } else {
                dif <- dif2
              }
            }
          }
        }
        pval1 <- pval2
        p.adjust1 <- p.adjust(pval1, method = adj.method)
        prov1 <- prov2
        if (min(p.adjust1, na.rm = TRUE) >= alpha) {
          DIFitems <- "No DIF item detected"
        } else {
          DIFitems <- which(!is.na(pval1) & p.adjust1 < alpha)
        }
      }
      if (!is.null(difPur)) {
        ro <- co <- NULL
        for (ir in 1:nrow(difPur)) {
          ro[ir] <- paste("Step", ir - 1, sep = "")
        }
        for (ic in 1:ncol(difPur)) {
          co[ic] <- paste("Item", ic, sep = "")
        }
        rownames(difPur) <- ro
        colnames(difPur) <- co
      }

      if (is.null(p.adjust.method)) {
        adjusted.p <- NULL
      } else {
        adjusted.p <- p.adjust1
      }

      RES <- list(
        Beta = prov1$Beta, SE = prov1$SE, X2 = prov1$X2,
        df = prov1$df, p.value = pval1, type = type,
        alpha = alpha, DIFitems = DIFitems, p.adjust.method = p.adjust.method,
        adjusted.p = adjusted.p, puriadjType = puriadjType, purification = purify, nrPur = nrPur,
        difPur = difPur, convergence = noLoop, names = colnames(DATA),
        anchor.names = NULL, save.output = save.output,
        output = output
      )
    }
    class(RES) <- "SIBTEST"
    return(RES)
  }
  resToReturn <- internalSIBTEST()
  if (save.output) {
    if (output[2] == "default") {
      wd <- paste(getwd(), "/", sep = "")
    } else {
      wd <- output[2]
    }
    fileName <- paste(wd, output[1], ".txt", sep = "")
    capture.output(resToReturn, file = fileName)
  }
  return(resToReturn)
}



#### METHODS

#' @export
plot.SIBTEST <- function(
    x, pch = 8, number = TRUE, col = "red", save.plot = FALSE,
    save.options = c("plot", "default", "pdf"), ...) {
  internalSIBTEST <- function() {
    res <- x
    thr <- qchisq(1 - res$alpha, res$df)
    yl <- c(0, max(c(res$X2, thr) + 0.5, na.rm = TRUE))
    if (!number) {
      plot(res$X2,
        xlab = "Item", ylab = "SIBTEST X2 statistic",
        ylim = yl, pch = pch, main = "SIBTEST"
      )
      if (!is.character(res$DIFitems)) {
        points(res$DIFitems, res$X2[res$DIFitems],
          pch = pch,
          col = col
        )
      }
    } else {
      plot(res$X2,
        xlab = "Item", ylab = "SIBTEST X2 statistic",
        ylim = yl, col = "white", main = "SIBTEST"
      )
      text(1:length(res$X2), res$X2, 1:length(res$X2))
      if (!is.character(res$DIFitems)) {
        text(res$DIFitems, res$X2[res$DIFitems], res$DIFitems,
          col = col
        )
      }
    }
    abline(h = thr[1])
  }
  internalSIBTEST()
  if (save.plot) {
    plotype <- NULL
    if (save.options[3] == "pdf") {
      plotype <- 1
    }
    if (save.options[3] == "jpeg") {
      plotype <- 2
    }
    if (is.null(plotype)) {
      cat(
        "Invalid plot type (should be either 'pdf' or 'jpeg').",
        "\n", "The plot was not captured!", "\n"
      )
    } else {
      if (save.options[2] == "default") {
        wd <- paste(getwd(), "/", sep = "")
      } else {
        wd <- save.options[2]
      }
      fileName <- paste(wd, save.options[1], switch(plotype,
        `1` = ".pdf",
        `2` = ".jpg"
      ), sep = "")
      if (plotype == 1) {
        {
          pdf(file = fileName)
          internalSIBTEST()
        }
        dev.off()
      }
      if (plotype == 2) {
        {
          jpeg(filename = fileName)
          internalSIBTEST()
        }
        dev.off()
      }
      cat("The plot was captured and saved into", "\n",
        " '", fileName, "'", "\n", "\n",
        sep = ""
      )
    }
  } else {
    cat("The plot was not captured!", "\n", sep = "")
  }
}





###

#' @export
print.SIBTEST <- function(x, ...) {
  res <- x
  cat("\n")
  cat(
    "Detection of Differential Item Functioning using SIBTEST method",
    "\n"
  )
  if (res$purification & is.null(res$anchor.names)) {
    pur <- "with "
  } else {
    pur <- "without "
  }
  cat(pur, "item purification", "\n", "\n", sep = "")
  if (res$purification & is.null(res$anchor.names)) {
    if (res$nrPur <= 1) {
      word <- " iteration"
    } else {
      word <- " iterations"
    }
    if (!res$convergence) {
      cat("WARNING: no item purification convergence after ",
        res$nrPur, word, "\n",
        sep = ""
      )
      loop <- NULL
      for (i in 1:res$nrPur) loop[i] <- sum(res$difPur[1, ] == res$difPur[i + 1, ])
      if (max(loop) != length(res$X2)) {
        cat("(Note: no loop detected in less than ",
          res$nrPur, word, ")", "\n",
          sep = ""
        )
      } else {
        cat("(Note: loop of length ", min((1:res$nrPur)[loop ==
          length(res$X2)]), " in the item purification process)",
        "\n",
        sep = ""
        )
      }
      cat(
        "WARNING: following results based on the last iteration of the purification",
        "\n", "\n"
      )
    } else {
      cat("Convergence reached after ", res$nrPur, word,
        "\n", "\n",
        sep = ""
      )
    }
  }
  if (res$type == "udif") {
    cat(
      "Investigation of uniform DIF using SIBTEST (Shealy and Stout, 1993)",
      "\n", "\n"
    )
  } else {
    cat(
      "Investigation of nonuniform DIF using Crossing-SIBTEST (Chalmers, 2018)",
      "\n", "\n"
    )
  }
  if (is.null(res$anchor.names)) {
    itk <- 1:length(res$X2)
    cat("No set of anchor items was provided", "\n", "\n")
  } else {
    itk <- (1:length(res$X2))[!is.na(res$X2)]
    cat("Anchor items (provided by the user):", "\n")
    if (is.null(res$names)) {
      mm <- NULL
      for (i in res$anchor.names) {
        mm <- c(mm, paste("Item",
          i,
          sep = ""
        ))
      }
    } else {
      if (is.numeric(res$anchor.names)) {
        mm <- res$names[res$anchor.names]
      } else {
        mm <- res$anchor.names
      }
    }
    mm <- cbind(mm)
    rownames(mm) <- rep("", nrow(mm))
    colnames(mm) <- ""
    print(mm, quote = FALSE)
    cat("\n", "\n")
  }
  if (is.null(res$p.adjust.method)) {
    cat(
      "No p-value adjustment for multiple comparisons",
      "\n", "\n"
    )
  } else {
    pAdjMeth <- switch(res$p.adjust.method,
      bonferroni = "Bonferroni",
      holm = "Holm",
      hochberg = "Hochberg",
      hommel = "Hommel",
      BH = "Benjamini-Hochberg",
      BY = "Benjamini-Yekutieli"
    )
    cat(
      "Multiple comparisons made with", pAdjMeth, "adjustement of p-values\n"
    )
    if (res$purification) {
      cat(paste0(
        "Multiple comparison applied after ",
        ifelse(res$puriadjType == "simple", "", "each iteration of "),
        "item purification \n\n"
      ))
    } else {
      cat("\n")
    }
  }
  if (is.null(res$p.adjust.method)) {
    symb <- symnum(res$p.value, c(
      0, 0.001, 0.01, 0.05, 0.1,
      1
    ), symbols = c("***", "**", "*", ".", ""))
  } else {
    symb <- symnum(round(res$adjusted.p, 4), c(
      0, 0.001,
      0.01, 0.05, 0.1, 1
    ), symbols = c(
      "***", "**", "*", ".",
      ""
    ))
  }
  m1 <- cbind(res$Beta[itk], res$SE[itk], res$X2[itk], res$p.value[itk])
  if (!is.null(res$p.adjust.method)) {
    m1 <- cbind(m1, round(res$adjusted.p[itk], 4))
  }
  m1 <- round(m1, 4)
  m1 <- noquote(cbind(format(m1, justify = "right"), symb[itk]))
  if (!is.null(res$names)) {
    rownames(m1) <- res$names[itk]
  } else {
    rn <- NULL
    for (i in 1:nrow(m1)) rn[i] <- paste("Item", i, sep = "")
    rownames(m1) <- rn[itk]
  }
  if (is.null(res$p.adjust.method)) {
    colnames(m1) <- c(
      "Beta", "SE", "X2 Stat.", "P-value",
      ""
    )
  } else {
    colnames(m1) <- c(
      "Beta", "SE", "X2 Stat.", "P-value",
      "Adj. P", ""
    )
  }
  print(m1)
  cat("\n")
  cat(
    "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ",
    "\n"
  )
  THR <- qchisq(1 - res$alpha, res$df[itk][1])
  cat("\n", "Detection threshold: ", round(THR, 4), " (significance level: ",
    res$alpha, ")", "\n", "\n",
    sep = ""
  )
  if (is.character(res$DIFitems)) {
    cat(
      "Items detected as DIF items:", res$DIFitems, "\n",
      "\n"
    )
  } else {
    cat("Items detected as DIF items:", "\n")
    if (!is.null(res$names)) {
      m2 <- res$names
    } else {
      rn <- NULL
      for (i in 1:length(res$X2)) {
        rn[i] <- paste("Item",
          i,
          sep = ""
        )
      }
      m2 <- rn
    }
    m2 <- cbind(m2[res$DIFitems])
    rownames(m2) <- rep("", nrow(m2))
    colnames(m2) <- ""
    print(m2, quote = FALSE)
    cat("\n", "\n")
  }
  if (!x$save.output) {
    cat("Output was not captured!", "\n")
  } else {
    if (x$output[2] == "default") {
      wd <- paste(getwd(), "/", sep = "")
    } else {
      wd <- x$output[2]
    }
    fileName <- paste(wd, x$output[1], ".txt", sep = "")
    cat("Output was captured and saved into file", "\n",
      " '", fileName, "'", "\n", "\n",
      sep = ""
    )
  }
}
