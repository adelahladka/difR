##' Mantel-Haenszel DIF method
##'
##' Performs DIF detection using Mantel-Haenszel method.
##'
##' The method of Mantel-Haenszel (1959) allows for detecting uniform
##' differential item functioning without requiring an item response model
##' approach.
##'
##' The \code{Data} is a matrix whose rows correspond to the subjects and
##' columns to the items. In addition, \code{Data} can hold the vector of group
##' membership. If so, \code{group} indicates the column of \code{Data} which
##' corresponds to the group membership, either by specifying its name or by
##' giving the column number. Otherwise, \code{group} must be a vector of same
##' length as \code{nrow(Data)}.
##'
##' Missing values are allowed for item responses (not for group membership)
##' but must be coded as \code{NA} values. They are discarded from sum-score
##' computation.
##'
##' The vector of group membership must hold only two different values, either
##' as numeric or character. The focal group is defined by the value of the
##' argument \code{focal.name}.
##'
##' The matching criterion can be either the test score or any other continuous
##' or discrete variable to be passed in the \code{\link{mantelHaenszel}}
##' function. This is specified by the \code{match} argument. By default, it
##' takes the value \code{"score"} and the test score (i.e. raw score) is
##' computed. The second option is to assign to \code{match} a vector of
##' continuous or discrete numeric values, which acts as the matching
##' criterion. Note that for consistency this vector should not belong to the
##' \code{Data} matrix.
##'
##' The DIF statistic is specified by the \code{MHstat} argument. By default,
##' \code{MHstat} takes the value \code{"MHChisq"} and the Mantel-Haenszel
##' chi-square statistic is used. The other optional value is \code{"logOR"},
##' and the log odds-ratio statistic (that is, the log of \code{alphaMH}
##' divided by the square root of \code{varLambda}) is used. See Penfield and
##' Camilli (2007), Philips and Holland (1987), and \code{\link{mantelHaenszel}}
##' help file.
##'
##' By default, the asymptotic Mantel-Haenszel statistic is computed. However,
##' the exact statistics and related P-values can be obtained by specifying the
##' logical argument \code{exact} to \code{TRUE}. See Agresti (1990, 1992) for
##' further details about exact inference.
##'
##' The threshold (or cut-score) for classifying items as DIF depends on the
##' DIF statistic. With the Mantel-Haenszel chi-squared statistic
##' (\code{MHstat = "MHChisq"}), it is computed as the quantile of the
##' chi-square distribution with lower-tail probability of one minus
##' \code{alpha} and with one degree of freedom. With the log odds-ratio
##' statistic (\code{MHstat = "logOR"}), it is computed as the quantile of the
##' standard normal distribution with lower-tail probability of
##' 1-\code{alpha}/2. With exact inference, it is simply the \code{alpha} level
##' since exact P-values are returned.
##'
##' By default, the continuity correction factor -0.5 is used (Holland & Thayer,
##' 1988). One can nevertheless remove it by specifying \code{correct = FALSE}.
##'
##' In addition, the Mantel-Haenszel estimates of the common odds ratios
##' \eqn{\alpha_{\text{MH}}} are used to measure the effect sizes of the items.
##' These are obtained by \eqn{\Delta_{\text{MH}} = -2.35 \log
##' \alpha_{\text{MH}}} (Holland & Thayer, 1985). According to the ETS delta
##' scale, the effect size of an item is classified as negligible if
##' \eqn{|\Delta_{\text{MH}}| \leq 1}, moderate if
##' \eqn{1 \leq |\Delta_{\text{MH}}| \leq 1.5}, and large if \eqn{|\Delta_{\text{MH}}| \geq
##' 1.5}. The values of the effect sizes, together with the ETS classification,
##' are printed with the output. Note that this is returned only for asymptotic
##' tests, i.e. when \code{exact} is \code{FALSE}.
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
##' The output of the \code{difMH}, as displayed by the \code{print.MH}
##' function, can be stored in a text file provided that \code{save.output} is
##' set to \code{TRUE} (the default value \code{FALSE} does not execute the
##' storage). In this case, the name of the text file must be given as a
##' character string into the first component of the \code{output} argument
##' (default name is \code{"out"}), and the path for saving the text file can
##' be given through the second component of \code{output}. The default value
##' is \code{"default"}, meaning that the file will be saved in the current
##' working directory. Any other path can be specified as a character string:
##' see the \bold{Examples} section for an illustration.
##'
##' The \code{plot.MH} function displays the DIF statistics in a plot, with
##' each item on the X axis. The type of point and the color are fixed by the
##' usual \code{pch} and \code{col} arguments. Option \code{number} permits to
##' display the item numbers instead. Also, the plot can be stored in a figure
##' file, either in PDF or JPEG format. Fixing \code{save.plot} to \code{TRUE}
##' allows this process. The figure is defined through the components of
##' \code{save.options}. The first two components perform similarly as those of
##' the \code{output} argument. The third component is the figure format, with
##' allowed values \code{"pdf"} (default) for PDF file and \code{"jpeg"} for
##' JPEG file. Note that no plot is returned for exact inference.
##'
##' @aliases difMH print.MH plot.MH
##' @param Data numeric: either the data matrix only, or the data matrix plus
##' the vector of group membership. See \bold{Details}.
##' @param group numeric or character: either the vector of group membership or
##' the column indicator (within \code{data}) of group membership. See
##' \bold{Details}.
##' @param focal.name numeric or character indicating the level of \code{group}
##' which corresponds to the focal group.
##' @param anchor either \code{NULL} (default) or a vector of item names (or
##' identifiers) to specify the anchor items. See \bold{Details}.
##' @param match specifies the type of matching criterion. Can be either
##' \code{"score"} (default) to compute the test score, or any continuous or
##' discrete variable with the same length as the number of rows of
##' \code{Data}. See \bold{Details}.
##' @param MHstat character: specifies the DIF statistic to be used for DIF
##' identification. Possible values are \code{"MHChisq"} (default) and
##' \code{"logOR"}. See \bold{Details}.
##' @param correct logical: should the continuity correction be used? (default
##' is \code{TRUE})
##' @param exact logical: should an exact test be computed? (default is
##' \code{FALSE}).
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
##' @param x the result from a \code{MH} class object.
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
##' @return A list of class "MH" with the following arguments:
##' \describe{
##'   \item{MH}{the values of the Mantel-Haenszel DIF statistics (either exact or asymptotic).}
##'   \item{p.value}{the p-values for the Mantel-Haenszel statistics (either exact or asymptotic).}
##'   \item{alphaMH}{the values of the mantel-Haenszel testimates of common odds ratios. Returned only if \code{exact} is \code{FALSE}.}
##'   \item{varLambda}{the values of the variances of the log odds-ratio statistics. Returned only if \code{exact} is \code{FALSE}.}
##'   \item{MHstat}{the value of the \code{MHstat} argument. Returned only if \code{exact} is \code{FALSE}.}
##'   \item{alpha}{the value of \code{alpha} argument.}
##'   \item{thr}{the threshold (cut-score) for DIF detection. Returned only if \code{exact} is \code{FALSE}.}
##'   \item{DIFitems}{either the column indicators of the items which were detected as DIF items, or \code{"No DIF item detected"}.}
##'   \item{correct}{the value of \code{correct} option.}
##'   \item{exact}{the value of \code{exact} option.}
##'   \item{match}{a character string, either \code{"score"} or \code{"matching variable"} depending on the \code{match} argument.}
##'   \item{p.adjust.method}{the value of the \code{p.adjust.method} argument.}
##'   \item{adjusted.p}{either \code{NULL} or the vector of adjusted p-values for multiple comparisons.}
##'   \item{purification}{the value of \code{purify} option.}
##'   \item{nrPur}{the number of iterations in the item purification process. Returned only if \code{purify} is \code{TRUE}.}
##'   \item{difPur}{a binary matrix with one row per iteration in the item purification process and one column per item. Zeros and ones in the \emph{i}-th row refer to items which were classified respectively as non-DIF and DIF items at the (\emph{i}-1)-th step. The first row corresponds to the initial classification of the items. Returned only if \code{purify} is \code{TRUE}.}
##'   \item{convergence}{logical indicating whether the iterative item purification process stopped before the maximal number \code{nrIter} of allowed iterations. Returned only if \code{purify} is \code{TRUE}.}
##'   \item{puriadjType}{the value of \code{puriadjType} option. Returned only when \code{purify} is \code{TRUE}.}
##'   \item{names}{the names of the items.}
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
##' Sebastien Beland \cr
##' Faculte des sciences de l'education \cr
##' Universite de Montreal (Canada) \cr
##' \email{sebastien.beland@@umontreal.ca} \cr
##'
##' Gilles Raiche \cr
##' Universite du Quebec a Montreal \cr
##' \email{raiche.gilles@@uqam.ca} \cr
##'
##' @seealso \code{\link{mantelHaenszel}}, \code{\link{dichoDif}}, \code{\link{p.adjust}}
##'
##' @references
##' Agresti, A. (1990). \emph{Categorical data analysis}. New York: Wiley.
##'
##' Agresti, A. (1992). A survey of exact inference for contingency tables.
##' \emph{Statistical Science, 7}, 131--177, \doi{10.1214/ss/1177011454}
##'
##' Hladká, A., Martinková, P., and Magis, D. (2023). Combining item purification
##' and multiple comparison adjustment methods in detection of differential item
##' functioning. \emph{Multivariate Behavioral Research, 59}(1), 46--61,
##' \doi{10.1080/00273171.2023.2205393}
##'
##' Holland, P. W. and Thayer, D. T. (1985). An alternative definition of the
##' ETS delta scale of item difficulty. \emph{Research Report RR-85-43}.
##' Princeton, NJ: Educational Testing Service.
##'
##' Holland, P. W. and Thayer, D. T. (1988). Differential item performance and
##' the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Ed.),
##' \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.
##'
##' Kim, J., and Oshima, T. C. (2013). Effect of multiple testing adjustment in
##' differential item functioning detection. \emph{Educational and
##' Psychological Measurement, 73}, 458--470, \doi{10.1177/0013164412467033}
##'
##' Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general
##' framework and an R package for the detection of dichotomous differential
##' item functioning. \emph{Behavior Research Methods, 42}, 847--862,
##' \doi{10.3758/BRM.42.3.847}
##'
##' Mantel, N. and Haenszel, W. (1959). Statistical aspects of the analysis of
##' data from retrospective studies of disease. \emph{Journal of the National
##' Cancer Institute, 22}, 719--748.
##'
##' Penfield, R. D., and Camilli, G. (2007). Differential item functioning and
##' item bias. In C. R. Rao and S. Sinharray (Eds.), \emph{Handbook of
##' Statistics 26: Psychometrics} (pp. 125--167). Amsterdam, The Netherlands:
##' Elsevier.
##'
##' Philips, A., and Holland, P. W. (1987). Estimators of the Mantel-Haenszel
##' log odds-ratio estimate. \emph{Biometrics, 43}, 425--431, \doi{10.2307/2531824}
##'
##' Raju, N. S., Bode, R. K. and Larsen, V. S. (1989). An empirical assessment
##' of the Mantel-Haenszel statistic to detect differential item functioning.
##' \emph{Applied Measurement in Education, 2}, 1--13, \doi{10.1207/s15324818ame0201_1}
##'
##' Uttaro, T. and Millsap, R. E. (1994). Factors influencing the
##' Mantel-Haenszel procedure in the detection of differential item
##' functioning. \emph{Applied Psychological Measurement, 18}, 15--25, \doi{10.1177/014662169401800102}
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
##'  r <- difMH(verbal, group = 25, focal.name = 1)
##'  difMH(verbal, group = "Gender", focal.name = 1)
##'  difMH(verbal[, 1:24], group = verbal[, 25], focal.name = 1)
##'
##'  # With log odds-ratio statistic
##'  r2 <- difMH(verbal, group = 25, focal.name = 1, MHstat = "logOR")
##'
##'  # With exact inference
##'  difMH(verbal, group = 25, focal.name = 1, exact = TRUE)
##'
##' # Multiple comparisons adjustment using Benjamini-Hochberg method
##'  difMH(verbal, group = 25, focal.name = 1, p.adjust.method = "BH")
##'
##'  # With item purification
##'  difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE)
##'  difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, nrIter = 5)
##'
##'  # With combination of item purification and multiple comparisons adjustment
##'  difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "simple")
##'  difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "combined")
##'
##'  # Without continuity correction and with 0.01 significance level
##'  difMH(verbal, group = "Gender", focal.name = 1, alpha = 0.01, correct = FALSE)
##'
##'  # With items 1 to 5 set as anchor items
##'  difMH(verbal, group = "Gender", focal.name = 1, anchor = 1:5)
##'  difMH(verbal, group = "Gender", focal.name = 1, anchor = 1:5, purify = TRUE)
##'
##'  # Saving the output into the "MHresults.txt" file (and default path)
##'  r <- difMH(verbal, group = 25, focal.name = 1, save.output = TRUE,
##'             output = c("MHresults","default"))
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
##' @export
difMH <- function(Data, group, focal.name, anchor = NULL, match = "score", MHstat = "MHChisq", correct = TRUE, exact = FALSE,
                  alpha = 0.05, purify = FALSE, nrIter = 10, p.adjust.method = NULL, puriadjType = "simple",
                  save.output = FALSE, output = c("out", "default")) {
  if (purify & match[1] != "score") {
    stop("purification not allowed when matching variable is not 'score'",
      call. = FALSE
    )
  }
  if (!is.character(puriadjType) || !puriadjType %in% c("simple", "combined")) {
    stop("'puriadjType' can be either 'simple' or 'combined'.",
         call. = FALSE
    )
  }
  internalMH <- function() {
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
    Q <- switch(MHstat,
      MHChisq = qchisq(1 - alpha, 1),
      logOR = qnorm(1 - alpha / 2)
    )

    if (is.null(Q)) {
      stop("'MHstat' argument not valid", call. = FALSE)
    }
    if (!is.null(anchor)) {
      dif.anchor <- anchor
      if (is.numeric(anchor)) {
        ANCHOR <- anchor
      } else {
        ANCHOR <- NULL
        for (i in 1:length(anchor)) ANCHOR[i] <- (1:ncol(DATA))[colnames(DATA) == anchor[i]]
      }
    } else {
      ANCHOR <- 1:ncol(DATA)
      dif.anchor <- NULL
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

    if (exact) {
      if (!purify | match[1] != "score" | !is.null(anchor)) {
        PROV <- mantelHaenszel(DATA, Group, match = match, correct = correct, exact = exact, anchor = ANCHOR)
        STATS <- PROV$resMH
        PVAL <- PROV$Pval
        P.ADJUST <- p.adjust(PVAL, method = adj.method)

        if (min(P.ADJUST, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
        } else {
          DIFitems <- (1:ncol(DATA))[PROV$Pval < alpha]
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- P.ADJUST
        }

        RES <- list(
          MH = STATS, p.value = PROV$Pval, alpha = alpha, DIFitems = DIFitems,
          correct = correct, exact = exact, match = PROV$match, p.adjust.method = p.adjust.method, adjusted.p = adjusted.p, purification = purify, names = colnames(DATA),
          anchor.names = dif.anchor, save.output = save.output, output = output
        )
        if (!is.null(anchor)) {
          RES$MH[ANCHOR] <- NA
          RES$Pval[ANCHOR] <- NA
          for (i in 1:length(RES$DIFitems)) {
            if (sum(RES$DIFitems[i] == ANCHOR) == 1) RES$DIFitems[i] <- NA
          }
          RES$DIFitems <- RES$DIFitems[!is.na(RES$DIFitems)]
        }
      } else {
        nrPur <- 0
        difPur <- NULL
        noLoop <- FALSE
        prov1 <- mantelHaenszel(DATA, Group, match = match, correct = correct, exact = exact)
        stats1 <- prov1$resMH
        pval1 <- prov1$Pval
        p.adjust1 <- p.adjust(pval1, method = puri.adj.method)
        if (min(p.adjust1, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
          noLoop <- TRUE
        } else {
          dif <- (1:ncol(DATA))[prov1$Pval < alpha]
          difPur <- rep(0, length(stats1))
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
              prov2 <- mantelHaenszel(DATA, Group,
                correct = correct,
                match = match, anchor = nodif, exact = exact
              )
              stats2 <- prov2$resMH
              pval2 <- prov2$Pval
              p.adjust2 <- p.adjust(pval2, method = puri.adj.method)
              if (min(p.adjust2, na.rm = T) >= alpha) {
                dif2 <- NULL
              } else {
                dif2 <- (1:ncol(DATA))[prov2$Pval < alpha]
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
          stats1 <- stats2
          prov1 <- prov2
          pval1 <- pval2
          p.adjust1 <- p.adjust(pval1, method = adj.method)
          if (min(p.adjust1, na.rm = T) >= alpha) {
            DIFitems <- "No DIF item detected"
          } else {
            DIFitems <- which(!is.na(stats1) & p.adjust1 < alpha)
          }
        }
        if (!is.null(difPur)) {
          rownames(difPur) <- paste0("Step", 1:nrow(difPur) - 1)
          colnames(difPur) <- colnames(DATA)
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- p.adjust1
        }

        RES <- list(
          MH = stats1, p.value = prov1$Pval, alpha = alpha, DIFitems = DIFitems,
          correct = correct, exact = exact, match = prov1$match, p.adjust.method = p.adjust.method,
          adjusted.p = adjusted.p, puriadjType = puriadjType, purification = purify, nrPur = nrPur,
          difPur = difPur, puri.adj.method = puri.adj.method, convergence = noLoop, puriadjType = puriadjType,
          names = colnames(DATA), anchor.names = NULL, save.output = save.output, output = output
        )
      }
    } else {
      if (!purify | match[1] != "score" | !is.null(anchor)) {
        PROV <- mantelHaenszel(DATA, Group, match = match, correct = correct, exact = exact, anchor = ANCHOR)
        if (MHstat == "MHChisq") {
          STATS <- PROV$resMH
          PVAL <- 1 - pchisq(STATS, 1)
        } else {
          STATS <- log(PROV$resAlpha) / sqrt(PROV$varLambda)
          PVAL <- 2 * (1 - pnorm(abs(STATS)))
        }
        P.ADJUST <- p.adjust(PVAL, method = adj.method)
        if (min(P.ADJUST, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
        } else {
          DIFitems <- which(!is.na(STATS) & P.ADJUST < alpha)
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- P.ADJUST
        }

        RES <- list(
          MH = STATS, p.value = PVAL, alphaMH = PROV$resAlpha,
          varLambda = PROV$varLambda, MHstat = MHstat,
          alpha = alpha, thr = Q, DIFitems = DIFitems,
          correct = correct, exact = exact, match = PROV$match,
          p.adjust.method = p.adjust.method, adjusted.p = adjusted.p,
          purification = purify, names = colnames(DATA),
          anchor.names = dif.anchor, save.output = save.output, output = output
        )
        if (!is.null(anchor)) {
          RES$MH[ANCHOR] <- NA
          RES$alphaMH[ANCHOR] <- NA
          RES$varLambda[ANCHOR] <- NA
          for (i in 1:length(RES$DIFitems)) {
            if (sum(RES$DIFitems[i] == ANCHOR) == 1) RES$DIFitems[i] <- NA
          }
          RES$DIFitems <- RES$DIFitems[!is.na(RES$DIFitems)]
        }
      } else {
        nrPur <- 0
        difPur <- NULL
        noLoop <- FALSE
        prov1 <- mantelHaenszel(DATA, Group, match = match, correct = correct, exact = exact)
        if (MHstat == "MHChisq") {
          stats1 <- prov1$resMH
          pval1 <- 1 - pchisq(stats1, 1)
        } else {
          stats1 <- log(prov1$resAlpha) / sqrt(prov1$varLambda)
          pval1 <- 2 * (1 - pnorm(abs(stats1)))
        }
        p.adjust1 <- p.adjust(pval1, method = puri.adj.method)

        if (min(p.adjust1, na.rm = T) >= alpha) {
          DIFitems <- "No DIF item detected"
          noLoop <- TRUE
        } else {
          dif <- which(!is.na(stats1) & p.adjust1 < alpha)
          difPur <- rep(0, length(stats1))
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

              prov2 <- mantelHaenszel(DATA, Group,
                match = match, correct = correct,
                anchor = nodif, exact = exact
              )
              if (MHstat == "MHChisq") {
                stats2 <- prov2$resMH
                pval2 <- 1 - pchisq(stats2, 1)
              } else {
                stats2 <- log(prov2$resAlpha) / sqrt(prov2$varLambda)
                pval2 <- 2 * (1 - pnorm(abs(stats2)))
              }
              p.adjust2 <- p.adjust(pval2, method = puri.adj.method)

              if (min(p.adjust2, na.rm = T) >= alpha) {
                dif2 <- NULL
              } else {
                dif2 <- which(!is.na(stats2) & p.adjust2 < alpha)
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
          stats1 <- stats2
          prov1 <- prov2
          pval1 <- pval2
          p.adjust1 <- p.adjust(pval1, method = adj.method)
          if (min(p.adjust1, na.rm = T) >= alpha) {
            DIFitems <- "No DIF item detected"
          } else {
            DIFitems <- which(!is.na(stats1) & p.adjust1 < alpha)
          }
        }
        if (!is.null(difPur)) {
          rownames(difPur) <- paste0("Step", 1:nrow(difPur) - 1)
          colnames(difPur) <- colnames(DATA)
        }

        if (is.null(p.adjust.method)) {
          adjusted.p <- NULL
        } else {
          adjusted.p <- p.adjust1
        }


        RES <- list(
          MH = stats1, p.value = pval1, alphaMH = prov1$resAlpha,
          varLambda = prov1$varLambda, MHstat = MHstat,
          alpha = alpha, thr = Q, DIFitems = DIFitems,
          correct = correct, exact = exact, match = prov1$match,
          p.adjust.method = p.adjust.method, adjusted.p = adjusted.p, puriadjType = puriadjType,
          purification = purify, nrPur = nrPur,
          difPur = difPur, convergence = noLoop, names = colnames(DATA),
          anchor.names = NULL, save.output = save.output, output = output
        )
      }
    }
    class(RES) <- "MH"
    return(RES)
  }
  resToReturn <- internalMH()
  if (save.output == TRUE) {
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



#' @export
plot.MH <- function(x, pch = 8, number = TRUE, col = "red", save.plot = FALSE, save.options = c("plot", "default", "pdf"), ...) {
  if (x$exact) stop("Error: plot is not available with exact Mantel-Haenszel test", call. = FALSE)
  internalMH <- function() {
    res <- x
    if (res$MHstat == "MHChisq") {
      yl <- c(0, max(c(res$MH, res$thr) + 1, na.rm = TRUE))
    } else {
      yl <- c(min(c(res$MH, -res$thr) - 0.5, na.rm = TRUE), max(c(
        res$MH,
        res$thr
      ) + 0.5, na.rm = TRUE))
    }
    ytitle <- switch(res$MHstat,
      MHChisq = "MH Chi-square statistic",
      logOR = "log OR statistic"
    )
    if (!number) {
      plot(res$MH,
        xlab = "Item", ylab = ytitle, ylim = yl,
        pch = pch, main = "Mantel-Haenszel"
      )
      if (!is.character(res$DIFitems)) {
        points(res$DIFitems, res$MH[res$DIFitems],
          pch = pch,
          col = col
        )
      }
    } else {
      plot(res$MH,
        xlab = "Item", ylab = ytitle, ylim = yl,
        col = "white", main = "Mantel-Haenszel"
      )
      text(1:length(res$MH), res$MH, 1:length(res$MH))
      if (!is.character(res$DIFitems)) {
        text(res$DIFitems, res$MH[res$DIFitems], res$DIFitems,
          col = col
        )
      }
    }
    abline(h = res$thr)
    if (res$MHstat == "logOR") {
      abline(h = -res$thr)
    }
  }
  internalMH()
  if (save.plot) {
    plotype <- NULL
    if (save.options[3] == "pdf") plotype <- 1
    if (save.options[3] == "jpeg") plotype <- 2
    if (is.null(plotype)) {
      cat("Invalid plot type (should be either 'pdf' or 'jpeg').", "\n", "The plot was not captured!", "\n")
    } else {
      if (save.options[2] == "default") {
        wd <- paste(getwd(), "/", sep = "")
      } else {
        wd <- save.options[2]
      }
      fileName <- paste(wd, save.options[1], switch(plotype,
        "1" = ".pdf",
        "2" = ".jpg"
      ), sep = "")
      if (plotype == 1) {
        {
          pdf(file = fileName)
          internalMH()
        }
        dev.off()
      }
      if (plotype == 2) {
        {
          jpeg(filename = fileName)
          internalMH()
        }
        dev.off()
      }
      cat("The plot was captured and saved into", "\n", " '", fileName, "'", "\n", "\n", sep = "")
    }
  } else {
    cat("The plot was not captured!", "\n", sep = "")
  }
}



#' @export
print.MH <- function(x, ...) {
  res <- x
  cat("\n")
  cat(
    "Detection of Differential Item Functioning using Mantel-Haenszel method",
    "\n"
  )
  if (res$correct & !res$exact) {
    corr <- "with "
  } else {
    corr <- "without "
  }
  if (res$purification & is.null(res$anchor.names)) {
    pur <- "with "
  } else {
    pur <- "without "
  }
  cat(corr, "continuity correction and ", pur, "item purification",
    "\n", "\n",
    sep = ""
  )
  if (res$exact) {
    cat("Results based on exact inference", "\n", "\n")
  } else {
    cat("Results based on asymptotic inference", "\n", "\n")
  }
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
      if (max(loop) != length(res$MH)) {
        cat("(Note: no loop detected in less than ",
          res$nrPur, word, ")", "\n",
          sep = ""
        )
      } else {
        cat("(Note: loop of length ", min((1:res$nrPur)[loop ==
          length(res$MH)]), " in the item purification process)",
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
  if (res$match[1] == "score") {
    cat("Matching variable: test score", "\n", "\n")
  } else {
    cat(
      "Matching variable: specified matching variable",
      "\n", "\n"
    )
  }
  if (is.null(res$anchor.names)) {
    itk <- 1:length(res$MH)
    cat("No set of anchor items was provided", "\n", "\n")
  } else {
    itk <- (1:length(res$MH))[!is.na(res$MH)]
    cat("Anchor items (provided by the user):", "\n")
    if (is.numeric(res$anchor.names)) {
      mm <- res$names[res$anchor.names]
    } else {
      mm <- res$anchor.names
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
  if (res$exact) {
    met <- "Exact statistic:"
  } else {
    met <- switch(res$MHstat,
      MHChisq = "Mantel-Haenszel Chi-square statistic:",
      logOR = "Log odds-ratio statistic:"
    )
  }
  cat(met, "\n", "\n")
  if (res$exact) {
    pval <- round(res$p.value, 4)
  } else {
    if (res$MHstat == "MHChisq") {
      pval <- round(1 - pchisq(res$MH, 1), 4)
    } else {
      pval <- round(
        2 * (1 - pnorm(abs(res$MH))),
        4
      )
    }
  }
  if (is.null(res$p.adjust.method)) {
    symb <- symnum(pval, c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", "")
    )
  } else {
    symb <- symnum(round(res$adjusted.p, 4), c(
      0, 0.001,
      0.01, 0.05, 0.1, 1
    ), symbols = c(
      "***", "**", "*", ".",
      ""
    ))
  }
  if (!res$exact) {
    m1 <- cbind(round(res$MH[itk], 4), pval[itk])
  } else {
    m1 <- cbind(round(res$MH[itk]), pval[itk])
  }
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
    colnames(m1) <- c("Stat.", "P-value", "")
  } else {
    colnames(m1) <- c("Stat.", "P-value", "Adj. P", "")
  }
  print(m1)
  cat("\n")
  cat(
    "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ",
    "\n"
  )
  if (res$exact) {
    cat("\n", "Significance level: ", res$alpha, "\n", "\n",
      sep = ""
    )
  } else {
    cat("\n", "Detection threshold: ", round(res$thr, 4),
      " (significance level: ", res$alpha, ")", "\n", "\n",
      sep = ""
    )
  }
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
      for (i in 1:length(res$MH)) {
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
  if (!res$exact) {
    cat("Effect size (ETS Delta scale):", "\n", "\n")
    cat("Effect size code:", "\n")
    cat(" 'A': negligible effect", "\n")
    cat(" 'B': moderate effect", "\n")
    cat(" 'C': large effect", "\n", "\n")
    r2 <- round(-2.35 * log(res$alphaMH), 4)
    symb1 <- symnum(abs(r2), c(0, 1, 1.5, Inf), symbols = c(
      "A",
      "B", "C"
    ))
    matR2 <- cbind(round(res$alphaMH[itk], 4), r2[itk])
    matR2 <- noquote(cbind(
      format(matR2, justify = "right"),
      symb1[itk]
    ))
    if (!is.null(res$names)) {
      rownames(matR2) <- res$names[itk]
    } else {
      rn <- NULL
      for (i in 1:nrow(matR2)) {
        rn[i] <- paste("Item", i,
          sep = ""
        )
      }
      rownames(matR2) <- rn[itk]
    }
    colnames(matR2) <- c("alphaMH", "deltaMH", "")
    print(matR2)
    cat("\n")
    cat("Effect size codes: 0 'A' 1.0 'B' 1.5 'C'", "\n")
    cat(" (for absolute values of 'deltaMH')", "\n", "\n")
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
