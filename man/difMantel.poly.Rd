
\name{difMantel.poly}
\alias{difMantel.poly}
\title{Mantel Differential Item Functionning Detection for Polytomous Items}
\description{Implements the Mantel (1963) test for detecting DIF in polytomous items.}
\usage{
difMantel.poly(data, group, focal.name, ref.name,
            match = "score", sig.level = 0.05,
            purify = FALSE, max.iter = 10)
}
\arguments{
  \item{data}{A matrix or data frame of polytomous item responses (one row per subject, one column per item).}
  \item{group}{A vector indicating group membership (same length as number of rows in \code{data}).}
  \item{focal.name}{The value in \code{group} corresponding to the focal group.}
  \item{ref.name}{The value in \code{group} corresponding to the reference group.}
  \item{match}{Specifies the matching variable. Can be \code{"score"} (default) for total score or \code{"restscore"} to exclude the item being tested from the matching score.}
  \item{sig.level}{Significance level for the DIF test (default = 0.05).}
  \item{purify}{Logical. If \code{TRUE}, performs iterative purification to exclude DIF items from the anchor set. Ignored if \code{match = "restscore"}.}
  \item{max.iter}{Maximum number of purification iterations (default = 10).}
}

\value{
  A \code{data.frame} with one column per item and the following columns:
  \item{Stat}{The Mantel test statistic.}
  \item{p.value}{Associated p-value for the DIF test.}
  \item{p.adj}{p-value adjusted for multiple comparisons using Holm's method.}
  \item{Psi_hat}{Liu-Agresti's estimate of the odds ratio.}
  \item{Alpha_hat}{Estimated difficulty ratio.}
  \item{SE_log_Psi}{Standard error of the log-odds ratio.}
  \item{rho.spear}{Spearman correlation between item score and matching score.}
  \item{LA.valid}{Logical indicator of whether Liu-Agresti estimates were valid for each item.}
}

\details{
Chi-square statistic computed for each item using the generalized Mantel (1963) procedure for ordinal responses. This test evaluates whether the distribution of item responses differs significantly between the reference and focal groups, conditioning on the matching score (either total score or rest score). The statistic asymptotically follows a chi-square distribution with 1 degree of freedom under the null hypothesis of no DIF.

If \code{match = "score"}, the total test score is used as the matching criterion. If \code{match = "restscore"}, the item under evaluation is excluded from the score, reducing contamination and improving DIF detection accuracy.

When \code{purify = TRUE}, anchor items are iteratively refined: items flagged as DIF (p < \code{sig.level}) are excluded from the matching score in subsequent iterations. The process stops when the anchor set stabilizes or after \code{max.iter} iterations. If no items remain, the last computed statistics are retained.

For each item, the Mantel statistic is computed. Additionally, Liu–Agresti cumulative odds ratios (\code{Psi_hat}, \code{Alpha_hat}) and their standard errors (\code{SE_log_Psi}) are reported when possible. The logical flag \code{LA.valid} indicates whether these estimates could be computed.

\strong{Note:} All response categories must be observed in both groups for Liu–Agresti estimates to be valid. Missing data should be removed prior to analysis.
}

\references{
Liu, I., & Agresti, A. (1996). Mantel–Haenszel–Type Inference for Cumulative Odds Ratios with a Stratified Ordinal Response. \emph{Biometrics}, 52(4), 1223–1234.

Mantel, N. (1963). Chi-square tests with one degree of freedom: Extensions of the Mantel-Haenszel procedure. \emph{Journal of the American Statistical Association}, 58, 690–700.
}

\author{
    Sebastien Beland \cr
    Faculte des sciences de l'education \cr
    Universite de Montreal (Canada) \cr
    \email{sebastien.beland@umontreal.ca} \cr
}

\examples{
\dontrun{
# Real data example
data(SCS)
# Without purification
difMantel.poly(data = SCS[, 1:10], group = SCS$Gender, focal.name = 1, 
ref.name = 2, purify = FALSE)

# Without purification and restscore
difMantel.poly(data = SCS[, 1:10], group = SCS$Gender, focal.name = 1, 
ref.name = 2, purify = TRUE,match = "restscore")

# With purification
difMantel.poly(data = SCS[, 1:10], group = SCS$Gender, focal.name = 1, 
ref.name = 2, purify = TRUE)

# With simulated data

set.seed(1234)

# original item parameters
a <- rlnorm(10, -0.5)  # slopes
b <- runif(10, -2, 2)  # difficulty
d <- list()
d[[1]] <- c(0, 2, .5, -.15, -1.1)
d[[2]] <- c(0, 2, .25, -.45, -.75)
d[[3]] <- c(0, 1, .5, -.65, -1)
d[[4]] <- c(0, 2, .5, -.85, -2)
d[[5]] <- c(0, 1, .25, -.05, -1)
d[[6]] <- c(0, 2, .5, -.95, -1)
d[[7]] <- c(0, 1, .25, -.35, -2)
d[[8]] <- c(0, 2, .5, -.15, -1)
d[[9]] <- c(0, 1, .25, -.25, -2)
d[[10]] <- c(0, 2, .5, -.35, -1)

# Uniform DIF
It <- 10; NR <- 1000; NF <- 1000
ItDIFa <- NULL; Ga <- NULL
ItDIFb <- c(1, 3)
Gb <- rep(.5, 2)

Out.Unif <- SimPolyDif(It, ItDIFa, ItDIFb, NR, NF, a, b, d,
                       ncat = 5, Ga = Ga, Gb = Gb)
Out.Unif$ipars
Data <- Out.Unif$data

# Without purification and rest score
difMantel.poly(data = Data[, 1:10], group = Data$group, focal.name = "G1", 
ref.name = "G2", purify = FALSE,match = "restscore")

# With purification
difMantel.poly(data = Data[, 1:10], group = Data$group, focal.name = "G1", 
ref.name = "G2", purify = TRUE)

# We implemented a specific S3 plot method: plot.Logistic. It can be used as follows:
res <- difMantel.poly(data = Data[, 1:10], group = Data$group, focal.name = "G1", 
ref.name = "G2", purify = FALSE)
plot.MHPoly(res)
}
}













