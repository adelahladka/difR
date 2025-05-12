\name{liu_agresti_ccor}
\alias{liu_agresti_ccor}

\title{Liu-Agresti Common Cumulative Odds Ratio}

\description{
Computes the Liu-Agresti estimate of the common cumulative odds ratio (\eqn{\Psi}) and its reciprocal (\eqn{\alpha}) for ordinal data from two independent groups. This statistic quantifies the direction and strength of ordinal association between groups, analogous to a Mantel-Haenszel odds ratio adapted for ordinal scales.
}

\usage{
liu_agresti_ccor(ordinal_values, groups)
}

\arguments{
  \item{ordinal_values}{A numeric vector of ordinal item responses. Categories must be coded as integers (e.g., 1 to 5 for a Likert-type scale).}
  \item{groups}{A grouping vector indicating the group to which each observation belongs. It must contain exactly two unique values (e.g., "ref" and "foc").}
}

\value{
A matrix with one row and three columns containing:
  \item{Psi_hat}{The Liu-Agresti estimate of the common cumulative odds ratio (\eqn{\hat{\Psi}}).}
  \item{Alpha_hat}{The reciprocal of \eqn{\hat{\Psi}}.}
  \item{SE_log_Psi}{The standard error of \eqn{\log(\hat{\Psi})}, which can be used to construct confidence intervals or conduct hypothesis testing.}
}

\details{
This function creates a 2 x J contingency table, where J is the number of distinct ordinal response categories. It computes cumulative marginal frequencies and estimates the odds ratio using Liu and Agresti's formulation (1996, Eq. 2). The variance of the log-transformed estimate is computed according to their Eq. 3.

The estimate \eqn{\hat{\Psi}} is based on cumulative frequencies and is designed for ordinal response categories. It quantifies the association between group membership and the likelihood of higher category responses.

\strong{Important limitation:} if one of the response categories is completely absent from one group, then the cumulative margins used in the computation may contain zero values. In such cases, either the numerator or the denominator of the Liu-Agresti formula will be zero, making the estimate undefined. When this occurs, the function returns \code{NA} and issues a warning.

It is recommended to check that all ordinal response categories are represented in both groups before using this function. Alternatively, smoothing procedures (e.g., adding pseudo-counts) may be implemented externally for sparse data, though this is not done by default to remain consistent with the original Liu-Agresti method.

The function does not support missing values; observations with \code{NA} should be removed prior to use.
}

\references{
Liu, I., & Agresti, A. (1996). Mantel-Haenszel-Type Inference for Cumulative Odds Ratios with a Stratified Ordinal Response. \emph{Biometrics}, 52(4), 1223–1234.
}

\author{
Sebastien Beland \cr
Faculte des sciences de l'education \cr
Universite de Montreal (Canada) \cr
\email{sebastien.beland@umontreal.ca} \cr       
}

\examples{
# Simulated balanced example
set.seed(123)
ordinal_values <- sample(1:4, 200, replace = TRUE)
groups <- rep(c("ref", "foc"), each = 100)

liu_agresti_ccor(ordinal_values, groups)
}
