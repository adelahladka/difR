\name{liu_agresti_ccor}
\alias{liu_agresti_ccor}

\title{Liu-Agresti Common Cumulative Odds Ratio}

\description{
Computes the Liu-Agresti estimate of the common cumulative odds ratio (\eqn{\Psi}) and its reciprocal (\eqn{\alpha}) for ordinal data from two independent groups. This statistic quantifies the direction and strength of ordinal association between groups.
}

\usage{
liu_agresti_ccor(responses, group)
}

\arguments{
  \item{responses}{A numeric vector of ordinal item responses. Categories must be coded as integers (e.g., 1 to 5 for a Likert-type scale).}
  \item{group}{A grouping vector indicating the group to which each observation belongs. It must contain exactly two unique values (e.g., "ref" and "foc").}
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

The function does not support missing values; observations with \code{NA} should be removed prior to use.

If one of the response categories is completely absent from one group, then the cumulative margins used in the computation may contain zero values. In such cases, either the numerator or the denominator of the Liu-Agresti formula will be zero, making the estimate undefined. When this occurs, the function returns \code{NA} and issues a warning.

About the notation: In the original article by Liu and Agresti (1996), the cumulative logistic model uses the parameters \eqn{\beta} and \eqn{\theta}. To avoid any confusion with a logistic model or the IRT framework, the symbol \eqn{\psi} is used here to denote the group effect. 

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

group <- rep(c("ref", "foc"), each = 100)  
stopifnot(length(group) == 200)

responses <- sample(1:4, size = length(group), replace = TRUE)
stopifnot(length(responses) == length(group))  

liu_agresti_ccor(as.integer(responses), factor(group))
}

