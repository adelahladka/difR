#' @export

liu_agresti_ccor <- function(ordinal_values, groups) {
  create_2xJ_table <- function(ordinal_values, groups, group_names) {
    if (length(ordinal_values) != length(groups)) stop("Vectors must have the same length.")

    unique_values <- sort(unique(ordinal_values))
    J <- length(unique_values)

    table <- matrix(0, nrow = 2, ncol = J)
    colnames(table) <- unique_values
    rownames(table) <- group_names

    for (i in seq_along(ordinal_values)) {
      value <- ordinal_values[i]
      group <- groups[i]
      column <- which(unique_values == value)

      if (group == group_names[1]) {
        table[1, column] <- table[1, column] + 1
      } else if (group == group_names[2]) {
        table[2, column] <- table[2, column] + 1
      } else {
        stop(paste("Group must be one of:", paste(group_names, collapse = ", ")))
      }
    }

    Ar <- cumsum(table[1, ])
    Cf <- cumsum(table[2, ])
    Br <- Ar - sum(table[1, ])
    Df <- Cf - sum(table[2, ])
    N <- sum(table)

    numerator <- sum(Ar * Df / N)
    denominator <- sum(Br * Cf / N)

    if (denominator == 0 || numerator == 0) {
      warning("liu_agresti_ccor: Numerator or denominator is zero - estimation not possible.")
      return(c(NA, NA, NA))
    }

    psi_hat <- numerator / denominator
    log_psi_hat <- log(psi_hat)

    # Var eq. 3
    term1 <- sum((Ar * Df / N)^2) / (numerator^2)
    term2 <- sum((Br * Cf / N)^2) / (denominator^2)
    var_log_psi <- term1 + term2
    se_log_psi <- sqrt(var_log_psi)

    return(c(psi_hat, 1 / psi_hat, se_log_psi))
  }

  group_names <- sort(unique(groups))
  if (length(group_names) != 2) stop("Exactly two groups required.")

  res <- create_2xJ_table(ordinal_values, groups, group_names)
  result <- matrix(round(res, 4), nrow = 1, byrow = TRUE)
  colnames(result) <- c("Psi_hat", "Alpha_hat", "SE_log_Psi")
  rownames(result) <- "Result"
  return(result)
}

