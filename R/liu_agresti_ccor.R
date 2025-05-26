#' @export
liu_agresti_ccor <- function(responses, group) {
  # Sécurisation des longueurs
  if (length(responses) != length(group)) {
    stop("'responses' and 'group' must have the same length.")
  }

  # Forcer les types attendus
  responses <- as.integer(responses)
  group <- as.character(group)

  # Vérifier le nombre de groupes
  groups <- sort(unique(group))
  if (length(groups) != 2) stop("There must be exactly two groups (reference and focal).")
  g1 <- groups[1]
  g2 <- groups[2]

  # Créer la table 2 × c
  tab <- table(group, responses)
  tab <- as.matrix(tab)
  tab <- tab[c(g1, g2), , drop = FALSE]

  c <- ncol(tab)
  log_Psi_vec <- numeric(0)
  SE_log_Psi_vec <- numeric(0)

  for (j in 1:(c - 1)) {
    a <- sum(tab[1, 1:j])
    b <- sum(tab[1, (j + 1):c])
    c_ <- sum(tab[2, 1:j])
    d <- sum(tab[2, (j + 1):c])

    if (all(c(a, b, c_, d) > 0)) {
      log_Psi <- log((a * d) / (b * c_))
      SE_log <- sqrt(1 / a + 1 / b + 1 / c_ + 1 / d)

      log_Psi_vec <- c(log_Psi_vec, log_Psi)
      SE_log_Psi_vec <- c(SE_log_Psi_vec, SE_log)
    }
  }

  if (length(log_Psi_vec) == 0) {
    return(c(Psi_hat = NA, Alpha_hat = NA, SE_log_Psi = NA))
  }

  # Moyenne pondérée inverse-variance
  weights <- 1 / (SE_log_Psi_vec^2)
  log_Psi_avg <- sum(weights * log_Psi_vec) / sum(weights)
  SE_log_Psi <- sqrt(1 / sum(weights))
  alpha <- log_Psi_avg / log(1 + sqrt(3) / pi)

  return(c(
    Psi_hat = round(exp(log_Psi_avg), 4),
    Alpha_hat = round(alpha, 4),
    SE_log_Psi = round(SE_log_Psi, 4)
  ))
}

