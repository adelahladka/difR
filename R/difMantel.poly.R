
difMantel.poly <- function(data, group, focal.name, ref.name,
                        match = "score", sig.level = 0.05,
                        purify = FALSE, max.iter = 10) {

  if (!match %in% c("score", "restscore"))
    stop("'match' must be either 'score' or 'restscore'.")

  if (purify && match == "restscore") {
    warning("Purification is not allowed when match = 'restscore'. Setting purify = FALSE.")
    purify <- FALSE
  }

  test.length <- ncol(data)
  items <- 1:test.length
  current_anchors <- items
  iter <- 0

  compute_scores <- function(data, i, anchors, match) {
    if (match == "restscore") {
      return(rowSums(data[, setdiff(anchors, i), drop = FALSE], na.rm = TRUE))
    } else {
      return(rowSums(data[, anchors, drop = FALSE], na.rm = TRUE))
    }
  }

  run_mantel <- function(data, group, anchors, match, items) {
    TS <- numeric(length(items))
    Psi <- Alpha <- SE_log_Psi <- rho.spear <- rep(NA, length(items))
    la.valid <- rep(TRUE, length(items))
    names(TS) <- colnames(data)[items]

    for (j in seq_along(items)) {
      i <- items[j]
      raw.scores <- compute_scores(data, i, anchors, match)
      raw.scores.F <- raw.scores[group == focal.name]
      raw.scores.R <- raw.scores[group == ref.name]

      max.categ <- max(data[, i], na.rm = TRUE)
      M <- max(raw.scores, na.rm = TRUE)
      S.Fm <- exp.S.Fm <- var.S.Fm <- rep(NA, M)

      for (m in 1:M) {
        temp.F <- data[group == focal.name & raw.scores == m, i]
        temp.R <- data[group == ref.name & raw.scores == m, i]
        temp <- data[raw.scores == m, i]

        if (length(temp.F) < 1 | length(temp.R) < 1 | length(temp) < 2) next

        c.vals.F <- as.numeric(names(table(temp.F)))
        c.vals <- as.numeric(names(table(temp)))

        S.Fm[m] <- sum(table(temp.F) * c.vals.F)
        N.Fm <- length(temp.F)
        N.Rm <- length(temp.R)
        N.m <- N.Fm + N.Rm

        exp.S.Fm[m] <- N.Fm * mean(temp, na.rm = TRUE)

        var.part1 <- (N.Fm * N.Rm) / ((N.m^2) * (N.m - 1))
        N.cm <- table(temp)
        var.part2 <- N.m * sum((c.vals^2) * N.cm) - (sum(c.vals * N.cm))^2

        var.S.Fm[m] <- var.part1 * var.part2
      }

      if (sum(!is.na(var.S.Fm)) > 0 && sum(var.S.Fm, na.rm = TRUE) > 0) {
        TS[j] <- ((sum(S.Fm, na.rm = TRUE) - sum(exp.S.Fm, na.rm = TRUE))^2) / sum(var.S.Fm, na.rm = TRUE)
      } else {
        TS[j] <- NA
      }

      responses <- data[, i]
      if (length(unique(responses[!is.na(responses)])) > 1 &&
          length(unique(responses[group == focal.name])) > 1 &&
          length(unique(responses[group == ref.name])) > 1) {
        la_result <- tryCatch({
          liu_agresti_ccor(responses, group)
        }, error = function(e) rep(NA, 3))
        Psi[j] <- la_result[1]
        Alpha[j] <- la_result[2]
        SE_log_Psi[j] <- la_result[3]
      } else {
        la.valid[j] <- FALSE
      }

      rho.spear[j] <- suppressWarnings(
        cor(raw.scores, responses, method = "spearman", use = "complete.obs")
      )
    }

    pval <- 1 - pchisq(TS, df = 1)
    pval.adj <- p.adjust(pval, method = "holm")

    result <- data.frame(
      Stat = round(TS, 3),
      p.value = round(pval, 4),
      p.adj = round(pval.adj, 4),
      Psi_hat = round(Psi, 4),
      Alpha_hat = round(Alpha, 4),
      SE_log_Psi = round(SE_log_Psi, 4),
      rho.spear = round(rho.spear, 3),
      LA.valid = la.valid
    )

    return(result)
  }

  if (!purify) {
    return(run_mantel(data, group, items, match, items))
  }

  repeat {
    iter <- iter + 1
    result <- run_mantel(data, group, current_anchors, "score", items)
    pval <- result$p.value
    new_anchors <- which(pval >= sig.level)

    if (setequal(current_anchors, new_anchors) || iter >= max.iter) {
      attr(result, "iterations") <- iter
      attr(result, "final_anchors") <- new_anchors
      return(result)
    } else {
      current_anchors <- new_anchors
    }
  }
}

##########################################################

#' @export
plot.MHPoly <- function(x, alpha = 0.05, cex = 0.8, col = "black", save.plot = FALSE, save.options = c("plot", getwd(), "pdf"), ...) {
  if (is.null(rownames(x))) {
    stop("Row names are required to use this function.")
  }

  name <- rownames(x)
  thr <- qchisq(1 - alpha, 1)

  point_cols <- ifelse(x$Stat > thr, "red", col)

  # Sauvegarde des paramètres graphiques originaux
  original_par <- par(no.readonly = TRUE)
  on.exit(par(original_par)) # Restauration des paramètres à la sortie de la fonction

  # Création du graphique
  plot(x$Stat, xlab = "Item", ylab = "Chi-square statistic", main = "Mantel (1963)", ylim = c(0, max(x$Stat) + 1), pch = "", col = point_cols, ...)
  text(x$Stat, labels = name, col = point_cols, cex = cex, ...)
  abline(h = thr, ...)

  if (save.plot) {
    file_name <- save.options[1]
    directory <- save.options[2]
    file_type <- tolower(save.options[3]) # Convertir en minuscules pour la comparaison

    if (!dir.exists(directory)) {
      stop(paste("Directory", directory, "does not exist."))
    }

    full_file_name <- file.path(directory, paste0(file_name, ".", file_type))

    if (file_type == "pdf") {
      pdf(file = full_file_name)
      plot(x$Stat, xlab = "Item", ylab = "Chi-square statistic", main = "Mantel (1963)", ylim = c(0, max(x$Stat) + 1), pch = "", col = point_cols, ...)
      text(x$Stat, labels = name, col = point_cols, cex = cex, ...)
      abline(h = thr, ...)
      dev.off()
      cat("The plot was captured and saved into '", full_file_name, "'\n", sep = "")
    } else if (file_type == "jpeg") {
      jpeg(filename = full_file_name)
      plot(x$Stat, xlab = "Item", ylab = "Chi-square statistic", main = "Mantel (1963)", ylim = c(0, max(x$Stat) + 1), pch = "", col = point_cols, ...)
      text(x$Stat, labels = name, col = point_cols, cex = cex, ...)
      abline(h = thr, ...)
      dev.off()
      cat("The plot was captured and saved into '", full_file_name, "'\n", sep = "")
    } else {
      cat("Invalid plot type (should be either 'pdf' or 'jpeg').\n")
      cat("The plot was not captured!\n")
    }
  } else {
    cat("The plot was not captured!\n")
  }
}
