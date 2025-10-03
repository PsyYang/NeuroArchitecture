suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
})

# Helper: safe lmer fit
fit_lmer_safe <- function(formula, data, REML = TRUE, maxfun = 1e5) {
  ctrl1 <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = maxfun))
  ctrl2 <- lmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = maxfun))
  fit <- tryCatch(lmer(formula, data = data, REML = REML, control = ctrl1),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    fit <- tryCatch(lmer(formula, data = data, REML = REML, control = ctrl2),
                    error = function(e) e)
  }
  if (inherits(fit, "error")) return(NULL)
  fit
}

# Helper: check convergence / singularity
model_ok <- function(m, grad_tol = 0.002) {
  out <- FALSE
  tryCatch({
    if (is.null(m)) return(FALSE)
    if (isSingular(m, tol = 1e-4)) return(FALSE)
    oi <- tryCatch(m@optinfo, error = function(e) NULL)
    if (!is.null(oi) && !is.null(oi$derivs) && !is.null(oi$derivs$gradient)) {
      maxgrad <- max(abs(oi$derivs$gradient))
      if (is.finite(maxgrad) && maxgrad > grad_tol) return(FALSE)
    }
    out <- TRUE
    return(out)
  }, error = function(e) {
    return(FALSE)
  })
}

# Helper: nested comparison
compare_nested <- function(m_small, m_big) {
  if (is.null(m_small) || is.null(m_big)) return(NULL)
  aa <- tryCatch(anova(m_small, m_big), error = function(e) NULL)
  if (is.null(aa)) return(NULL)
  p <- suppressWarnings(aa$`Pr(>Chisq)`[2])
  if (!is.na(p) && p < 0.05) m_big else m_small
}

# ---- Simplified function ----
select_lmm <- function(data, dv, fixed_main, group_subj, group_item = NULL, verbose = TRUE) {
  
  fixed_str <- paste(fixed_main, collapse = " + ")
  rand_base <- paste0("(1|", group_subj, ")",
                      if (!is.null(group_item)) paste0(" + (1|", group_item, ")") else "")
  
  # Candidate random structures (uncorrelated slopes || for stability)
  re_list <- list(rand_base)
  for (fx in fixed_main) {
    re_list <- c(re_list,
                 paste0(rand_base, " + (0 + ", fx, "||", group_subj, ")"),
                 paste0(rand_base, " + (1 + ", fx, "||", group_subj, ")"))
  }
  if (length(fixed_main) > 1) {
    all_slopes <- paste0("(0 + ", paste(fixed_main, collapse = " + "), "||", group_subj, ")")
    all_slopes1 <- paste0("(1 + ", paste(fixed_main, collapse = " + "), "||", group_subj, ")")
    re_list <- c(re_list,
                 paste(rand_base, all_slopes, sep = " + "),
                 paste(rand_base, all_slopes1, sep = " + "))
  }
  re_list <- unique(re_list)
  
  if (verbose) cat("\n[Stage A] Random effects search (REML)\n")
  fit_rand <- list()
  for (i in seq_along(re_list)) {
    fml <- as.formula(paste(dv, "~", fixed_str, "+", re_list[[i]]))
    fit <- fit_lmer_safe(fml, data, REML = TRUE)
    fit_rand[[i]] <- fit
    if (verbose) cat("  - Candidate", i, ":", deparse(fml), " [", if (model_ok(fit)) "OK" else "unstable", "]\n")
  }
  
  ok_idx <- which(sapply(fit_rand, model_ok))
  if (length(ok_idx) == 0) stop("All random-effects models failed.")
  cur <- if (1 %in% ok_idx) 1 else ok_idx[1]
  for (i in ok_idx) {
    if (i == cur) next
    f_cur <- nchar(re_list[[cur]])
    f_i   <- nchar(re_list[[i]])
    if (f_i <= f_cur) next
    win <- compare_nested(fit_rand[[cur]], fit_rand[[i]])
    if (!is.null(win) && identical(win, fit_rand[[i]])) cur <- i
  }
  best_random <- fit_rand[[cur]]
  random_part <- re_list[[cur]]
  if (verbose) cat("\n[Stage A] Best random structure:\n ", random_part, "\n")
  
  # Stage B: fixed effects selection (ML)
  if (verbose) cat("\n[Stage B] Fixed effects selection (ML)\n")
  f_main <- as.formula(paste(dv, "~", fixed_str, "+", random_part))
  m_main <- fit_lmer_safe(f_main, data, REML = FALSE)
  if (!model_ok(m_main)) stop("Main effects model unstable.")
  m_best <- m_main; f_best <- f_main
  
  if (length(fixed_main) >= 2) {
    combs <- t(combn(fixed_main, 2))
    for (k in seq_len(nrow(combs))) {
      inter <- paste(combs[k, ], collapse = ":")
      f_try <- as.formula(paste(dv, "~", fixed_str, "+", inter, "+", random_part))
      m_try <- fit_lmer_safe(f_try, data, REML = FALSE)
      if (!model_ok(m_try)) next
      win <- compare_nested(m_best, m_try)
      if (!is.null(win) && identical(win, m_try)) {
        m_best <- m_try; f_best <- f_try
        if (verbose) cat("  + Keep interaction:", inter, "\n")
      }
    }
  }
  
  if (verbose) {
    cat("\n[RESULT] Best model:\n ", deparse(f_best), "\n")
    cat("AIC:", AIC(m_best), " BIC:", BIC(m_best), " logLik:", logLik(m_best), "\n\n")
  }
  return(m_best)
}




