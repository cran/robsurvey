# weighted trimmed mean (depends on pkg survey)
svymean_trimmed <- function(x, design, LB = 0.05, UB = 1 - LB, na.rm = FALSE)
{
    if (!is.language(x))
        stop("Argument 'x' must be a formula object\n", call. = FALSE)
    dat <- .checkformula(x, design, na.rm)
    # in the presence of NA's
    if (dat$failure)
        return(structure(list(characteristic = "mean",
            estimator = list(string = paste0("Weighted trimmed estimator (",
                LB, ", ", UB, ")"), LB = LB, UB = UB),
            estimate = stats::setNames(NA, dat$yname), variance = NA,
            residuals = NA, model = NA, design = design, call = match.call()),
            class = "svystat_rob"))
    # otherwise
    design <- dat$design
    res <- weighted_mean_trimmed(dat$y, dat$w, LB, UB, TRUE, FALSE)
    # influence function
    infl <- .infl_trimmed(dat$y, dat$w, LB, UB, res$estimate)
    # variance
    infl <- infl * dat$w / sum(dat$w)
    res$variance <- survey::svyrecvar(infl, design$cluster, design$strata,
        design$fpc, postStrata = design$postStrata)
    names(res$estimate) <- dat$yname
    res$call <- match.call()
    res$design <- design
    class(res) <- "svystat_rob"
    res
}
# weighted trimmed total (depends on pkg survey)
svytotal_trimmed <- function(x, design, LB = 0.05, UB = 1 - LB, na.rm = FALSE)
{
    res <- svymean_trimmed(x, design, LB, UB, na.rm)
    res$call <- match.call()
    if (is.na(res$estimate)) {
        res$characteristic <- "total"
        return(res)
    }
    sum_w <- sum(res$model$w)
    res$estimate <- res$estimate * sum_w
    res$variance <- res$variance * sum_w^2
    res$characteristic <- "total"
    res
}
# influence function, Huber (1981, p. 58)
.infl_trimmed <- function(x, w, LB, UB, tm)
{
    qs <- weighted_quantile(x, w, probs = c(LB, UB))
    x_wins <- pmin.int(qs[2], pmax.int(qs[1], x))
    # functional W corresponding to winsorized mean
    W <- (UB - LB) * tm + LB * qs[1] + (1 - UB) * qs[2]
    # return
    (x_wins - W) / (UB - LB)
}