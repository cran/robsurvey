# Huber M-estimator of the weighted mean (depends on pkg survey)
svymean_huber <- function(x, design, k, type = "rhj", asym = FALSE,
    na.rm = FALSE, verbose = TRUE, ...)
{
    dat <- .checkformula(x, design, na.rm)
    # in the presence of NA's
    if (dat$failure)
        return(structure(list(characteristic = "mean",
            estimator = list(string = paste0("Huber M-estimator (type = ",
                type, ifelse(asym, "; asym. psi", ""), ")"), type = type,
                psi = ifelse(asym, 1, 0), psi_fun = "Huber", k = k),
            estimate = stats::setNames(NA, dat$yname), variance = NA,
            residuals = NA, model = NA, design = design, call = match.call()),
            class = "svystat_rob"))
    # otherwise
    design <- dat$design
    res <- weighted_mean_huber(dat$y, dat$w, k, type, asym, TRUE, FALSE,
        verbose, ...)
    # modify residuals for type 'rht' (only for variance estimation)
    r <- if (type == "rht")
        sqrt(res$model$var) * res$model$y - res$estimate
    else
        res$residuals
   # compute variance
    infl <- res$robust$robweights * r * dat$w / sum(dat$w)
    res$variance <- survey::svyrecvar(infl, design$cluster, design$strata,
        design$fpc, postStrata = design$postStrata)
    names(res$estimate) <- dat$yname
    res$call <- match.call()
    res$design <- design
    class(res) <- c("svystat_rob", "mer_capable")
    res
}
# Huber M-estimator of the weighted total (depends on pkg survey)
svytotal_huber <- function(x, design, k, type = "rhj", asym = FALSE,
    na.rm = FALSE, verbose = TRUE, ...)
{
    dat <- .checkformula(x, design, na.rm)
    # in the presence of NA's
    if (dat$failure)
        return(structure(list(characteristic = "total",
            estimator = list(string = paste0("Huber M-estimator (type = ",
                type, ifelse(asym, "; asym. psi", ""), ")"), type = type,
                psi = ifelse(asym, 1, 0), psi_fun = "Huber", k = k),
            estimate = stats::setNames(NA, dat$yname), variance = NA,
            residuals = NA, model = NA, design = design, call = match.call()),
            class = "svystat_rob"))
    # otherwise
    design <- dat$design
    res <- weighted_total_huber(dat$y, dat$w, k, type, asym, TRUE, FALSE,
        verbose, ...)
    # compute variance
    infl <- res$robust$robweights * dat$y * dat$w
    res$variance <- survey::svyrecvar(infl, design$cluster, design$strata,
        design$fpc, postStrata = design$postStrata)
    names(res$estimate) <- dat$yname
    res$call <- match.call()
    res$design <- design
    class(res) <- c("svystat_rob", "mer_capable")
    res
}