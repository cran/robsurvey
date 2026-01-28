## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "",
    prompt = TRUE,
    dpi = 36,
    fig.align = "center"
)

## ----eval=FALSE---------------------------------------------------------------
# library("robsurvey", quietly = TRUE)
# library("survey")

## ----echo = FALSE-------------------------------------------------------------
library(robsurvey, quietly = TRUE)
suppressPackageStartupMessages(library(survey))

## ----echo = FALSE, results = "asis"-------------------------------------------
survey_version <- packageVersion("survey")
if (survey_version < "4.2") {
cat(paste0('<div class="my-sidebar-orange">\n
<p style="color: #ce5b00;">
**IMPORTANT: PRE-CALIBRATED WEIGHTS ARE NOT SUPPORTED**
</p>
This vignette has been built with version **', survey_version,
'** of the **survey** package. Therefore, `svydesign()` is called without
the `calibrate.formula` argument. As a consequence, some of the variance and
standard error estimates may differ from those with pre-calibrated weights,
i.e., the default specification.<p>
</p>
</div>'))
}

## -----------------------------------------------------------------------------
data(counties)
head(counties, 3)

## ----eval = FALSE-------------------------------------------------------------
# dn <- svydesign(ids = ~1, fpc = ~fpc, weights = ~weights,
#                 data = counties[counties$farmpop > 0, ],
#                 calibrate.formula = ~1)

## ----echo = FALSE-------------------------------------------------------------
dn <- if (packageVersion("survey") >= "4.2") {
        svydesign(ids = ~1, fpc = ~fpc, weights = ~weights,
                  data = counties[counties$farmpop > 0, ],
                  calibrate.formula = ~1)
    } else {
        svydesign(ids = ~1, fpc = ~fpc, weights = ~weights,
                  data = counties[counties$farmpop > 0, ])
    }

## ----echo = FALSE, fig.show = "hold", out.width = "50%", fig.align = "default"----
plot(farmpop ~ numfarm, dn$variables, xlab = "numfarm", ylab = "farmpop",
     cex.axis = 1.2, cex.lab = 1.4)
plot(farmpop ~ numfarm, dn$variables, xlab = "numfarm (log)",
     ylab = "farmpop (log)", log = "xy", cex.axis = 1.2, cex.lab = 1.4)
points(farmpop ~ numfarm, dn$variables[3, ], pch = 19, col = 2)

## -----------------------------------------------------------------------------
m <- svyreg(farmpop ~ numfarm, dn, na.rm = TRUE)
m

## -----------------------------------------------------------------------------
summary(m)

## ----echo = FALSE, out.width = "50%"------------------------------------------
plot(m, which = 1)

## -----------------------------------------------------------------------------
dn <- update(dn, vi = sqrt(numfarm))

## -----------------------------------------------------------------------------
svyreg(farmpop ~ -1 + numfarm, dn, var = ~vi, na.rm = TRUE)

## -----------------------------------------------------------------------------
m <- svyreg_huberM(farmpop ~ -1 + numfarm, dn, var = ~vi, k = 1.3,
                   na.rm = TRUE)
m


## -----------------------------------------------------------------------------
summary(m)

## ----echo = FALSE, out.width = "50%"------------------------------------------
plot(residuals(m), robweights(m), cex.axis = 1.2, cex.lab = 1.4)

## -----------------------------------------------------------------------------
dn <- svydesign(ids = ~1, fpc = ~fpc, weights = ~weights,
                data = counties[counties$farmpop > 0, ])
dn_exclude <- na.exclude(dn)

## -----------------------------------------------------------------------------
f <- log(farmpop) ~ log(numfarm) + log(totpop) + log(farmacre)

## -----------------------------------------------------------------------------
xmat <- model.matrix(f, dn_exclude$variables)[, -1]

## ----echo = FALSE, out.width = "50%"------------------------------------------
pairs(xmat)

## -----------------------------------------------------------------------------
if (requireNamespace("wbacon", quietly = TRUE)) {
    # package wbacon is available
    mv <- wbacon::wBACON(xmat, weights = weights(dn_exclude))
    # distances
    dis <- wbacon::distance(mv)
} else {
    # package wbacon is not available
    center <- c(6.285968, 10.195002, 12.047715)
    scatter <- matrix(0, 3, 3)
    scatter[lower.tri(scatter, TRUE)] <- c(0.678646, 0.441020, 0.415634,
                                           2.191174, -0.302097, 1.040932)
    scatter <- scatter + t(scatter) - diag(3) * scatter
    # distances
    dis <- sqrt(mahalanobis(xmat, center, scatter))
}

## ----echo = FALSE, out.width = "50%", fig.asp = 0.5---------------------------
boxplot(dis, horizontal = TRUE, xlab = "dis")

## ----echo = FALSE, fig.show = "hold", out.width = "33%", fig.align = "default"----
x <- seq(0, 6, length = 500)
cex_axis <- 1.5; cex_lab <- 1.5; cex_main <- 1.8
plot(x, huberWgt(x, k = 2), type = "l", xlab = "x", ylab = "",
     cex.axis = cex_axis, cex.lab = cex_lab, main = "huberWgt(x, k = 2)",
     cex.main = cex_main, ylim = c(0, 1))
plot(x, tukeyWgt(x, k = 4), type = "l", xlab = "x", ylab = "",
     cex.axis = cex_axis, cex.lab = cex_lab, main = "tukeyWgt(x, k = 4)",
     cex.main = cex_main, ylim = c(0, 1))
plot(x, simpsonWgt(x, Inf, 4), type = "l", xlab = "x", ylab = "",
     cex.axis = cex_axis, cex.lab = cex_lab,
     main = "simpsonWgt(x, a = Inf, b = 4)", cex.main = cex_main,
     ylim = c(0, 1))

## -----------------------------------------------------------------------------
m <- svyreg_tukeyGM(f, dn_exclude, k = 4.6, xwgt = tukeyWgt(dis))
summary(m)

## -----------------------------------------------------------------------------
dn0 <- svydesign(ids = ~1, weights = ~1,
                 data = counties[counties$farmpop > 0, ],
                 calibrate.formula = ~1)

## -----------------------------------------------------------------------------
m <- svyreg_huberM(log(farmpop) ~ log(numfarm), dn0, k = 1.3, na.rm = TRUE)

## -----------------------------------------------------------------------------
summary(m, mode = "model")

## ----echo=FALSE---------------------------------------------------------------
if (requireNamespace("MASS", quietly = TRUE)) {
    summary(MASS::rlm(log(farmpop) ~ log(numfarm),
                      data = counties[counties$farmpop > 0, ],
                      k = 1.3, na.action = na.omit))
} else {
    cat("Package MASS is not available\n")
}

## -----------------------------------------------------------------------------
data(MU284strat)
dn <- svydesign(ids = ~1, strata = ~ Stratum, fpc = ~fpc, weights = ~weights,
                data = MU284strat, calibrate.formula = ~-1 + Stratum)

## -----------------------------------------------------------------------------
m <- svyreg_huberM(RMT85 ~ REV84 + P85 + S82 + CS82, dn, k = 2)

## -----------------------------------------------------------------------------
summary(m, mode = "compound")

