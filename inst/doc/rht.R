## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "",
    prompt = TRUE,
    dpi = 36,
    fig.align = "center"
)

## -----------------------------------------------------------------------------
library("robsurvey", quietly = TRUE)

## -----------------------------------------------------------------------------
attach(workplace)

## -----------------------------------------------------------------------------
head(workplace, 3)

## ----eval = FALSE-------------------------------------------------------------
#  library("survey")

## ----echo = FALSE-------------------------------------------------------------
suppressPackageStartupMessages(library(survey))

## ----eval = FALSE-------------------------------------------------------------
#  dn <- svydesign(ids = ~ID, strata = ~strat, fpc = ~fpc, weights = ~weight,
#                  data = workplace, calibrate.formula = ~-1 + strat)

## ----echo = FALSE-------------------------------------------------------------
dn <- if (packageVersion("survey") >= "4.2") {
        svydesign(ids = ~ID, strata = ~strat, fpc = ~fpc, weights = ~weight,
                  data = workplace, calibrate.formula = ~-1 + strat)
    } else {
        svydesign(ids = ~ID, strata = ~strat, fpc = ~fpc, weights = ~weight,
                  data = workplace)
    }

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

## ----echo = FALSE, fig.show = "hold", out.width = "50%", fig.asp = 0.5--------
layout(matrix(1:2, ncol = 2))
par(mar = c(4, 1, 1, 1))
svyboxplot(payroll~ 1, dn, all.outliers = TRUE, xlab = "payroll",
           horizontal = TRUE)
svyboxplot(log(payroll) ~ 1, dn, all.outliers = TRUE, xlab = "log(payroll)",
           horizontal = TRUE)

## ----echo = FALSE, out.width = "50%"------------------------------------------
par(mar = c(5, 4, 1, 0))
plot(weights(dn), dn$variables$payroll, ylab = "payroll (log scale)",
     log = "y", panel.first = grid(col = "grey", lty = 2))

## -----------------------------------------------------------------------------
weighted_total_huber(payroll, weight, k = 8, type = "rht")

## -----------------------------------------------------------------------------
m <- svytotal_huber(~payroll, dn, k = 8, type = "rht")
m

## -----------------------------------------------------------------------------
summary(m)

## -----------------------------------------------------------------------------
coef(m)
vcov(m)
SE(m)

## -----------------------------------------------------------------------------
scale(m)

## ----eval = FALSE-------------------------------------------------------------
#  plot(residuals(m), robweights(m))

## ----echo = FALSE, out.width = "50%"------------------------------------------
par(mar = c(5, 4, 1, 0))
plot(residuals(m), robweights(m))

