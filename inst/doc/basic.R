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
data("losdata")
attach(losdata)

## -----------------------------------------------------------------------------
head(losdata, 3)

## ----eval = FALSE-------------------------------------------------------------
#  library("survey")

## ----echo = FALSE-------------------------------------------------------------
suppressPackageStartupMessages(library(survey))

## ----eval = FALSE-------------------------------------------------------------
#  dn <- svydesign(ids = ~1, fpc = ~fpc, weights = ~weight, data = losdata,
#                  calibrate.formula = ~1)

## ----echo = FALSE-------------------------------------------------------------
dn <- if (packageVersion("survey") >= "4.2") {
        svydesign(ids = ~1, fpc = ~fpc, weights = ~weight, data = losdata,
                  calibrate.formula = ~1)
    } else {
        svydesign(ids = ~1, fpc = ~fpc, weights = ~weight, data = losdata)
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

## ----echo = FALSE, out.width = "50%", fig.asp = 0.3---------------------------
layout(matrix(1:2, ncol = 2))
par(mar = c(4, 1, 1, 1))
svyboxplot(los ~ 1, dn, all.outliers = TRUE, xlab = "los", horizontal = TRUE)
svyboxplot(log(los) ~ 1, dn, all.outliers = TRUE, xlab = "log(los)",
           horizontal = TRUE)

## -----------------------------------------------------------------------------
weighted_mean_trimmed(los, weight, LB = 0, UB = 0.95)

## -----------------------------------------------------------------------------
m <- weighted_mean_trimmed(los, weight, LB = 0, UB = 0.95, info = TRUE)
names(m)

## -----------------------------------------------------------------------------
m <- svymean_trimmed(~los, dn, LB = 0, UB = 0.95)
m

## -----------------------------------------------------------------------------
coef(m)
vcov(m)
SE(m)

## -----------------------------------------------------------------------------
summary(m)

## -----------------------------------------------------------------------------
weighted_mean_winsorized(los, weight, LB = 0, UB = 0.95)

## -----------------------------------------------------------------------------
weighted_mean_k_winsorized(los, weight, k = 1)

## ----echo = FALSE, out.width = "50%"------------------------------------------
par(mar = c(4, 4, 1, 0))
plot(los, weight * los, xlab = "los", ylab = "weight * los")
abline(h = 1500, lty = 3)

## -----------------------------------------------------------------------------
weighted_mean_dalen(los, weight, censoring = 1500)

## -----------------------------------------------------------------------------
weighted_mean_huber(los, weight, type = "rwm", k = 8)

## -----------------------------------------------------------------------------
huber2(los, weight, k = 8)

## -----------------------------------------------------------------------------
m <- svymean_huber(~los, dn, type = "rwm", k = 8)
m

## -----------------------------------------------------------------------------
summary(m)

## ----eval = FALSE-------------------------------------------------------------
#  plot(residuals(m), robweights(m))

## ----echo = FALSE, out.width = "50%"------------------------------------------
par(mar = c(4, 4, 1, 0))
plot(residuals(m), robweights(m))

## -----------------------------------------------------------------------------
m <- svymean_huber(~los, dn, type = "rwm", k = 8)

## -----------------------------------------------------------------------------
mer(m)

## -----------------------------------------------------------------------------
weighted_quantile(los, weight, probs = c(0.1, 0.9))
weighted_median(los, weight)

## -----------------------------------------------------------------------------
weighted_mad(los, weight)

## -----------------------------------------------------------------------------
weighted_IQR(los, weight)

