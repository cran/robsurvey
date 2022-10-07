## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "",
    prompt = TRUE
)

## -----------------------------------------------------------------------------
library("robsurvey", quietly = TRUE)
data("losdata")
attach(losdata)

## -----------------------------------------------------------------------------
head(losdata, 3)

## ---- eval = FALSE------------------------------------------------------------
#  library("survey")

## ---- eval = FALSE------------------------------------------------------------
#  dn <- svydesign(ids = ~1, fpc = ~fpc, weights = ~weight, data = losdata)

## ---- echo = FALSE------------------------------------------------------------
suppressPackageStartupMessages(library(survey))
dn <- svydesign(ids = ~1, fpc = ~fpc, weights = ~weight, data = losdata)

## ---- echo = FALSE, fig.height = 2, fig.align = "center"----------------------
layout(matrix(1:2, ncol = 2))
par(mar = c(4, 1, 1, 1))
svyboxplot(los ~ 1, dn, all.outliers = TRUE, xlab = "los", horizontal = TRUE)
svyboxplot(log(los) ~ 1, dn, all.outliers = TRUE, xlab = "log(los)", horizontal = TRUE)

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

## ---- echo = FALSE, fig.height = 3, fig.width = 5-----------------------------
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

## ---- eval = FALSE, fig.align = "center"--------------------------------------
#  plot(residuals(m), robweights(m))

## ---- echo = FALSE, fig.height = 3, fig.width = 5, fig.align = "center"-------
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

