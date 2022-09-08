## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "",
    prompt = TRUE
)

## -----------------------------------------------------------------------------
library("robsurvey", quietly = TRUE)

## -----------------------------------------------------------------------------
attach(workplace)

## -----------------------------------------------------------------------------
head(workplace, 3)

## ---- eval = FALSE------------------------------------------------------------
#  library("survey")

## ---- echo = FALSE------------------------------------------------------------
suppressPackageStartupMessages(library(survey))

## -----------------------------------------------------------------------------
dn <- svydesign(ids = ~ID, strata = ~strat, fpc = ~fpc, weights = ~weight, data = workplace)

## ---- echo = FALSE------------------------------------------------------------
dn

## ---- echo = FALSE, fig.height = 2, fig.align = "center"----------------------
layout(matrix(1:2, ncol = 2))
par(mar = c(4, 1, 1, 1))
svyboxplot(payroll~ 1, dn, all.outliers = TRUE, xlab = "payroll", horizontal = TRUE)
svyboxplot(log(payroll) ~ 1, dn, all.outliers = TRUE, xlab = "log(payroll)", horizontal = TRUE)

## ---- echo = FALSE, fig.height=4, fig.width=5---------------------------------
par(mar = c(5, 4, 1, 0))
plot(weights(dn), dn$variables$payroll, ylab = "payroll (log scale)", log = "y",
    panel.first = grid(col = "grey", lty = 2))

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

## ---- eval = FALSE, fig.align = "center"--------------------------------------
#  plot(residuals(m), robweights(m))

## ---- echo = FALSE, out.width="60%", fig.align = "center"---------------------
par(mar = c(5, 4, 1, 0))
plot(residuals(m), robweights(m))

