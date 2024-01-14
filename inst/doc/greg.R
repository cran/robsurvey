## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "",
    prompt = TRUE,
    dpi = 36,
    fig.align = "center"
)

## ----eval=FALSE---------------------------------------------------------------
#  library("robsurvey", quietly = TRUE)
#  library("survey")
#  data("MU284pps")

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

## ----eval = FALSE-------------------------------------------------------------
#  dn <- svydesign(ids = ~LABEL, fpc = ~pi, data = MU284pps, pps = "brewer",
#                  calibrate = ~1)

## ----echo = FALSE-------------------------------------------------------------
dn <- if (packageVersion("survey") >= "4.2") {
        svydesign(ids = ~LABEL, fpc = ~pi, data = MU284pps, pps = "brewer",
                  calibrate = ~1)
    } else {
        svydesign(ids = ~LABEL, fpc = ~pi, data = MU284pps, pps = "brewer")
    }

## ----out.width = "80%"--------------------------------------------------------
svyplot(RMT85 ~ P85, dn, xlab = "P85", ylab = "RMT85", inches = 0.1)

## -----------------------------------------------------------------------------
rat <- svyratio_huber(~RMT85, ~P85, dn, k = Inf)
rat

## -----------------------------------------------------------------------------
tot <- svytotal_ratio(rat, total = 8339)
tot

## -----------------------------------------------------------------------------
mse(tot) / 1e6

## -----------------------------------------------------------------------------
rat_rob <- svyratio_huber(~RMT85, ~P85, dn, k = 20)
tot_rob <- svytotal_ratio(rat_rob, total = 8339)
tot_rob

## -----------------------------------------------------------------------------
mse(tot_rob) / 1e6

## -----------------------------------------------------------------------------
wls <- svyreg(RMT85 ~ P85 + SS82, dn)
wls

## -----------------------------------------------------------------------------
summary(wls)

## ----out.width = "50%", fig.align = "default"---------------------------------
plot(wls)

## -----------------------------------------------------------------------------
tot <- svytotal_reg(wls, totals = c(P85 = 8339, SS82 = 6301), N = 284,
                    type = "ADU")
tot

## -----------------------------------------------------------------------------
mse(tot) / 1e6

## -----------------------------------------------------------------------------
rob <- svyreg_tukeyM(RMT85 ~ P85 + SS82, dn, k = 15)
rob

## ----out.width = "50%", fig.align = "default"---------------------------------
plot(rob)

## -----------------------------------------------------------------------------
tot <- svytotal_reg(rob, totals = c(P85 = 8339, SS82 = 6301), N = 284,
                    type = "huber", k = 50)
tot

## -----------------------------------------------------------------------------
mse(tot) / 1e6

