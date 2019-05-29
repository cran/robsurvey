## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE-----------------------------------
## load packages
library(robsurvey)
library(survey)

# load the api dataset
data(api)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(apistrat[ ,c("sname","stype","api00","pw","fpc")]))

## ------------------------------------------------------------------------
# define survey design
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
                    data = apistrat, fpc = ~fpc)

## ------------------------------------------------------------------------
# compute the robust Horvitz-Thompson M-estimator of the mean
svymean_huber(~api00, dstrat, k = 2)

# compute the robust trimmed Horvitz-Thompson mean
svymean_trimmed(~api00, dstrat, k = 2)

# compute the robust winsorized Horvitz-Thompson mean
svymean_winsorized(~api00, dstrat, k = 2)

## ------------------------------------------------------------------------
# Domain estimates
svyby(~api00, by = ~stype, design = dstrat, svymean_huber, k = 2)

## ------------------------------------------------------------------------
# compute the robust HTE for the total
svytotal_huber(~api00, dstrat, k = 2)

# compute the robust trimmed total
svytotal_trimmed(~api00, dstrat, k = 2)

# compute the robust winsorized total
svytotal_winsorized(~api00, dstrat, k = 2)

## ------------------------------------------------------------------------
# bare-bone function to compute robust Horvitz-Thompson M-estimator of the mean
weighted_mean_huber(apistrat$api00, weights(dstrat), k = 2)

## ----plot1, fig.width=6, fig.height=5------------------------------------
# weighted resistant line
out <- weighted_line(apistrat$api00 ~ apistrat$api99, w = weights(dstrat))

# what are coefficients?
coef(out)

# plot data
plot(apistrat$api00 ~ apistrat$api99)
abline(out, col = "green")

## ----plot2, fig.width=6, fig.height=5------------------------------------
# weighted median line based on slopes
out_s <- weighted_median_line(apistrat$api00 ~ apistrat$api99, w = weights(dstrat))

# weighted median line based on products
out_p <- weighted_median_line(apistrat$api00 ~ apistrat$api99, w = weights(dstrat), type = "products")

# what are coefficients?
coef(out_s)
coef(out_p)

# plot data
plot(apistrat$api00 ~ apistrat$api99)
abline(out_s, col = "blue")
abline(out_p, col = "red")

## ------------------------------------------------------------------------
# compute weighted median for api00
weighted_median(apistrat$api00, weights(dstrat))

