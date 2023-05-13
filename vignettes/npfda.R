## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache = FALSE, fig.height=5, fig.width=7, 
                      fig.align = 'center', out.width = '100%')
# knitr::spin("npsp_intro2.R", knit = FALSE)

## -----------------------------------------------------------------------------
library(npfda)

## -----------------------------------------------------------------------------
fd <- npf.data(ozone, dimnames = "day")
plot(fd, y = as.numeric(fd$ynames))
plot(fd, y = as.numeric(fd$ynames), 
     col = hcl.colors(32, palette = "Blue-Red 3", alpha = 0.6))

## -----------------------------------------------------------------------------
plot(fd, col = "lightgray", legend = FALSE)
x <- coords(fd)
y <- f.mean(fd)
lines(x, y)
matlines(x, y + sqrt(f.var(fd, mean = y)) %o% c(-1, 1), col = 1, lty = 2)

## -----------------------------------------------------------------------------
trend.h <- 36
lp <- locpol(fd, h = trend.h)
# Plot
plot(fd, col = "lightgray", legend = FALSE)
lines(lp$data$x, lp$biny, lty = 2) # x = coords(fd)
lines(lp$data$x, lp$est)

## -----------------------------------------------------------------------------
bin <- npf.binning(fd) # binning
lp <- locpol(bin, h = trend.h)

## -----------------------------------------------------------------------------
# bin <- npf.binning(fd)
trend.h.cv <- h.cv(bin)$h
trend.h.cv

## -----------------------------------------------------------------------------
# Linear Local trend estimates
lp.cv <- locpol(bin, h = trend.h.cv)
# Plot
with(lp.cv, {
  plot(data, col = "lightgray", legend = FALSE)
  lines(data$x, biny, lty = 2)
  lines(data$x, est)
})

## -----------------------------------------------------------------------------
var.h <- 33
lp.var <- np.var(lp, h = var.h)
# Plot data + estimated trend -+ estimated std. dev.
plot(lp$data, col = "lightgray", legend = FALSE)
x <- lp$data$x
y <- lp$est
lines(x, y)
matlines(x, y + sqrt(lp.var$est) %o% c(-1, 1), col = 1, lty = 2)

## -----------------------------------------------------------------------------
bin.res2 <- npf.bin.res2(lp)
var.h.cv <- h.cv(bin.res2)$h
var.h.cv

## -----------------------------------------------------------------------------
# Linear Local variance estimate
lp.cv.var <- np.var(lp, h = var.h.cv)
# Plot data + estimated trend -+ estimated std. dev.
plot(lp$data, col = "lightgray", legend = FALSE)
x <- lp$data$x
y <- lp$est
lines(x, y)
matlines(x, y + sqrt(lp.cv.var$est) %o% c(-1, 1), col = 1, lty = 2)

## -----------------------------------------------------------------------------
bin.svar <- svar.bin(lp, lp.var, maxlag = 100)
h.svar <- 1.5*h.cv(bin.svar)$h
h.svar
svar.np <- np.svar(bin.svar, h = h.svar)
# plot(svar.np)

## -----------------------------------------------------------------------------
svm <- fitsvar.sb.iso(svar.np, dk = 0) 
plot(svm)

## -----------------------------------------------------------------------------
# Estimated correlation matrix
corr.est <- varcov(svm, lp$data$x)
# Trend bandwidth selection (under heteroscedasticity and dependence)
trend.h.new <- h.cv(bin, lp.var, cor = corr.est)$h
trend.h.new

## -----------------------------------------------------------------------------
error.h.trend <- mean(abs(trend.h/trend.h.new - 1))
error.h.trend

## -----------------------------------------------------------------------------
# Variance bandwidth selection (under heteroscedasticity and dependence)
var.h.new <- h.cv(bin.res2, lp.var, cor = corr.est)$h
var.h.new
error.var.h <- mean(abs(var.h/var.h.new - 1))
error.var.h

## ----eval=FALSE---------------------------------------------------------------
#  np.fit <- npf.model(lp, lp.var, svm)

