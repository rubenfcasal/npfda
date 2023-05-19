## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache = FALSE, fig.height=5, fig.width=7, 
                      fig.align = 'center', out.width = '100%')
# knitr::spin("npsp_intro2.R", knit = FALSE)

## -----------------------------------------------------------------------------
library(npfda)

## -----------------------------------------------------------------------------
fd <- npf.data(ozone, dimnames = "day")
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
plot(lp, lp.var)

## -----------------------------------------------------------------------------
bin.res2 <- npf.bin.res2(lp)
var.h.cv <- h.cv(bin.res2)$h
var.h.cv

## -----------------------------------------------------------------------------
# Linear Local variance estimate
lp.cv.var <- np.var(lp, h = var.h.cv)
# Plot data + estimated trend -+ estimated std. dev.
plot(lp, lp.cv.var)

## -----------------------------------------------------------------------------
bin.svar <- svar.bin(lp, lp.var, maxlag = 100)
svar.h <- 1.5*h.cv(bin.svar)$h
svar.h
svar.np <- np.svar(bin.svar, h = svar.h)
# plot(svar.np)

## -----------------------------------------------------------------------------
svm <- fitsvar.sb.iso(svar.np, dk = 0) 
plot(svm)

## -----------------------------------------------------------------------------
# Estimated correlation matrix
cor.est <- varcov(svm, lp$data$x)
# Trend bandwidth selection (under heteroscedasticity and dependence)
trend.h.new <- h.cv(bin, lp.var, cor = cor.est)$h
trend.h.new

## -----------------------------------------------------------------------------
error.trend.h <- abs(trend.h/trend.h.new - 1)
error.trend.h

## -----------------------------------------------------------------------------
# Variance bandwidth selection (under heteroscedasticity and dependence)
var.h.new <- h.cv(bin.res2, lp.var, cor = cor.est)$h
var.h.new
error.var.h <- abs(var.h/var.h.new - 1)
error.var.h

## -----------------------------------------------------------------------------
np.fit <- npf.model(lp, lp.var, svm)

## -----------------------------------------------------------------------------
np.fit2 <- npf.fit(fd, var.h = 30, maxlag = 100, verbose = TRUE)
plot(np.fit2)

