#out: gpd(data,threshold). data=daily rainfall. 
library(evir)

rlevel.gpd <- function (out, threshold,k.blocks = 20, add = FALSE, ...) 
{
    par.ests <- out$par.ests
    mu <- threshold
    beta <- par.ests["beta"]
    xi <- par.ests["xi"]
    pp <- 1/k.blocks
    v <- qgpd((1 - pp), xi, mu, beta)
#    return(as.numeric(v))
    if (add) 
        abline(h = v)
    data <- out$data
    overallmax <- out$nllh.final
    sigma0 <- sqrt(6 * var(data))/pi
    xi0 <- 0.01
    theta <- c(xi0, sigma0)
    parloglik <- function(theta, tmp, pp, rli) {
        mu <- rli + (theta[2] * (1 - (-logb(1 - pp))^(-theta[1])))/theta[1]
        y <- 1 + (theta[1] * (tmp - mu))/theta[2]
        if ((theta[2] < 0) | (min(y) < 0)) 
            out <- 1e+06
        else {
            term1 <- length(tmp) * logb(theta[2])
            term2 <- sum((1 + 1/theta[1]) * logb(y))
            term3 <- sum(y^(-1/theta[1]))
            out <- term1 + term2 + term3
        }
        out
    }
    parmax <- NULL
    rl <- v * c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 1, 1.1, 
        1.2, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 
        4.5)
    for (i in 1:length(rl)) {
        fit <- optim(theta, parloglik, hessian = FALSE, tmp = data, 
            pp = pp, rli = rl[i])
        parmax <- rbind(parmax, fit$value)
    }
    parmax <- -parmax
    overallmax <- -overallmax
    crit <- overallmax - qchisq(0.9999, 1)/2
    cond <- parmax > crit
    rl <- rl[cond]
    parmax <- parmax[cond]
    smth <- spline(rl, parmax, n = 200)
    aalpha <- qchisq(0.95, 1)
    if (!add) {
        plot(rl, parmax, type = "p", ...)
        abline(h = overallmax - aalpha/2)
        abline(v = v)
        lines(smth)
    }
    ind <- smth$y > overallmax - aalpha/2
    ci <- range(smth$x[ind])
    if (add) {
        abline(h = ci[1], lty = 2, col = 2)
        abline(h = ci[2], lty = 2, col = 2)
    }
    as.numeric(c(ci[1], v, ci[2]))
}

