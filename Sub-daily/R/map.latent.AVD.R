map.latent.AVD <- function (fitted, x, y, covariates = NULL, param = "quant", ret.per = 100, 
    col = terrain.colors(64), plot.contour = TRUE, fun = mean, 
    level = 0.95, show.data = TRUE, control = list(nlines = 500), 
    ...) 
{
    if (!(param %in% c("loc", "scale", "shape", "quant"))) 
        stop("'param' should be one of 'loc', 'scale', 'shape' or 'quant'")
    if (ncol(fitted$coord) > 2) 
        stop("It's not possible to use this function when the coordinate space has a dimension > 2")
    if (is.null(covariates) && !is.null(fitted$marg.cov)) 
        stop("Your model seems to make use of covariate but you supplied none")
    if (!is.null(covariates)) {
        if (missing(x) || missing(y)) 
            stop("if 'covariates' is supplied 'x' and 'y' must be too")
        if (!is.array(covariates)) 
            stop("'covariates' must be an array - see the example")
        covariates.names <- dimnames(covariates)[[3]]
        model.names <- colnames(fitted$marg.cov)
        if (is.null(model.names)) 
            stop("Your fitted model doesn't seem to make use of covariates")
        if (!all(model.names %in% covariates.names)) 
            stop("Some required covariates are missing. Please check")
        dim.cov <- dim(covariates)
        if ((dim.cov[1] != length(x)) || (dim.cov[2] != length(y))) 
            stop("'covariates' doesn't match with 'x' and 'y'")
    }
    else covariates.names <- NULL
    if (missing(x)) {
        x.range <- range(fitted$coord[, 1])
        x <- seq(x.range[1], x.range[2], length = 100)
    }
    if (missing(y)) {
        y.range <- range(fitted$coord[, 2])
        y <- seq(y.range[1], y.range[2], length = 100)
    }
    n.x <- length(x)
    n.y <- length(y)
    n.chain <- nrow(fitted$chain.loc)
    n.loccoeff <- ncol(fitted$loc.dsgn.mat)
    n.scalecoeff <- ncol(fitted$scale.dsgn.mat)
    n.shapecoeff <- ncol(fitted$shape.dsgn.mat)
    ans <- array(NA, c(n.x, n.y, n.chain))
    new.data <- matrix(NA, nrow = n.x * n.y, ncol = 2 + length(covariates.names))
    for (i in 1:n.x) new.data[(n.y * (i - 1) + 1):(n.y * i), 
        ] <- cbind(x[i], y, covariates[i, , ])
    colnames(new.data) <- c(colnames(fitted$coord), covariates.names)
    loc.dsgn.mat <- modeldef(new.data, fitted$loc.form)$dsgn.mat
    scale.dsgn.mat <- modeldef(new.data, fitted$scale.form)$dsgn.mat
    shape.dsgn.mat <- modeldef(new.data, fitted$shape.form)$dsgn.mat

    if (param == "loc") {
        for (i in 1:n.chain) {
            loc <- matrix(loc.dsgn.mat %*% fitted$chain.loc[i, 
                1:n.loccoeff], n.x, n.y, byrow = TRUE)
            res <- as.numeric(fitted$chain.loc[i, -(1:(n.loccoeff + 
                3))] - fitted$loc.dsgn.mat %*% fitted$chain.loc[i, 
                1:n.loccoeff])
            ans[, , i] <- loc + condrgp(1, cbind(x, y), fitted$coord, 
                res, cov.mod = fitted$cov.mod[1], sill = fitted$chain.loc[i, 
                  "sill"], range = fitted$chain.loc[i, "range"], 
                smooth = fitted$chain.loc[i, "smooth"], grid = TRUE, 
                control = control)$cond.sim
        }
    }
    else if (param == "scale") {
        for (i in 1:n.chain) {
            scale <- matrix(scale.dsgn.mat %*% fitted$chain.scale[i, 
                1:n.scalecoeff], n.x, n.y, byrow = TRUE)
            res <- as.numeric(fitted$chain.scale[i, -(1:(n.scalecoeff + 
                3))] - fitted$scale.dsgn.mat %*% fitted$chain.scale[i, 
                1:n.scalecoeff])
            n.trials <- 1
            flag <- FALSE
            while (!flag) {
                scale <- scale + condrgp(1, cbind(x, y), fitted$coord, 
                  res, cov.mod = fitted$cov.mod[2], sill = fitted$chain.scale[i, 
                    "sill"], range = fitted$chain.scale[i, "range"], 
                  smooth = fitted$chain.shape[i, "smooth"], grid = TRUE, 
                  control = control)$cond.sim
                flag <- all(scale > 0)
                n.trials <- n.trials + 1
                if (n.trials >= 100) {
                  ans[, , i] <- NA
                  break
                }
            }
            ans[, , i] <- scale
        }
    }
    else if (param == "shape") {
        for (i in 1:n.chain) {
            shape <- matrix(shape.dsgn.mat %*% fitted$chain.shape[i, 
                1:n.shapecoeff], n.x, n.y, byrow = TRUE)
            res <- as.numeric(fitted$chain.shape[i, -(1:(n.shapecoeff + 
                3))] - fitted$shape.dsgn.mat %*% fitted$chain.shape[i, 
                1:n.shapecoeff])
            ans[, , i] <- shape + condrgp(1, cbind(x, y), fitted$coord, 
                res, cov.mod = fitted$cov.mod[1], sill = fitted$chain.shape[i, 
                  "sill"], range = fitted$chain.shape[i, "range"], 
                smooth = fitted$chain.shape[i, "smooth"], grid = TRUE, 
                control = control)$cond.sim
        }
    }
    else {
	print(n.chain)
        for (i in 1:n.chain) {
            print(i)
            loc <- matrix(loc.dsgn.mat %*% fitted$chain.loc[i, 
                1:n.loccoeff], n.x, n.y, byrow = TRUE)
            res <- as.numeric(fitted$chain.loc[i, -(1:(n.loccoeff + 
                3))] - fitted$loc.dsgn.mat %*% fitted$chain.loc[i, 
                1:n.loccoeff])
            loc <- loc + condrgp(1, cbind(x, y), fitted$coord, 
                res, cov.mod = fitted$cov.mod[1], sill = fitted$chain.loc[i, 
                  "sill"], range = fitted$chain.loc[i, "range"], 
                smooth = fitted$chain.loc[i, "smooth"], grid = TRUE, 
                control = control)$cond.sim
            flag <- FALSE
            scale <- matrix(scale.dsgn.mat %*% fitted$chain.scale[i, 
                1:n.scalecoeff], n.x, n.y, byrow = TRUE)
            res <- as.numeric(fitted$chain.scale[i, -(1:(n.scalecoeff + 
                3))] - fitted$scale.dsgn.mat %*% fitted$chain.scale[i, 
                1:n.scalecoeff])
            n.trials <- 1
            while (!flag) {
                scale <- scale + condrgp(1, cbind(x, y), fitted$coord, 
                  res, cov.mod = fitted$cov.mod[2], sill = fitted$chain.scale[i, 
                    "sill"], range = fitted$chain.scale[i, "range"], 
                  smooth = fitted$chain.shape[i, "smooth"], grid = TRUE, 
                  control = control)$cond.sim
                flag <- all(scale > 0)
                n.trials <- n.trials + 1
                if (n.trials >= 100)   #This becomes = 100! Because not all scale are > 0. Use log(scale)
                  break
            }
            if (n.trials < 100) {
                shape <- matrix(shape.dsgn.mat %*% fitted$chain.shape[i, 
                  1:n.shapecoeff], n.x, n.y, byrow = TRUE)
                res <- as.numeric(fitted$chain.shape[i, -(1:(n.shapecoeff + 
                  3))] - fitted$shape.dsgn.mat %*% fitted$chain.shape[i, 
                  1:n.shapecoeff])
                shape <- shape + condrgp(1, cbind(x, y), fitted$coord, 
                  res, cov.mod = fitted$cov.mod[1], sill = fitted$chain.shape[i, 
                    "sill"], range = fitted$chain.shape[i, "range"], 
                  smooth = fitted$chain.shape[i, "smooth"], grid = TRUE, 
                  control = control)$cond.sim
                ans[, , i] <- .qgev(1 - 1/ret.per, loc, scale, 
                  shape)
            }
            else ans[, , i] <- NA
        }
    }
    prob.low <- (1 - level)/2
    prob.up <- 1 - prob.low
    post.sum <- ci.low <- ci.up <- matrix(NA, n.x, n.y)
    for (i in 1:n.x) {
        post.sum[i, ] <- apply(ans[i, , ], 1, fun, na.rm = TRUE)
        ci.low[i, ] <- apply(ans[i, , ], 1, quantile, prob.low, 
            na.rm = TRUE)
        ci.up[i, ] <- apply(ans[i, , ], 1, quantile, prob.up, 
            na.rm = TRUE)
    }
    op <- par(no.readonly = TRUE)
    layout(matrix(1:6, 1), widths = rep(c(1, 0.4), 3))
    title <- c("Low CI","Mean","High CI")
    for (i in 1:3) {
        par(mar = rep(0.5, 4))
        dummy <- switch(as.character(i), `1` = ci.low, `2` = post.sum, 
            `3` = ci.up)
        breaks <- seq(min(dummy), max(dummy), length = length(col) + 
            1)
        image(x, y, dummy, ..., col = col, breaks = breaks, xaxt = "n", 
            yaxt = "n",main=paste(title[i]))
        if (plot.contour) 
            contour(x, y, dummy, add = TRUE)
        if (show.data) 
            points(fitted$coord)
        mar <- c(0.5, 1, 0.5, 3)
        par(las = 1, pty = "m", mar = mar)
        plot.new()
        plot.window(xlim = c(0, 1), ylim = range(breaks), xaxs = "i", 
            yaxs = "i")
        rect(0, breaks[-length(breaks)], 1, breaks[-1], col = col, 
            border = NA)
        axis(4, at = pretty(dummy))
        box()
    }
    on.exit(par(op))
    invisible(list(x = x, y = y, post.sum = post.sum, ci.low = ci.low, 
        ci.up = ci.up))
}
