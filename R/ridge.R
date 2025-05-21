#' Ridge regression
#'
#' Lightweight routine for ridge regression, fitted via a singular value
#' decomposition. The penalty may be automatically determined via leave-one-out
#' cross validation.
#'
#' @param formula A model formula; see [formula].
#' @param data An optional data frame or object in which to interpret the
#'   variables occurring in formula.
#' @param penalty The ridge penalty. Must be a single numeric or the string
#'   "auto", in which case the penalty will be determined via leave-one-out
#'   cross validation to minimize the mean squared error.
#' @param ... Further arguments, passed on to [model.frame()] and
#'   [model.matrix()]. These must be provided to [predict.ridge()] as well,
#'   if used.
#'
#' @returns An object of class `ridge` with components including:
#'   - `coef`, a vector of coefficients.
#'   - `fitted`, a vector of fitted values.
#'   - `penalty`, the penalty value.
#'
#' @examples
#' m_lm = lm(mpg ~ ., mtcars)
#' m_ridge = ridge(mpg ~ ., mtcars, penalty=1e3)
#' plot(fitted(m_lm), fitted(m_ridge), ylim=c(10, 30))
#' abline(a=0, b=1, col="red")
#'
#' @export
ridge <- function(formula, data, penalty = "auto", ...) {
    m = if (missing(data)) {
        model.frame(formula, ...)
    } else {
        model.frame(formula, data, ...)
    }
    tt = attr(m, "terms")
    attr(tt, "intercept") = 0
    y = model.response(m)
    mean_y = mean(y)
    y = y - mean_y
    X = model.matrix(tt, m, ...)

    n = nrow(X)
    udv = svd(X)
    uy = crossprod(udv$u, y)

    if (penalty == "auto") {
        loo_mse = function(lpen) {
            d_pen_f = udv$d^2 / (udv$d^2 + 10^lpen)
            hat1m = 1 - rowSums(udv$u^2 * rep(d_pen_f, each=n))
            resid = y - udv$u %*% (d_pen_f * uy)
            mean((resid / hat1m)^2)
        }
        penalty = 10^(optimize(loo_mse, c(-8, 8), tol=0.01)$minimum)
    }
    if (!is.numeric(penalty) && length(penalty) == 1) {
        abort('`penalty` must be a single numeric value, or the string "auto"')
    }

    d_pen_c = udv$d / (udv$d^2 + penalty)
    out = list(
        mean_y = mean_y,
        coef = c(udv$v %*% (d_pen_c * uy)),
        fitted = mean_y + c(udv$u %*% (d_pen_c * udv$d * uy)),
        penalty = penalty,
        terms = tt
    )
    names(out$coef) = colnames(X)
    names(out$fitted) = rownames(X)
    class(out) = "ridge"

    out
}

#' @export
print.ridge <- function(x, ...) {
    cat("Ridge regression model\n")
    cat("  Predictors: ", length(x$coef), "\n")
    cat("  Penalty:    ", x$penalty, "\n")
    invisible(x)
}

#' @describeIn ridge Fitted values
#' @param object A fitted [ridge()] model.
#' @export
fitted.ridge <- function(object, ...) {
    x$fitted
}

#' @describeIn ridge Coefficients
#' @export
coef.ridge <- function(object, ...) {
    x$coef
}

#' @describeIn ridge Predicted values
#' @param newdata A data frame containing the new data to predict
#' @export
predict.ridge <- function(object, newdata, ...) {
    tt = delete.response(terms(x))
    m = if (missing(newdata)) {
        model.frame(tt, ...)
    } else {
        model.frame(tt, newdata, ...)
    }
    if (!is.null(classes <- attr(tt, "dataClasses"))) {
        .checkMFClasses(classes, m)
    }
    X = model.matrix(tt, m, ...)

    out = c(X %*% x$coef + x$mean_y)
    names(out) = rownames(X)
    out
}
