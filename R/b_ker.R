#' Exact kernel feature basis
#'
#' Generates a design matrix that exactly represents a provided kernel, so that
#' the Gram matrix is equal to the kernel matrix. The feature map is \deqn{
#'      \phi(x') = K_{x,x}^{-1/2} k_{x,x'},
#' } where \eqn{K_{x,x}} is the kernel matrix for the data points \eqn{x} and
#' \eqn{k_{x, x'}} is the vector of kernel function evaluations at the data
#' points and the new value.
#' While exact, this function is not particularly computationally efficient.
#' Both fitting and prediction require backsolving the Cholesky decomposition of
#' the kernel matrix for the original data points.
#'
#' @inheritParams b_rff
#' @param x The (training) data points at which to evaluate the kernel. If
#'   provided, overrides `...`.
#' @param L_inv The inverse of the Cholesky factor of the kernel matrix at the
#'   training points. Will be automatically computed if not provided, but in
#'   order to avoid recomputing it for new predictions, pass `L_inv = TRUE`,
#'   which will save and re-use this matrix for future calls.
#'
#' @returns A matrix of kernel features.
#'
#' @examples
#' data(quakes)
#'
#' # exact kernel ridge regression
#' k = k_rbf(0.1)
#' m = ridge(depth ~ b_ker(lat, long, kernel = k), quakes)
#' cor(fitted(m), quakes$depth)^2
#'
#' # Forecasting example involving combined kernels
#' data(AirPassengers)
#' x = seq(1949, 1961 - 1/12, 1/12)
#' y = as.numeric(AirPassengers)
#' x_pred = seq(1961 - 1/2, 1965, 1/12)
#'
#' k = k_per(scale = 0.2, period = 1) * k_rbf(scale = 4)
#' m = ridge(y ~ b_ker(x, kernel = k, stdize="none"))
#' plot(x, y, type='l', xlab="Year", ylab="Passengers (thousands)",
#'     xlim=c(1949, 1965), ylim=c(100, 800))
#' lines(x_pred, predict(m, newdata = list(x = x_pred)), lty="dashed")
#'
#' @export
b_ker <- function(..., kernel = k_rbf(),
                  stdize = c("scale", "box", "symbox", "none"),
                  x = NULL, shift = NULL, scale = NULL, L_inv = NULL) {
    y = as.matrix(cbind(...))
    std = do_std(y, stdize, shift, scale)
    y = std$x
    if (is.null(x)) x = y

    save = isTRUE(L_inv)
    if (is.null(L_inv) || save) {
        II = diag(nrow(x))
        K = kernel(x, x) + 1e-9 * II
        L_inv = backsolve(chol(K), II)
    }
    m = kernel(y, x) %*% L_inv

    attr(m, "x") = x
    attr(m, "shift") = std$shift
    attr(m, "scale") = std$scale
    if (save) {
        attr(m, "L_inv") = L_inv
    }
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_ker", "matrix", "array")

    m
}

#' @export
predict.b_ker <- function (object, newdata, ...)  {
    if (missing(newdata)) {
        return(object)
    }
    rlang::eval_tidy(makepredictcall(object, attr(object, "call")), newdata)
}

#' @export
makepredictcall.b_ker <- function(var, call) {
    if (as.character(call)[1L] == "b_ker" ||
        (is.call(call) && identical(eval(call[[1L]]), b_ker))) {
        at = attributes(var)[c("x", "shift",  "scale", "L_inv")]
        call[names(at)] = at
    }
    call
}
