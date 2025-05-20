#' Kernel functions
#'
#' These functions return vectorized kernel functions that can be used to
#' calculate kernel matrices, or provided directly to other basis functions.
#' These functions are designed to take a maximum value of one when identical
#' inputs are provided.
#'
#' @param scale The kernel length scale.
#'
#' @returns A function which calculates a kernel matrix for vector arguments `x`
#'   and `y`.
#'
#' @name kernels
NULL

#' @describeIn kernels Radial basis function kernel
#' @export
k_rbf = function(scale = 1) {
    if (scale <= 0) abort("`scale` must be positive")
    fn = function(x, y) {
        exp(-0.5 * dist_l2(x, y) / scale^2)
    }
    attr(fn, "name") = "rbf"
    attr(fn, "scale") = scale
    fn
}

#' @describeIn kernels Laplace kernel
#' @export
k_lapl = function(scale = 1) {
    if (scale <= 0) abort("`scale` must be positive")
    fn = function(x, y) {
        exp(-dist_l1(x, y) / scale)
    }
    attr(fn, "name") = "lapl"
    attr(fn, "scale") = scale
    fn
}

#' @describeIn kernels Rational quadratic kernel. \eqn{alpha=1} is the Cauchy kernel.
#' @export
k_rq = function(scale = 1, alpha = 2) {
    if (scale <= 0) abort("`scale` must be positive")
    if (alpha <= 0) abort("`alpha` must be positive")
    fn = function(x, y) {
        (1 + dist_l2(x, y) / (2 * alpha * scale^2))^(-alpha)
    }
    attr(fn, "name") = if (alpha == 1) "cauchy" else "rq"
    attr(fn, "scale") = scale
    attr(fn, "alpha") = alpha
    fn
}

#' @describeIn kernels Matern kernel. \eqn{nu=0.5} is the Ornstein–Uhlenbeck kernel.
#' @export
k_matern = function(scale = 1, nu = 1.5) {
    if (scale <= 0) abort("`scale` must be positive")
    if (nu <= 0) abort("`nu` must be positive")

    if (nu == 0.5) {
        fn = function(x, y) {
            exp(-sqrt(dist_l2(x, y)) / scale)
        }
    } else if (nu == 1.5) {
        fn = function(x, y) {
            d = sqrt(3 * dist_l2(x, y)) / scale
            (1 + d) * exp(-d)
        }
    } else if (nu == 2.5) {
        fn = function(x, y) {
            d = sqrt(5 * dist_l2(x, y)) / scale
            (1 + d + d^2/3) * exp(-d)
        }
    } else {
        fn = function(x, y) {
            d = sqrt(2 * nu * dist_l2(x, y)) / scale
            k = (2^(1 - nu) / gamma(nu)) * d^nu * besselK(d, nu)
            k[d < .Machine$double.eps] = 1 # besselK -> Inf at d=0
            k
        }
    }
    attr(fn, "name") = "matern"
    attr(fn, "scale") = scale
    attr(fn, "nu") = nu
    fn
}
