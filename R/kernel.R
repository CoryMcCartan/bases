#' Kernel functions
#'
#' These functions return vectorized kernel functions that can be used to
#' calculate kernel matrices, or provided directly to other basis functions.
#' These functions are designed to take a maximum value of one when identical
#' inputs are provided. Kernels can be combined with arithmetic expressions; see
#' [`?kernel-arith`][kernel-arith].
#'
#' @param scale The kernel length scale.
#'
#' @returns A function which calculates a kernel matrix for vector arguments `x`
#'   and `y`. The function has class `c("kernel", "function")`.
#'
#' @concept kernels
#' @name kernels
#'
#' @examples
#' k = k_rbf()
#' x = seq(-1, 1, 0.5)
#' k(0, 0)
#' k(0, x)
#' k(x, x)
#'
#' k = k_per(scale=0.2, period=0.3)
#' round(k(x, x))
#'
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
    class(fn) = c("kernel", "function")
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
    class(fn) = c("kernel", "function")
    fn
}

#' @describeIn kernels Rational quadratic kernel.
#' @param alpha The shape/df parameter. \eqn{\alpha=1} is the Cauchy kernel.
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
    class(fn) = c("kernel", "function")
    fn
}

#' @describeIn kernels Matérn kernel.
#' @param nu The smoothness parameter. \eqn{\nu=0.5} is the Ornstein–Uhlenbeck kernel.
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
    class(fn) = c("kernel", "function")
    fn
}

#' @describeIn kernels Periodic (exp-sine-squared) kernel.
#' @param period The period, in the same units as `scale`.
#' @export
k_per = function(scale = 1, period = 1) {
    if (scale <= 0) abort("`scale` must be positive")
    if (period <= 0) abort("`period` must be positive")
    fn = function(x, y) {
        exp(-2 * sin(pi * sqrt(dist_l2(x, y)) / period)^2 / scale^2)
    }
    attr(fn, "name") = "per"
    attr(fn, "scale") = scale
    attr(fn, "period") = period
    class(fn) = c("kernel", "function")
    fn
}


#' Kernel arithmetic
#'
#' Kernel functions (see [`?kernels`][kernels]) may be multiplied by constants,
#' multiplied by each other, or added together.
#'
#' @name kernel-arith
#' @concept kernels
#' @md
#' @returns A new kernel function, with class `c("kernel", "function")`.
#'
#' @examples
#' x = seq(-1, 1, 0.5)
#' k = k_rbf()
#' k2 = k_per(scale=0.2, period=0.3)
#'
#' k_add = k2 + 0.5*k
#' print(k_add)
#' image(k_add(x, x))
#'
NULL

#' @rdname kernel-arith
#'
#' @param x a numeric or a `kernel` function
#' @param k2 a `kernel` function
#'
#' @export
`*.kernel` <- function(x, k2) {
    stopifnot(is.numeric(x) || inherits(x, "kernel"))
    stopifnot(inherits(k2, "kernel"))

    if (is.numeric(x)) {
        rlang::fn_body(k2) <- rlang::expr({!!x * (!!rlang::fn_body(k2)[[2]])})
        k2
    } else {
        k1 = x
        fn <- function(x, y) { k1(x, y) * k2(x, y) }
        attr(fn, "name") = paste(attr(k1, "name"), "*", attr(k2, "name"))
        fn
    }
}

#' @rdname kernel-arith
#'
#' @param k1 a `kernel` function
#'
#' @export
`+.kernel` <- function(k1, k2) {
    stopifnot(inherits(k1, "kernel"))
    stopifnot(inherits(k2, "kernel"))

    fn <- function(x, y) { k1(x, y) + k2(x, y) }
    attr(fn, "name") = paste(attr(k1, "name"), "+", attr(k2, "name"))
    fn
}


