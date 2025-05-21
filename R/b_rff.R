#' Random Fourier feature basis
#'
#' Generates a random Fourier feature basis matrix for a provided kernel,
#' optionally rescaling the data to lie in the unit hypercube.
#' Random features are of the form \deqn{
#'   \phi(x) = \cos(\omega^T x + b),
#' } where \eqn{\omega} is a vector of frequencies sampled from the Fourier
#' transform of the kernel, and \eqn{b\sim\mathrm{Unif}[-\pi, \pi]} is a random
#' phase shift. The input data `x` may be shifted and rescaled before the
#' feature mapping is applied, according to the `stdize` argument.
#'
#' @param ... The variable(s) to build features for. A single data frame or
#'   matrix may be provided as well. Missing values are not allowed.
#' @param p The number of random features.
#' @param kernel A kernel function. If one of the recognized kernel functions
#'   such as [k_rbf()] is provided, then the computations will be exact.
#'   Otherwise, the fast Fourier transform of the provided kernel function is
#'   used to generate the random features. The kernel should be shift-invariant
#'   and decay to zero at positive and negative infinity.
#' @param stdize How to standardize the predictors, if at all. The default
#'   `"scale"` applies `scale()` to the input so that the features have mean
#'   zero and unit variance, `"box"` scales the data along each dimension
#'   to lie in the unit hypercube, and `"symbox"` scales the data along each
#'   dimension to lie in \eqn{[-0.5, 0.5]^d}.
#' @param n_approx The number of discrete frequencies to use in calculating the
#'   Fourier transform of the provided kernel.  Not used for certain kernels for
#'   which an analytic Fourier transform is available; see above.
#' @param freqs Matrix of frequencies to use; `ncol(freqs)` must match the number
#'   of predictors. If provided, overrides those calculated automatically, thus
#'   ignoring `p` and `kernel`.
#' @param phases Vector of phase shifts to use. If provided, overrides those
#'   calculated automatically, thus ignoring `p` and `kernel`.
#' @param shift Vector of shifts, or single shift value, to use. If provided,
#'   overrides those calculated according to `stdize`.
#' @param scale Vector of scales, or single scale value, to use. If provided,
#'   overrides those calculated according to `stdize`.
#'
#' @returns A matrix of random Fourier features.
#'
#' @references
#' Rahimi, A., & Recht, B. (2007). *Random features for large-scale kernel
#' machines.* Advances in neural information processing systems, 20.
#'
#' @examples
#' data(quakes)
#'
#' m = ridge(depth ~ b_rff(lat, long), quakes)
#' plot(fitted(m), quakes$depth)
#'
#' # more random featues means a higher ridge penalty
#' m500 = ridge(depth ~ b_rff(lat, long, p = 500), quakes)
#' c(default = m$penalty, p500 = m500$penalty)
#'
#' # A shorter length scale fits the data better (R^2)
#' m_025 = ridge(depth ~ b_rff(lat, long, kernel = k_rbf(scale = 0.25)), quakes)
#' c(
#'   len_1 = cor(quakes$depth, fitted(m))^2,
#'   len_025 = cor(quakes$depth, fitted(m_025))^2
#' )
#' @export
b_rff <- function(..., p = 100, kernel = k_rbf(),
                  stdize = c("scale", "box", "symbox", "none"), n_approx = nextn(4*p),
                  freqs = NULL, phases = NULL, shift = NULL, scale = NULL) {
    x = as.matrix(cbind(...))
    n = nrow(x)
    d = ncol(x)

    std = do_std(x, stdize, shift, scale)
    x = std$x

    if (is.null(phases))
        phases = runif(p, -pi, pi)
    if (is.null(freqs)) {
        # analytical Fourier transform
        if (!is.null(kern_name <- attr(kernel, "name"))) {
            distr = switch(
                kern_name,
                rbf = function (p) rnorm(p, 0, 1 / attr(kernel, "scale")),
                lapl = function (p) rcauchy(p, 0, 1 / attr(kernel, "scale")),
                cauchy = function (p) rexp(p, attr(kernel, "scale")),
                NULL
            )

            if (!is.null(distr))
                freqs = matrix(distr(p * d), nrow=d, ncol=p)
        }
        # manual Fourier transform
        if (is.null(freqs)) {  # check handles case where `name` doesn't match above
            # find scale of kernel, calibrated to match RBF length scale
            k_scale = attr(kernel, "scale")
            if (is.null(k_scale)) { # fallback
                k_scale = exp(uniroot(function (x) 1e-8 - kernel(0, exp(x)),
                                      c(-20, 20), tol=1e-8)$root) / (2*pi)
            }
            # FFT
            fx = seq(-128*k_scale, 128*k_scale, length.out=n_approx)
            kx = kernel(fx, 0)
            px = abs(fft(kx))[1:(n_approx/2)]
            # convert to frequencies
            freqs = (sample(length(px), p*d, replace=TRUE, prob=px) - 0.5) /
                256 * (2 * pi / k_scale)
            freqs = matrix(freqs, nrow=d, ncol=p)
        }
    }

    if (ncol(freqs) != length(phases)) {
        abort("Number of columns in `freqs` must match length of `phases`")
    }

    m = cos(x %*% freqs + rep(phases, each=n))
    attr(m, "freqs") = freqs
    attr(m, "phases") = phases
    attr(m, "shift") = std$shift
    attr(m, "scale") = std$scale
    class(m) = c("b_rff", "matrix")

    m
}

#' @export
predict.b_rff <- function (object, newx, ...)  {
    if (missing(newx)) {
        return(object)
    }
    at = attributes(var)[c("freqs", "phases", "shift",  "scale")]
    do.call(b_rff, list(x = newx, at))
}

#' @export
makepredictcall.b_rff <- function(var, call) {
    if (as.character(call)[1L] == "b_rff" ||
            (is.call(call) && identical(eval(call[[1L]]), b_rff))) {
        at = attributes(var)[c("freqs", "phases", "shift",  "scale")]
        call[names(at)] = at
    }
    call
}
