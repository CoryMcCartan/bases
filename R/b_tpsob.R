#' Tensor-product Sobolev space basis
#'
#' Generates features from a tensor-product Sobolev space basis for estimating
#' functions in a Sobolev space with dominating mixed derivatives. Basis
#' functions are of the form \deqn{
#'     \psi_{\mathbf{j}}(\mathbf{x}) = \prod_{k=1}^d \psi_{j_k}(x_k),
#' } where \deqn{
#'      \phi_1(x) = 1 \quad\text{and}\quad
#'      \phi_j(x) = \sqrt{2}\cos(\pi (j-1) x).
#' }
#' The multi-indices \eqn{\mathbf{j}} are generated in a specific order to
#' maximize statistical efficiency.
#' All inputs are standardized to lie in the unit hypercube \eqn{[0, 1]^d}.
#'
#' @inheritParams b_rff
#' @param p The number of basis functions to generate.
#'
#' @returns A matrix of tensor-product Sobolev space basis features.
#'
#' @references
#' Zhang, T., & Simon, N. (2023). Regression in tensor product spaces by the
#' method of sieves. _Electronic journal of statistics_, 17(2), 3660.
#'
#' @examples
#' data(quakes)
#'
#' m = ridge(depth ~ b_tpsob(lat, long, p = 100), quakes)
#' plot(fitted(m), quakes$depth)
#'
#' x = 1:150
#' y = as.numeric(BJsales)
#' m = lm(y ~ b_tpsob(x, p = 10))
#' plot(x, y)
#' lines(x, fitted(m), col="blue")
#' @export
b_tpsob <- function(
    ...,
    p = 100,
    shift = NULL,
    scale = NULL
) {
    x = as.matrix(cbind(...))
    n = nrow(x)
    d = ncol(x)

    std = do_std(x, "box", shift, scale)
    x = std$x

    rlang::check_installed("Sieve", "for this basis function.")
    idx = Sieve::create_index_matrix(d, p + 1L, interaction_order = p)
    idx = idx[1L + seq_len(p), -1, drop = FALSE]

    m = matrix(nrow = n, ncol = p)
    for (j in seq_len(p)) {
        resc = 2^(sum(idx[j, ] > 1L) / 2)
        m[, j] = resc * row_prod(cos(pi * x * rep(idx[j, ] - 1L, each = n)))
    }

    attr(m, "shift") = std$shift
    attr(m, "scale") = std$scale
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_tpsob", "matrix", "array")

    m
}


#' @export
predict.b_tpsob <- function(object, newdata, ...) {
    if (missing(newdata)) {
        return(object)
    }
    rlang::eval_tidy(makepredictcall(object, attr(object, "call")), newdata)
}

#' @export
makepredictcall.b_tpsob <- function(var, call) {
    if (
        as.character(call)[1L] == "b_tpsob" ||
            (is.call(call) && identical(eval(call[[1L]]), b_tpsob))
    ) {
        at = attributes(var)[c("shift", "scale")]
        call[names(at)] = at
    }
    call
}
