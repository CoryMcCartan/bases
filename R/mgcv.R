#' `mgcv` integration
#'
#' Provides methods so that `bases` expansions can be used as user-defined
#' smooth classes in [mgcv::s()].
#' The `k` argument to `s()` maps to the main dimension parameter of each basis.
#' Other arguments should be passed via the `xt` argument to `s()`, and will be
#' forwarded to the basis function.
#'
#' @examples
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'     x = 1:150
#'     z = c(1:50, rep(1, 100))
#'     y = as.numeric(BJsales)
#'     m = mgcv::gam(y ~ s(x, bs = "b_bart", k=10) + s(z, bs = "b_bart", k=20))
#'     summary(m)
#'     plot(x, y)
#'     lines(x, fitted(m), type="s", col="blue")
#' }
#'
#' @concept interfaces
#' @name bases_mgcv
NULL


#   mgcv::gam(y ~ s(x1, x2, bs = "b_rff", k = 200, xt = list(kernel = k_rbf(0.5))))
#
# The `k` argument in s() maps to the main dimension parameter of each basis:
#   b_rff   -> p
#   b_bart  -> trees
#   b_nn    -> p
#   b_tpsob -> p
#   b_ker   -> (ignored; basis dim = n)
#   b_inter -> depth

# Internal helper: build the basis matrix given a basis function, data terms,
# the mgcv smooth spec object, the name of the k-mapped parameter, and its
# default value.
mgcv_construct <- function(object, data, basis_fn, k_param, k_default) {
    x_list <- lapply(object$term, function(t) data[[t]])
    k <- if (object$bs.dim < 0) k_default else object$bs.dim

    args <- c(x_list, stats::setNames(list(k), k_param))
    if (!is.null(object$xt)) {
        args <- c(args, object$xt)
    }

    basis <- do.call(basis_fn, args)

    p <- ncol(basis)
    object$X <- unclass(basis)
    if (!object$fixed) {
        object$S <- list(diag(p))
    }
    object$rank <- p
    object$null.space.dim <- 0L
    object$C <- matrix(nrow = 0, ncol = p) # no constraint
    object$df <- p
    object$bs.dim <- p

    # store basis attributes and k info for Predict.matrix
    object$.basis_attrs <- attributes(basis)
    object$.k_param <- k_param
    object$.k_value <- k

    object
}

# Internal helper: build the prediction matrix from stored basis attributes.
mgcv_predict <- function(object, data, basis_fn) {
    x_list <- lapply(object$term, function(t) data[[t]])
    ba <- object$.basis_attrs

    args <- c(x_list, stats::setNames(list(object$.k_value), object$.k_param))
    # Only pass back attributes that are formal arguments of the basis function
    fn_args <- names(formals(basis_fn))
    for (nm in names(ba)) {
        if (nm %in% fn_args) {
            args[[nm]] <- ba[[nm]]
        }
    }

    basis <- do.call(basis_fn, args)
    unclass(basis)
}

# --- b_rff ----------------------------------------------------------------

#' @exportS3Method mgcv::smooth.construct
smooth.construct.b_rff.smooth.spec <- function(object, data, knots) {
    object <- mgcv_construct(object, data, b_rff, "p", 100L)
    class(object) <- "b_rff.smooth"
    object
}

#' @exportS3Method mgcv::Predict.matrix
Predict.matrix.b_rff.smooth <- function(object, data) {
    mgcv_predict(object, data, b_rff)
}

# --- b_bart ---------------------------------------------------------------

#' @exportS3Method mgcv::smooth.construct
smooth.construct.b_bart.smooth.spec <- function(object, data, knots) {
    object <- mgcv_construct(object, data, b_bart, "trees", 100L)
    class(object) <- "b_bart.smooth"
    object
}

#' @exportS3Method mgcv::Predict.matrix
Predict.matrix.b_bart.smooth <- function(object, data) {
    mgcv_predict(object, data, b_bart)
}

# --- b_nn -----------------------------------------------------------------

#' @exportS3Method mgcv::smooth.construct
smooth.construct.b_nn.smooth.spec <- function(object, data, knots) {
    object <- mgcv_construct(object, data, b_nn, "p", 100L)
    class(object) <- "b_nn.smooth"
    object
}

#' @exportS3Method mgcv::Predict.matrix
Predict.matrix.b_nn.smooth <- function(object, data) {
    mgcv_predict(object, data, b_nn)
}

# --- b_tpsob --------------------------------------------------------------

#' @exportS3Method mgcv::smooth.construct
smooth.construct.b_tpsob.smooth.spec <- function(object, data, knots) {
    object <- mgcv_construct(object, data, b_tpsob, "p", 100L)
    class(object) <- "b_tpsob.smooth"
    object
}

#' @exportS3Method mgcv::Predict.matrix
Predict.matrix.b_tpsob.smooth <- function(object, data) {
    mgcv_predict(object, data, b_tpsob)
}

# --- b_ker ----------------------------------------------------------------

#' @exportS3Method mgcv::smooth.construct
smooth.construct.b_ker.smooth.spec <- function(object, data, knots) {
    # b_ker basis dim = n; k is ignored
    x_list <- lapply(object$term, function(t) data[[t]])
    args <- x_list
    if (!is.null(object$xt)) {
        args <- c(args, object$xt)
    }

    basis <- do.call(b_ker, args)

    p <- ncol(basis)
    object$X <- unclass(basis)
    if (!object$fixed) {
        object$S <- list(diag(p))
    }
    object$rank <- p
    object$null.space.dim <- 0L
    object$C <- matrix(nrow = 0, ncol = p)
    object$df <- p
    object$bs.dim <- p
    object$.basis_attrs <- attributes(basis)

    class(object) <- "b_ker.smooth"
    object
}

#' @exportS3Method mgcv::Predict.matrix
Predict.matrix.b_ker.smooth <- function(object, data) {
    x_list <- lapply(object$term, function(t) data[[t]])
    ba <- object$.basis_attrs

    args <- x_list
    fn_args <- names(formals(b_ker))
    for (nm in names(ba)) {
        if (nm %in% fn_args) {
            args[[nm]] <- ba[[nm]]
        }
    }

    basis <- do.call(b_ker, args)
    unclass(basis)
}

# --- b_inter --------------------------------------------------------------

#' @exportS3Method mgcv::smooth.construct
smooth.construct.b_inter.smooth.spec <- function(object, data, knots) {
    # b_inter uses substitute() internally, so pass a named matrix via do.call
    x_mat <- do.call(cbind, lapply(object$term, function(t) data[[t]]))
    colnames(x_mat) <- object$term
    k <- if (object$bs.dim < 0) 2L else object$bs.dim

    args <- list(x_mat, depth = k)
    if (!is.null(object$xt)) {
        args <- c(args, object$xt)
    }

    basis <- do.call(b_inter, args)

    p <- ncol(basis)
    object$X <- unclass(basis)
    if (!object$fixed) {
        object$S <- list(diag(p))
    }
    object$rank <- p
    object$null.space.dim <- 0L
    object$C <- matrix(nrow = 0, ncol = p)
    object$df <- p
    object$bs.dim <- p
    object$.basis_attrs <- attributes(basis)
    object$.k_param <- "depth"
    object$.k_value <- k

    class(object) <- "b_inter.smooth"
    object
}

#' @exportS3Method mgcv::Predict.matrix
Predict.matrix.b_inter.smooth <- function(object, data) {
    x_mat <- do.call(cbind, lapply(object$term, function(t) data[[t]]))
    colnames(x_mat) <- object$term
    ba <- object$.basis_attrs

    args <- list(x_mat, depth = object$.k_value)
    fn_args <- names(formals(b_inter))
    for (nm in names(ba)) {
        if (nm %in% fn_args) {
            args[[nm]] <- ba[[nm]]
        }
    }

    basis <- do.call(b_inter, args)
    unclass(basis)
}
