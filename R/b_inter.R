#' N-way interaction basis
#'
#' Generates a design matrix that contains all possible interactions of the
#' input variables up to a specified maximum depth.
#' The default `"symbox"` standardization, which maps inputs to
#' \eqn{[-0.5, 0.5]^d}, is strongly recommended, as it means that the interaction
#' terms will have smaller variance and thus be penalized more by methods like
#' the Lasso or ridge regression (see Gelman et al., 2008).
#'
#' @inheritParams b_rff
#' @param depth The maximum interaction depth. The default is 2, which means
#'   that all pairwise interactions are included.
#'
#' @returns A matrix with the rescaled and interacted features.
#'
#' @references Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). A
#'   weakly informative default prior distribution for logistic and other
#'   regression models.
#'
#' @examples
#' # default: all pairwise interactions
#' lm(mpg ~ b_inter(cyl, hp, wt), mtcars)
#'
#' # how number of features depends on interaction depth
#' for (d in 2:6) {
#'     X = with(mtcars, b_inter(cyl, disp, hp, drat, wt, depth=d))
#'     print(ncol(X))
#' }
#'
#' @export
b_inter <- function(
    ...,
    depth = 2,
    stdize = c("symbox", "box", "scale", "none"),
    shift = NULL,
    scale = NULL
) {
    if (depth <= 1) {
        abort("`depth` must be at least 2")
    }

    ee = substitute(list(...))
    x = as.matrix(cbind(...))
    colnames(x) = vapply(ee[-1], deparse, "")
    std = do_std(x, stdize, shift, scale)
    data = as.data.frame(std$x)

    if (depth) {
        form = as.formula(paste0("~ 0 + .^", depth))
    }
    m = model.matrix(form, data)

    attr(m, "shift") = std$shift
    attr(m, "scale") = std$scale
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_inter", "matrix", "array")

    m
}


#' @export
predict.b_inter <- function(object, newdata, ...) {
    if (missing(newdata)) {
        return(object)
    }
    rlang::eval_tidy(makepredictcall(object, attr(object, "call")), newdata)
}

#' @export
makepredictcall.b_inter <- function(var, call) {
    if (
        as.character(call)[1L] == "b_inter" ||
            (is.call(call) && identical(eval(call[[1L]]), b_inter))
    ) {
        at = attributes(var)[c("shift", "scale")]
        call[names(at)] = at
    }
    call
}
