#' Bayesian Additive Regression Tree (BART) features
#'
#' Generates random features from a BART prior on symmetric trees. Equivalently,
#' the features are the interaction of a small number of indicator functions.
#' The number of interacted indicators is the depth of the symmetric tree, and
#' is drawn from a prior on the tree depth which is calibrated to match the
#' traditional BART prior of Chipman et al. (2010). The variable at each tree
#' node is selected uniformly, and thresholds are selected uniformly from the
#' range of each variable.
#'
#' @inheritParams b_rff
#' @param trees The number of trees to sample.
#' @param depths The depths of each tree. By default, these are drawn from a
#'   Poisson distribution calibrated to produce trees with around 2.5 leaves, on
#'   average, matching the traditional BART prior.
#' @param vars Integer indices of the variables to use for each tree. If
#'   provided, overrides those generated automatically by sampling uniformly
#'   from the available input features. Provided in flat form, so should have
#'   length equal to `sum(depths)`.
#' @param thresh The thresholds for each variable. If provided, overrides those
#'   generated automatically by sampling uniformly from `ranges`, which defaults
#'   to the range of each input feature. Provided in flat form, so should have
#'   length equal to `sum(depths)`.
#' @param drop Columns in the calculated indicator matrix to drop. By default,
#'   any leaves which match zero input rows are dropped.  If provided, overrides
#'   this default.
#' @param min_drop Controls the default dropping of columns. Leaves which match
#'   `min_drop` or fewer input rows are dropped. Defaults to 0, so only empty
#'    leaves are dropped.
#' @param ranges The range of the input features, provided as a matrix with two
#'   rows and a column for each input feature. The first row is the minimum and
#'   the second row is the maximum.
#'
#' @returns A matrix of indicator variables encoding the random features.
#'
#' @references Hugh A. Chipman. Edward I. George. Robert E. McCulloch. "BART:
#'   Bayesian additive regression trees." Ann. Appl. Stat. 4 (1) 266 - 298,
#'   March 2010. https://doi.org/10.1214/09-AOAS285
#'
#' @examples
#' X = with(mtcars, b_bart(cyl, disp, hp, drat, wt, trees = 50))
#' all(colSums(X) > 0) # TRUE; empty leaves are pruned away
#' # each row belongs to 1 leaf node per tree; some trees pruned away
#' all(rowSums(X) == rowSums(X)[1]) # TRUE
#' all(rowSums(X) <= 50) # TRUE
#'
#' x = 1:150
#' y = as.numeric(BJsales)
#' m = ridge(y ~ b_bart(x, trees=25))
#' plot(x, y)
#' lines(x, fitted(m), type="s", col="blue")
#'
#' @export
b_bart <- function(..., trees = 100, depths = bart_depth_prior()(trees),
                   vars = NULL, thresh = NULL, drop = NULL, min_drop = 0L, ranges = NULL) {
    x = as.matrix(cbind(...))
    storage.mode(x) = "double"
    if (is.null(ranges)) ranges = apply(x, 2, range)
    if (ncol(ranges) != ncol(x) || nrow(ranges) != 2)
        abort("`ranges` must have two rows and a column for each input variable")

    if (length(depths) != trees) abort("`depths` must have length `trees`")
    k = sum(depths)
    if (is.null(vars)) {
        vars = sample(ncol(x), k, replace=TRUE)
    }
    if (is.null(thresh)) {
        thresh = runif(k, ranges[1, vars], ranges[2, vars])
    }
    if (k != length(vars)) abort("`depths` is inconsistent with `vars`")
    if (length(thresh) != length(vars)) abort("`thresh` and `vars` must have the same length")

    m = forest_mat(x, as.integer(depths), vars, thresh)
    if (is.null(drop)) {
        if (!is.numeric(min_drop) || length(min_drop) != 1 || min_drop < 0)
            abort("`min_drop` must be a single non-negative integer")
        tot = colSums(m)
        drop = which(tot <= min_drop | tot >= nrow(x) - min_drop)
    }
    if (length(drop) > 0) m = m[, -drop]

    attr(m, "depths") = depths
    attr(m, "vars") = vars
    attr(m, "thresh") = thresh
    attr(m, "drop") = drop
    attr(m, "ranges") = ranges
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_bart", "matrix", "array")

    m
}

#' @describeIn b_bart Poisson depth prior for random trees, parametrized in
#'   terms of mean tree depth. Returns a function which generates samples from
#'   the prior with argument giving the number of samples. The default prior
#'   closely matches the average number of leaves in the original (asymmetric)
#'   BART prior.
#' @param mean_depth The mean prior depth of each tree, where a single node has
#'   depth zero and a two-leaf tree has depth 1. This value minus one becomes
#'   the rate parameter of a Poisson distribution, whose samples are then
#'   shifted up by one. In this way, no zero-depth trees (which produce trivial
#'   features) are sampled.
#' @export
bart_depth_prior <- function(mean_depth = 1.25) {
    function(n) 1L + rpois(n, mean_depth - 1)
}

#' @export
predict.b_bart <- function (object, newdata, ...)  {
    if (missing(newdata)) {
        return(object)
    }
    rlang::eval_tidy(makepredictcall(object, attr(object, "call")), newdata)
}

#' @export
makepredictcall.b_bart <- function(var, call) {
    if (as.character(call)[1L] == "b_bart" ||
        (is.call(call) && identical(eval(call[[1L]]), b_bart))) {
        at = attributes(var)[c("depths", "vars", "thresh", "drop", "ranges")]
        call[names(at)] = at
    }
    call
}

# # matching the prior to the default BART prior
# function() {
#     rtree = function() {
#         leaves = 1
#         while ((cur <- tail(leaves, 1)) > 0) {
#             depth = length(leaves)
#             splits = rbinom(1, cur, 0.95 / depth^2)
#             leaves = c(leaves, 2*splits)
#             leaves[depth] = leaves[depth] - splits
#         }
#         c(leaves = sum(leaves), depth = length(leaves) - 2)
#     }
#
#     avgl_bart = mean(replicate(1e4, rtree())["leaves", ])
#
#     lambdas = seq(0.0, 0.5, 0.01)
#     avgl_sym = sapply(lambdas, function(l) mean(2^(1 + rpois(1e5, l))))
#     plot(lambdas, avgl_sym)
#
#     m = lm(lambdas ~ avgl_sym + I(avgl_sym^2) + I(avgl_sym^3))
#
#     lambda = predict(m, newdata=list(avgl_sym=avgl_bart))
# }
