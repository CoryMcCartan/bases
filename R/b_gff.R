#' Graph Fourier Feature basis
#'
#' Generates features as the low-magnitude eigenvectors of a graph Laplacian,
#' which can be thought of as a generalization of the Fourier basis to
#' graph-structured data.
#'
#' @param x An integer-indexed adjacency list, [igraph::igraph], or [adj::adj] object.
#' @param p The number of features to generate.
#' @param symmetric Whether `x` is assumed symmetric.
#'
#' @returns A matrix of graph Fourier features.
#'
#' @examples
#' if (requireNamespace("adj", quietly = TRUE) && requireNamespace("RSpectra", quietly = TRUE)) {
#'     pal = hcl.colors(256)
#'     a = adj::adj(
#'          c(6, 2), c(1, 7, 3), c(2, 8, 4), c(3, 9, 5), c(4, 10), c(1, 11, 7),  c(6, 12, 2, 8),
#'          c(7, 13, 3, 9), c(8, 14, 4, 10), c(9, 5, 15), c(6, 12, 16), c(11, 7, 13, 17),
#'          c(12, 8, 18, 14), c(13, 9, 19, 15), c(14, 20, 10), c(11, 21, 17), c(12, 16, 22, 18),
#'          c(13, 17, 23, 19), c(18, 24, 14, 20), c(19, 15, 25), c(16, 22), c(21, 17, 23),
#'          c(22, 18, 24), c(23, 19, 25), c(24, 20)
#'     )
#'
#'     m = b_gff(a, p = 3, symmetric = TRUE)
#'     image(matrix(m[, 1], 5, 5), col = pal)
#'     image(matrix(m[, 3], 5, 5), col = pal)
#'
#'     if (requireNamespace("igraph", quietly = TRUE)) {
#'         a = igraph::make_lattice(c(100, 100))
#'         xy = igraph::layout_on_grid(a)
#'         m = b_gff(a, p = 25, symmetric = TRUE)
#'         eig_25 = m[, 25] # 25th Fourier feature
#'         image(matrix(eig_25, 100, 100), col=pal)
#'     }
#' }
#' @export
b_gff <- function(x, p = min(length(x) - 1L, 50), symmetric = FALSE) {
    rlang::check_installed("RSpectra", reason = "to use `b_gff()`")

    if (inherits(x, "igraph")) {
        rlang::check_installed("igraph")
        sm = igraph::laplacian_matrix(x, sparse = TRUE)
    } else {
        rlang::check_installed("adj", reason = "to use `b_gff()`")
        if (!inherits(x, "adj")) {
            x = adj::adj(x, duplicates = "allow", self_loops = "allow")
        }
        sm = adj::adj_laplacian(x, sparse = TRUE)
    }
    if (p >= length(x)) {
        abort("Cannot generate more features than the number of nodes minus one.")
    }
    if (p <= 0) {
        abort("`p` must be a positive integer.")
    }

    fn = if (isTRUE(symmetric)) RSpectra::eigs_sym else RSpectra::eigs
    res = fn(sm, k = p + 1L, which = "LM", sigma = 0)
    ord = order(res$values)[-1]
    m = res$vectors[, ord, drop = FALSE]
    attr(m, "symmetric") = symmetric
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_gff", "matrix", "array")

    m
}