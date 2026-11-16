# Squared L2 distances, with rows of x indexing rows and rows of y indexing columns.
dist_l2 <- function(x, y) {
    x = as.matrix(x)
    y = as.matrix(y)
    if (ncol(x) != ncol(y)) {
        stop("x and y must have the same number of columns")
    }
    dist_l2_mat(x, y)
}

dist_l2_mat <- quick(
    function(x, y) {
        declare(type(x = double(NA, p)), type(y = double(NA, p)))
        out = matrix(0, nrow(x), nrow(y))
        for (j in seq_len(nrow(y))) {
            for (i in seq_len(nrow(x))) {
                acc = 0
                for (k in seq_len(ncol(x))) {
                    diff = x[i, k] - y[j, k]
                    acc = acc + diff * diff
                }
                out[i, j] = acc
            }
        }
        out
    },
    "dist_l2_mat"
)

# L1 distances, with rows of x indexing rows and rows of y indexing columns.
dist_l1 <- function(x, y) {
    x = as.matrix(x)
    y = as.matrix(y)
    if (ncol(x) != ncol(y)) {
        stop("x and y must have the same number of columns")
    }
    dist_l1_mat(x, y)
}

dist_l1_mat <- quick(
    function(x, y) {
        declare(type(x = double(NA, p)), type(y = double(NA, p)))
        out = matrix(0, nrow(x), nrow(y))
        for (j in seq_len(nrow(y))) {
            for (i in seq_len(nrow(x))) {
                acc = 0
                for (k in seq_len(ncol(x))) {
                    acc = acc + abs(x[i, k] - y[j, k])
                }
                out[i, j] = acc
            }
        }
        out
    },
    "dist_l1_mat"
)
