#include <cpp11.hpp>

using namespace cpp11;

/*
 * L2 distance between `x` and `y`, as a matrix with a row for each row of `x`
 * and a column for each row of `y`.
 */
[[cpp11::register]]
doubles_matrix<> dist_l2(const doubles_matrix<> x, const doubles_matrix<> y) {
    int nx = x.nrow();
    int ny = y.nrow();
    int p = x.ncol();
    if (y.ncol() != p) stop("x and y must have the same number of columns");
    writable::doubles_matrix<> out(nx, ny);

    for (int j = 0; j < ny; j++) {
        for (int i = 0; i < nx; i++) {
            out(i, j) = 0;
            for (int k = 0; k < p; k++) {
                double diff = x(i, k) - y(j, k);
                out(i, j) += diff * diff;
            }
        }
    }

    return out;
}

/*
 * L1 distance between `x` and `y`, as a matrix with a row for each row of `x`
 * and a column for each row of `y`.
 */
[[cpp11::register]]
doubles_matrix<> dist_l1(const doubles_matrix<> x, const doubles_matrix<> y) {
    int nx = x.nrow();
    int ny = y.nrow();
    int p = x.ncol();
    if (y.ncol() != p) stop("x and y must have the same number of columns");
    writable::doubles_matrix<> out(nx, ny);

    for (int j = 0; j < ny; j++) {
        for (int i = 0; i < nx; i++) {
            out(i, j) = 0;
            for (int k = 0; k < p; k++) {
                double diff = x(i, k) - y(j, k);
                out(i, j) += std::abs(diff);
            }
        }
    }

    return out;
}


/*
 * L1 distance between `x` and `y`, as a matrix with a row for each row of `x`
 * and a column for each row of `y`.
 */
[[cpp11::register]]
doubles row_prod(const doubles_matrix<> x) {
    int n = x.nrow();
    int p = x.ncol();
    writable::doubles out(n);

    for (int i = 0; i < n; i++) {
        out[i] = x(i, 0);
    }
    for (int j = 1; j < p; j++) {
        for (int i = 0; i < n; i++) {
            out[i] *= x(i, j);
        }
    }

    return out;
}

