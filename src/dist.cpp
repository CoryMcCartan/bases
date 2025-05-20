#include <cpp11.hpp>
using namespace cpp11;

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
