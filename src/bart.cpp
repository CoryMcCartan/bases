#include <vector>
#include <cpp11.hpp>

using namespace cpp11;

/*
 * Construct an indicator matrix for a forest of binary trees and data `x`.
 * Forests are specified as a list of tree `depths` (of length = the # trees),
 * and flat vectors `vars` containing 1-indexed variable indices (columns of
 * `x`) and `thresh` containing the threshold values for each variable.
 * It must be the case that sum(depths) == length(vars) == length(thresh).
 * Returns a nrow(x) -by- sum(2^depths) integer indicator matrix.
 */
[[cpp11::register]]
integers_matrix<> forest_mat(const doubles_matrix<> x, const integers depths,
                             const integers vars, const doubles thresh) {
    int n = x.nrow();
    int p = 0;
    for (int d : depths) {
        p += 1 << d;
    }
    writable::integers_matrix<> out(n, p);
    std::vector<uint16_t> idx(n);

    int vt_off = 0;
    int out_off = 0;
    // iterate over each tree
    for (int d : depths) {
        std::fill(idx.begin(), idx.end(), 0);
        for (int k = vt_off; k < vt_off + d; k++) {
            int j = vars[k] - 1;
            for (int i = 0; i < n; i++) {
                idx[i] |= (x(i, j) <= thresh[k]) << (k - vt_off);
            }
        }

        // store 1s in output matrix
        int ncols = 1 << d;
        for (int j = 0; j < ncols; j++) {
            for (int i = 0; i < n; i++) {
                out(i, out_off + j) = idx[i] == j;
            }
        }

        vt_off += d;
        out_off += ncols;
    }

    return out;
}
