#include <cpp11.hpp>
#include <vector>
#include <iostream>
using namespace cpp11;

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
