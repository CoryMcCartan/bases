#include <vector>
#include <cpp11.hpp>

using namespace cpp11;

typedef std::vector<int> vec_int;


void factor_partitions(int n, int k, vec_int& current,
                      std::vector<vec_int>& result) {
    if (k == 0) {
        if (n == 1) {
            result.push_back(current);
        }
        return;
    }

    for (int f = 1; f <= n; f++) {
        if (n % f == 0) {
            current.push_back(f);
            factor_partitions(n / f, k - 1, current, result);
            current.pop_back();
        }
    }
}

/*
 * All multiplicative partitions of `n` into `k` factors
 */
[[cpp11::register]]
integers_matrix<> mult_partition(int n, int k) {
    std::vector<vec_int> result;
    vec_int current;
    factor_partitions(n, k, current, result);

    writable::integers_matrix<> out(result.size(), k);
    for (int i = 0; i < result.size(); i++) {
        for (int j = 0; j < k; ++j) {
            out(i, j) = result[i][j];
        }
    }
    return out;
}

/*
 * Row products
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