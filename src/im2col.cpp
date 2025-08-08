#include <cpp11.hpp>

using namespace cpp11;

/*
 * Convert an (h, w, c) image to a (h'*w'*c, s*s)  matrix so that convolutions can
 * be performed by matrix multiplication.
 *  - `size` is the height/width of the kernel (which must be square)
 *  - `stride` is the spacing between kernel evaluations
 *  - No padding is performed
 *  - The output columns are in column-major order, ready for multiplication by
 *     a column-major-order kernel
 */
[[cpp11::register]]
doubles_matrix<> im2col(const doubles& x, int h, int w, int c,
                        int size, int stride) {
    int h_out = (h - size + 1) / stride;
    int w_out = (w - size + 1) / stride;
    writable::doubles_matrix<> out(h_out * w_out * c, size * size);

    for (int l = 0; l < c; l++) { // channels
        for (int j = 0; j < w_out; j++) { // columns
            for (int i = 0; i < h_out; i++) { // rows
                int out_row = i + h_out * (j + w_out * l);
                int idx_in = i*stride + h * (j*stride + w * l);

                if (size == 2) {
                    out(out_row, 0) = x[idx_in];
                    out(out_row, 1) = x[idx_in + 1];
                    out(out_row, 2) = x[idx_in + h];
                    out(out_row, 3) = x[idx_in + 1 + h];
                } else if (size == 3) {
                    out(out_row, 0) = x[idx_in];
                    out(out_row, 1) = x[idx_in + 1];
                    out(out_row, 2) = x[idx_in + 2];
                    out(out_row, 3) = x[idx_in + h];
                    out(out_row, 4) = x[idx_in + 1 + h];
                    out(out_row, 5) = x[idx_in + 2 + h];
                    out(out_row, 6) = x[idx_in + 2*h];
                    out(out_row, 7) = x[idx_in + 1 + 2*h];
                    out(out_row, 8) = x[idx_in + 2 + 2*h];
                } else if (size == 4) {
                    out(out_row, 0) = x[idx_in];
                    out(out_row, 1) = x[idx_in + 1];
                    out(out_row, 2) = x[idx_in + 2];
                    out(out_row, 3) = x[idx_in + 3];
                    out(out_row, 4) = x[idx_in + h];
                    out(out_row, 5) = x[idx_in + 1 + h];
                    out(out_row, 6) = x[idx_in + 2 + h];
                    out(out_row, 7) = x[idx_in + 3 + h];
                    out(out_row, 8) = x[idx_in + 2*h];
                    out(out_row, 9) = x[idx_in + 1 + 2*h];
                    out(out_row, 10) = x[idx_in + 2 + 2*h];
                    out(out_row, 11) = x[idx_in + 3 + 2*h];
                    out(out_row, 12) = x[idx_in + 3*h];
                    out(out_row, 13) = x[idx_in + 1 + 3*h];
                    out(out_row, 14) = x[idx_in + 2 + 3*h];
                    out(out_row, 15) = x[idx_in + 3 + 3*h];
                } else {
                    // general double loop for other sizes
                    for (int k_j = 0; k_j < size; k_j++) {
                        for (int k_i = 0; k_i < size; k_i++) {
                            int out_col = k_i + size * k_j;
                            out(out_row, out_col) = x[idx_in + k_i + h*k_j];
                        }
                    }
                }
            }
        }
    }

    return out;
}