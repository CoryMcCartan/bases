#' Random convolutional features
#'
#' Generates random convolutional features from a list of images.
#' Convolutional kernels are generated randomly (either
#' from a Gaussian distribution or as patches extracted from the training
#' images), applied to each image via efficient matrix multiplication, and then
#' pooled to produce a fixed-size feature vector per image.
#'
#' @inheritParams b_rff
#' @param x A list of images, where each image is a matrix (for grayscale) or a
#'   3D array with dimensions (height, width, channels) for color images. Images
#'   may have different dimensions, but must be large enough to accommodate the
#'   convolution kernel size. Missing values are not allowed.
#' @param p The number of random convolutional kernels to generate.
#' @param size The size of the square convolutional kernel (e.g., 3 means a
#'   3x3 kernel).
#' @param stride The stride for the convolution operation, i.e., how many
#'   pixels to skip between kernel applications. Default is 1.
#' @param kernel_gen Method for generating convolutional kernels. Either `"rnorm"`
#'   to generate kernels with entries drawn i.i.d. from a standard Normal
#'   distribution, or `"patch"` to extract random patches from the input images.
#' @param activation A function to pool convolution outputs within each cell of
#'   `pool_grid`, separately for each kernel.
#'   Defaults to [max()]. The function should accept a numeric vector and return
#'   a scalar or vector of pooled values. Common choices include [max()],
#'   [mean()], functions like the proportion of positive values (PPV), which
#'   can be implemented with `function(x) mean(x > 0)`. Multivariate pooling
#'   functions are also supported.
#' @param kernels Optional matrix of pre-specified convolutional kernels, where
#'   each column is a kernel in column-major format. The number of rows must be
#'   a positive perfect square, from which the kernel size is inferred. If
#'   provided, overrides `p`, `size`, and `kernel_gen`.
#' @param pool_grid Number of spatial pooling cells. A positive integer specifies
#'   a square grid; a two-element vector specifies the numbers of cell rows and
#'   columns. The default, 1, pools globally. Color channels are pooled together
#'   within each spatial cell.
#'
#' @returns A matrix of random convolutional features with one row per image in
#'   `x`. Columns are ordered by kernel, then pooling cell (in column-major
#'   order), then component of `activation`. With a scalar activation, each
#'   kernel contributes one column per pooling cell.
#'
#' @examples
#' x = outer(1:28, 1:28, function(x, y) {
#'     d = sqrt(4*(x - 14)^2 + (y - 14)^2)
#'     dnorm(d, mean = 10, sd = 0.8)
#' })
#' pal = gray.colors(256, 1, 0)
#' image(x, col = pal)
#'
#' # one random kernel (no activation)
#' m = b_conv(list(x), p=1, activation=function(x) x)
#' image(matrix(m, nrow = 26), col = pal)
#'
#' # many kernels (realistic use case)
#' m = b_conv(list(x), p = 100, size = 3)
#' str(m)
#'
#' # retain coarse spatial location using a 4x4 pooling grid
#' m = b_conv(list(x), p = 100, size = 3, pool_grid = 4)
#' dim(m)
#' @export
b_conv <- function(
    x,
    p = 100,
    size = 3,
    stride = 1,
    kernel_gen = c("rnorm", "patch"),
    activation = max,
    stdize = c("scale", "box", "symbox", "none"),
    kernels = NULL,
    shift = NULL,
    scale = NULL,
    pool_grid = 1
) {
    stdize = rlang::arg_match(stdize)
    if (
        !is.numeric(pool_grid) ||
            is.complex(pool_grid) ||
            !length(pool_grid) %in% c(1L, 2L) ||
            any(!is.finite(pool_grid)) ||
            any(pool_grid < 1) ||
            any(pool_grid != floor(pool_grid)) ||
            any(pool_grid > .Machine$integer.max)
    ) {
        abort("`pool_grid` must be a positive integer or a two-element vector of positive integers")
    }
    pool_grid = rep_len(as.integer(pool_grid), 2L)

    if (!is.list(x)) {
        abort("`x` must be a list of images (matrices or arrays)")
    }
    n = length(x)
    if (n == 0) {
        abort("`x` must contain at least one image")
    }

    if (is.null(kernels)) {
        kernel_gen = rlang::arg_match(kernel_gen)
    } else {
        if (!is.matrix(kernels)) {
            abort("`kernels` must be a matrix")
        }
        size = sqrt(nrow(kernels))
        if (size < 1 || size != floor(size)) {
            abort("`kernels` must have a positive perfect square number of rows")
        }
        p = ncol(kernels)
    }

    # Validate and normalize images to 3D arrays (h, w, c)
    img_info = vector("list", n)
    for (i in seq_len(n)) {
        img = x[[i]]
        if (!is.numeric(img) && !is.array(img)) {
            abort("All images must be numeric matrices or arrays")
        }
        if (anyNA(img)) {
            abort("Missing values are not allowed in images")
        }

        if (length(dim(img)) == 2) {
            dim(img) = c(dim(img), 1L)
        } else if (length(dim(img)) != 3) {
            abort("Images must be 2D matrices or 3D arrays")
        }

        dims = dim(img)
        x[[i]] = img
        img_info[[i]] = list(h = dims[1], w = dims[2], c = dims[3], n_pixels = prod(dims))
    }

    # Check dimensions are sufficient and channels are consistent
    lapply(img_info, function(info) {
        if (info$h < size || info$w < size) {
            abort("All images must be at least as large as the kernel `size`")
        }
    })

    # Standardization: collect all pixels and standardize once
    all_pixels = unlist(x, use.names = FALSE)
    std = do_std(matrix(all_pixels, ncol = 1), stdize, shift, scale)

    # Reshape standardized pixels back into image structures
    x_std = vector("list", n)
    start_idx = 1
    for (i in seq_along(x)) {
        n_pix = img_info[[i]]$n_pixels
        img_std = std$x[start_idx:(start_idx + n_pix - 1)]
        dim(img_std) = c(img_info[[i]]$h, img_info[[i]]$w, img_info[[i]]$c)
        x_std[[i]] = img_std
        start_idx = start_idx + n_pix
    }

    # Generate kernels if not supplied
    s2 = size^2
    if (is.null(kernels)) {
        if (kernel_gen == "rnorm") {
            kernels = matrix(rnorm(s2 * p), nrow = s2, ncol = p)
        } else if (kernel_gen == "patch") {
            kernels = generate_patch_kernels(x_std, img_info, p, size)
        }
    }

    cell_cache = new.env(parent = emptyenv())
    features_l = lapply(seq_along(x_std), function(i) {
        img = x_std[[i]]
        info = img_info[[i]]
        h_out = (info$h - as.integer(size)) %/% as.integer(stride) + 1L
        w_out = (info$w - as.integer(size)) %/% as.integer(stride) + 1L
        key = paste(h_out, w_out, info$c, sep = "_")
        if (!exists(key, cell_cache, inherits = FALSE)) {
            cell_cache[[key]] = conv_pool_cells(h_out, w_out, info$c, pool_grid)
        }
        cells = cell_cache[[key]]

        # Convolve: multiply im2col() by kernels
        im_mat = im2col(
            as.double(img),
            info$h,
            info$w,
            info$c,
            as.integer(size),
            as.integer(stride)
        )
        conv_output = im_mat %*% kernels

        # Pool
        if (
            (identical(activation, max) || identical(activation, mean)) &&
                !is.complex(conv_output) &&
                nrow(conv_output) > 0 &&
                !anyNA(conv_output)
        ) {
            pool = if (identical(activation, max)) pool_max else pool_mean
            c(pool(conv_output, cells$h, cells$w, cells$channels,
                   cells$row_bounds, cells$col_bounds))
        } else {
            if (is.null(cells$rows)) {
                positions = array(seq_len(h_out * w_out * info$c), c(h_out, w_out, info$c))
                cells$rows = list()
                for (col in seq_len(pool_grid[2])) {
                    for (row in seq_len(pool_grid[1])) {
                        rows = seq.int(cells$row_bounds[row], cells$row_bounds[row + 1L] - 1L)
                        cols = seq.int(cells$col_bounds[col], cells$col_bounds[col + 1L] - 1L)
                        cells$rows[[
                            length(cells$rows) + 1L
                        ]] = c(positions[rows, cols, , drop = FALSE])
                    }
                }
                cell_cache[[key]] = cells
            }
            conv_pool_apply(conv_output, activation, cells$rows)
        }
    })

    # Combine into matrix
    m = do.call(rbind, features_l)

    # Set attributes
    attr(m, "kernels") = kernels
    attr(m, "shift") = std$shift
    attr(m, "scale") = std$scale
    attr(m, "size") = size
    attr(m, "stride") = stride
    attr(m, "pool_grid") = pool_grid
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_conv", "matrix", "array")

    m
}

generate_patch_kernels <- function(x_std, img_info, p, size) {
    n = length(x_std)
    kernels = matrix(nrow = size^2, ncol = p)
    idxs = ceiling(runif(p, 0, n))
    for (j in 1:p) {
        img = x_std[[idxs[j]]]
        info = img_info[[idxs[j]]]

        h_start = ceiling(runif(1, 0, info$h - size + 1))
        w_start = ceiling(runif(1, 0, info$w - size + 1))
        h_end = h_start + size - 1
        w_end = w_start + size - 1
        ch = ceiling(runif(1, 0, info$c))

        patch = img[h_start:h_end, w_start:w_end, ch, drop = FALSE]
        kernels[, j] = c(patch)
    }

    kernels
}

#' @export
predict.b_conv <- function(object, newdata, ...) {
    if (missing(newdata)) {
        return(object)
    }
    rlang::eval_tidy(makepredictcall(object, attr(object, "call")), newdata)
}

#' @export
makepredictcall.b_conv <- function(var, call) {
    if (
        as.character(call)[1L] == "b_conv" ||
            (is.call(call) && identical(eval(call[[1L]]), b_conv))
    ) {
        at = attributes(var)[c("kernels", "shift", "scale", "size", "stride", "pool_grid")]
        at = at[!vapply(at, is.null, logical(1))]
        call[names(at)] = at
    }
    call
}

# Convert an image to patches; dimensions and stride must already be validated.
# Takes a double pixel vector and integer dimensions, kernel size, and stride.
# Rows index valid positions within each channel; columns index kernel pixels.
im2col <- quick(
    function(x, h, w, channels, size, stride) {
        declare(
            type(x = double(NA)),
            type(h = integer(1)),
            type(w = integer(1)),
            type(channels = integer(1)),
            type(size = integer(1)),
            type(stride = integer(1)),
            type(
                out = double(
                    ((h - size) %/% stride + 1L) * ((w - size) %/% stride + 1L) * channels,
                    size * size
                )
            )
        )

        h_out = (h - size) %/% stride + 1L
        w_out = (w - size) %/% stride + 1L
        out = matrix(0, h_out * w_out * channels, size * size)

        # Write contiguous columns of the patch matrix
        for (k_j in seq_len(size)) {
            for (k_i in seq_len(size)) {
                out_col = k_i + size * (k_j - 1L)
                for (ch in seq_len(channels)) {
                    for (j in seq_len(w_out)) {
                        for (i in seq_len(h_out)) {
                            out_row = i + h_out * ((j - 1L) + w_out * (ch - 1L))
                            idx_in = (i - 1L) * stride + h * ((j - 1L) * stride + w * (ch - 1L))
                            out[out_row, out_col] = x[idx_in + k_i + h * (k_j - 1L)]
                        }
                    }
                }
            }
        }

        out
    },
    "im2col"
)

# Partition the response map once; reuse these bounds across kernels and images.
conv_pool_cells <- function(h, w, channels, grid) {
    if (any(!is.finite(c(h, w))) || any(c(h, w) < 1L)) {
        abort("`size` and `stride` must produce a nonempty convolution response map")
    }
    if (any(grid > c(h, w))) {
        abort("`pool_grid` cannot exceed the convolution response-map dimensions")
    }
    list(
        h = as.integer(h), w = as.integer(w), channels = as.integer(channels),
        row_bounds = as.integer(ceiling((0:grid[1]) * h / grid[1]) + 1L),
        col_bounds = as.integer(ceiling((0:grid[2]) * w / grid[2]) + 1L)
    )
}

conv_pool_apply <- function(x, activation, rows) {
    out = unlist(
        lapply(seq_len(ncol(x)), function(j) {
            lapply(rows, function(i) activation(x[i, j]))
        }),
        use.names = FALSE
    )
    if (is.null(out)) numeric() else out
}

# Cell maxima without extracting response vectors. A single cell pools globally.
# Input must be a real matrix with at least one row and no missing values.
pool_max <- quick(
    function(x, h, w, channels, row_bounds, col_bounds) {
        declare(
            type(x = double(NA, NA)),
            type(h = integer(1)),
            type(w = integer(1)),
            type(channels = integer(1)),
            type(row_bounds = integer(NA)),
            type(col_bounds = integer(NA))
        )

        grid_h = length(row_bounds) - 1L
        grid_w = length(col_bounds) - 1L
        out = matrix(0, grid_h * grid_w, ncol(x))

        # Pool each spatial cell for each kernel
        for (j in seq_len(ncol(x))) {
            for (gc in seq_len(grid_w)) {
                for (gr in seq_len(grid_h)) {
                    best = x[row_bounds[gr] + h * (col_bounds[gc] - 1L), j]

                    # Find the maximum across positions and channels
                    for (ch in seq_len(channels)) {
                        for (col in col_bounds[gc]:(col_bounds[gc + 1L] - 1L)) {
                            base = h * (col - 1L + w * (ch - 1L))
                            for (row in row_bounds[gr]:(row_bounds[gr + 1L] - 1L)) {
                                if (x[base + row, j] > best) best = x[base + row, j]
                            }
                        }
                    }

                    out[gr + grid_h * (gc - 1L), j] = best
                }
            }
        }

        out
    },
    "pool_max"
)

pool_mean <- quick(
    function(x, h, w, channels, row_bounds, col_bounds) {
        declare(
            type(x = double(NA, NA)),
            type(h = integer(1)),
            type(w = integer(1)),
            type(channels = integer(1)),
            type(row_bounds = integer(NA)),
            type(col_bounds = integer(NA))
        )

        grid_h = length(row_bounds) - 1L
        grid_w = length(col_bounds) - 1L
        out = matrix(0, grid_h * grid_w, ncol(x))

        # Pool each spatial cell for each kernel
        for (j in seq_len(ncol(x))) {
            for (gc in seq_len(grid_w)) {
                for (gr in seq_len(grid_h)) {
                    total = 0
                    count = (row_bounds[gr + 1L] - row_bounds[gr]) *
                        (col_bounds[gc + 1L] - col_bounds[gc]) *
                        channels

                    # Sum across positions and channels
                    for (ch in seq_len(channels)) {
                        for (col in col_bounds[gc]:(col_bounds[gc + 1L] - 1L)) {
                            base = h * (col - 1L + w * (ch - 1L))
                            for (row in row_bounds[gr]:(row_bounds[gr + 1L] - 1L)) {
                                total = total + x[base + row, j]
                            }
                        }
                    }

                    out[gr + grid_h * (gc - 1L), j] = total / count
                }
            }
        }

        out
    },
    "pool_mean"
)
