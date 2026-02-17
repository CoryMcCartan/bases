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
#' @param activation A function to pool the convolution outputs for each kernel.
#'   Defaults to [max()]. The function should accept a numeric vector and return
#'   a scalar or vector of pooled values. Common choices include [max()],
#'   [mean()], functions like the proportion of positive values (PPV), which
#'   can be implemented with `function(x) mean(x > 0)`. Multivariate pooling
#'   functions are also supported.
#' @param kernels Optional matrix of pre-specified convolutional kernels, where
#'   each column is a kernel in column-major format. If provided, overrides `p`,
#'   `size`, and `kernel_gen`.
#'
#' @returns A matrix of random convolutional features with one row per image in
#'   `x` and one column per kernel (or more columns if `activation` is
#'   multivariate).
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
    scale = NULL
) {
    kernel_gen = rlang::arg_match(kernel_gen)
    stdize = rlang::arg_match(stdize)

    if (!is.list(x)) {
        abort("`x` must be a list of images (matrices or arrays)")
    }
    n = length(x)
    if (n == 0) {
        abort("`x` must contain at least one image")
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

    # Generate or validate kernels
    s2 = size^2
    if (is.null(kernels)) {
        if (kernel_gen == "rnorm") {
            kernels = matrix(rnorm(s2 * p), nrow = s2, ncol = p)
        } else if (kernel_gen == "patch") {
            kernels = generate_patch_kernels(x_std, img_info, p, size)
        }
    } else {
        # Validate provided kernels
        if (!is.matrix(kernels)) {
            abort("`kernels` must be a matrix")
        }
        if (nrow(kernels) != s2) {
            abort(sprintf(
                "`kernels` must have %d rows (size^2), but has %d",
                s2, nrow(kernels)
            ))
        }
        p = ncol(kernels)
    }

    features_l = lapply(seq_along(x_std), function(i) {
        img = x_std[[i]]
        info = img_info[[i]]

        # Convolve: multiply im2col() by kernels
        im_mat = im2col(c(img), info$h, info$w, info$c, size, stride)
        conv_output = im_mat %*% kernels

        # Pool
        if (identical(activation, mean)) {
            colMeans(conv_output)
        } else {
            c(apply(conv_output, 2, activation))
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
        at = attributes(var)[c("kernels", "shift", "scale", "size", "stride")]
        call[names(at)] = at
    }
    call
}
