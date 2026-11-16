#' Wavelet scattering features
#'
#' Generates deterministic image features using Morlet wavelets at multiple
#' scales and orientations. Each convolution is followed by a complex magnitude.
#' First-order maps are summarized and also passed to broader wavelets to form
#' second-order features. An interior crop based on the nominal filter supports
#' is retained, using four Gaussian standard deviations along the longest axis.
#'
#' This is a draft implementation. The FFT route evaluates the Gaussian Fourier
#' formulas directly; the direct route truncates spatial wavelets at four
#' Gaussian standard deviations. They use the same nominal parameters and crop,
#' but need not give identical coefficients. The FFT crop suppresses boundary
#' effects without eliminating the Gaussian tails. Intermediate maps are sampled
#' every `2^j` pixels at wavelet scale `j` (starting at zero). The FFT route folds
#' the filtered spectrum before the inverse transform; the direct route evaluates
#' only the corresponding patches. Discrete frame bounds and equivalence to
#' ScatNet are not established.
#'
#' @param x A list of real matrices or arrays with dimensions (height, width,
#'   channels). Values must be finite and channel counts must match.
#' @param scales Number of wavelet scales, with widths doubling at each scale.
#' @param angles Number of orientations, equally spaced over a half-turn.
#' @param order Maximum scattering order, either 1 or 2. All lower orders,
#'   including the mean or smoothed image at order zero, are retained.
#' @param pooling Either `"global"` for one spatial mean per path and channel,
#'   or `"local"` for Gaussian averaging followed by sampling every `2^scales`
#'   pixels on a common interior grid. Local pooling requires matching image
#'   dimensions, including at prediction time. Different global features can
#'   summarize different interior regions because their filter supports differ.
#' @param fft Whether to use FFT convolution, for comparing implementations.
#' @param stdize How to standardize input pixels: `"none"`, `"scale"`, `"box"`,
#'   or `"symbox"`, as in [b_conv()]. Parameters are shared across channels and
#'   estimated across training pixels, without joining all images in memory.
#' @param bank Optional Morlet bank specification, ordinarily saved for
#'   prediction. If supplied, its `scales` and `angles` override those arguments.
#'   It stores parameters and the output layout, not cached Fourier arrays.
#' @param shift,scale Optional scalar centering and divisor parameters for input
#'   standardization. Intermediate maps are never standardized.
#'
#' @returns A real matrix with one row per image. Attributes record the bank,
#'   paths, local grid (if any), preprocessing parameters, and original call.
#' @references
#' Bruna and Mallat (2013), <https://arxiv.org/abs/1203.1513>.
#' The nominal Morlet widths and frequencies follow the Kymatio 2D filter bank:
#' <https://github.com/kymatio/kymatio/blob/main/kymatio/scattering2d/filter_bank.py>.
#' @export
b_scatter <- function(
    x,
    scales = 3,
    angles = 6,
    order = 2,
    pooling = c("global", "local"),
    fft = TRUE,
    stdize = c("none", "scale", "box", "symbox"),
    bank = NULL,
    shift = NULL,
    scale = NULL
) {
    pooling = rlang::arg_match(pooling)
    stdize = rlang::arg_match(stdize)
    if (!is.logical(fft) || length(fft) != 1L || is.na(fft)) {
        abort("`fft` must be TRUE or FALSE")
    }
    if (!is.numeric(order) || length(order) != 1L || !order %in% c(1, 2)) {
        abort("`order` must be 1 or 2")
    }
    if (is.null(bank)) {
        bank = scatter_bank(scales, angles)
    } else {
        scatter_check_bank(bank)
    }
    scales = bank$scales
    angles = bank$angles
    paths = scatter_paths(bank, order, pooling)
    if (!is.list(x) || length(x) == 0L) {
        abort("`x` must be a nonempty list of images")
    }

    dims = lapply(x, function(img) {
        d = dim(img)
        if (
            !is.numeric(img) || is.complex(img) ||
                !length(d) %in% c(2L, 3L) || any(d < 1L)
        ) {
            abort("Images must be nonempty real numeric matrices or 3D arrays")
        }
        if (any(!is.finite(img))) abort("Image values must be finite")
        if (length(d) == 2L) d = c(d, 1L)
        d
    })
    channels = dims[[1]][3]
    if (any(vapply(dims, function(d) d[3] != channels, logical(1)))) {
        abort("All images must have the same number of channels")
    }
    if (!is.null(bank$channels) && channels != bank$channels) {
        abort("Images must have the same number of channels as the fitted bank")
    }
    bank$channels = channels

    required = if (pooling == "local") {
        max(paths$left) + max(paths$right) + 1L
    } else max(paths$left + paths$right) + 1L
    if (any(vapply(dims, function(d) any(d[1:2] < required), logical(1)))) {
        abort(sprintf(
            "Images must have at least %d rows and columns for these filters and pooling",
            required
        ))
    }
    grid = NULL
    if (pooling == "local") {
        shape = dims[[1]][1:2]
        if (any(vapply(dims, function(d) !identical(d[1:2], shape), logical(1)))) {
            abort("Local pooling requires images with matching spatial dimensions")
        }
        if (!is.null(bank$shape) && !identical(shape, bank$shape)) {
            abort("Local pooling requires the spatial dimensions of the fitted bank")
        }
        bank$shape = shape
        grid = list(
            row = seq(max(paths$left) + 1L, shape[1] - max(paths$right), by = 2^scales),
            col = seq(max(paths$left) + 1L, shape[2] - max(paths$right), by = 2^scales)
        )
    }
    std = scatter_standardize(x, stdize, shift, scale)

    # Cache by filter and map dimensions, and reuse across images in this call.
    # Only the compact bank specification is saved on the returned basis.
    cache = new.env(parent = emptyenv())
    n_paths = nrow(paths)
    n_pos = if (is.null(grid)) 1L else length(grid$row) * length(grid$col)
    m = matrix(NA_real_, length(x), channels * n_paths * n_pos)
    first_path = match(seq_len(nrow(bank$wavelets)), paths$first)
    second_path = matrix(NA_integer_, nrow(bank$wavelets), nrow(bank$wavelets))
    ids = which(paths$order == 2L)
    second_path[cbind(paths$first[ids], paths$second[ids])] = ids

    for (i in seq_along(x)) {
        img = array((as.double(x[[i]]) - std$shift) / std$scale, dims[[i]])
        for (ch in seq_len(channels)) {
            result = matrix(NA_real_, n_pos, n_paths)
            input = scatter_context(img[, , ch], fft, 2^scales)
            result[, 1L] = scatter_pool(input$x, bank, grid, cache, fft)
            for (j in seq_len(scales)) {
                first_ids = which(bank$wavelets$j == j - 1L)
                first_maps = scatter_convolve(input, first_ids, bank, cache, fft)
                for (a in seq_along(first_ids)) {
                    first = first_ids[a]
                    u = first_maps[[a]]
                    result[, first_path[first]] = scatter_pool(u, bank, grid, cache, fft)
                    if (order == 1L || j == scales) next
                    branch = scatter_context(u, fft, 2^scales)
                    for (k in seq.int(j + 1L, scales)) {
                        second_ids = which(bank$wavelets$j == k - 1L)
                        second_maps = scatter_convolve(
                            branch, second_ids, bank, cache, fft
                        )
                        for (b in seq_along(second_ids)) {
                            second = second_ids[b]
                            result[, second_path[first, second]] = scatter_pool(
                                second_maps[[b]], bank, grid, cache, fft
                            )
                        }
                    }
                }
            }
            cols = (ch - 1L) * n_paths * n_pos + seq_len(n_paths * n_pos)
            m[i, cols] = c(result)
        }
    }

    path_names = vapply(seq_len(n_paths), function(i) {
        if (paths$order[i] == 0L) return("s0")
        first = bank$wavelets[paths$first[i], ]
        name = sprintf("s1_j%d_a%d", first$j + 1L, first$angle)
        if (paths$order[i] == 2L) {
            second = bank$wavelets[paths$second[i], ]
            name = sprintf("s2_j%d_a%d_j%d_a%d", first$j + 1L, first$angle,
                           second$j + 1L, second$angle)
        }
        name
    }, character(1))
    if (!is.null(grid)) {
        positions = expand.grid(row = grid$row, col = grid$col)
        path_names = unlist(lapply(path_names, function(name) {
            paste0(name, "_r", positions$row, "_c", positions$col)
        }), use.names = FALSE)
    }
    colnames(m) = unlist(lapply(seq_len(channels), function(ch) {
        paste0("ch", ch, "_", path_names)
    }), use.names = FALSE)
    rownames(m) = names(x)
    attr(m, "bank") = bank
    attr(m, "paths") = paths
    attr(m, "grid") = grid
    attr(m, "scales") = scales
    attr(m, "angles") = angles
    attr(m, "order") = order
    attr(m, "pooling") = pooling
    attr(m, "fft") = fft
    attr(m, "stdize") = stdize
    attr(m, "shift") = std$shift
    attr(m, "scale") = std$scale
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_scatter", "matrix", "array")
    m
}

# Bank entries describe filters; coefficients are built for the chosen backend.
scatter_bank <- function(scales, angles) {
    for (value in list(scales, angles)) {
        if (
            !is.numeric(value) || length(value) != 1L || !is.finite(value) ||
                value < 1 || value != floor(value)
        ) {
            abort("`scales` and `angles` must be positive integers")
        }
    }
    # Check support before constructing any potentially large coefficient arrays.
    if (scales > 25L || angles > 360L) abort("Requested bank is too large")
    scales = as.integer(scales)
    angles = as.integer(angles)
    wavelets = expand.grid(angle = seq_len(angles), j = seq_len(scales) - 1L)
    wavelets$theta = (wavelets$angle - 1L) * pi / angles
    wavelets$sigma = 0.8 * 2^wavelets$j
    wavelets$frequency = (3 * pi / 4) / 2^wavelets$j
    wavelets$slant = min(1, 4 / angles)
    wavelets$radius = ceiling(4 * wavelets$sigma / wavelets$slant)
    average_sigma = 0.8 * 2^(scales - 1L)
    list(
        version = 1L, scales = scales, angles = angles, wavelets = wavelets,
        average = list(sigma = average_sigma, radius = ceiling(4 * average_sigma))
    )
}

scatter_check_bank <- function(bank) {
    if (!is.list(bank) || !identical(bank$version, 1L)) {
        abort("`bank` must be a Morlet bank specification from b_scatter()")
    }
    expected = scatter_bank(bank$scales, bank$angles)
    if (!identical(bank$wavelets, expected$wavelets) ||
        !identical(bank$average, expected$average)) {
        abort("`bank` has inconsistent Morlet filter parameters")
    }
}

scatter_paths <- function(bank, order, pooling = "global") {
    w = bank$wavelets
    paths = data.frame(order = 0L, first = NA_integer_, second = NA_integer_, radius = 0)
    paths = rbind(paths, data.frame(
        order = 1L, first = seq_len(nrow(w)), second = NA_integer_, radius = w$radius
    ))
    if (order == 2L) {
        pairs = expand.grid(first = seq_len(nrow(w)), second = seq_len(nrow(w)))
        pairs = pairs[w$j[pairs$second] > w$j[pairs$first], , drop = FALSE]
        paths = rbind(paths, data.frame(
            order = rep(2L, nrow(pairs)), first = pairs$first, second = pairs$second,
            radius = w$radius[pairs$first] + w$radius[pairs$second]
        ))
    }
    # All sampling grids are anchored at original pixel 1. Track the first
    # retained center and the support to its right, including rounding when a
    # filter is evaluated on a coarser parent map.
    paths$left = paths$right = paths$spacing = numeric(nrow(paths))
    for (i in seq_len(nrow(paths))) {
        spacing = 1L
        left = right = 0L
        ids = c(paths$first[i], paths$second[i])
        for (id in ids[!is.na(ids)]) {
            radius = ceiling(w$radius[id] / spacing) * spacing
            spacing = 2^w$j[id]
            left = ceiling((left + radius) / spacing) * spacing
            right = right + radius
        }
        if (pooling == "local") {
            radius = ceiling(bank$average$radius / spacing) * spacing
            spacing = 2^bank$scales
            left = ceiling((left + radius) / spacing) * spacing
            right = right + radius
        }
        paths$left[i] = left
        paths$right[i] = right
        paths$spacing[i] = spacing
    }
    rownames(paths) = NULL
    paths
}

# Combine per-image moments without retaining a concatenated pixel vector.
scatter_standardize <- function(x, stdize, shift, scale) {
    if (stdize == "none") return(list(shift = 0, scale = 1))
    if (is.null(shift) || is.null(scale)) {
        count = 0
        center = 0
        ss = 0
        lo = Inf
        hi = -Inf
        for (img in x) {
            v = as.double(img)
            n = length(v)
            if (stdize == "scale") {
                mu = mean(v)
                delta = mu - center
                ss = ss + sum((v - mu)^2) + delta^2 * count * n / (count + n)
                center = center + delta * n / (count + n)
                count = count + n
            } else {
                lo = min(lo, min(v))
                hi = max(hi, max(v))
            }
        }
        if (is.null(shift)) {
            shift = switch(stdize, scale = center, box = lo, symbox = (lo + hi) / 2)
        }
        if (is.null(scale)) {
            scale = if (stdize == "scale") sqrt(ss / (count - 1)) else hi - lo
        }
    }
    if (!is.numeric(shift) || length(shift) != 1L || !is.finite(shift) ||
        !is.numeric(scale) || length(scale) != 1L || !is.finite(scale) || scale <= 0) {
        abort("Input standardization requires a finite scalar shift and positive scale")
    }
    list(shift = shift, scale = scale)
}

scatter_context <- function(x, use_fft, max_spacing = 1L) {
    x = as.matrix(x)
    spacing = attr(x, "spacing")
    origin = attr(x, "origin")
    if (is.null(spacing)) spacing = 1L
    if (is.null(origin)) origin = c(1L, 1L)
    context = list(x = x, spacing = spacing, origin = origin)
    if (use_fft) {
        # Make every later sampling factor divide the FFT dimensions exactly.
        factor = max(1, max_spacing / spacing)
        nr = stats::nextn(ceiling(nrow(x) / factor)) * factor
        nc = stats::nextn(ceiling(ncol(x) / factor)) * factor
        padded = matrix(0, nr, nc)
        padded[seq_len(nrow(x)), seq_len(ncol(x))] = x
        context$spectrum = stats::fft(padded)
    }
    context
}

scatter_spatial_filter <- function(spec, wavelet = TRUE) {
    pos = seq.int(-spec$radius, spec$radius)
    if (!wavelet) {
        g = exp(-outer(pos^2, pos^2, "+") / (2 * spec$sigma^2))
        return(g / sum(g))
    }
    along = outer(pos * sin(spec$theta), pos * cos(spec$theta), "+")
    across = outer(pos * cos(spec$theta), -pos * sin(spec$theta), "+")
    g = exp(-(along^2 + spec$slant^2 * across^2) / (2 * spec$sigma^2))
    carrier = exp(1i * spec$frequency * along)
    (carrier - sum(g * carrier) / sum(g)) * g / sum(g)
}

scatter_frequencies <- function(n) {
    k = seq_len(n) - 1L
    k[k > floor((n - 1L) / 2)] = k[k > floor((n - 1L) / 2)] - n
    2 * pi * k / n
}

# Evaluate the Fourier transform of a Gaussian-envelope Morlet analytically.
# Sum neighboring frequency copies to account for sampling on a pixel lattice.
# No spatial coefficient array or filter FFT is needed for this backend.
scatter_fourier_filter <- function(spec, nr, nc, wavelet = TRUE) {
    wy = scatter_frequencies(nr)
    wx = scatter_frequencies(nc)
    low = matrix(0, nr, nc)
    band = if (wavelet) matrix(0, nr, nc) else NULL
    theta = if (wavelet) spec$theta else 0
    slant = if (wavelet) spec$slant else 1
    along = outer(wy * sin(theta), wx * cos(theta), "+")
    across = outer(wy * cos(theta), -wx * sin(theta), "+")
    for (a in -1:1) {
        for (b in -1:1) {
            u = along + 2 * pi * (a * sin(theta) + b * cos(theta))
            v = across + 2 * pi * (a * cos(theta) - b * sin(theta))
            low = low + exp(-0.5 * spec$sigma^2 * (u^2 + (v / slant)^2))
            if (wavelet) {
                band = band + exp(-0.5 * spec$sigma^2 * (
                    (u - spec$frequency)^2 + (v / slant)^2
                ))
            }
        }
    }
    if (!wavelet) return(low / low[1, 1])
    out = (band - (band[1, 1] / low[1, 1]) * low) / low[1, 1]
    out[1, 1] = 0
    out
}

# Folding is the Fourier-domain equivalent of retaining every factor-th pixel.
# Taking the mean of aliases preserves the inverse transform's normalization.
scatter_fold_spectrum <- function(x, factor) {
    if (factor == 1L) return(x)
    nr = nrow(x) / factor
    nc = ncol(x) / factor
    out = matrix(0 + 0i, nr, nc)
    for (j in seq_len(factor) - 1L) {
        for (i in seq_len(factor) - 1L) {
            out = out + x[i * nr + seq_len(nr), j * nc + seq_len(nc), drop = FALSE]
        }
    }
    out / factor^2
}

# All ids in one call share a scale and support, allowing a single patch matrix.
# id = 0 selects the real Gaussian averaging filter instead of a wavelet group.
scatter_convolve <- function(context, ids, bank, cache, use_fft) {
    wavelet = !identical(ids, 0L)
    specs = if (wavelet) {
        lapply(ids, function(id) as.list(bank$wavelets[id, ]))
    } else list(bank$average)
    target_spacing = if (wavelet) 2^specs[[1]]$j else 2^bank$scales
    factor = as.integer(target_spacing / context$spacing)
    if (factor < 1L) abort("Cannot apply a finer filter to a coarser map")
    specs = lapply(specs, function(spec) {
        spec$radius = ceiling(spec$radius / context$spacing)
        spec$sigma = spec$sigma / context$spacing
        if (wavelet) spec$frequency = spec$frequency * context$spacing
        spec
    })
    radius = specs[[1]]$radius
    h = nrow(context$x)
    w = ncol(context$x)
    # Align every branch to original pixel 1 rather than to its cropped edge.
    offset = (-(context$origin - 1L) / context$spacing) %% factor
    first = ceiling((radius - offset) / factor)
    last = floor((c(h, w) - radius - 1L - offset) / factor)
    if (any(first > last)) abort("Image is too small for the sampled filter path")
    rows = seq.int(first[1] + 1L, last[1] + 1L)
    cols = seq.int(first[2] + 1L, last[2] + 1L)
    origin = context$origin + context$spacing * (offset + factor * first)
    finish = function(z) {
        z = if (wavelet) Mod(z) else Re(z)
        attr(z, "spacing") = target_spacing
        attr(z, "origin") = origin
        z
    }
    if (use_fft) {
        nr = nrow(context$spectrum)
        nc = ncol(context$spectrum)
        spectrum = context$spectrum
        if (any(offset != 0L)) {
            phase_key = paste("phase", nr, nc, offset[1], offset[2], sep = "_")
            if (!exists(phase_key, cache, inherits = FALSE)) {
                cache[[phase_key]] = outer(
                    exp(1i * scatter_frequencies(nr) * offset[1]),
                    exp(1i * scatter_frequencies(nc) * offset[2]), "*"
                )
            }
            spectrum = spectrum * cache[[phase_key]]
        }
        return(lapply(seq_along(ids), function(i) {
            key = paste("fft", ids[i], context$spacing, nr, nc, sep = "_")
            if (!exists(key, cache, inherits = FALSE)) {
                cache[[key]] = scatter_fourier_filter(specs[[i]], nr, nc, wavelet)
            }
            filtered = scatter_fold_spectrum(spectrum * cache[[key]], factor)
            z = stats::fft(filtered, inverse = TRUE) / length(filtered)
            finish(z[rows, cols, drop = FALSE])
        }))
    }
    kernels = lapply(seq_along(ids), function(i) {
        key = paste("direct", ids[i], context$spacing, sep = "_")
        if (!exists(key, cache, inherits = FALSE)) {
            # im2col multiplies patches without reversing them; reverse here
            # to compute convolution using the spatial wavelet convention.
            k = scatter_spatial_filter(specs[[i]], wavelet)
            cache[[key]] = c(k[nrow(k):1L, ncol(k):1L])
        }
        cache[[key]]
    })
    kernels = do.call(cbind, kernels)
    start = offset + 1L + factor * first - radius
    x = context$x[seq.int(start[1], h), seq.int(start[2], w), drop = FALSE]
    patches = im2col(as.double(x), as.integer(nrow(x)), as.integer(ncol(x)), 1L,
                     as.integer(2 * radius + 1L), as.integer(2 * radius + 1L), factor)
    n = length(ids)
    if (wavelet) {
        z = patches %*% cbind(Re(kernels), Im(kernels))
        lapply(seq_len(n), function(i) {
            finish(matrix(complex(real = z[, i], imaginary = z[, n + i]),
                          length(rows), length(cols)))
        })
    } else {
        list(finish(matrix(patches %*% kernels, length(rows), length(cols))))
    }
}

scatter_pool <- function(x, bank, grid, cache, use_fft) {
    if (is.null(grid)) return(mean(x))
    context = scatter_context(x, use_fft, 2^bank$scales)
    smoothed = scatter_convolve(context, 0L, bank, cache, use_fft)[[1L]]
    origin = attr(smoothed, "origin")
    spacing = attr(smoothed, "spacing")
    rows = as.integer((grid$row - origin[1]) / spacing + 1L)
    cols = as.integer((grid$col - origin[2]) / spacing + 1L)
    c(smoothed[rows, cols, drop = FALSE])
}

#' @export
predict.b_scatter <- function(object, newdata, ...) {
    if (missing(newdata)) return(object)
    rlang::eval_tidy(makepredictcall.b_scatter(object, attr(object, "call")), newdata)
}

#' @export
makepredictcall.b_scatter <- function(var, call) {
    if (
        as.character(call)[1L] == "b_scatter" ||
            (is.call(call) && identical(eval(call[[1L]]), b_scatter))
    ) {
        at = attributes(var)[c("bank", "scales", "angles", "order", "pooling",
                               "fft", "stdize", "shift", "scale")]
        call[names(at)] = at
    }
    call
}
