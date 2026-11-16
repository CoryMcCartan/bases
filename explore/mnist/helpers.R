# Load the working R code and current compiled helpers without pkgload, which
# could regenerate native files outside this experiment directory.
load_basis_code <- function() {
    root = normalizePath("../..")
    dll = dyn.load(file.path(root, "src", "bases.so"))
    env = new.env(parent = asNamespace("bases"))
    env$quick = function(fun, name) {
        symbol = paste0(name, "_")
        env[[symbol]] = getNativeSymbolInfo(symbol, dll)$address
        body(fun) = as.call(c(quote(.External), as.name(symbol),
                             lapply(names(formals(fun)), as.name)))
        fun
    }
    sys.source(file.path(root, "R", "transform.R"), env)
    sys.source(file.path(root, "R", "b_conv.R"), env)
    sys.source(file.path(root, "explore", "b_scatter.R"), env)
    env
}

image_list <- function(pixels) {
    lapply(seq_len(nrow(pixels)), function(i) {
        matrix(as.double(pixels[i, ]) / 255, 28, 28, byrow = TRUE)
    })
}

pad_images <- function(images, pad) {
    lapply(images, function(img) {
        out = matrix(0, 28 + 2 * pad, 28 + 2 * pad)
        out[pad + seq_len(28), pad + seq_len(28)] = img
        out
    })
}

scatter_padding <- function(basis, scales, pooling) {
    bank = basis$scatter_bank(scales, 6)
    paths = basis$scatter_paths(bank, 2, pooling)
    ceiling(max(paths$left, paths$right) / 2^scales) * 2^scales
}

scatter_features <- function(basis, images, scales, pooling, fft, pad) {
    z = basis$b_scatter(pad_images(images, pad), scales = scales, angles = 6,
                       order = 2, pooling = pooling, fft = fft, stdize = "none")
    if (pooling == "local") {
        # Retain summaries centered on original digit pixels, not on the added
        # background. Column order is channel, path, then column-major location.
        grid = attr(z, "grid")
        positions = expand.grid(row = grid$row, col = grid$col)
        keep_pos = with(positions, row >= pad + 1 & row <= pad + 28 &
                            col >= pad + 1 & col <= pad + 28)
        keep = rep(keep_pos, nrow(attr(z, "paths")))
        z = z[, keep, drop = FALSE]
    }
    matrix(as.numeric(z), nrow(z), ncol(z), dimnames = dimnames(z))
}

log_progress <- function(...) {
    cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
    flush.console()
}

local_max_pool <- function(size, grid = 4L) {
    side = 28L - size + 1L
    groups = split(seq_len(side), ceiling(seq_len(side) * grid / side))
    function(values) {
        map = matrix(values, side, side)
        unlist(lapply(groups, function(cols) {
            vapply(groups, function(rows) max(map[rows, cols, drop = FALSE]), 0.0)
        }), use.names = FALSE)
    }
}
