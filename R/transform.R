# Helper to handle rescaling
do_std <- function(
    x,
    stdize = c("scale", "box", "symbox", "none"),
    shift = NULL,
    scale = NULL
) {
    stdize = rlang::arg_match(stdize, error_call = parent.frame())

    if (stdize == "scale") {
        if (is.null(shift)) {
            shift = colMeans(x)
        }
        if (is.null(scale)) scale = apply(x, 2, sd)
    } else if (stdize == "box") {
        rgs = apply(x, 2, range)
        if (is.null(shift)) {
            shift = rgs[1, ]
        }
        if (is.null(scale)) scale = rgs[2, ] - rgs[1, ]
    } else if (stdize == "symbox") {
        rgs = apply(x, 2, range)
        if (is.null(shift)) {
            shift = 0.5 * (rgs[1, ] + rgs[2, ])
        }
        if (is.null(scale)) scale = rgs[2, ] - rgs[1, ]
    } else {
        shift = 0
        scale = 1
    }
    if (length(shift) == 1) {
        shift = rep(shift, ncol(x))
    }
    if (length(scale) == 1) {
        scale = rep(scale, ncol(x))
    }

    if (!all(shift == 0)) {
        x = x - rep(shift, each = nrow(x))
    }
    if (!all(scale == 1)) {
        x = x / rep(scale, each = nrow(x))
    }

    list(x = x, shift = shift, scale = scale)
}
