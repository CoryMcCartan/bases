# Compile during development; installed packages use the generated native code.
# This wrapper must be loaded before the top-level quick() calls.
quick <- function(fun, name) {
    if (nzchar(Sys.getenv("DEVTOOLS_LOAD"))) {
        if (requireNamespace("quickr", quietly = TRUE)) {
            return(quickr::quick(fun, name))
        }
        warning("quickr is not installed; returning uncompiled function")
        return(fun)
    }

    body(fun) <- as.call(c(
        quote(.External),
        as.name(paste0(name, "_")),
        lapply(names(formals(fun)), as.name)
    ))
    fun
}
