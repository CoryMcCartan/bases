#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang abort
#' @rawNamespace import(stats, except = c("filter", "lag"))
#' @importFrom methods as
#' @useDynLib bases, .registration = TRUE
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
    rlang::run_on_load()
}

rlang::on_load(rlang::local_use_cli(format = TRUE, inline = TRUE))
