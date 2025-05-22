#' Recipe step for basis expansions
#'
#' `step_basis()` is a single function that creates a *specification* of a recipe
#' step that will create new columns that are basis expansions, using any of the
#' basis expansion functions in this package.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose variables for this step.
#'   See [recipes::selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the new columns created by this step from the
#'   original variables will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param fn The basis function to use, e.g., [b_rff()].
#' @param options A list of options for the basis function `fn`.
#' @param object The basis object created once the step has been trained.
#' @param prefix The prefix to use for the new column names. Numbers will be
#'   appended, per [recipes::names0], to create column names.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [recipes::bake()]?
#' @param id A character string that is unique to this step to identify it.
#'
#' @returns An updated version of recipe with the new step added to the sequence
#'   of any existing operations.
#'
#' @details
#' # Tuning Parameters
#'
#' The tuning parameters depend on the chosen basis `fn` and are provided via
#' the `objects` argument.
#'
#' # Case Weights
#'
#' The underlying operation does not use case weights.
#'
#' @examplesIf rlang::is_installed("recipes")
#' rec = recipes::recipe(depth ~ lat + long + mag, quakes)
#' rec_rff = step_basis(rec, lat, long, fn = b_rff,
#'                      options = list(p = 5, kernel = k_rbf(2), stdize="none"))
#' recipes::bake(recipes::prep(rec_rff), new_data=NULL)
#'
#' @concept interfaces
#' @export
step_basis <- function(recipe, ..., role = NA, trained = FALSE,
                       fn = NULL, options = list(), object = NULL,
                       prefix = deparse(substitute(fn)),
                       skip = FALSE, id = recipes::rand_id("basis")) {
    rlang::check_installed("recipes")

    recipes::add_step(
        recipe,
        step_basis_new(
            terms = rlang::enquos(...),
            role = role,
            trained = trained,
            fn = list(rlang::enquo(fn)),
            options = options,
            object = object,
            prefix = prefix,
            skip = skip,
            id = id
        )
    )
}

step_basis_new <- function(terms, role, trained, fn, options, object, prefix, skip, id) {
    rlang::check_installed("recipes")
    recipes::step(subclass = "basis", terms = terms, role = role,
                  trained = trained, fn = fn, options = options,
                  object = object, prefix = prefix, skip = skip, id = id)
}

#' @exportS3Method recipes::prep
prep.step_basis <- function(x, training, info = NULL, ...) {
    rlang::check_installed("recipes")
    col_names <- recipes::recipes_eval_select(x$terms, training, info)
    recipes::check_type(training[, col_names], types = c("double", "integer"))

    qq = x$fn[[1]]
    cc = rlang::call2(rlang::quo_get_expr(qq), !!!c(rlang::syms(unname(col_names)), x$options))
    obj = rlang::eval_tidy(cc, training, rlang::quo_get_env(qq))

    attr(obj, "col_names") = col_names
    colnames(obj) = recipes::names0(ncol(obj), prefix=x$prefix)

    step_basis_new(terms = x$terms, role = x$role, trained = TRUE,
                   fn = x$fn, options= x$options, object = obj,
                   prefix = x$prefix, skip = x$skip, id = x$id)
}

#' @exportS3Method recipes::bake
bake.step_basis <- function(object, new_data, ...) {
    rlang::check_installed("recipes")
    rlang::check_installed("tibble")
    col_names = attr(object$object, "col_names")
    recipes::check_new_data(col_names, object, new_data)

    new_values = if (is.null(new_data)) {
        object$object
    } else {
        predict(object$object, newx=new_data[, col_names])
    }
    new_values = as.data.frame(new_values)
    colnames(new_values) = colnames(object$object)
    new_values = recipes::check_name(new_values, new_data, object, names(new_values))
    new_data = cbind(new_data, new_values)
    new_data = recipes::recipes_remove_cols(new_data, object, col_names)

    tibble::as_tibble(new_data)
}

#' @export
print.step_basis <- function(x, width = max(20, options()$width - 35), ...) {
    rlang::check_installed("recipes")
    if (!is.null(x$object)) {
        title = paste0("`", class(x$object)[1],"()` basis expansion on")
    } else {
        title = "Basis expansion on"
    }
    recipes::print_step(attr(x$object, "col_names"), x$terms, x$trained, title, width)
    invisible(x)
}
