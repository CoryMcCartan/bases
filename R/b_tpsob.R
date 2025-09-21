#' Tensor-product Sobolev space basis
#'
#' Generates features from a tensor-product Sobolev space basis for estimating
#' functions in a Sobolev space with dominating mixed derivatives. Basis
#' functions are of the form \deqn{
#'     \psi_{\mathbf{j}}(\mathbf{x}) = \prod_{k=1}^d \psi_{j_k}(x_k),
#' } where \deqn{
#'      \phi_1(x) = 1 \quad\text{and}\quad
#'      \phi_j(x) = \sqrt{2}\cos(\pi (j-1) x).
#' }
#'
b_tpsob <- function(..., p = 100, stdize = c("scale", "box", "symbox", "none"),
                    shift = NULL, scale = NULL) {

}