#' Random convolutional features
#'
#' Generates random convolutional features from a list of images, where each
#' image is an array or matrix of pixel values. The convolutional kernels have
#' entries drawn iid from a standard Normal distribution.
#'
#' @param x A list of images, each an array or matrix of pixel values between 0
#    and 1. This is the format used by most image reading functions in R.
#' @param p The number of features to generate.
#' @param size The size of the convolutional kernel, e.g., 3 means a 3x3 kernel.
#' @param stride The stride of the convolution, i.e., how many pixels to skip
#'   between each convolution. The default is the same as `size` so that the
#'   convolution areas tile the image.
#' @param activation The activation function to apply to the convolved images so
#'   that each convolution kernel produces a single feature. The default `"max"`
#'   is also known as "max pooling"; "mean" is average pooling, and "ppv" is
#'   the proportion of positive pixel values after convolution.
#' @param kernels A matrix of convolutional kernels, where each column is a
#'   kernel in column-major format. Overrides `p` and `size` if provided.
#'
#' @returns A matrix of random convolutional features.
#'
#' @examples
#' FALSE
#'
#' @export
b_conv <- function(
    x,
    p = 100,
    size = 3,
    stride = size,
    activation = c("max", "mean", "ppv"),
    kernels = NULL
) {}
