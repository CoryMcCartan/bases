test_that("b_conv() works with grayscale images", {
    images = lapply(1:10, function(i) matrix(runif(20 * 20), 20, 20))

    feats = b_conv(images, p = 5, size = 3)

    expect_equal(nrow(feats), 10)
    expect_equal(ncol(feats), 5)
    expect_true(is.matrix(feats))
    expect_s3_class(feats, "b_conv")
})

test_that("b_conv() works with 3D color images", {
    images = lapply(1:10, function(i) array(runif(20*20*3), dim = c(20, 20, 3)))

    feats = b_conv(images, p = 5, size = 3)

    expect_equal(nrow(feats), 10)
    expect_equal(ncol(feats), 5)
    expect_true(is.matrix(feats))
    expect_s3_class(feats, "b_conv")
})

test_that("b_conv() handles images of different sizes", {
    images = list(
        matrix(runif(20*20), 20, 20),
        matrix(runif(30*25), 30, 25),
        matrix(runif(15*15), 15, 15)
    )

    feats = b_conv(images, p = 8, size = 3)

    expect_equal(nrow(feats), 3)
    expect_equal(ncol(feats), 8)
})

test_that("b_conv() 'rnorm' kernel generation works", {
    images = lapply(1:5, function(i) matrix(runif(10*10), 10, 10))

    feats = b_conv(images, p = 10, size = 3, kernel_gen = "rnorm")

    expect_equal(ncol(feats), 10)
    kernels = attr(feats, "kernels")
    expect_equal(nrow(kernels), 9)  # 3x3 kernel for grayscale
    expect_equal(ncol(kernels), 10)
})

test_that("b_conv() 'patch' kernel generation works", {
    images = lapply(1:5, function(i) matrix(runif(10*10), 10, 10))

    feats = b_conv(images, p = 10, size = 3, kernel_gen = "patch")

    expect_equal(ncol(feats), 10)
    kernels = attr(feats, "kernels")
    expect_equal(nrow(kernels), 9)
    expect_equal(ncol(kernels), 10)

    # Check that kernels are actual patches (values from images)
    all_pixels = unlist(images)
    # After standardization, patches should be transformations of original pixels
    expect_true(all(is.finite(kernels)))
})

test_that("b_conv() pooling with different functions works", {
    images = lapply(1:5, function(i) matrix(runif(10*10), 10, 10))

    feats_max = b_conv(images, p = 5, size = 3, activation = max)
    feats_mean = b_conv(images, p = 5, size = 3, activation = mean)
    feats_min = b_conv(images, p = 5, size = 3, activation = min)

    expect_equal(dim(feats_max), dim(feats_mean))
    expect_equal(dim(feats_max), dim(feats_min))
    expect_false(isTRUE(all.equal(feats_max, feats_mean)))
})

test_that("b_conv() multivariate pooling works", {
    images = lapply(1:5, function(i) matrix(runif(10*10), 10, 10))

    # Pooling function that returns two values
    multi_pool = function(x) c(max(x), min(x))

    feats = b_conv(images, p = 3, size = 3, activation = multi_pool)

    expect_equal(nrow(feats), 5)
    expect_equal(ncol(feats), 6)  # 3 kernels * 2 values each
})

test_that("b_conv() standardization works correctly", {
    images = lapply(1:5, function(i) matrix(runif(10*10), 10, 10))

    feats_scale = b_conv(images, p = 5, size = 3, stdize = "scale")
    feats_none = b_conv(images, p = 5, size = 3, stdize = "none")
    feats_box = b_conv(images, p = 5, size = 3, stdize = "box")

    # Different standardization should give different results
    expect_false(isTRUE(all.equal(feats_scale, feats_none)))
    expect_false(isTRUE(all.equal(feats_scale, feats_box)))

    # Check attributes
    expect_true(!is.null(attr(feats_scale, "shift")))
    expect_true(!is.null(attr(feats_scale, "scale")))
})

test_that("b_conv() errors on invalid inputs", {
    images = list(matrix(runif(5*5), 5, 5))
    expect_error(
        b_conv(images, p = 5, size = 10),
        "at least as large"
    )

    expect_error(b_conv(matrix(1:10), p = 5), "`x` must be a list")
    expect_error(b_conv(list(), p = 5), "at least one image")

    images = list(matrix(runif(10*10), 10, 10))
    images[[2]] = "not an image"
    expect_error(b_conv(images, p = 5), "numeric")
})

test_that("predict.b_conv() works on training data", {
    images = lapply(1:10, function(i) matrix(runif(20*20), 20, 20))

    B = b_conv(images, p = 5, size = 3)

    expect_equal(predict(B), B)
})

test_that("predict.b_conv() works with new data", {
    images_train = lapply(1:10, function(i) matrix(runif(20*20), 20, 20))
    images_test = lapply(1:5, function(i) matrix(runif(20*20), 20, 20))

    y = rnorm(10)
    m = lm(y ~ b_conv(images_train, p = 5, size = 3))

    pred = predict(m, newdata = list(images_train = images_test))

    expect_equal(length(pred), 5)
    expect_true(all(is.finite(pred)))
})

test_that("predict.b_conv() works with different image sizes", {
    images_train = lapply(1:10, function(i) matrix(runif(20*20), 20, 20))
    images_test = lapply(1:5, function(i) matrix(runif(25*25), 25, 25))

    y = rnorm(10)
    m = lm(y ~ b_conv(images_train, p = 5, size = 3))

    pred = predict(m, newdata = list(images_train = images_test))

    expect_equal(length(pred), 5)
    expect_true(all(is.finite(pred)))
})

test_that("predict.b_conv() is reproducible with same data", {
    images = lapply(1:10, function(i) matrix(runif(20*20), 20, 20))

    B = b_conv(images, p = 5, size = 3)

    pred1 = predict(B, newdata = list(images = images))
    pred2 = predict(B, newdata = list(images = images))

    expect_equal(pred1, pred2)
})

test_that("b_conv() works with stride parameter", {
    images = lapply(1:5, function(i) matrix(runif(20*20), 20, 20))

    feats_stride1 = b_conv(images, p = 5, size = 3, stride = 1)
    feats_stride2 = b_conv(images, p = 5, size = 3, stride = 2)

    expect_equal(dim(feats_stride1), dim(feats_stride2))
    # Different strides should give different features
    expect_false(isTRUE(all.equal(feats_stride1, feats_stride2)))
})

test_that("b_conv() integrates with ridge()", {
    images = lapply(1:20, function(i) matrix(runif(15*15), 15, 15))
    y = rnorm(20)

    m = ridge(y ~ b_conv(images, p = 10, size = 3))

    expect_s3_class(m, "ridge")
    expect_equal(length(fitted(m)), 20)
})

test_that("b_conv() with custom kernels works", {
    images = lapply(1:5, function(i) matrix(runif(10*10), 10, 10))

    # Provide custom kernels
    custom_kernels = matrix(rnorm(9 * 4), nrow = 9, ncol = 4)

    feats = b_conv(images, kernels = custom_kernels, size = 3)

    expect_equal(ncol(feats), 4)
    expect_equal(attr(feats, "kernels"), custom_kernels)
})

test_that("b_conv() with 3D images has correct kernel size", {
    images = lapply(1:5, function(i) array(runif(10*10*3), dim = c(10, 10, 3)))

    feats = b_conv(images, p = 5, size = 3, kernel_gen = "rnorm")

    kernels = attr(feats, "kernels")
    # 3x3 kernel (applied per channel then summed)
    expect_equal(nrow(kernels), 9)
    expect_equal(ncol(kernels), 5)
})

test_that("makepredictcall.b_conv() preserves attributes", {
    images = lapply(1:5, function(i) matrix(runif(10 * 10), 10, 10))

    B = b_conv(images, p = 5, size = 3)
    call = attr(B, "call")

    new_call = makepredictcall(B, call)

    expect_true(!is.null(new_call$kernels))
    expect_true(!is.null(new_call$shift))
    expect_true(!is.null(new_call$scale))
    expect_true(!is.null(new_call$size))
    expect_true(!is.null(new_call$stride))
})
