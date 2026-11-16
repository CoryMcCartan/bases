test_that("b_conv() works with grayscale images", {
    images = lapply(1:10, function(i) matrix(runif(20 * 20), 20, 20))

    feats = b_conv(images, p = 5, size = 3)

    expect_equal(nrow(feats), 10)
    expect_equal(ncol(feats), 5)
    expect_true(is.matrix(feats))
    expect_s3_class(feats, "b_conv")
})

test_that("b_conv() works with 3D color images", {
    images = lapply(1:10, function(i) array(runif(20 * 20 * 3), dim = c(20, 20, 3)))

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
    images = lapply(1:5, function(i) matrix(runif(10 * 10), 10, 10))

    feats = b_conv(images, p = 10, size = 3, kernel_gen = "rnorm")

    expect_equal(ncol(feats), 10)
    kernels = attr(feats, "kernels")
    expect_equal(nrow(kernels), 9) # 3x3 kernel for grayscale
    expect_equal(ncol(kernels), 10)
})

test_that("b_conv() 'patch' kernel generation works", {
    images = lapply(1:5, function(i) matrix(runif(10 * 10), 10, 10))

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
    images = lapply(1:5, function(i) matrix(runif(10 * 10), 10, 10))

    feats_max = b_conv(images, p = 5, size = 3, activation = max)
    feats_mean = b_conv(images, p = 5, size = 3, activation = mean)
    feats_min = b_conv(images, p = 5, size = 3, activation = min)

    expect_equal(dim(feats_max), dim(feats_mean))
    expect_equal(dim(feats_max), dim(feats_min))
    expect_false(isTRUE(all.equal(feats_max, feats_mean)))
})

test_that("b_conv() multivariate pooling works", {
    images = lapply(1:5, function(i) matrix(runif(10 * 10), 10, 10))

    # Pooling function that returns two values
    multi_pool = function(x) c(max(x), min(x))

    feats = b_conv(images, p = 3, size = 3, activation = multi_pool)

    expect_equal(nrow(feats), 5)
    expect_equal(ncol(feats), 6) # 3 kernels * 2 values each
})

test_that("b_conv() standardization works correctly", {
    images = lapply(1:5, function(i) matrix(runif(10 * 10), 10, 10))

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
    images = lapply(1:10, function(i) matrix(runif(20 * 20), 20, 20))

    B = b_conv(images, p = 5, size = 3)

    expect_equal(predict(B), B)
})

test_that("predict.b_conv() works with new data", {
    images_train = lapply(1:10, function(i) matrix(runif(20 * 20), 20, 20))
    images_test = lapply(1:5, function(i) matrix(runif(20 * 20), 20, 20))

    y = rnorm(10)
    m = lm(y ~ b_conv(images_train, p = 5, size = 3))

    pred = predict(m, newdata = list(images_train = images_test))

    expect_equal(length(pred), 5)
    expect_true(all(is.finite(pred)))
})

test_that("predict.b_conv() works with different image sizes", {
    images_train = lapply(1:10, function(i) matrix(runif(20 * 20), 20, 20))
    images_test = lapply(1:5, function(i) matrix(runif(25 * 25), 25, 25))

    y = rnorm(10)
    m = lm(y ~ b_conv(images_train, p = 5, size = 3))

    pred = predict(m, newdata = list(images_train = images_test))

    expect_equal(length(pred), 5)
    expect_true(all(is.finite(pred)))
})

test_that("predict.b_conv() is reproducible with same data", {
    images = lapply(1:10, function(i) matrix(runif(20 * 20), 20, 20))

    B = b_conv(images, p = 5, size = 3)

    pred1 = predict(B, newdata = list(images = images))
    pred2 = predict(B, newdata = list(images = images))

    expect_equal(pred1, pred2)
})

test_that("b_conv() works with stride parameter", {
    images = lapply(1:5, function(i) matrix(runif(20 * 20), 20, 20))

    feats_stride1 = b_conv(images, p = 5, size = 3, stride = 1)
    feats_stride2 = b_conv(images, p = 5, size = 3, stride = 2)

    expect_equal(dim(feats_stride1), dim(feats_stride2))
    # Different strides should give different features
    expect_false(isTRUE(all.equal(feats_stride1, feats_stride2)))
})

test_that("b_conv() retains all valid strided patches", {
    img = array(as.double(1:(8 * 9 * 2)), c(8, 9, 2))

    for (size in 2:5) {
        kernel = matrix(seq_len(size^2), size, size)
        for (stride in c(1, 2, 4, 10)) {
            expected = c()
            for (ch in 1:2) {
                for (j in seq(1, 9 - size + 1, by = stride)) {
                    for (i in seq(1, 8 - size + 1, by = stride)) {
                        patch = img[i:(i + size - 1), j:(j + size - 1), ch]
                        expected = c(expected, sum(patch * kernel))
                    }
                }
            }
            feats = b_conv(
                list(img),
                kernels = matrix(c(kernel), ncol = 1),
                size = size,
                stride = stride,
                activation = identity,
                stdize = "none"
            )
            expect_equal(as.numeric(feats), expected)
        }
    }

    feats = b_conv(
        list(matrix(as.double(1:9), 3, 3)),
        kernels = matrix(1, 9, 1),
        stride = 4,
        activation = identity,
        stdize = "none"
    )
    expect_equal(as.numeric(feats), 45)
})

test_that("b_conv() integrates with ridge()", {
    images = lapply(1:20, function(i) matrix(runif(15 * 15), 15, 15))
    y = rnorm(20)

    m = ridge(y ~ b_conv(images, p = 10, size = 3))

    expect_s3_class(m, "ridge")
    expect_equal(length(fitted(m)), 20)
})

test_that("b_conv() with custom kernels works", {
    images = lapply(1:5, function(i) matrix(runif(10 * 10), 10, 10))

    # Provide custom kernels
    custom_kernels = matrix(rnorm(9 * 4), nrow = 9, ncol = 4)

    feats = b_conv(images, kernels = custom_kernels, size = 3)

    expect_equal(ncol(feats), 4)
    expect_equal(attr(feats, "kernels"), custom_kernels)
})

test_that("b_conv() infers size from custom kernels before checking images", {
    img = matrix(as.double(1:12), 3, 4)
    kernels = matrix(1, 4, 2)
    feats = b_conv(
        list(img),
        kernels = kernels,
        size = 100,
        p = 99,
        kernel_gen = "unused",
        activation = mean,
        stdize = "none"
    )

    expect_equal(attr(feats, "size"), 2)
    expect_equal(dim(feats), c(1, 2))
    expect_equal(as.numeric(feats), c(26, 26))

    feats_default = b_conv(list(img), kernels = kernels, stdize = "none")
    expect_equal(attr(feats_default, "size"), 2)

    expect_error(b_conv(list(img), kernels = 1:4), "must be a matrix")
    expect_error(
        b_conv(list(img), kernels = matrix(1, 6, 2)),
        "positive perfect square"
    )
    expect_error(
        b_conv(list(img), kernels = matrix(numeric(), 0, 2)),
        "positive perfect square"
    )
    expect_error(
        b_conv(list(img), kernels = matrix(1, 25, 1)),
        "at least as large"
    )
})

test_that("b_conv() with 3D images has correct kernel size", {
    images = lapply(1:5, function(i) array(runif(10 * 10 * 3), dim = c(10, 10, 3)))

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
    expect_identical(new_call$pool_grid, c(1L, 1L))
})

test_that("im2col() matches direct patch extraction", {
    for (dims in list(c(8L, 9L, 2L), c(3L, 3L, 1L), c(18L, 23L, 3L))) {
        x = as.double(seq_len(prod(dims)))
        img = array(x, dims)
        for (size in seq_len(min(dims[1:2]))) {
            for (stride in c(1L, 2L, 4L, 25L)) {
                patches = list()
                for (ch in seq_len(dims[3])) {
                    for (j in seq(1, dims[2] - size + 1, by = stride)) {
                        for (i in seq(1, dims[1] - size + 1, by = stride)) {
                            patches[[length(patches) + 1L]] = c(
                                img[i:(i + size - 1), j:(j + size - 1), ch]
                            )
                        }
                    }
                }
                expected = do.call(rbind, patches)
                actual = im2col(x, dims[1], dims[2], dims[3], size, stride)
                expect_identical(actual, expected)
            }
        }
    }
})

test_that("pool_max() matches columnwise max", {
    global_max = function(x) {
        c(pool_max(
        x, as.integer(nrow(x)), 1L, 1L, c(1L, as.integer(nrow(x)) + 1L), 1:2
    ))
    }
    for (dims in list(c(7, 4), c(1, 4), c(7, 1))) {
        x = matrix(rnorm(prod(dims)), dims[1], dims[2])
        expect_identical(global_max(x), c(apply(x, 2, max)))
    }

    x = matrix(c(-Inf, -Inf, -3, 2, Inf, 1), 2, 3)
    expect_identical(global_max(x), c(-Inf, 2, Inf))
    expect_identical(global_max(matrix(numeric(), 3, 0)), numeric())
})

test_that("pool_grid pools the correct spatial regions in the documented order", {
    img = array(as.double(seq_len(9 * 13 * 3)), c(9, 13, 3))
    kernels = matrix(c(seq_len(9), -seq_len(9)), 9, 2)
    grid = c(3, 4)
    starts_h = seq(1, 9 - 3 + 1, by = 2)
    starts_w = seq(1, 13 - 3 + 1, by = 2)
    maps = array(0, c(length(starts_h), length(starts_w), 3, 2))
    for (k in 1:2) {
        for (ch in 1:3) {
            for (j in seq_along(starts_w)) {
                for (i in seq_along(starts_h)) {
                    patch = img[starts_h[i] + 0:2, starts_w[j] + 0:2, ch]
                    maps[i, j, ch, k] = sum(c(patch) * kernels[, k])
                }
            }
        }
    }
    for (activation in list(max, mean, function(x) c(max(x), min(x)))) {
        expected = c()
        for (k in 1:2) {
            for (j in seq_len(grid[2])) {
                for (i in seq_len(grid[1])) {
                    rows = seq(
                        ceiling((i - 1) * dim(maps)[1] / grid[1]) + 1,
                        ceiling(i * dim(maps)[1] / grid[1])
                    )
                    cols = seq(
                        ceiling((j - 1) * dim(maps)[2] / grid[2]) + 1,
                        ceiling(j * dim(maps)[2] / grid[2])
                    )
                    expected = c(expected, activation(c(maps[rows, cols, , k])))
                }
            }
        }
        actual = b_conv(
            list(img),
            kernels = kernels,
            stride = 2,
            activation = activation,
            pool_grid = grid,
            stdize = "none"
        )
        expect_equal(as.numeric(actual), expected)
    }
})

test_that("pool_grid accepts scalar grids and rejects invalid or empty cells", {
    images = list(matrix(runif(10 * 12), 10, 12))
    kernels = matrix(rnorm(9 * 2), 9, 2)
    scalar = b_conv(images, kernels = kernels, pool_grid = 4)
    vector = b_conv(images, kernels = kernels, pool_grid = c(4, 4))
    expect_identical(as.numeric(scalar), as.numeric(vector))
    expect_equal(dim(scalar), c(1, 32))
    expect_identical(attr(scalar, "pool_grid"), c(4L, 4L))

    invalid = list(0, -1, 1.5, NA, NaN, Inf, "4", numeric(), 1:3, TRUE, 1+0i, 1e10)
    for (grid in invalid) {
        expect_error(b_conv(images, pool_grid = grid), "`pool_grid` must be")
    }
    expect_error(b_conv(images, pool_grid = c(9, 2)), "response-map dimensions")
    expect_error(b_conv(images, stride = 2, pool_grid = 5), "response-map dimensions")
})

test_that("global grid pooling preserves max, mean, and custom summaries", {
    images = list(matrix(rnorm(8 * 9), 8, 9), array(rnorm(10 * 11 * 2), c(10, 11, 2)))
    kernels = matrix(rnorm(4 * 3), 4, 3)
    for (activation in list(max, mean, function(x) c(mean(x), sd(x)))) {
        out = b_conv(images, kernels = kernels, activation = activation, stdize = "none")
        for (i in seq_along(images)) {
            img = images[[i]]
            d = dim(img)
            if (length(d) == 2) {
                d = c(d, 1L)
            }
            responses = im2col(as.double(img), d[1], d[2], d[3], 2L, 1L) %*% kernels
            expect_equal(as.numeric(out[i, ]), c(apply(responses, 2, activation)))
        }
    }
})

test_that("grid pooling preserves missing and complex response behavior", {
    images = list(matrix(rnorm(8 * 9), 8, 9))
    for (value in list(NA_real_, NaN, 1+1i)) {
        kernels = matrix(c(value, 2, 3, 4), 4, 1)
        for (activation in if (is.complex(value)) list(mean) else list(max, mean)) {
            out = b_conv(
                images,
                kernels = kernels,
                activation = activation,
                pool_grid = c(2, 3),
                stdize = "none"
            )
            ref = b_conv(
                images,
                kernels = kernels,
                activation = function(x) activation(x),
                pool_grid = c(2, 3),
                stdize = "none"
            )
            expect_identical(as.vector(out), as.vector(ref))
        }
    }
    expect_error(
        b_conv(images, kernels = matrix(1 + 1i, 4, 1), pool_grid = 2, stdize = "none"),
        "complex"
    )
})

test_that("prediction freezes pool_grid and supports different image sizes", {
    images = lapply(1:20, function(i) matrix(runif(10 * 12), 10, 12))
    new_images = lapply(1:5, function(i) matrix(runif(12 * 14), 12, 14))
    grid = c(2, 3)
    B = b_conv(images, p = 2, pool_grid = grid)
    grid = 1
    pred = predict(B, newdata = list(images = new_images))
    expect_equal(dim(pred), c(5, 12))
    expect_identical(attr(pred, "pool_grid"), c(2L, 3L))

    y = rnorm(20)
    fit = lm(y ~ b_conv(images, p = 2, pool_grid = c(2, 3)))
    expect_length(predict(fit, newdata = list(images = new_images)), 5)

    old = b_conv(images, p = 2)
    attr(old, "pool_grid") = NULL
    expect_equal(ncol(predict(old, newdata = list(images = new_images))), 2)
})

test_that("b_conv() preserves max pooling results", {
    images = list(matrix(as.double(1:30), 5, 6))
    kernels = matrix(c(-1, 2, -3, 4, 2, 3, 4, 5), 4, 2)
    fast = b_conv(images, kernels = kernels, activation = max, stdize = "none")
    reference = b_conv(
        images,
        kernels = kernels,
        activation = function(x) max(x),
        stdize = "none"
    )
    expect_identical(as.numeric(fast), as.numeric(reference))

    kernels[1, 1] = NaN
    fast = b_conv(images, kernels = kernels, activation = max, stdize = "none")
    reference = b_conv(
        images,
        kernels = kernels,
        activation = function(x) max(x),
        stdize = "none"
    )
    expect_identical(as.numeric(fast), as.numeric(reference))

    kernels[1, 1] = NA_real_
    fast = b_conv(images, kernels = kernels, activation = max, stdize = "none")
    reference = b_conv(
        images,
        kernels = kernels,
        activation = function(x) max(x),
        stdize = "none"
    )
    expect_identical(as.numeric(fast), as.numeric(reference))
})
