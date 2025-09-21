test_that("NN features improve prediction fit", {
    y = quakes$depth

    m = lm(y ~ lat + long, quakes)
    r2_lm = cor(fitted(m), y)^2

    m = lm(y ~ b_nn(lat, long, p = 32), quakes)
    r2_nn = cor(fitted(m), y)^2

    expect_gt(r2_nn - r2_lm, 0.8)
})

test_that("predict() method works correctly", {
    m = ridge(mpg ~ b_nn(disp, cyl, hp, wt, p = 50), mtcars[1:20, ])

    expect_equal(predict(m, mtcars[1:20, ]), fitted(m), tolerance = 1e-5)

    pred_m = suppressWarnings(predict(m, newdata = mtcars))
    expect_equal(pred_m[1:20], fitted(m)[1:20], tolerance = 1e-5)

    B = with(mtcars, b_nn(disp, cyl, hp, wt, p = 50))
    expect_equal(predict(B, newdata = mtcars), predict(B, newdata = mtcars))
})
