test_that("invarants are as expected", {
    for (trees in c(0, 1, 5, 100)) {
        X = with(mtcars, b_bart(cyl, disp, hp, drat, wt, trees = trees))
        expect_true(all(rowSums(X) == rowSums(X)[1]))
        expect_true(all(rowSums(X) <= trees))
        expect_true(all(colSums(X) > 0))
    }
})

test_that("predict() method works correctly", {
    m = ridge(mpg ~ b_bart(disp, cyl, hp, wt, trees = 50), mtcars[1:20, ])

    expect_equal(predict(m, mtcars[1:20, ]), fitted(m), tolerance = 1e-5)

    pred_m = suppressWarnings(predict(m, newdata = mtcars))
    expect_equal(pred_m[1:20], fitted(m)[1:20], tolerance = 1e-5)

    B = with(mtcars, b_bart(disp, cyl, hp, wt, trees = 50))
    expect_equal(predict(B, newdata = mtcars), predict(B, newdata = mtcars))
})
