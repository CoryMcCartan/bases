test_that("b_inter() matches manual result", {
    m0 = lm(mpg ~ (disp + cyl + hp + wt)^3, mtcars)
    m1 = lm(mpg ~ b_inter(disp, cyl, hp, wt, depth=3), mtcars)
    expect_equal(fitted(m0), fitted(m1))
})

test_that("predict() method works correctly", {
    m = lm(mpg ~ b_inter(disp, cyl, hp, wt, depth=2), mtcars[1:20, ])

    expect_equal(predict(m), fitted(m))

    pred_m = suppressWarnings(predict(m, newdata=mtcars))
    expect_equal(pred_m[1:20], fitted(m)[1:20])
})
