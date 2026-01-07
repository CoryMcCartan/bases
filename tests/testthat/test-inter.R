test_that("b_inter() matches manual result", {
    m0 = lm(mpg ~ (disp + cyl + hp + wt)^3, mtcars)
    m1 = lm(mpg ~ b_inter(disp, cyl, hp, wt, depth = 3), mtcars)
    expect_equal(fitted(m0), fitted(m1))
})

test_that("predict() method works correctly", {
    m = lm(mpg ~ b_inter(disp, cyl, hp, wt, depth = 2), mtcars[1:20, ])

    expect_equal(predict(m), fitted(m))
    expect_equal(predict(m), predict(m, mtcars[1:20, ]))

    pred_m = suppressWarnings(predict(m, newdata = mtcars))
    expect_equal(pred_m[1:20], fitted(m)[1:20])
})

test_that("b_inter() works with entire data frames", {
    # Test with data frame
    df = data.frame(x1 = rnorm(100), x2 = rnorm(100))
    result_df = b_inter(df)
    expect_s3_class(result_df, "b_inter")
    expect_identical(sort(colnames(result_df)), c("x1", "x1:x2", "x2"))

    # Test with matrix with column names
    mat = matrix(rnorm(200), ncol = 2)
    colnames(mat) = c("v1", "v2")
    result_mat = b_inter(mat)
    expect_s3_class(result_mat, "b_inter")
    expect_identical(sort(colnames(result_mat)), c("v1", "v1:v2", "v2"))

    # Test with matrix without column names
    mat_no_names = matrix(rnorm(200), ncol = 2)
    result_no_names = b_inter(mat_no_names)
    expect_s3_class(result_no_names, "b_inter")
    expect_identical(sort(colnames(result_no_names)), c("V1", "V1:V2", "V2"))

    # Test that it gives the same result as passing columns individually
    m1 = lm(mpg ~ b_inter(mtcars[, c("disp", "cyl", "hp")]), mtcars)
    m2 = lm(mpg ~ b_inter(disp, cyl, hp), mtcars)
    expect_equal(fitted(m1), fitted(m2))
})
