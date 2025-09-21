test_that("ridge regression matches analytic expression", {
    X = model.matrix(mpg ~ 0 + ., mtcars)
    y = mtcars$mpg - mean(mtcars$mpg)
    p = ncol(X)
    for (lambda in c(0, 0.01, 0.1, 1, 10, 100)) {
        m_ridge = ridge(mpg ~ ., mtcars, penalty = lambda)
        analytic = solve(crossprod(X) + lambda * diag(p)) %*% crossprod(X, y)
        expect_equal(unname(coef(m_ridge)), c(analytic))
    }
})

test_that("predict() works properly for ridge()", {
    y = c(BJsales)
    x = seq(0, 1, length.out = 150)
    xn = x + 0.5

    m = ridge(y ~ scale(x) + poly(x, 3), penalty = 1e-12)
    m0 = lm(y ~ scale(x) + poly(x, 3))

    expect_equal(predict(m), predict(m0))
    expect_equal(predict(m), fitted(m))
    expect_equal(
        predict(m, newdata = list(x = xn)),
        predict(m0, newdata = list(x = xn))
    )
})
