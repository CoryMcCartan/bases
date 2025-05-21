test_that("Exact kernel matches analytical result", {
    y = c(BJsales)
    x = seq_along(y)
    xs = c(do_std(matrix(x))$x)

    k = k_rbf(0.2)
    yhat_gp = c(mean(y) + k(xs, xs) %*% solve(k(xs, xs) + 2*diag(length(y))) %*% (y - mean(y)))
    yhat_ker = unname(fitted(ridge(y ~ b_ker(x, kernel=k), penalty=2)))

    expect_equal(yhat_ker, yhat_gp)
})

test_that("predict() method works correctly", {
    y = c(BJsales)
    x = seq_along(y)
    xn = c(0:20, 150:200)

    m = lm(y ~ b_ker(x))
    m0 = lm(y ~ x + I(x^2))

    expect_equal(predict(m), fitted(m), tolerance=1e-3)

    pred_m = suppressWarnings(predict(m, newdata=list(x=xn)))
    expect_equal(unname(pred_m[2:21]), unname(fitted(m)[1:20]), tolerance=1e-3)
})
