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

    expect_equal(predict(m), fitted(m), tolerance=1e-4)
    expect_equal(predict(m), predict(m, list(x=x)))

    pred_m = suppressWarnings(predict(m, newdata=list(x=xn)))
    expect_equal(unname(pred_m[2:21]), unname(fitted(m)[1:20]), tolerance=1e-4)
})

test_that("predict() works with saved L_inv", {
    y = c(BJsales)
    x = seq_along(y)
    xn = c(0:20, 150:200)

    m0 = lm(y ~ b_ker(x))
    m1 = lm(y ~ b_ker(x, L_inv=TRUE))

    expect_equal(predict(m1), predict(m0))
    expect_equal(predict(m1), fitted(m1), tolerance=1e-4)
    expect_equal(predict(m1), predict(m1, list(x=x)))

    skip_on_cran() # don't run timing tests on CRAN
    t0 = system.time(for (i in 1:50) predict(m0, list(x = x)))["elapsed"]
    t1 = system.time(for (i in 1:50) predict(m1, list(x = x)))["elapsed"]
    expect_lt(t1, t0)
})
