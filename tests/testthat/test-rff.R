test_that("Random features improve prediction fit", {
    y = quakes$depth

    m = lm(y ~ lat + long, quakes)
    r2_lm = cor(fitted(m), y)^2

    m = lm(y ~ b_rff(lat, long, p=32), quakes)
    r2_rff = cor(fitted(m), y)^2

    expect_gt(r2_rff - r2_lm, 0.8)
})

test_that("Manual Fourier transform matches known", {
    skip_on_cran()
    y = c(BJsales)
    x = matrix(seq_along(y))

    kernels = c(k_rbf(0.2), k_lapl(0.5), k_rq(0.1, 1))
    for (k1 in kernels) {
        k2 = k1
        attr(k2, "name") = NULL

        yhat1 = rowMeans(replicate(100, {
            fitted(lm(y ~ b_rff(x, p=100, n_approx=2048, kernel=k1)))
        }))
        yhat2 = rowMeans(replicate(100, {
            fitted(lm(y ~ b_rff(x, p=100, n_approx=2048, kernel=k2)))
        }))

        expect_lt(sd(yhat1 - yhat2) / sd(y),
                  0.05 + 0.05*(attr(k1, "name") == "lapl"),
                  label=paste0(attr(k1, "name"), "() difference"))
    }
})

test_that("Random features closely approximate Gaussian process", {
    skip_on_cran()
    y = c(BJsales)
    x = seq_along(y)
    xs = c(do_std(matrix(x))$x)

    k = k_rbf(0.1)
    yhat_gp = mean(y) + k(xs, xs) %*% solve(k(xs, xs) + 1e-4*diag(length(y))) %*% (y - mean(y))
    yhat_rff = rowMeans(replicate(50, {
        fitted(ridge(y ~ b_rff(x, kernel=k, p=100), penalty=1e-4))
    }))
    yhat_rff2 = rowMeans(replicate(50, {
        fitted(ridge(y ~ b_rff(x, kernel=k, p=400), penalty=1e-4))
    }))

    expect_lt(sd(yhat_rff - yhat_gp) / sd(y), 0.05)
    expect_lt(sd(yhat_rff2 - yhat_gp) / sd(y), sd(yhat_rff - yhat_gp) / sd(y))
})

test_that("predict() method works correctly", {
    y = c(BJsales)
    x = seq_along(y)
    xn = c(0:20, 150:200)

    m = lm(y ~ b_rff(x, p=5))
    m2 = lm(y ~ b_rff(x, p=5))
    m0 = lm(y ~ x + I(x^2))

    expect_equal(predict(m), fitted(m))

    pred_m1 = unname(predict(m, newdata=list(x=xn)))
    pred_m2 = unname(predict(m2, newdata=list(x=xn)))
    expect_equal(pred_m1[2:21], unname(fitted(m)[1:20]))

    skip_on_cran()
    expect_gt(sd(pred_m2[2:21] - fitted(m)[1:20]), 0.01)

    B = b_rff(x, p=5)
    expect_equal(predict(B, newx=list(x=x)), predict(B, newx=list(x=x)))
    expect_equal(predict(B, newx=list(x=xn)), predict(B, newx=list(x=xn)))
    expect_equal(nrow(predict(B, newx=list(x=xn))), length(xn))
})
