test_that("mgcv smooth construct and predict work for b_rff", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 100
    dat <- data.frame(x1 = runif(n), x2 = runif(n))
    dat$y <- sin(dat$x1 * 2 * pi) + rnorm(n, sd = 0.1)

    m <- mgcv::gam(y ~ s(x1, x2, bs = "b_rff", k = 50), data = dat)
    expect_s3_class(m, "gam")

    pred <- predict(m, newdata = dat)
    expect_length(pred, n)

    # prediction on new data
    new_dat <- data.frame(x1 = runif(10), x2 = runif(10))
    pred2 <- predict(m, newdata = new_dat)
    expect_length(pred2, 10)
})

test_that("mgcv smooth construct and predict work for b_bart", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 100
    dat <- data.frame(x1 = runif(n), x2 = runif(n))
    dat$y <- dat$x1^2 + rnorm(n, sd = 0.1)

    m <- mgcv::gam(y ~ s(x1, x2, bs = "b_bart", k = 20), data = dat)
    expect_s3_class(m, "gam")

    pred <- predict(m, newdata = dat)
    expect_length(pred, n)

    new_dat <- data.frame(x1 = runif(10), x2 = runif(10))
    pred2 <- predict(m, newdata = new_dat)
    expect_length(pred2, 10)
})

test_that("mgcv smooth construct and predict work for b_nn", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 100
    dat <- data.frame(x1 = runif(n), x2 = runif(n))
    dat$y <- dat$x1 + dat$x2 + rnorm(n, sd = 0.1)

    m <- mgcv::gam(y ~ s(x1, x2, bs = "b_nn", k = 50), data = dat)
    expect_s3_class(m, "gam")

    pred <- predict(m, newdata = dat)
    expect_length(pred, n)

    new_dat <- data.frame(x1 = runif(10), x2 = runif(10))
    pred2 <- predict(m, newdata = new_dat)
    expect_length(pred2, 10)
})

test_that("mgcv smooth construct and predict work for b_tpsob", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 100
    dat <- data.frame(x1 = runif(n), x2 = runif(n))
    dat$y <- cos(dat$x1 * pi) + rnorm(n, sd = 0.1)

    m <- mgcv::gam(y ~ s(x1, x2, bs = "b_tpsob", k = 30), data = dat)
    expect_s3_class(m, "gam")

    pred <- predict(m, newdata = dat)
    expect_length(pred, n)

    new_dat <- data.frame(x1 = runif(10), x2 = runif(10))
    pred2 <- predict(m, newdata = new_dat)
    expect_length(pred2, 10)
})

test_that("mgcv smooth construct and predict work for b_ker", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 50
    dat <- data.frame(x1 = runif(n))
    dat$y <- sin(dat$x1 * 2 * pi) + rnorm(n, sd = 0.1)

    m <- mgcv::gam(y ~ s(x1, bs = "b_ker"), data = dat)
    expect_s3_class(m, "gam")

    pred <- predict(m, newdata = dat)
    expect_length(pred, n)

    new_dat <- data.frame(x1 = runif(10))
    pred2 <- predict(m, newdata = new_dat)
    expect_length(pred2, 10)
})

test_that("mgcv smooth construct and predict work for b_inter", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 100
    dat <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
    dat$y <- dat$x1 * dat$x2 + rnorm(n, sd = 0.1)

    m <- mgcv::gam(y ~ s(x1, x2, x3, bs = "b_inter", k = 3), data = dat)
    expect_s3_class(m, "gam")

    pred <- predict(m, newdata = dat)
    expect_length(pred, n)

    new_dat <- data.frame(x1 = runif(10), x2 = runif(10), x3 = runif(10))
    pred2 <- predict(m, newdata = new_dat)
    expect_length(pred2, 10)
})

test_that("mgcv xt arguments are forwarded", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 100
    dat <- data.frame(x1 = runif(n), x2 = runif(n))
    dat$y <- sin(dat$x1 * 2 * pi) + rnorm(n, sd = 0.1)

    m <- mgcv::gam(
        y ~ s(x1, x2, bs = "b_rff", k = 50,
              xt = list(kernel = k_rbf(0.5))),
        data = dat
    )
    expect_s3_class(m, "gam")
})

test_that("mgcv default k works (k not specified)", {
    skip_if_not_installed("mgcv")

    set.seed(42)
    n <- 100
    dat <- data.frame(x1 = runif(n), x2 = runif(n))
    dat$y <- dat$x1 + rnorm(n, sd = 0.1)

    m <- mgcv::gam(y ~ s(x1, x2, bs = "b_rff"), data = dat)
    expect_s3_class(m, "gam")
})
