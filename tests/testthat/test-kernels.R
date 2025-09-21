test_that("Kernels handle vector and matrix inputs", {
    xv = as.double(1:10)
    xm = matrix(xv)
    x2 = cbind(xm, rnorm(10))

    kernels = list(k_rbf(), k_lapl(), k_matern())
    for (k in kernels) {
        expect_equal(k(xv, xv), k(xm, xm))
        expect_error(k(xv, x2), "number of columns")
        expect_equal(dim(k(x2, x2)), c(10, 10))
    }
})

test_that("Kernels are symmetric and positive semidefinite", {
    x = 10^(-5:2) - 1
    y = seq(0, 1, 0.1)
    kernels = list(
        k_rbf(1),
        k_rbf(0.01),
        k_rbf(100),
        k_lapl(),
        k_rq(alpha = 1),
        k_rq(alpha = 3),
        k_matern(nu = 0.5),
        k_matern(nu = 8)
    )
    for (k in kernels) {
        expect_true(all(eigen(k(x, x))$values > -10 * .Machine$double.eps))
        expect_true(isSymmetric(k(x, x)))
        expect_equal(k(x, y), t(k(y, x)))
    }
})

test_that("Matern kernel special cases calculated correctly", {
    x = seq(-5, 5, length.out = 51)
    for (nu in c(0.5, 1.5, 2.5)) {
        k_spec = k_matern(nu = nu)
        k_univ = k_matern(nu = nu + 1e-9)
        expect_false(identical(
            as.character(rlang::fn_body(k_univ)),
            as.character(rlang::fn_body(k_spec))
        ))
        expect_equal(k_spec(x, x), k_univ(x, x))
    }
})

test_that("Matern limit is Gaussian", {
    x = seq(-5, 5, length.out = 51)
    expect_equal(k_matern(nu = 100)(x, x), k_rbf()(x, x), tolerance = 5e-3)
})


test_that("Kernel arithmetic works as expected", {
    x = seq(-5, 5, length.out = 51)

    k1 = k_rbf()
    k2 = k_lapl(scale = 2)

    expect_equal(0.5 * k2(x, x), (0.5 * k2)(x, x))
    expect_equal(k1(x, x) * k2(x, x), (k1 * k2)(x, x))
    expect_equal(k1(x, x) + k2(x, x), (k1 + k2)(x, x))
})
