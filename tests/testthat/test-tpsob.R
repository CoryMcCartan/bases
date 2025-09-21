test_that("Tensor product indices are generated correctly", {
    d = 3
    p = 73
    # code to recreate:
    # i0 = Sieve::create_index_matrix(d, p + 1L, interaction_order = p)
    # i0 = i0[1L + seq_len(p), -1, drop = FALSE]
    i0 = structure(c(2, 1, 1, 3, 1, 1, 2, 2, 1, 4, 1, 1, 5, 1, 1, 2, 2,
        1, 3, 3, 1, 6, 1, 1, 7, 1, 1, 2, 2, 2, 1, 4, 4, 1, 8, 1, 1, 3,
        3, 1, 9, 1, 1, 2, 2, 1, 5, 5, 1, 10, 1, 1, 11, 1, 1, 2, 2, 3,
        2, 2, 1, 6, 6, 1, 3, 3, 1, 4, 4, 1, 12, 1, 1, 1, 2, 1, 1, 3,
        1, 2, 1, 2, 1, 4, 1, 1, 5, 1, 3, 1, 2, 2, 1, 3, 1, 6, 1, 1, 7,
        1, 2, 4, 1, 2, 2, 1, 4, 1, 8, 1, 3, 1, 3, 1, 9, 1, 5, 1, 2, 2,
        1, 5, 1, 10, 1, 1, 11, 1, 2, 3, 2, 6, 1, 2, 2, 1, 6, 4, 1, 3,
        3, 1, 4, 1, 12, 1, 1, 1, 2, 1, 1, 3, 1, 2, 2, 1, 1, 4, 1, 1,
        5, 1, 3, 3, 1, 2, 2, 1, 1, 6, 1, 1, 7, 2, 1, 4, 4, 1, 2, 2, 1,
        1, 8, 1, 3, 3, 1, 1, 9, 1, 5, 5, 1, 2, 2, 1, 1, 10, 1, 1, 11,
        3, 2, 2, 1, 6, 6, 1, 2, 2, 1, 4, 4, 1, 3, 3, 1, 1, 12),
        dim = c(73L, 3L))

    i1 = make_index_mat(d, p)
    storage.mode(i1) = "double"

    expect_true(all(diff(row_prod(i1)) >= 0))
    expect_equal(nrow(i1), p)

    cols_list = function(m) apply(m, 2, function(x) x, simplify = FALSE)
    i0 = i0[do.call(order, cols_list(cbind(row_prod(i0), i0))), ]
    i1 = i1[do.call(order, cols_list(cbind(row_prod(i1), i1))), ]

    expect_equal(i0, i1)
})

test_that("Sobolev features improve prediction fit", {
    y = quakes$depth

    m = lm(y ~ lat + long, quakes)
    r2_lm = cor(fitted(m), y)^2

    m = lm(y ~ b_tpsob(lat, long, p = 32), quakes)
    r2_tp = cor(fitted(m), y)^2

    expect_gt(r2_tp - r2_lm, 0.8)
})

test_that("predict() method works correctly", {
    m = ridge(mpg ~ b_tpsob(disp, cyl, hp, wt, p = 50), mtcars[1:20, ])

    expect_equal(predict(m, mtcars[1:20, ]), fitted(m), tolerance = 1e-5)

    pred_m = suppressWarnings(predict(m, newdata = mtcars))
    expect_equal(pred_m[1:20], fitted(m)[1:20], tolerance = 1e-5)

    B = with(mtcars, b_tpsob(disp, cyl, hp, wt, p = 50))
    expect_equal(predict(B, newdata = mtcars), predict(B, newdata = mtcars))
})
