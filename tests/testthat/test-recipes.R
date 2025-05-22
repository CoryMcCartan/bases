test_that("recipe environment magic works", {
    rec = recipes::recipe(depth ~ lat + long + mag, quakes)
    rec = step_basis(rec, lat, long, fn = b_rff,
                     options = list(p = 5, kernel = k_rbf(2), stdize="none"))
    rec_p = recipes::prep(rec)

    b0 = recipes::bake(rec_p, new_data=NULL)
    b1 = recipes::bake(rec_p, new_data=quakes[2:8, ])
    b2 = recipes::bake(rec_p, new_data=quakes[2:8, ])
    expect_equal(nrow(b0), nrow(quakes))
    expect_equal(nrow(b1), 7)
    expect_equal(b1, b2)
})
