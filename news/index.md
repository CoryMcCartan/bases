# Changelog

## bases 0.2.1

CRAN release: 2026-08-04

- Updated methods references for random tree/BART features
- Fixed normalizing bug in
  [`b_rff()`](http://corymccartan.com/bases/reference/b_rff.md) when
  `freqs`/`phases` were passed manually

## bases 0.2.0

CRAN release: 2026-02-27

- `mgcv` smooth interface via `s()` for more flexible penalization
- New [`b_nn()`](http://corymccartan.com/bases/reference/b_nn.md) for
  neural network basis expansion
- New [`b_tpsob()`](http://corymccartan.com/bases/reference/b_tpsob.md)
  for tensor product Sobolev space basis expansion (Zhang and Simon
  2023)
- New [`b_gff()`](http://corymccartan.com/bases/reference/b_gff.md) for
  graph Fourier features for regression on spatial and graph-structured
  data. Requires `RSpectra` package for efficient eigendecomposition,
  and either `adj` or `igraph` for graph representation.
- New [`b_conv()`](http://corymccartan.com/bases/reference/b_conv.md)
  for random convolutional features for regression on images
- More efficient
  [`b_ker()`](http://corymccartan.com/bases/reference/b_ker.md) option
  for many predictions
- Control automatic leaf pruning in
  [`b_bart()`](http://corymccartan.com/bases/reference/b_bart.md)
- New vignette on other packages that help produce basis expansions or
  embeddings.

## bases 0.1.2

CRAN release: 2025-05-29

- Basis expansions for Gaussian processes / kernel ridge regression,
  random Fourier features, BART prior features, and n-way interactions
- Lightweight ridge regression routine
- Gaussian, Laplace, Rational quadratic, Matérn, and periodic kernels
- Support for `recipes` package
