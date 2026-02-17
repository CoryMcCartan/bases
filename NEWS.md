# bases 0.2.0

* `mgcv` smooth interface via `s()` for more flexible penalization
* New `b_nn()` for neural network basis expansion
* New `b_tpsob()` for tensor product Sobolev space basis expansion (Zhang and Simon 2023)
* New `b_gff()` for graph Fourier features for regression on spatial and graph-structured data.
  Requires `RSpectra` package for efficient eigendecomposition, and either
  `adj` or `igraph` for graph representation.
* New `b_conv()` for random convolutional features for regression on images
* More efficient `b_ker()` option for many predictions
* Control automatic leaf pruning in `b_bart()`
* New vignette on other packages that help produce basis expansions or embeddings.

# bases 0.1.2

* Basis expansions for Gaussian processes / kernel ridge regression,
  random Fourier features, BART prior features, and n-way interactions
* Lightweight ridge regression routine
* Gaussian, Laplace, Rational quadratic, Matérn, and periodic kernels
* Support for `recipes` package
