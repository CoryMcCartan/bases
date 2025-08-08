# bases 0.2.0

* `mgcv` smooth interface via `s()` for more flexible penalization
* New `b_nn()` for neural network basis expansion
* New `b_conv()` for random convolutional features for regression on images TODO im2col()
* New `b_rocket()` for random convolutional features for regression on time series TODO https://arxiv.org/pdf/2012.08791
* New `b_echo()` for echo state network features for time series forecasting TODO https://www.ai.rug.nl/minds/uploads/PracticalESN.pdf
* More efficient `b_ker()` option for many predictions
* New vignette on other packages that help produce basis expansions or embeddings.

# bases 0.1.2

* Basis expansions for Gaussian processes / kernel ridge regression,
  random Fourier features, BART prior features, and n-way interactions
* Lightweight ridge regression routine
* Gaussian, Laplace, Rational quadratic, Matérn, and periodic kernels
* Support for `recipes` package
