# Package index

## Basis expansions

- [`b_rff()`](http://corymccartan.com/bases/reference/b_rff.md) : Random
  Fourier feature basis
- [`b_bart()`](http://corymccartan.com/bases/reference/b_bart.md)
  [`bart_depth_prior()`](http://corymccartan.com/bases/reference/b_bart.md)
  : Bayesian Additive Regression Tree (BART) features
- [`b_conv()`](http://corymccartan.com/bases/reference/b_conv.md) :
  Random convolutional features
- [`b_gff()`](http://corymccartan.com/bases/reference/b_gff.md) : Graph
  Fourier Feature basis
- [`b_inter()`](http://corymccartan.com/bases/reference/b_inter.md) :
  N-way interaction basis
- [`b_ker()`](http://corymccartan.com/bases/reference/b_ker.md) : Exact
  kernel feature basis
- [`b_nn()`](http://corymccartan.com/bases/reference/b_nn.md) : Neural
  network basis
- [`b_tpsob()`](http://corymccartan.com/bases/reference/b_tpsob.md) :
  Tensor-product Sobolev space basis

## Kernels

- [`k_rbf()`](http://corymccartan.com/bases/reference/kernels.md)
  [`k_lapl()`](http://corymccartan.com/bases/reference/kernels.md)
  [`k_rq()`](http://corymccartan.com/bases/reference/kernels.md)
  [`k_matern()`](http://corymccartan.com/bases/reference/kernels.md)
  [`k_per()`](http://corymccartan.com/bases/reference/kernels.md) :
  Kernel functions
- [`` `*`( ``*`<kernel>`*`)`](http://corymccartan.com/bases/reference/kernel-arith.md)
  [`` `+`( ``*`<kernel>`*`)`](http://corymccartan.com/bases/reference/kernel-arith.md)
  : Kernel arithmetic

## Modeling

Functions for modeling and interfacing with other modeling packages.

- [`ridge()`](http://corymccartan.com/bases/reference/ridge.md)
  [`fitted(`*`<ridge>`*`)`](http://corymccartan.com/bases/reference/ridge.md)
  [`coef(`*`<ridge>`*`)`](http://corymccartan.com/bases/reference/ridge.md)
  [`predict(`*`<ridge>`*`)`](http://corymccartan.com/bases/reference/ridge.md)
  : Ridge regression

- [`bases_mgcv`](http://corymccartan.com/bases/reference/bases_mgcv.md)
  :

  `mgcv` integration

- [`step_basis()`](http://corymccartan.com/bases/reference/step_basis.md)
  : Recipe step for basis expansions
