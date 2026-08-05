# bases: Basis Expansions for Regression Modeling

Provides various basis expansions for flexible regression modeling,
including random Fourier features (Rahimi & Recht, 2007)
<https://proceedings.neurips.cc/paper_files/paper/2007/file/013a006f03dbc5392effeb8f18fda755-Paper.pdf>,
exact kernel / Gaussian process feature maps, prior features for
Bayesian Additive Regression Trees (BART) (McCartan & Huang, 2026)
[doi:10.48550/arXiv.2607.28844](https://doi.org/10.48550/arXiv.2607.28844)
, and a helpful interface for n-way interactions. The provided functions
may be used within any modeling formula, allowing the use of kernel
methods and other basis expansions in modeling functions that do not
otherwise support them. Along with the basis expansions, a number of
kernel functions are also provided, which support kernel arithmetic to
form new kernels. Basic ridge regression functionality is included as
well.

## See also

Useful links:

- <https://corymccartan.com/bases/>

- <https://github.com/CoryMcCartan/bases/>

- Report bugs at <https://github.com/CoryMcCartan/bases/issues>

## Author

**Maintainer**: Cory McCartan <mccartan@psu.edu>
([ORCID](https://orcid.org/0000-0002-6251-669X)) \[copyright holder\]

Authors:

- Cory McCartan <mccartan@psu.edu>
  ([ORCID](https://orcid.org/0000-0002-6251-669X)) \[copyright holder\]
