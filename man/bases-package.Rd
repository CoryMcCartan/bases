% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bases-package.R
\docType{package}
\name{bases-package}
\alias{bases}
\alias{bases-package}
\title{bases: Basis Expansions for Regression Modeling}
\description{
\if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}

Provides various basis expansions for flexible regression modeling, including random Fourier features (Rahimi & Recht, 2007) \url{https://proceedings.neurips.cc/paper_files/paper/2007/file/013a006f03dbc5392effeb8f18fda755-Paper.pdf}, exact kernel / Gaussian process feature maps, Bayesian Additive Regression Trees (BART) (Chipman et al., 2010) \doi{10.1214/09-AOAS285} prior features, and a helpful interface for n-way interactions. The provided functions may be used within any modeling formula, allowing the use of kernel methods and other basis expansions in modeling functions that do not otherwise support them. Along with the basis expansions, a number of kernel functions are also provided, which support kernel arithmetic to form new kernels. Basic ridge regression functionality is included as well.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://corymccartan.com/bases/}
  \item \url{https://github.com/CoryMcCartan/bases/}
  \item Report bugs at \url{https://github.com/CoryMcCartan/bases/issues}
}

}
\author{
\strong{Maintainer}: Cory McCartan \email{mccartan@psu.edu} (\href{https://orcid.org/0000-0002-6251-669X}{ORCID}) [copyright holder]

}
\keyword{internal}
