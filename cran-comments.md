## R CMD check results

0 errors | 0 warnings | 1 note

* The NOTE is about the 'adj' package in Suggests not being in mainstream repositories. 
  This package is only used for optional graph-related functionality in `b_gff()`. 
  All examples and tests using 'adj' are conditional on its availability.

## Test environments
* local R installation (macOS), R 4.5.0
* win-builder (devel)
* windows-latest (on gh-actions), (release)
* macos-latest (on gh-actions), (release)
* ubuntu-latest (on gh-actions), (release)
* ubuntu-latest (on gh-actions), (devel)
* ubuntu-latest (on gh-actions), (old-release)

