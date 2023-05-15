
<!-- 
README.md is generated from README.Rmd. 
Please edit that file 
-->

# npfda: Nonparametric functional data analysis

### Version 0.1.4

This package implements nonparametric methods for inference on
(multidimensional) functional data using the tools available in package
[`npsp`](https://rubenfcasal.github.io/npsp).

## Main functions

- `npf.data()` defines a (multidimensional) functional data set.

Nonparametric methods for inference on the functional trend, the
functional variance and the variogram:

- `npf.fit()` (automatically) fits a nonparametric functional model by
  estimating the trend, the conditional variance and the variogram.

- `locpol()`, `np.var()` and `np.svar()` methods use local polynomial
  kernel smoothing to compute nonparametric estimates of the functional
  trend, the functional variance and the (intra-curve) variogram (or
  their first derivatives), respectively.

See the
[Reference](https://rubenfcasal.github.io/npfda/reference/index.html)
for the complete list of functions.

## Installation

`npfda` is not available from CRAN, but you can install the development
version from github with:

``` r
# install.packages("remotes")
remotes::install_github("rubenfcasal/npfda")
```

<!-- 
Note also that, as this package requires compilation, Windows users need to have previously installed the appropriate version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/), and OS X users need to have installed [Xcode](https://apps.apple.com/us/app/xcode/id497799835).

Alternatively, Windows users may install the corresponding *npfda_X.Y.Z.zip* file in the [releases section](https://github.com/rubenfcasal/npfda/releases/latest) of the github repository.

For R versions 4.2.x under Windows: 
``` r
install.packages('https://github.com/rubenfcasal/npfda/releases/download/v0.1-1/npfda_0.1-1.zip',
                 repos = NULL)
``` 
-->

## Authors

- [Rubén Fernández-Casal](https://rubenfcasal.github.io) (Dep.
  Mathematics, University of A Coruña, Spain).

- Sergio Castillo-Páez (Universidad de las Fuerzas Armadas ESPE,
  Ecuador).

- Miguel Flores (Faculty of Administrative Sciences, Escuela Politécnica
  Nacional, Ecuador).

Please send comments, error reports or suggestions to
<rubenfcasal@gmail.com>.

## Acknowledgments

This research has been supported by MICINN (Grant PID2020-113578RB-I00).
The research of Rubén Fernández-Casal has been supported by the Xunta de
Galicia (Grupos de Referencia Competitiva ED431C-2020-14 and Centro de
Investigación del Sistema Universitario de Galicia ED431G 2019/01). All
these grants were co-financed by the ERDF. The research of Sergio
Castillo Páez has been supported by the Universidad de las Fuerzas
Armadas ESPE, from Ecuador.

## References

- Fernández-Casal R. (2023) npsp: Nonparametric spatial (geo)statistics.
  R package version 0.7-11, *<https://rubenfcasal.github.io/npsp>*.

- Fernández-Casal R., Castillo-Páez S. and García-Soidán P. (2017),
  Nonparametric estimation of the small-scale variability of
  heteroscedastic spatial processes, *Spa. Sta.*, **22**, 358-370,
  [DOI](https://doi.org/10.1016/j.spasta.2017.04.001).

- Shapiro A. and Botha J.D. (1991) Variogram fitting with a general
  class of conditionally non-negative definite functions. *Computational
  Statistics and Data Analysis*, **11**, 87-96.
