
# Estimate epidemiological delay distributions with `brms`<a href='https://epidist.epinowcast.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/epinowcast/epidist/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/epinowcast/epidist/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epinowcast/epidist/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epinowcast/epidist)

[![Universe](https://epinowcast.r-universe.dev/badges/epidist)](https://epinowcast.r-universe.dev/epidist)
[![MIT
license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/epinowcast/epidist/blob/master/LICENSE.md/)
[![GitHub
contributors](https://img.shields.io/github/contributors/epinowcast/epidist)](https://github.com/epinowcast/epidist/graphs/contributors)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14213017.svg)](https://doi.org/10.5281/zenodo.14213017)
<!-- badges: end -->

## Summary

Understanding and accurately estimating epidemiological delay
distributions is important for public health policy. These estimates
influence epidemic situational awareness, control strategies, and
resource allocation. This package provides methods to address the key
challenges in estimating these distributions, including truncation,
interval censoring, and dynamical biases. These issues are frequently
overlooked, resulting in biased conclusions.

## Quickstart

To learn more about `epidist` we recommend reading the vignettes in this
order:

- [Getting started with
  `epidist`](https://epidist.epinowcast.org/articles/epidist.html)
- [Using `epidist` to estimate delay between symptom onset and positive
  test for an Ebola outbreak in Sierra
  Leone](https://epidist.epinowcast.org/articles/ebola.html)
- [Approximate Bayesian inference in
  `epidist`](https://epidist.epinowcast.org/articles/approx-inference.html)

## Installation

<details>
<summary>
Installing the package
</summary>

You can install the latest released version using the normal `R`
function, though you need to point to `r-universe` instead of CRAN:

``` r
install.packages(
  "epidist",
  repos = "https://epinowcast.r-universe.dev"
)
```

Alternatively, you can use the [`remotes`
package](https://remotes.r-lib.org/) to install the development version
from Github (warning! this version may contain breaking changes and/or
bugs):

``` r
remotes::install_github(
  file.path("epinowcast", "epidist"),
  dependencies = TRUE
)
```

Similarly, you can install historical versions by specifying the release
tag (e.g. this installs
[`0.1.0`](https://github.com/epinowcast/epidist/releases/tag/v0.1.0)):

``` r
remotes::install_github(
  file.path("epinowcast", "epidist"),
  dependencies = TRUE, ref = "v0.1.0"
)
```

*Note: You can also use that last approach to install a specific commit
if needed, e.g. if you want to try out a specific unreleased feature,
but not the absolute latest developmental version.*

</details>
<details>
<summary>
Installing CmdStan (optional)
</summary>

By default `epidist` uses the `rstan` package for fitting models. If you
wish to use the `cmdstanr` package instead, you will need to install
[CmdStan](https://mc-stan.org/users/interfaces/cmdstan), which also
entails having a suitable C++ toolchain setup. We recommend using the
[`cmdstanr` package](https://mc-stan.org/cmdstanr/) to manage CmdStan.
The Stan team provides instructions in the [*Getting started with
`cmdstanr`*](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
vignette, with other details and support at the [package
site](https://mc-stan.org/cmdstanr/), but the brief version is:

``` r
# if you have not yet installed `epidist`, or you installed it without
# `Suggests` dependencies
install.packages(
  "cmdstanr",
  repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
)

# once `cmdstanr` is installed
cmdstanr::install_cmdstan()
```

*Note: You can speed up CmdStan installation using the `cores` argument.
If you are installing a particular version of `epidist`, you may also
need to install a past version of CmdStan, which you can do with the
`version` argument.*

</details>

## Resources

<details>
<summary>
Organisation Website
</summary>

Our [organisation website](https://www.epinowcast.org/) includes links
to other resources, [guest posts](https://www.epinowcast.org/blog.html),
and [seminar schedule](https://www.epinowcast.org/seminars.html) for
both upcoming and past recordings.

</details>
<details>
<summary>
Community Forum
</summary>

Our [community forum](https://community.epinowcast.org/) has areas for
[question and answer](https://community.epinowcast.org/c/interface/15)
and [considering new methods and
tools](https://community.epinowcast.org/c/projects/11), among others. If
you are generally interested in real-time analysis of infectious
disease, you may find this useful even if do not use `epidist`.

</details>

## Contributing

We welcome contributions and new contributors! We particularly
appreciate help on [identifying and identified
issues](https://github.com/epinowcast/epidist/issues). Please check and
add to the issues, and/or add a [pull
request](https://github.com/epinowcast/epidist/pulls) and see our
[contributing
guide](https://github.com/epinowcast/.github/blob/main/CONTRIBUTING.md)
for more information.

### How to make a bug report or feature request

Please briefly describe your problem and what output you expect in an
[issue](https://github.com/epinowcast/epidist/issues).

If you have a question, please don’t open an issue. Instead, ask on our
[forum](https://community.epinowcast.org/).

See our [contributing
guide](https://github.com/epinowcast/.github/blob/main/CONTRIBUTING.md)
for more information.

### Code of Conduct

Please note that the `epidist` project is released with a [Contributor
Code of
Conduct](https://github.com/epinowcast/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citation

If you use `epidist` in your work, please consider citing it using
`citation("epidist")`.

<details>
<summary>
Package citation information
</summary>

``` r
citation("epidist")
To cite package 'epidist' in publications use:

  Adam Howes, Park S, Sam Abbott (NULL). _epidist: Estimate
  Epidemiological Delay Distributions With brms_.
  doi:10.5281/zenodo.14213017
  <https://doi.org/10.5281/zenodo.14213017>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {epidist: Estimate Epidemiological Delay Distributions With brms},
    author = {{Adam Howes} and Sang Woo Park and {Sam Abbott}},
    year = {NULL},
    doi = {10.5281/zenodo.14213017},
  }
```

</details>

If using our methodology, or the methodology on which ours is based,
please cite the relevant papers. This may include:

- [Estimating epidemiological delay distributions for infectious
  diseases](https://www.medrxiv.org/content/10.1101/2024.01.12.24301247v1)
  by Park *et al.* (2024)
- [Best practices for estimating and reporting epidemiological delay
  distributions of infectious diseases using public health surveillance
  and healthcare
  data](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1012520)
  by Charniga *et al.* (2024)

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [all-contributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

<a href="https://github.com/epinowcast/epidist/commits?author=seabbs">seabbs</a>,
<a href="https://github.com/epinowcast/epidist/commits?author=athowes">athowes</a>,
<a href="https://github.com/epinowcast/epidist/commits?author=parksw3">parksw3</a>,
<a href="https://github.com/epinowcast/epidist/commits?author=sbfnk">sbfnk</a>,
<a href="https://github.com/epinowcast/epidist/commits?author=cherz4">cherz4</a>,
<a href="https://github.com/epinowcast/epidist/commits?author=damonbayer">damonbayer</a>,
<a href="https://github.com/epinowcast/epidist/commits?author=medewitt">medewitt</a>

### Issue Authors

<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+author%3Akgostic">kgostic</a>,
<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+author%3ATimTaylor">TimTaylor</a>,
<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+author%3Ajamesmbaazam">jamesmbaazam</a>,
<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+author%3Ajonathonmellor">jonathonmellor</a>

### Issue Contributors

<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+commenter%3Apearsonca">pearsonca</a>,
<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+commenter%3ASamuelBrand1">SamuelBrand1</a>,
<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+commenter%3Azsusswein">zsusswein</a>,
<a href="https://github.com/epinowcast/epidist/issues?q=is%3Aissue+commenter%3Akcharniga">kcharniga</a>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
