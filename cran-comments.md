## Test environments

- local Ubuntu 22.04, R 4.6.0
- GitHub Actions: ubuntu-latest (release, oldrel-1), macOS-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Sam Abbott <contact@samabbott.co.uk>'

New submission

Suggests or Enhances not in mainstream repositories:
  cmdstanr
Availability using Additional_repositories specification:
  cmdstanr   yes   https://stan-dev.r-universe.dev
```

This is a new submission.

`cmdstanr` is an optional backend and is not on CRAN.
It is declared in `Suggests` and reached through `Additional_repositories`.
The package uses the `rstan` backend by default and the full test suite and all vignettes build without `cmdstanr` installed.

## Notes for the reviewer

The examples for `epidist()` and `epidist_diagnostics()` fit a Bayesian model, so they are wrapped in `\donttest{}`.
They take roughly two minutes each, which is unavoidable because Stan compiles the model before sampling.
They are still run under `--run-donttest` and pass.
