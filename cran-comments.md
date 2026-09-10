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
  epireview
Availability using Additional_repositories specification:
  epireview   yes   https://mrc-ide.r-universe.dev

Found the following (possibly) invalid URLs:
  URL: https://www.pnas.org/doi/full/10.1073/pnas.1518587113
    From: man/sierra_leone_ebola_data.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.medrxiv.org/content/10.1101/2024.01.12.24301247v1
    From: README.md
    Status: 403
    Message: Forbidden
```

This is a new submission.

`epireview` is used by one test and by a vignette section, and is not on CRAN.
It is declared in `Suggests` and reached through `Additional_repositories`.
Everything that uses it is skipped when it is absent.

Both URLs are the canonical publisher landing pages for the works cited, and match their published DOIs.
`pnas.org` and `medrxiv.org` return 403 to automated requests, including through `doi.org`, so no form of the link avoids the note.

## Notes for the reviewer

The examples for `epidist()` and `epidist_diagnostics()` fit a Bayesian model.
They are wrapped in `\donttest{}`.
They are still run under `--run-donttest` and pass.

The vignettes that fit models are precomputed from a `.Rmd.orig` source into a committed `.Rmd` holding static output, so building the package does not fit a model.
