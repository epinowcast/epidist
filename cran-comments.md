## Test environments

- local macOS Tahoe 26.5, R 4.6.0, `R CMD check --as-cran` with the remote incoming checks
- GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest (release), windows-latest (release)
- GitHub Actions: `R CMD check --as-cran` on ubuntu-latest (release)
- CRAN incoming pre-test, Debian (R-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Sam Abbott <contact@samabbott.co.uk>'

New submission

Possibly misspelled words in DESCRIPTION:
  Charniga (31:43)
  al (30:65, 31:55)
  et (30:62, 31:52)
```

This is a new submission.
The flagged words are an author's name and "et al." in the references to the methods.

The examples that fit a model are wrapped in `\dontrun{}`, as in `brms`, because they compile a Stan model, which needs a C++ toolchain and matching `rstan` and `StanHeaders` builds.
The other examples run in under five seconds.
