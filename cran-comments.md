## Resubmission

This is a resubmission.
The Windows pre-test of the first submission stopped during "checking CRAN incoming feasibility", with the status "check log incomplete, web timeout?".
win-builder stopped at the same point.
The package then suggested `epireview`, which is not on CRAN, through `Additional_repositories`.
It no longer does, so every dependency is on CRAN and the package names no other repository.

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

The `\donttest{}` examples each fit a Stan model through `rstan`.
They take under a minute each, mostly compiling the model, and about 3.5 minutes in total.
