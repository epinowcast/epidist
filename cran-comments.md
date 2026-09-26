## Test environments

- local macOS Tahoe 26.5, R 4.6.0
- GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest (release), windows-latest (release)
- GitHub Actions: `R CMD check --as-cran` on ubuntu-latest (release)
- win-builder (devel)

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
```

This is a new submission.

`epireview` is used by one test, one example and a vignette section, and is not on CRAN.
It is declared in `Suggests`, reached through `Additional_repositories`, and every use is conditional on it being installed.

The `\donttest{}` examples each fit a Stan model through `rstan`.
They take under a minute each, mostly compiling the model, and about 5 minutes in total.
