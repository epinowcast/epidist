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
```

This is a new submission.

`epireview` is used by one test and by a vignette section, and is not on CRAN.
It is declared in `Suggests` and reached through `Additional_repositories`.
