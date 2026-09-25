## Test environments

- local Ubuntu 22.04, R 4.3.0
- GitHub Actions: ubuntu-latest (release, oldrel-1), macOS-latest (release), windows-latest (release)
- GitHub Actions: `R CMD check --as-cran` on ubuntu-latest (release)

## R CMD check results

0 errors | 0 warnings | 2 notes

```
* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'codemeta.json'
```

`codemeta.json` holds the package metadata in the CodeMeta format and is shipped deliberately.

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

The `\donttest{}` examples fit a Stan model each through `rstan`.
They take about two minutes each, mostly compiling the model.
