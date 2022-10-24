# Bash scripts

This folder contains `bash` scripts used to orchestrate project tasks. The table below summarises key files.



Folder | Purpose
---|---
`render-targets.sh` | Updates the targets workflow by rendering the `_targets.Rmd` file.
`make-targets.sh` | Runs `targets::tar_make()`. Sequentially updates targets. This is not recommended if actually updating the workflow as the compute requirements of the nowcast models means this could take a very long time.
`make-parallel-targets.sh` | Uses `targets::tar_make_future()` to update targets in parallel. By default in the code this uses a `future.callr` backend and allocates half the number of available cores (with 2 cores used per model fit). If running the pipeline yourself this likely needs configuration.
`update-targets.sh` | Updates the workflow and then runs it in parallel using `render-targets.sh` and `make-parallel-targets.sh`. Updating the pipeline is run using `nohup` as a background process. Output is sent to `render.log` and `targets.log` respectively.
`update-targets-remote.sh` | Takes a snapshot of the `_targets` directory using `gittargets::tar_git_snapshot()` and then pushes this to the remote archive using functionality in `R/targets-archive.R`.
