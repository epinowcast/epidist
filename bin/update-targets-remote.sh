#!/bin/bash

Rscript -e "gittargets::tar_git_snapshot()"

Rscript -e "source('R/targets-archive.R'); upload_targets_archive()"
