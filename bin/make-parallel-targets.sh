#!/bin/bash

Rscript -e "targets::tar_make_future(workers = floor(future::availableCores() / 2))"
