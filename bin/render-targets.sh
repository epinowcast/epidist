#!/bin/bash

Rscript -e "suppressMessages(rmarkdown::render('_targets.Rmd'))" 
