## This is a new, repo Makefile for asymptomatic variant 2022 May 17 (Tue)

all: main.pdf main.pdf.go
-include target.mk

# -include makestuff/perl.def

vim_session:
	 bash -cl "vmt main.tex main.bib"

######################################################################

Sources += README.md

Ignore += asymptomaticvariant ## For testing instructions

######################################################################

## Main document (implicit rules)

# Sources += main.tex main.bib
## main.pdf: main.tex

## main.tex.HEAD~1.oldfile
## main_olddiff.pdf: main.tex

######################################################################

Sources += $(wildcard *.R)

param.Rout: param.R
	$(wrapR)
	
data_exponential.Rout: data_exponential.R param.rda
	$(pipeR)

######################################################################
### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/texi.mk
-include makestuff/pipeR.mk
-include makestuff/pdfpages.mk

-include makestuff/git.mk
-include makestuff/visual.mk
