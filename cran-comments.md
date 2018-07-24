---
title: "CRAN comments"
output: html_document
---

# Test environments

* local windows install, R 3.5.1
* Windows Server 2008 R2 SP1, R-devel and R-release, 32/64 bit (via r-hub builder)
* Debian Linux, R-devel and R-release, GCC (via r-hub builder)
* macOS 10.11 El Capitan, R-release (via r-hub builder)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE (only local windows install):

* checking compiled code ... NOTE
  File 'weibulltools/libs/x64/weibulltools.dll':
  Found no calls to: 'R_registerRoutines', 'R_useDynamicSymbols'

  It is good practice to register native routines and to disable symbol
  search.
  
Only occurs due to an interaction between RStudio and devtools on a Windows machine.
If the package is checked on winbuilder or on r-hub.io, the note does not appear.

