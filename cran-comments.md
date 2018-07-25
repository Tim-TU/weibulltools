---
title: "CRAN comments"
output: html_document
---

# This is a resubmission
* I've omitted redundancy in the Description of the DESCRIPTION file  
* I've added a more detailed description and references for the methods in the 
  DESCRIPTION file

# Test environments

* local windows install, 
    R version 3.5.1, 
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    
* win-builder, 
    R Under development (unstable) (2018-07-23 r75001)
    Platform: x86_64-w64-mingw32 (64-bit)

* Ubuntu Linux 16.04 LTS, R-release, GCC (via r-hub builder), 
    R version 3.4.4, 
    Platform: x86_64-pc-linux-gnu (64-bit)
    
* Ubuntu Linux 16.04 LTS, R-devel, GCC (via r-hub builder)
    R Under development (unstable) (2018-07-21 r74997), 
    Platform: x86_64-pc-linux-gnu (64-bit) 
    
* Debian Linux, R-release, GCC (via r-hub builder)
    R version 3.5.1, 
    Platform: x86_64-pc-linux-gnu (64-bit)
    
* Debian Linux, R-devel, GCC (via r-hub builder)
    R Under development (unstable) (2018-07-21 r74997), 
    Platform: x86_64-pc-linux-gnu (64-bit)
    
* macOS 10.11 El Capitan, R-release (experimental), (via r-hub builder)
    R version 3.5.0, 
    Platform: x86_64-apple-darwin15.6.0 (64-bit) 
    
* Windows Server 2008 R2 SP1, R-devel and R-release, 32/64 bit (via r-hub builder)

## R CMD check results

There were no ERRORs or WARNINGs for 
local windows install, Windows Server 2008 R2 SP1, win_builder, 
Ubuntu Linux 16.04 LTS, Debian Linux and macOS 10.11 El Capitan.

There was 1 NOTE (only on win-builder check):

* checking CRAN incoming feasibility ... NOTE

Maintainer: Hensel Tim-Gunnar <tim-gunnar.hensel@tu-berlin.de>
  
New submission

Possibly mis-spelled words in DESCRIPTION:
weibulltools (8:18)
  
This note can be safely ignored, since Mr. Ligges stated in this post 
https://mailman.stat.ethz.ch/pipermail/r-devel/2014-March/068497.html that this 
Note just reminds CRAN maintainers... 
