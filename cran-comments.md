---
title: "CRAN comments"
output: html_document
---

# This is a resubmission of version v1.0.0 (2019-01-28)
CRAN Package Check Results for Package weibulltools has thrown the following error:  
em_mixture.cpp:1:10: fatal error: 'ext/algorithm' file not found 
#include <ext/algorithm> ^~~~~~~~~~~~~~~ 1 error generated.

* I've fixed this error by removing lines 'ext/algorithm' and '#include <ext/algorithm>'

## Package Size 
* Provided vignettes, where package _plotly_ was used to create interactive visualizsations 
  increased the package size. But tarball is less than 5Mb (exactly 4.06Mb). 

## Test environments

* local windows install, 
    R version 3.5.2, 
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    
* win-builder, 
    using R version 3.5.2 (2018-12-20) 
    Platform: x86_64-w64-mingw32 (64-bit)
    
    R Under development (unstable) (2019-01-25 r76015)
    Platform: x86_64-w64-mingw32 (64-bit)
    
* Ubuntu Linux 16.04 LTS, R-release, GCC (via r-hub builder), 
    R version 3.4.4, 
    Platform: x86_64-pc-linux-gnu (64-bit)
    
* Ubuntu Linux 16.04 LTS, R-devel, GCC (via r-hub builder)
    R Under development (unstable) (2019-01-18 r75994), 
    Platform: x86_64-pc-linux-gnu (64-bit) 
    
* Debian Linux, R-release, GCC (via r-hub builder)
    R version 3.5.2, 
    Platform: x86_64-pc-linux-gnu (64-bit)
    
* Debian Linux, R-devel, GCC (via r-hub builder)
    R Under development (unstable) (2019-01-18 r75994), 
    Platform: x86_64-pc-linux-gnu (64-bit)
    
* Windows Server 2008 R2 SP1, R-release and R-devel, 32/64 bit (via r-hub builder)

## R CMD check results

There were no ERRORs or WARNINGs for 
local windows install, Windows Server 2008 R2 SP1, win_builder, 
Ubuntu Linux 16.04 LTS and Debian Linux

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is 13.8Mb 
  sub-directories of 1Mb or more: 
    doc   11.9Mb
    libs  1.6Mb
 
 Package size increased by vignettes using interactive visualizations. 
 But the tarbull is less than 5Mb (exactly 4.06Mb)

There was an additional NOTE for win-builder: 
* checking CRAN incoming feasibility ... NOTE

Maintainer: 'Hensel Tim-Gunnar <tim-gunnar.hensel@tu-berlin.de>

Possibly mis-spelled words in DESCRIPTION:
  plotly (32:37)
  
<br><br><br><br>  

[//]: # (Everything below this comment was for submission of v0.5.4)  
# In the following the old CRAN comments are presented! 
# This is a resubmission (2018-07-25)
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
