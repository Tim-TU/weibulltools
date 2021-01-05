---
title: "CRAN comments"
output: html_document
---

## Release summary

This is the major release 2.0.0.

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
    
* Fedora Linux, R-devel, clang, gfortran (via r-hub builder)
    R Under development (unstable) (2019-01-26 r76018), 
    Platform: x86_64-pc-linux-gnu (64-bit)
    
* Windows Server 2008 R2 SP1, R-release and R-devel, 32/64 bit (via r-hub builder)

## R CMD check results

There were no ERRORs for 
local windows install, Windows Server 2008 R2 SP1, 
win_builder, Ubuntu Linux 16.04 LTS and Debian Linux

There was 1 WARNING:

* checking package dependencies ... WARNING
  Requires orphaned package: 'plotly'

This warning is not critical as it is already known and discussed here: 
https://github.com/ropensci/plotly/issues/1906
  
There was 1 NOTE:

* checking compiled code ... NOTE
  Note: information on .o files for i386 is not available
  Note: information on .o files for x64 is not available
  File 'C:/Users/Tim.Hensel/Desktop/weibulltools.Rcheck/weibulltools/libs/i386/weibulltools.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
  File 'C:/Users/Tim.Hensel/Desktop/weibulltools.Rcheck/weibulltools/libs/x64/weibulltools.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
    
This note is also not critical and only emerges with R 4.0.3 and rtools40.
In the attached link it is already discussed and it is mentioned that it has no 
implications for CRAN checks/submissions: 
https://stackoverflow.com/questions/64402688/information-on-o-files-for-x64-is-not-available-note-on-r-package-checks-using
