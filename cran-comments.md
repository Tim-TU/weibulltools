---
title: "CRAN comments"
output: html_document
---

## This is a resubmission

* Uwe Ligges confirmed that the version number can be kept as 2.0.0 for the resubmission
* omitted the package name of the own package in the description field
* single quoted packages in the description field
* fixed invalid URLs
* CRAN URL in canonical form 

## Release summary

This is the major release 2.0.0.

## Test environments

* local windows install, R 4.0.3
* win-builder (devel, old-release and release)
* macOS 10.13.6 High Sierra, R 4.0.3 (brew and CRAN setup via r-hub builder)
* Oracle Solaris 10, x86, 32 bit, R 4.0.3 (via r-hub builder)
* ubuntu 20.04 (release, devel), macOS-latest (release), windows-latest (release) (on github actions)

## R CMD check results

There were no ERRORs or WARNINGs. 
  
There was 1 NOTE for local windows install:

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
    
This note only emerges with R 4.0.3 and rtools40.
In the attached link it is mentioned that this has no implication for CRAN checks/submissions: 
https://stackoverflow.com/questions/64402688/information-on-o-files-for-x64-is-not-available-note-on-r-package-checks-using

There was 1 NOTE for win-builder:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Tim-Gunnar Hensel <tim-gunnar.hensel@tu-berlin.de>'

New maintainer:
  Tim-Gunnar Hensel <tim-gunnar.hensel@tu-berlin.de>
Old maintainer(s):
  Hensel Tim-Gunnar <tim-gunnar.hensel@tu-berlin.de>

Switched surname and name. 
