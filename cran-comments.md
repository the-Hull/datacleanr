## Test environments
* local win 10 install (release, old-rel)
* win-builder (devel)
* r-hub (Windows Server 2008 R2 SP1, R-devel, 32/64 bit)
* x86_64-pc-linux-gnu 64-bit (release; on CircleCI)
* ubuntu 18.04, 64-bit (release)

## R CMD check results
0 errors v | 0 warnings v | 1 notes x

The Note can safely be ignored.

> checking CRAN incoming feasibility ... NOTE
>
>  Maintainer: 'Alexander Hurley <agl.hurley@gmail.com>'
>  
>  New submission
>  
>  Possibly mis-spelled words in DESCRIPTION:
>    datacleanr (11:3)
>    interoperability (12:31)
>    reproducibility (11:62)

## Re-Submission

* This is a new release, and resubmitted due to the following issue: 



1. Please always write package names, software names and API (application
programming interface) names in single quotes in title and description.
e.g: --> 'datacleanr'

- FIXED


2. Please do not modify the global environment (e.g. by using <<-) in your
functions. This is not allowed by the CRAN policies.

- The call to `<<-` sits within a non-exported, `shiny` module function, i.e. server side. 
The global environment is not affected when this code executes.
(see function `module_server_summary` in `module_data_summary.R` , lines 27 - 70)



3. Please always add all authors, contributors and copyright holders in the
Authors@R field with the appropriate roles.
e.g.: Dean Attali

- Dean Attali is not a package author, but developed a function (available on GitHub), which I use. 
I removed the author tag from the function, and added an appropriate copyright statement (MIT License) in the LICENSE file of the package instead.


