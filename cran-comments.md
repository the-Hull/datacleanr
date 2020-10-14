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

 >Found the following (possibly) invalid file URI:
 >    URI: .github/CODE_OF_CONDUCT.md
 >      From: README.md

The issue was fixed. 
