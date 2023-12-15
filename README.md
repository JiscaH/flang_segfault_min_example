# flang_segfault_min_example
This is a minimum working example of the issue in R package sequoia, resulting in segfault when compiled with flang. No issues when compiling with gfortran, no issues detected by valgrind or ASAN. 


## R CMD check

R CMD check --as-cran --install-args=--configure-args=--enable-lto=check minWE_0.1.0.tar.gz 

> Rel_list <- GetRelA(Ped = minWE::Ped_HSg5, List=TRUE)

 *** caught segfault ***
address 0x7ffc7e4525c8, cause 'memory not mapped'

Traceback:
 1: GetRelA(Ped = minWE::Ped_HSg5, List = TRUE)
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)



## Warning during installation (readelf: ..)

R Under development (unstable) (2023-12-08 r85664) -- "Unsuffered Consequences"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu

(...)

> install.packages('minWE_0.1.0.tar.gz')
Installing package into ‘/home/jisca/R/x86_64-pc-linux-gnu-library/4.4’
(as ‘lib’ is unspecified)
inferring 'repos = NULL' from 'pkgs'
* installing *source* package ‘minWE’ ...
** using staged installation
** libs
using C compiler: ‘Ubuntu clang version 17.0.6 (++20231209124227+6009708b4367-1~exp1~20231209124336.77)’
using Fortran compiler: ‘Ubuntu flang-new version 17.0.6 (++20231209124227+6009708b4367-1~exp1~20231209124336.77)’
flang-new-17  -fpic  -g -O2 -flto -c  MkRelA.f90 -o MkRelA.o
clang-17 -I"/usr/local/lib64/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2 -flto -c init.c -o init.o
clang-17 -shared -g -O2 -flto -fpic -L/usr/local/lib64 -o minWE.so MkRelA.o init.o -L/usr/lib/llvm-17/lib -lFortran_main -lFortranRuntime -lFortranDecimal -lm
installing to /home/jisca/R/x86_64-pc-linux-gnu-library/4.4/00LOCK-minWE/00new/minWE/libs
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
readelf: Warning: Unrecognized form: 0x22
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (minWE)
