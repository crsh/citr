## Test environments

* local OS X 10.12.6 install, R 3.5.2
* Fedora Linux, R-devel, clang, gfortran (r-hub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (r-hub)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (r-hub)
* Windows Server 2008, R-oldrelease (win-builder)
* Windows Server 2008, R-release (win-builder)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

r-hub CRAN check notes "Author field differs from that derived from Authors@R" due to the ORCID id auto-expansion to a URL.

## Downstream dependencies

`revdepcheck::revdep_check()` reported no problems for `rmd`.
