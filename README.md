
# prop-618 ![(travis build status)](https://travis-ci.org/lloda/prop-618.svg?branch=master) #

## Propagation models for Earth-space telecommunications systems

This is a translation into code of [ITU-R P.618](https://www.itu.int/dms_pubrec/itu-r/rec/p/R-REC-P.618-13-201712-I!!PDF-E.pdf) and other references given there and in this [ITU table](https://www.itu.int/en/ITU-R/study-groups/rsg3/Pages/iono-tropo-spheric.aspx). The implementations I could find were all in Matlabese (from ITU itself or national offices), which I couldn't use, or don't seem to give the source [(like this one)](https://logiciels.cnes.fr/fr/content/propa).

You probably need to be familiar with the relevant ITU-R papers to use this at all.

The library is written in Fortran (`-std=f2018`). It uses the C compatibility feature (`bind(c)`) which makes it easy to call from C, or anything with an FFI.

To build and test, do `cd build && cmake .. && make && make test`. There is no installation procedure, and both the libraries and the tests require access to the data folder in `../data` — I'll fix that eventually.

## Conventions

* Same units as in ITU's documentation (latitude/longitude is in °, probabilities are in %, frequencies are in GHz, and so on).
* Similar variable names as in ITU's reports. An exception is that I use `el` instead of `θ` or `th` where that means elevation.

## Outlook

The library is about 30% complete. Look at the [TODO](TODO) file or the tests in [src/test0.f95](src/test0.f95) to see what's implemented.

This is the first time I write anything in Fortran. I'm using gfortran 8.2 and 8.3. There is a header for C and bindings for Python, which are automatically generated. There is also a Guile sandbox at [mod/prop.scm](mod/prop.scm). My priority is to have useful functionality before working on the documentation or the bindings.

## Random links

* [Fortran libraries](https://github.com/rabbiabram/awesome-fortran)
* [Generating Fortran interfaces](http://fortranwiki.org/fortran/show/Generating+C+Interfaces)
* [Fortran guide](http://www.egr.unlv.edu/~ed/fortranv3.pdf)
* [Fortran wiki book](https://en.wikibooks.org/wiki/Category:Book:Fortran)
* [Array functions in Fortran](https://www.phy.ornl.gov/csep/pl/node18.html)
* [gfortran intrinsic procedures](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures)
* [UseLATEX.cmake](https://gitlab.kitware.com/kmorel/UseLATEX/blob/master/UseLATEX.pdf)
* [Water humidity calculator](https://www.cactus2000.de/uk/unit/masshum.shtml)
* [Fortran & C interoperability](https://gcc.gnu.org/onlinedocs/gfortran/Interoperability-with-C.html)
* [Fortran & C interoperability example](https://stackoverflow.com/a/30430656)
* [More on Fortran & C interoperability](https://stackoverflow.com/a/14503508)
* [ITU's Earth-Space propagation prediction methods validation examples spreedsheet](https://www.itu.int/en/ITU-R/study-groups/rsg3/ionotropospheric/CG-3M3J-13-ValEx-Rev4_2.xlsx)
