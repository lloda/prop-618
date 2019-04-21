
## Propagation models for Earth-space telecommuncations systems

This is a translation into code of [P.618](https://www.itu.int/dms_pubrec/itu-r/rec/p/R-REC-P.618-13-201712-I!!PDF-E.pdf) and other references given there and in this [ITU table](https://www.itu.int/en/ITU-R/study-groups/rsg3/Pages/iono-tropo-spheric.aspx). The ones I could find were in Matlabese (from ITU & national offices) or gave no source [(from CNES)](https://logiciels.cnes.fr/fr/content/propa).

The library is written in Fortran and it uses the C compatibility feature (`bind(c)`) which makes it easy to call from C, or anything with an FFI.

To build and test, do `cd build && cmake .. && make && make test`.

## Conventions

* I use the same units as in ITU's documentation (latitude/longitude is in °, probabilities are in %, frequencies are in GHz, and so on).
* I try to use similar names to those in ITU's reports. There are some exceptions where I don't think the ITU name works in code (like `freq` for `f` or `temp` for `T`. I also use `el` instead of θ where that means elevation (I really bad habit by ITU if I may say).

## Outlook

This is the first time I write Fortran. I've read somewhere that `.f95` is customary for Fortran >95 code, including later Fortran versions, so the Fortran files have that extension. I'm using `-std=f2018`. There will be a C/C++ header and Python bindings at some point.

The library is about 20% complete. Look at the tests `src/test0.f95` to see what's implemented. My priority is to have useful functionality before working on the documentation or the bindings. Probably want to autogenerate those in some way.

## Links

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
