# Colossus 0.9

* Added a `NEWS.md` file to track changes to the package.

# Colossus 1.0.0

* Initial submission
* C++ code modified to not apply OpenMP code if OpenMP isn't detected, to resolve MacOS installation failures

# Colossus 1.0.1

* Configuration improved to detect compiler information
* Linux configuration depends on the system wide c++ default and the compiler used to compile R
* OpenMP support is not used if the c++ default or the R compiler is clang
