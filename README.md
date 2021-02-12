<!-- badges: start -->
[![R-CMD-check](https://github.com/tpilz/telemac/workflows/R-CMD-check/badge.svg)](https://github.com/tpilz/telemac/actions)
<!-- badges: end -->

# telemac

## Description

The `telemac` package provides an interface to the integrated suite of solvers for free-surface modelling [TELEMAC-MASCARET](http://www.opentelemac.co.uk). In its  current state the primary focus is on the module for 2-D hydrodynamic modelling (TELEMAC-2D). The package provides a number of functions to assist the user in the pre- and post-processing of TELEMAC-2D simulations.

## Installation

It is planned to submit the package to the CRAN repository. So far it is only available via github and the source code needs to be compiled by yourself. The most convenient way is to compile and install it in R via the `tidyverse` package:

```r
# install.packages("devtools")
devtools::install_github("tpilz/telemac")
```

## Usage

### Overview
The package basically provides a number of functions to setup a TELEMAC-2D project:

- `tin()` to create the model mesh, a triangulated irregular network (TIN)
- `geo()` to derive an object that incorporates all information for the *geometry file*
- `cli()` to generate the *boundary conditions*
- `cas()` for the *steering parameters*
- `optionals()` to handle optional input files
- `t2d()` to initialise the full TELEMAC setup

Each function generates an object `t2d_*` incorporating the specific information, e.g. the output of `tin()` is an object of class `t2d_tin` with information defining the TIN for generating the model mesh.

After writing the model setup to disk using `write_t2d()` the model can be run either manually or using function `simulate_t2d()` (the latter should only be used for small test configurations). In the following the results can be imported with `results()` for further investigation.

The package contains a number of further functionalities to support the user with various processing steps, e.g. `plot()` methods for each object for ad-hoc investigation or `tin2grid()` to interpolate the TIN-based mesh values to a regular grid, etc.

### Examples
To illustrate the functionalities of the package two vignettes exist so far:

- `vignette("t2d_basics")` illustrating the absolute basics
- `vignette("t2d_rainfall_runoff")` showing how to use the `telemac` package to prepare a rainfall--runoff simulation with TELEMAC-2D

More examples and further advices can also be found in the function documentations within R. 
