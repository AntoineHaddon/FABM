Forked repository of FABM for the implementation of CSIB2 in GOTM-FABM

CSIB2 code is in FABM

    src/models/uvic

## The Framework for Aquatic Biogeochemical Models (FABM)

FABM is a Fortran 2003 programming framework for biogeochemical models of marine and freshwater systems.

Documentation is available at [the FABM wiki](http://fabm.net/wiki).

The latest code is available from [FABM's git repository](http://fabm.net/code).
Stable releases are published with DOI on Zenodo.
The latest stable version is:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3774497.svg)](https://doi.org/10.5281/zenodo.3774497)

FABM is further described in:

Bruggeman, J., Bolding, K., 2014. A general framework for aquatic biogeochemical models. Environmental Modelling & Software 61: 249–265. DOI: [10.1016/j.envsoft.2014.04.002](http://doi.org/10.1016/j.envsoft.2014.04.002)




# CSIB2 in GOTM-FABM

Instructions on downloading, compiling and running the model.

## Download code

Get GOTM and FABM code (this will downlowd code from various repos)
    
    git clone --recursive https://github.com/AntoineHaddon/GOTMcode.git

Get folders with forcing and parameter files for simulations
    
    git clone https://github.com/AntoineHaddon/GOTMcases.git


## Compiling

Code can compile succesfull but then fail to run if compiled with certain (newer?) versions of netcdf. Works with netCDF 4.7.3

Make some directories
    
    mkdir build && cd build && mkdir gotm && mkdir fabm

Build FABM
    
    cd fabm
    cmake ../../GOTMcode/extern/fabm -DFABM_HOST=gotm
    make install

Build GOTM
    
    cd ../gotm
    FABM=true && FABMDIR=../../GOTMcode/extern/fabm && FABM_PREFIX=~/local/fabm/gotm && FORTRAN_COMPILER=GFORTRAN && NETCDF_VERSION=NETCDF4
    cmake ../../GOTMcode -DGOTM_USE_STIM=ON -DGOTM_USE_FABM=ON
    make install


## Runing a simulation

    cd ../../GOTMcases/resolute
    ../../build/gotm/gotm
