#!/bin/bash
# Compilation script for WRF Intermediate Surface Data Converter
# Author: Yan Chen

echo "Compiling WRF Intermediate Surface Data Converter..."

gfortran -o wrf_intermediate_sfc \
    ../modules/mod_nc_reader.f90 \
    ../modules/mod_wrf_writer.f90 \
    ../src/wrf_intermediate_sfc.f90 \
    -lnetcdff -lnetcdf

if [ $? -eq 0 ]; then
    echo "Compilation successful!"
    echo "Run with: ./wrf_intermediate_sfc"
else
    echo "Compilation failed!"
    exit 1
fi
