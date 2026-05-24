# wrf_intermediate_sfc
Convert ECMWF/ERA-Interim NetCDF surface data to WRF intermediate format.
## Author
Yan Chen

## Features
- Read NetCDF format surface meteorological data
- Support 18 surface variables (SST, soil temperature/moisture, wind fields, etc.)
- Automatic processing of 365-day time series
- Output WRF-compatible binary intermediate format (Lambert projection)

## Supported Variables
| Variable | Description |
|----------|-------------|
| SEAICE | Sea ice fraction |
| SST | Sea surface temperature |
| SM000007 | Soil moisture (0-7cm) |
| SM007028 | Soil moisture (7-28cm) |
| SM028100 | Soil moisture (28-100cm) |
| SM100289 | Soil moisture (100-289cm) |
| ST000007 | Soil temperature (0-7cm) |
| ST007028 | Soil temperature (7-28cm) |
| ST028100 | Soil temperature (28-100cm) |
| ST100289 | Soil temperature (100-289cm) |
| PMSL | Mean sea level pressure |
| SNOW_EC | Snow depth |
| SKINTEMP | Skin temperature |
| PSFC | Surface pressure |
| TT | Temperature |
| DEWPT | Dew point temperature |
| UU | U-wind component |
| VV | V-wind component |

## Dependencies
- Fortran 90/95 compiler (gfortran recommended)
- NetCDF-Fortran library
- WRF environment (for data assimilation)

## Compilation

```bash
cd build
make
# OR
./compile.sh
