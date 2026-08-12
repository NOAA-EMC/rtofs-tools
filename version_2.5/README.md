# RTOFS v2.5
- Declared operational on [Jul 29, 2025.](https://www.weather.gov/media/notification/pdf_2025/scn25-47_updated_RTOFS_V.2.5.0_aaa.pdf)

# See the following _high_ level description:

## RTOFS is an ocean and sea ice prediction system with a horizontal resolution of 0.08-deg.
   - The ocean model is the Hybrid Coordinate Ocean Model (HYCOM).
   - The sea ice model is the Community Ice CodE version 4 (CICE4).
   - Data assimilation (1-day window) provides initial conditions for forecasts up to 8-days for both sea ice and ocean.
   - Output consists of hourly 2D surface fields and 3-hourly 3D fields.

## RTOFS version 2.5 includes the following changes:
   - Added climatological constraints for temperature and salinity for sea surface height (SSH) assimilation 
     in the analysis (to reduce subsurface negative salinity bias in the Caribbean Sea).
   - Added new observing systems and updated associated quality control routines to 
     maintain an up to date data assimilation system.

## RTOFS products:
   - [Where can I find RTOFS data?](https://github.com/NOAA-EMC/RTOFS_GLO/wiki/Where-can-I-find-RTOFS-data%3F)
   - [What is in RTOFS output?](https://github.com/NOAA-EMC/RTOFS_GLO/wiki/What-is-in-RTOFS-output%3F)

--- 

# This directory contains utilties that work with the above version of RTOFS.
  - Their documentation is minimal or non-existent; you are welcome to modify.
