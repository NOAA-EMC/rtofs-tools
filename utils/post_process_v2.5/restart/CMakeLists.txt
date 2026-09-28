cmake_minimum_required(VERSION 3.15)

project(rtofs2mom6 LANGUAGES Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -xHost -fp-model precise -convert big_endian -assume byterecl -traceback")
endif()

# Add the C file to the sources
set(SOURCES
    src/mod_rtofs_io.f90
    src/rtofs2mom6.f90
)

add_executable(rtofs2mom6 ${SOURCES})

find_library(NETCDFF_LIB NAMES netcdff HINTS ENV NetCDF_ROOT ENV NETCDF_ROOT ENV NETCDF_DIR PATH_SUFFIXES lib lib64)
find_library(NETCDF_LIB  NAMES netcdf  HINTS ENV NetCDF_ROOT ENV NETCDF_ROOT ENV NETCDF_DIR PATH_SUFFIXES lib lib64)

if(NETCDFF_LIB AND NETCDF_LIB)
    target_link_libraries(rtofs2mom6 ${NETCDFF_LIB} ${NETCDF_LIB})
else()
    target_link_libraries(rtofs2mom6 "$ENV{NETCDF_LDFLAGS}")
endif()
