cmake_minimum_required(VERSION 2.8)

project(easy-netcdf Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none -g -fbacktrace")
endif ()

if (${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME})
  add_subdirectory(lib/container)
endif ()

if (DEFINED ENV{NETCDF} AND (NOT DEFINED ENV{NETCDF_ROOT}))
  SET(ENV{NETCDF_ROOT} $ENV{NETCDF})
endif ()
if (DEFINED ENV{NETCDF_ROOT})
  include_directories("$ENV{NETCDF_ROOT}/include")
  link_directories("$ENV{NETCDF_ROOT}/lib")
  list(APPEND EASY_NETCDF_INC "$ENV{NETCDF_ROOT}/include")
  LIST(APPEND EASY_NETCDF_LIB "$ENV{NETCDF_ROOT}/lib")
endif ()

set (sources
  src/netcdf_param_mod.F90
  src/netcdf_tool_mod.F90
  src/netcdf_io_mod.F90
  src/easy_netcdf.F90
)

add_library (easy_netcdf SHARED ${sources})