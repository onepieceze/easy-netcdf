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
  list(APPEND EASY_NETCDF_INC "$ENV{NETCDF_ROOT}/indlude")
  LIST(APPEND EASY_NETCDF_LIB "$ENV{NETCDF_ROOT}/lib")
endif ()

set (sources
  src/netcdf_xtype_mod.F90
  src/netcdf_param_mod.F90
  src/netcdf_tool_mod.F90
  src/base_type_mod.F90
  src/dimension_type_mod.F90
  src/variable_type_mod.F90
  src/netcdf_i_mod.F90
  src/netcdf_o_mod.F90
  src/netcdf_type_mod.F90
  src/easy_netcdf.F90
)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/example")

add_library (easy_netcdf SHARED ${sources})
target_link_libraries (easy_netcdf fortran_container netcdff)

add_executable (read_test example/test_read_nc.F90)
target_link_libraries (read_test easy_netcdf netcdff)

add_executable (write_test example/test_write_nc.F90)
target_link_libraries (write_test easy_netcdf netcdff)
