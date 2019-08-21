<a name="top"></a>
# [easy-netcdf](https://github.com/onepieceze/easy-netcdf) 
Fortran OOP Interface to the netcdf [fortran library](https://github.com/Unidata/netcdf-fortran).

## Content

+ [Overview](#overview)

+ [Prerequisites](#Prerequisites)

+ [Installation](#installation)

+ [Example](#example)

+ [Compiler Support](#compiler-support)


## Overview
This repository is an simple Fortran package to using netcdf library.

Using two open sources code from : [Fortran Date Time Library](https://github.com/dongli/fortran-container.git) & [fortran-container](https://github.com/dongli/fortran-datetime.git)

## Prerequisites:
Unidata netcdf-fortran library

## Installation
A CMake-Setup is provided.

## Example
xtype:
```
int2      int4      int8      real4      real8
```
### Writing:
```Fortran
program write_demo

  use easy_netcdf

  type(netcdf_type)    :: F
  type(variable_type)  :: TMP
  type(dimension_type) :: LON
  type(dimension_type) :: LAT

  call F%add_file("./test.nc", "w")

  LON%name = "lon"
  LON%xtype = int4
  call LON%attribute("long_name", "longitude")
  LON = [1, 2]

  LAT%name = "lat"
  LAT%xtype = int4
  call LAT%attribute("long_name", "latitude")
  LAT = [1, 2]

  TMP%name = "TMP"
  TMP%xtype = int4
  call TMP%dimension([LON, LAT])
  call TMP%attribute("units", "K")
  call TMP%attribute("range", [-100, 100])
  TMP = reshape([26, 27, 28, 27], [2, 2])

  call F%add_variable(variable=TMP)
  call F%add_variable(variable=LON)
  call F%add_variable(variable=LAT)
  call F%attribute("author", "onepieceze")

  call F%write()

end program write_demo
```
Setting unlimited dimension:
```Fortran
  time%unlimited = .true.
```
### Reading:
```Fortran
program read_demo

  use easy_netcdf

  type(netcdf_type)    :: F
  type(variable_type)  :: TMP
  type(dimension_type) :: LON
  type(dimension_type) :: LAT
  integer              :: LON_value(2)
  integer              :: LAT_value(2)
  integer              :: TMP_value(2, 2)
  integer              :: range(2)
  character(30)        :: lon_long_name
  character(30)        :: lat_long_name
  character(30)        :: units
  character(30)        :: author


  call f%add_file("./test.nc", "r")

  LON%name = "lon"
  call LON%attribute("long_name", lon_long_name)
  LON = LON_value

  LAT%name = "lat"
  call LAT%attribute("long_name", lat_long_name)
  LAT = LAT_value

  call F%attribute("author", author)

  TMP%name = "TMP"
  call TMP%dimension([LON, LAT])
  call TMP%attribute("units", units)
  call TMP%attribute("range", range)
  TMP = TMP_value

  call F%add_variable(variable=TMP)
  call F%add_variable(variable=LON)
  call F%add_variable(variable=LAT)
  
  call F%read()

  print*, "x_long_name:  ", lon_long_name
  print*, "y_long_name:  ", lat_long_name
  print*, "author     :  ", author
  print*, "units      :  ", units
  print*, "lon        :  ", LON_value
  print*, "lat        :  ", LAT_value
  print*, "data       :  ", TMP_value
  print*, "range      :  ", range

end program read_demo
```
Result:
```
 x_long_name:  longitude                     
 y_long_name:  latitude                      
 author     :  onepieceze                    
 units      :  K                             
 lon        :             1           2
 lat        :             1           2
 data       :            26          27          28          27
```

## Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v15.0.2.187+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()

<sub>Go to [Top](#top)</sub>
