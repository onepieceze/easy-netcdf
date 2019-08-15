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
int2
int4
int8
real4
real8
```
### Writing:
```Fortran
program write_demo

  use easy_netcdf

  type(netcdf_type)   :: f
  type(variable_type) :: TMP

  call f%add_file("./test.nc", "w")

  f%x%name = "lon"
  f%x%xtype = int4
  call f%x%attribute("long_name", "longitude")
  f%x = [1, 2]

  f%y%name = "lat"
  f%y%xtype = int4
  call f%y%attribute("long_name", "latitude")
  f%y = [1, 2]

  call f%attribute("author", "onepieceze")

  TMP%name = "TMP"
  TMP%xtype = int4
  call TMP%attribute("units", "K")

  TMP = reshape([26, 27, 28, 27], [2, 2])

  call f%add_variable(variable=TMP)

  call f%write()

end program write_demo
```
### Reading:
```Fortran
program read_demo

  use easy_netcdf

  type(netcdf_type)    :: f
  type(variable_type)  :: TMP
  integer              :: lon(2)
  integer              :: lat(2)
  integer              :: data(2, 2)
  character(30)        :: x_long_name
  character(30)        :: y_long_name
  character(30)        :: units
  character(30)        :: author

  call f%add_file("./test.nc", "r")

  f%x%name = "lon"
  call f%x%attribute("long_name", x_long_name)
  f%x = lon

  f%y%name = "lat"
  call f%y%attribute("long_name", y_long_name)
  f%y = lat

  call f%attribute("author", author)

  TMP%name = "TMP"
  call TMP%attribute("units", units)
  TMP = data

  call f%add_variable(TMP)

  call f%read()

  print*, "x_long_name:  ", x_long_name
  print*, "y_long_name:  ", y_long_name
  print*, "author     :  ", author
  print*, "units      :  ", units
  print*, "lon        :  ", lon
  print*, "lat        :  ", lat
  print*, "data       :  ", data

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
