# Overview
This repository is an simple Fortran package to using netcdf library.

Using two open sources code from : https://github.com/dongli/fortran-container.git
                                 & https://github.com/dongli/fortran-datetime.git

# Denpendencies
Unidata netcdf-fortran library

# Installation
A CMake-Setup is provided.

# Writing Example
```
program write_demo

  use easy_netcdf

  type(netcdf_type)   :: f
  type(variable_type) :: TMP

  call f%add_file("./test.nc", "w")

  f%x%name = "lon"
  f%x%xtype = int
  call f%x%attribute("long_name", "longitude")
  f%x = [1, 2]

  f%y%name = "lat"
  f%y%xtype = int
  call f%y%attribute("long_name", "latitude")
  f%y = [1, 2]

  call f%global("author", "onepieceze")

  TMP%name = "TMP"
  TMP%xtype = int
  call TMP%attribute("units", "K")

  TMP = reshape([26, 27, 28, 27], [2, 2])

  call f%add_variable(variable=TMP)

  call f%write()

end program write_demo
```

# Reading Example:
```
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

  call f%global("author", author)

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
