program read_demo

  use easy_netcdf

  type(netcdf_type)    :: f
  type(variable_type)  :: TMP
  integer              :: lon(2)
  integer              :: lat(2)
  integer              :: data(2, 2)
  integer              :: range(2)
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
  call TMP%attribute("range", range)
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
  print*, "range      :  ", range

end program read_demo
