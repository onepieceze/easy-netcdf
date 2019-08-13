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
