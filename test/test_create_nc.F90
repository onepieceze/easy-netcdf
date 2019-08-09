program test_create_nc

  use easy_netcdf

  type(netcdf_type) :: f

  call f%netcdf_create("./test.nc")

  call f%netcdf_define_coordinate(nx=2, ny=2)

  call f%netcdf_define_variable("TMP", int)

  call f%netcdf_define_global()

  call f%netcdf_write_coordinate(lon=[1.,2.], lat=[1.,2.])

  call f%netcdf_write_data("TMP", data=reshape([25,24,26,27],[2,2,1]))

  call f%netcdf_close()

end program test_create_nc
