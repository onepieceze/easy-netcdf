program main

  use easy_netcdf

  type(netcdf_type)    :: f
  integer, allocatable :: lon(:)
  integer              :: tmp(2, 2, 1, 1)

  call f%netcdf_open("./test.nc")

  call f%netcdf_inquire_coordinate()

  call f%netcdf_get_coordinate("lon", lon)

  print*, lon

  deallocate(lon)

  call f%netcdf_get_data("TMP", tmp)

  print*, tmp

  


end program main
