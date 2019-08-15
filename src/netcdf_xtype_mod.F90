module netcdf_xtype_mod

  use netcdf, only: nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double

  integer, parameter :: int2  = nf90_short
  integer, parameter :: int4  = nf90_int
  integer, parameter :: int8  = nf90_int64
  integer, parameter :: real4 = nf90_float
  integer, parameter :: real8 = nf90_double

end module netcdf_xtype_mod
