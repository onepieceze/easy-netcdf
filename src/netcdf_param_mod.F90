module netcdf_param_mod

  use netcdf, only: nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double

  integer, parameter :: short  = nf90_short
  integer, parameter :: int    = nf90_int
  integer, parameter :: long   = nf90_int64
  integer, parameter :: float  = nf90_float
  integer, parameter :: double = nf90_double

end module netcdf_param_mod