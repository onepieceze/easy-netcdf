module netcdf_io_mod

  !! The io module to create netcdf file. 

  use netcdf
  use datetime
  use link_list_type
  use netcdf_tools_mod

  implicit none

  private

  public :: netcdf_type

  type :: netcdf_type
    character(:), allocatable, private :: iotype
    integer                  , private  :: ncid
    integer     , allocatable, private  :: dimids(:)
    integer                  , private  :: nx = 1
    integer                  , private  :: ny = 1
    integer                  , private  :: nz = 1
    integer                  , private  :: nt = 1
    integer                  , private  :: x_varid = -999
    integer                  , private  :: y_varid = -999
    integer                  , private  :: z_varid = -999
    integer                  , private  :: t_varid = -999
    type(link_list_type)     , private  :: variable_list
    logical                  , private  :: define_end = .false.
  contains
    procedure                           :: netcdf_open
    procedure                           :: netcdf_create
    procedure                           :: netcdf_define_att
    procedure   , private               :: netcdf_write_data_short
    procedure   , private               :: netcdf_write_data_int
    procedure   , private               :: netcdf_write_data_long
    procedure   , private               :: netcdf_write_data_float
    procedure   , private               :: netcdf_write_data_double
    procedure                           :: netcdf_close
    generic                             :: netcdf_write_data => &
                                           netcdf_write_data_short, netcdf_write_data_int, &
                                           netcdf_write_data_long,  netcdf_write_data_float, &
                                           netcdf_write_data_double
  end type

contains


  subroutine netcdf_open(this, filename)

    type(netcdf_type), intent(inout) :: this
    character(*)     , intent(in)    :: filename

    integer                          :: ncid
    
    if (allocated(this%iotype)) stop "Error: netcdf file have been open or create."

    this%iotype = "read"

    call check(nf90_open(filename, nf90_nowrite, ncid))

    this%ncid = ncid


  end subroutine netcdf_open


  subroutine netcdf_create(this, filename)

    type(netcdf_type), intent(inout) :: this
    character(*)     , intent(in)    :: filename

    integer                          :: ncid

    if (allocated(this%iotype)) stop "Error: netcdf file have been open or create."

    this%iotype = "write"

    call check(nf90_create(filename, NF90_CLOBBER, ncid))

    this%ncid = ncid

  end subroutine netcdf_create


  subroutine netcdf_inquire_coordinate(this, )


  end subroutine netcdf_inquire_coordinate


  subroutine netcdf_define_coordinate(this, nx, ny, nz, nt)

    type(hash_table_type), intent(inout)            :: this
    logical              , intent(in)   , optional  :: nx
    logical              , intent(in)   , optional  :: ny
    logical              , intent(in)   , optional  :: nz
    logical              , intent(in)   , optional  :: nt

    integer                                         :: x_varid
    integer                                         :: y_varid
    integer                                         :: z_varid
    integer                                         :: t_varid
    integer                                         :: x_dimid
    integer                                         :: y_dimid
    integer                                         :: z_dimid
    integer                                         :: t_dimid
    integer                                         :: dim_number

    if (.not. allocated(this%iotype)) stop "Error: netcdf file have not open or create."
    if (this%iotype /= "write")       stop "Error: netcdf open model is not write model."
    if (allocated(this%dimids))       stop "Error: coordinate have been define."

    dim_number = 0

    if (present(nx)) then
      dim_number = dim_number + 1
      call check(nf90_def_dim(ncid, "lon", nx, x_dimid))
      call check(nf90_def_var(ncid, "lon", nf90_real, x_dimid, x_varid))
      call check(nf90_put_att(ncid, x_varid. "long_name", "longitude"))
      call check(nf90_put_att(ncid, x_varid, "units", "degrees_east"))
      this%x_varid
      this%nx = nx
    end if

    if (present(ny)) then
      dim_number = dim_number + 1
      call check(nf90_def_dim(ncid, "lat", ny, y_dimid))
      call check(nf90_def_var(ncid, "lat", nf90_real, y_dimid, y_varid))
      call check(nf90_put_att(ncid, y_varid, "long_name", "latitude"))
      call check(nf90_put_att(ncid, y_varid, "units", "degrees_north"))
      this%y_varid
      this%ny = ny
    end if

    if (present(nz)) then
      dim_number = dim_number + 1
      call check(nf90_def_dim(ncid, "level", nz, z_dimid))
      call check(nf90_def_var(ncid, "level", nf90_int, z_dimid, z_varid))
      call check(nf90_put_att(ncid, z_varid, "long_name", "Isobaric surface"))
      call check(nf90_put_att(ncid, z_varid, "units", "Pa"))
      this%z_varid
      this%nz = nz
    end if

    if (present(nt)) then
      dim_number = dim_number + 1
      call check(nf90_def_dim(ncid, "time", nf90_unlimited, t_dimid))
      call check(nf90_def_var(ncid, "time", nf90_double, t_dimid, t_varid))
      call check(nf90_put_att(ncid, t_varid, "long_name", "Time"))
      call check(nf90_put_att(ncid, t_varid, "units", "seconds since 1970-01-01 00:00"))
      this%t_varid
      this%nt = nt
    end if

    allocate(this%dimids(dim_number))

    select case (dim_number)
    case(4)
      this%dimids = [x_dimid, y_dimid, z_dimid, t_dimid]
    case(3)
      if (present(nz)) this%dimids = [x_dimid, y_dimid, z_dimid]
      if (present(nt)) this%dimids = [x_dimid, y_dimid, t_dimid]
    case(2)
      this%dimids = [x_dimid, y_dimid]
    case(1, 0)
      stop "Error: dimension number of netcdf file can not less than 1"
    end select

  end subroutine netcdf_define_coordinate


  subroutine netcdf_define_variable(this, var_name, data_type, var_att_list, init_time)

    type(netcdf_type)  , intent(inout)        :: ncid
    character(*)       , intent(in)           :: var_name
    integer            , intent(in)           :: data_type
    type(datetime_type), intent(in), optional :: init_time

    integer                                   :: varid

    if (.not. allocated(this%iotype)) stop "Error: netcdf file have not open or create."
    if (this%iotype /= "write")       stop "Error: netcdf open model is not write model."
    if (.not. allocated(this%dimdis)) stop "Error: netcdf file dimension was not define."

    if(var_name(1:4) == 'APCP') then
      if(.not. present(init_time)) stop "Error: wirting APCP data should input initial time"
      call check(nf90_def_var(ncid, "APCP", data_type, dimids, varid))
      call check(nf90_put_att(ncid, varid, "long_name", "Total_precipitation"))
      select case(var_name(6:len_trim(var_name)))
      case('A1')
        call check(nf90_put_att(ncid, varid, "level", "A1"))
        call check(nf90_put_att(ncid, varid, "accum_time", "010000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 3600))
      case('A3')
        call check(nf90_put_att(ncid, varid, "level", "A3"))
        call check(nf90_put_att(ncid, varid, "accum_time", "030000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 10800))
      case('A6')
        call check(nf90_put_att(ncid, varid, "level", "A6"))
        call check(nf90_put_att(ncid, varid, "accum_time", "060000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 21600))
      case('A12')
        call check(nf90_put_att(ncid, varid, "level", "A12"))
        call check(nf90_put_att(ncid, varid, "accum_time", "120000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 43200))
      case('A24')
        call check(nf90_put_att(ncid, varid, "level", "A24"))
        call check(nf90_put_att(ncid, varid, "accum_time", "240000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 86400))
      end select
      call check(nf90_put_att(ncid, varid, "units", "kg/m^2"))
      call check(nf90_put_att(ncid, varid, "grib_code", 61))
      call check(nf90_put_att(ncid, varid, "_FillValue", -999.0))
      call check(nf90_put_att(ncid, varid, "init_time", "00000000_000000"))
      call check(nf90_put_att(ncid, varid, "init_time_ut", 0))
      call check(nf90_put_att(ncid, varid, "valid_time", trim(init_time%format("%Y%m%d_%H%M%S"))))
      call check(nf90_put_att(ncid, varid, "valid_time_ut", init_time%timestamp()))
    else
      select case
      case()
      end select
    end if

    variable_list%append(var_name, varid)
    
  end subroutine netcdf_define_variable

  
  subroutine netcdf_define_global(ncid, data_type, met_version)

    integer, intent(in)                :: ncid
    character(*), intent(in)           :: data_type
    character(*), intent(in), optional :: met_version
    
    type(datetime_type)                :: datetime

    call datetime%init()

    select case(data_type)
    case('CMORPH_0P10')
      if(.not. present(met_version)) stop "Error: transport CMORPH to netcdf is for run met, met_version is not input"
      call check(nf90_put_att(ncid, nf90_global, "Profection", "LatLon"))
      call check(nf90_put_att(ncid, nf90_global, "lat_ll", "15.05 degrees_north"))
      call check(nf90_put_att(ncid, nf90_global, "lon_ll", "70.05 degrees_east"))
      call check(nf90_put_att(ncid, nf90_global, "delta_lat", "0.1 degrees"))
      call check(nf90_put_att(ncid, nf90_global, "delta_lon", "0.1 degrees"))
      call check(nf90_put_att(ncid, nf90_global, "Nlat", "440 grid_points"))
      call check(nf90_put_att(ncid, nf90_global, "Nlon", "700 grid_points"))
      call check(nf90_put_att(ncid, nf90_global, "MET_version", met_version))
      call check(nf90_put_att(ncid, nf90_global, "creation_date", trim(datetime%isoformat())))
    case default
      call check(nf90_put_att(ncid, nf90_global, "creation_date", trim(datetime%isoformat())))
    end select

    call check(nf90_enddef(ncid))

  end subroutine netcdf_define_global


  subroutine netcdf_write_coordinate(this, lon, lat, lev, time)

    type(netcdf_type), intent(in)           :: this
    real             , intent(in), optional :: lon(:)
    real             , intent(in), optional :: lat(:)
    real             , intent(in), optional :: lev(:)
    real             , intent(in), optional :: time(:)

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    if (precent(lon))  call check(nf90_put_var(this%ncid, this%x_varid, lon))
    if (precent(lat))  call check(nf90_put_var(this%ncid, this%y_varid, lat))
    if (precent(lev))  call check(nf90_put_var(this%ncid, this%z_varid, lev))
    if (precent(time)) call check(nf90_put_var(this%ncid, this%t_varid, time))

  end subroutine netcdf_write_coordinate


  subroutine netcdf_write_data_short(this, var_name, data)

    type(netcdf_type), intent(in) :: this
    integer(2)       , intent(in) :: data(this%nx, this%ny, this%nz, this%nt)
    character(*)     , intent(in) :: var_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(var_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_short


  subroutine netcdf_write_data_int(this, var_name, data)

    type(netcdf_type), intent(in) :: this
    integer(4)       , intent(in) :: data(this%nx, this%ny, this%nz, this%nt)
    character(*)     , intent(in) :: var_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(var_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_int


  subroutine netcdf_write_data_long(this, var_name, data)

    type(netcdf_type), intent(in) :: this
    integer(8)       , intent(in) :: data(this%nx, this%ny, this%nz, this%nt)
    character(*)     , intent(in) :: var_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(var_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_long


  subroutine netcdf_write_data_float(this, var_name, data)

    type(netcdf_type), intent(in) :: this
    real(4)          , intent(in) :: data(this%nx, this%ny, this%nz, this%nt)
    character(*)     , intent(in) :: var_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(var_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_float


  subroutine netcdf_write_data_double(this, var_name, data)

    type(netcdf_type), intent(in) :: this
    real(8)          , intent(in) :: data(this%nx, this%ny, this%nz, this%nt)
    character(*)     , intent(in) :: var_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(var_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_double


  subroutine netcdf_close(nthis)

    type(netcdf_type), intent(in) :: this

    call check(nf90_close(this%ncid))

  end subroutine netcdf_close



end module netcdf_io_mod

