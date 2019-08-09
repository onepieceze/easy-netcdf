module netcdf_io_mod

  !! The io module to open or create netcdf file. 

  use netcdf
  use datetime
  use linked_list_mod
  use hash_table_mod
  use netcdf_tool_mod

  implicit none

  private

  public :: netcdf_type

  type :: dimension_type
    integer, private                   :: length = 1
    integer, private                   :: varid  = -999
    type(hash_table_type)              :: attribute
  end type dimension_type


  type :: variable_type
    character(:), allocatable          :: name
    integer     , private              :: varid
    type(hash_table_type)              :: attribute
  end type


  type :: netcdf_type
    character(:), allocatable, private :: iotype
    integer                  , private :: ncid
    integer     , allocatable, private :: dimids(:)
    type(dimension_type)               :: x
    type(dimension_type)               :: y
    type(dimension_type)               :: z
    type(dimension_type)               :: t
    type(linked_list_type)   , private :: variable_list
    logical                  , private :: define_end  = .false.
    logical                  , private :: inquire_end = .false.
  contains
    !! open model
    procedure                          :: netcdf_open
    procedure                          :: netcdf_inquire_coordinate
    procedure                , private :: netcdf_get_coordinate_int
    procedure                , private :: netcdf_get_coordinate_real
    generic                            :: netcdf_get_coordinate => &
                                          netcdf_get_coordinate_int, netcdf_get_coordinate_real
    procedure                , private :: netcdf_get_data_short
    procedure                , private :: netcdf_get_data_int
    procedure                , private :: netcdf_get_data_long
    procedure                , private :: netcdf_get_data_float
    procedure                , private :: netcdf_get_data_double
    generic                            :: netcdf_get_data => &
                                          netcdf_get_data_short, netcdf_get_data_int,   &
                                          netcdf_get_data_long,  netcdf_get_data_float, &
                                          netcdf_get_data_double
    !! create model  
    procedure                          :: netcdf_create
    procedure                          :: netcdf_define_coordinate
    procedure                          :: netcdf_define_variable
    procedure                          :: netcdf_define_global
    procedure                          :: netcdf_write_coordinate
    procedure                , private :: netcdf_write_data_short
    procedure                , private :: netcdf_write_data_int
    procedure                , private :: netcdf_write_data_int_3d
    procedure                , private :: netcdf_write_data_long
    procedure                , private :: netcdf_write_data_float
    procedure                , private :: netcdf_write_data_double
    generic                            :: netcdf_write_data => &
                                          netcdf_write_data_short, netcdf_write_data_int,   &
                                          netcdf_write_data_long,  netcdf_write_data_float, &
                                          netcdf_write_data_double, netcdf_write_data_int_3d
    !! close netcdf file
    procedure                          :: netcdf_close
  end type netcdf_type

contains


  subroutine netcdf_open(this, file_name)

    class(netcdf_type), intent(inout) :: this
    character(*)      , intent(in)    :: file_name

    integer                           :: ncid
    
    if (allocated(this%iotype)) stop "Error: netcdf file have been open or create."

    this%iotype = "read"

    call check(nf90_open(file_name, nf90_nowrite, ncid))

    this%ncid = ncid

  end subroutine netcdf_open


  subroutine netcdf_inquire_coordinate(this)
    
    class(netcdf_type), intent(inout) :: this

    character(20)                     :: dimension_name   
    integer                           :: dimension_number
    integer                           :: i
    integer                           :: length

    if (.not. allocated(this%iotype)) stop "Error: netcdf file is not open or create."
    if (this%iotype /= "read")        stop "Error: netcdf file open model is not read model."

    call check(nf90_inquire(this%ncid, dimension_number))

    do i=1, dimension_number
      call check(nf90_inquire_dimension(this%ncid, i, dimension_name, length))
      select case (dimension_name)
      case("lon", "longitude")
        call check(nf90_inq_varid(this%ncid, dimension_name, this%x%varid))
        this%x%length = length
      case("lat", "latitude")
        call check(nf90_inq_varid(this%ncid, dimension_name, this%y%varid))
        this%y%length = length
      case("lev", "level", "height", "altitude", "depth")
        call check(nf90_inq_varid(this%ncid, dimension_name, this%z%varid))
        this%z%length = length
      case("time")
        call check(nf90_inq_varid(this%ncid, dimension_name, this%t%varid))
        this%t%length = length
      case default
        stop "Error: check dimension name case in the source code."
      end select
    end do

    this%inquire_end = .true.

  end subroutine netcdf_inquire_coordinate


  subroutine netcdf_get_coordinate_int(this, dimension_name, data)

    class(netcdf_type), intent(in)               :: this
    character(*)      , intent(in)               :: dimension_name
    integer           , allocatable, intent(out) :: data(:)

    integer                                      :: varid
    integer                                      :: n

    if (.not. this%inquire_end)       stop "Error: netcdf_inquire_coordinate not done."

    select case (dimension_name)
    case("lon", "longitude")
      if (this%x%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%x%length))
      call check(nf90_get_var(this%ncid, this%x%varid, data))
    case("lat", "latitude")
      if (this%y%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%y%length))
      call check(nf90_get_var(this%ncid, this%y%varid, data))
    case("lev", "level", "height", "altitude", "depth")
      if (this%z%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%z%length))
      call check(nf90_get_var(this%ncid, this%z%varid, data))
    case("time")
      if (this%t%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%t%length))
      call check(nf90_get_var(this%ncid, this%t%varid, data))
    case default
      stop "Error: input dimension name is not correct."
    end select

  end subroutine netcdf_get_coordinate_int


  subroutine netcdf_get_coordinate_real(this, dimension_name, data)

    class(netcdf_type), intent(in)               :: this
    character(*)      , intent(in)               :: dimension_name
    real              , allocatable, intent(out) :: data(:)

    integer                                      :: varid
    integer                                      :: n

    if (.not. this%inquire_end)       stop "Error: netcdf_inquire_coordinate not done."

    select case (dimension_name)
    case("lon", "longitude")
      if (this%x%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%x%length))
      call check(nf90_get_var(this%ncid, this%x%varid, data))
    case("lat", "latitude")
      if (this%y%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%y%length))
      call check(nf90_get_var(this%ncid, this%y%varid, data))
    case("lev", "level", "height", "altitude", "depth")
      if (this%z%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%z%length))
      call check(nf90_get_var(this%ncid, this%z%varid, data))
    case("time")
      if (this%t%varid == -999) stop "Error: netcdf_inquire_coordinate not done or dimension not exist."
      allocate(data(this%t%length))
      call check(nf90_get_var(this%ncid, this%t%varid, data))
    case default
      stop "Error: input dimension name is not correct."
    end select

  end subroutine netcdf_get_coordinate_real


  subroutine netcdf_get_data_short(this, variable_name, data)

    class(netcdf_type), intent(in)  :: this
    character(*)      , intent(in)  :: variable_name
    integer(2)        , intent(out) :: data(this%x%length, this%y%length, this%z%length, this%t%length)

    integer                         :: varid

    if (.not. this%inquire_end)       stop "Error: netcdf_inquire_coordinate not done."

    call check(nf90_inq_varid(this%ncid, variable_name, varid))
    call check(nf90_get_var(this%ncid, varid, data))

  end subroutine netcdf_get_data_short


  subroutine netcdf_get_data_int(this, variable_name, data)

    class(netcdf_type), intent(in)  :: this
    character(*)      , intent(in)  :: variable_name
    integer(4)        , intent(out) :: data(this%x%length, this%y%length, this%z%length, this%t%length)

    integer                         :: varid

    if (.not. this%inquire_end)       stop "Error: netcdf_inquire_coordinate not done."

    call check(nf90_inq_varid(this%ncid, variable_name, varid))
    call check(nf90_get_var(this%ncid, varid, data))

  end subroutine netcdf_get_data_int


  subroutine netcdf_get_data_long(this, variable_name, data)

    class(netcdf_type), intent(in)  :: this
    character(*)      , intent(in)  :: variable_name
    integer(8)        , intent(out) :: data(this%x%length, this%y%length, this%z%length, this%t%length)

    integer                         :: varid

    if (.not. this%inquire_end)       stop "Error: netcdf_inquire_coordinate not done."

    call check(nf90_inq_varid(this%ncid, variable_name, varid))
    call check(nf90_get_var(this%ncid, varid, data))

  end subroutine netcdf_get_data_long


  subroutine netcdf_get_data_float(this, variable_name, data)

    class(netcdf_type), intent(in)  :: this
    character(*)      , intent(in)  :: variable_name
    real(4)           , intent(out) :: data(this%x%length, this%y%length, this%z%length, this%t%length)

    integer                         :: varid
 
    if (.not. this%inquire_end)       stop "Error: netcdf_inquire_coordinate not done."

    call check(nf90_inq_varid(this%ncid, variable_name, varid))
    call check(nf90_get_var(this%ncid, varid, data))

  end subroutine netcdf_get_data_float


  subroutine netcdf_get_data_double(this, variable_name, data)

    class(netcdf_type), intent(in)  :: this
    character(*)      , intent(in)  :: variable_name
    real(8)           , intent(out) :: data(this%x%length, this%y%length, this%z%length, this%t%length)

    integer                         :: varid

    if (.not. this%inquire_end)       stop "Error: netcdf_inquire_coordinate not done."

    call check(nf90_inq_varid(this%ncid, variable_name, varid))
    call check(nf90_get_var(this%ncid, varid, data))

  end subroutine netcdf_get_data_double


  subroutine netcdf_create(this, file_name)

    class(netcdf_type), intent(inout) :: this
    character(*)      , intent(in)    :: file_name

    integer                           :: ncid

    if (allocated(this%iotype)) stop "Error: netcdf file have been open or create."

    this%iotype = "write"

    call check(nf90_create(file_name, NF90_CLOBBER, ncid))

    this%ncid = ncid

  end subroutine netcdf_create


  subroutine netcdf_define_coordinate(this, nx, ny, nz, nt)

    class(netcdf_type) , intent(inout)             :: this
    integer            , optional     , intent(in) :: nx
    integer            , optional     , intent(in) :: ny
    integer            , optional     , intent(in) :: nz
    integer            , optional     , intent(in) :: nt
    
    type(hash_table_iterator_type)                 :: iter

    integer                                        :: x_varid
    integer                                        :: y_varid
    integer                                        :: z_varid
    integer                                        :: t_varid
    integer                                        :: x_dimid
    integer                                        :: y_dimid
    integer                                        :: z_dimid
    integer                                        :: t_dimid
    logical                                        :: is_x_dim_def = .false.
    logical                                        :: is_y_dim_def = .false.
    logical                                        :: is_z_dim_def = .false.
    logical                                        :: is_t_dim_def = .false.
    integer                                        :: dim_number

    if (.not. allocated(this%iotype)) stop "Error: netcdf file have not open or create."
    if (this%iotype /= "write")       stop "Error: netcdf open model is not write model."

    dim_number = 0

    if (present(nx)) then
      dim_number = dim_number + 1
      if (this%x%attribute%size /= 0) then
        iter = hash_table_iterator(this%x%attribute)
        do while (.not. iter%ended())
          select type(value => iter%value)
          type is (character(*))
            if (iter%key == "name") then
              call check(nf90_def_dim(this%ncid, value, nx, x_dimid))
              call check(nf90_def_var(this%ncid, value, nf90_real, x_dimid, x_varid))
              is_x_dim_def = .true.
            else
              call check(nf90_put_att(this%ncid, x_varid, iter%key, value))
            end if
          end select
          call iter%next()
        end do
        if (.not. is_x_dim_def) stop "Error: input x_table have not x dimension name."
      else
        call check(nf90_def_dim(this%ncid, "lon", nx, x_dimid))
        call check(nf90_def_var(this%ncid, "lon", nf90_real, x_dimid, x_varid))
        call check(nf90_put_att(this%ncid, x_varid, "long_name", "longitude"))
        call check(nf90_put_att(this%ncid, x_varid, "units", "degrees_east"))
      end if
      this%x%varid = x_varid
      this%x%length = nx
    end if

    if (present(ny)) then
      dim_number = dim_number + 1
      if (this%y%attribute%size /= 0) then
        iter = hash_table_iterator(this%y%attribute)
        do while (.not. iter%ended())
          select type(value => iter%value)
          type is (character(*))
            if (iter%key == "name") then
              call check(nf90_def_dim(this%ncid, value, ny, y_dimid))
              call check(nf90_def_var(this%ncid, value, nf90_real, y_dimid, y_varid))
              is_y_dim_def = .true.
            else
              call check(nf90_put_att(this%ncid, y_varid, iter%key, value))
            end if
          end select
          call iter%next()
        end do
        if (.not. is_y_dim_def) stop "Error: input y_table have not y dimension name."
      else
        call check(nf90_def_dim(this%ncid, "lat", ny, y_dimid))
        call check(nf90_def_var(this%ncid, "lat", nf90_real, y_dimid, y_varid))
        call check(nf90_put_att(this%ncid, y_varid, "long_name", "latitude"))
        call check(nf90_put_att(this%ncid, y_varid, "units", "degrees_north"))
        this%y%varid = y_varid
        this%y%length = ny
      end if
    end if

    if (present(nz)) then
      dim_number = dim_number + 1
      if (this%z%attribute%size /= 0) then
        iter = hash_table_iterator(this%z%attribute)
        do while (.not. iter%ended())
          select type(value => iter%value)
          type is (character(*))
            if (iter%key == "name") then
              call check(nf90_def_dim(this%ncid, value, nz, z_dimid))
              call check(nf90_def_var(this%ncid, value, nf90_real, z_dimid, z_varid))
              is_z_dim_def = .true.
            else
              call check(nf90_put_att(this%ncid, z_varid, iter%key, value))
            end if
          end select
          call iter%next()
        end do
        if (.not. is_z_dim_def) stop "Error: input z_table have not z dimension name."
      else
        call check(nf90_def_dim(this%ncid, "level", nz, z_dimid))
        call check(nf90_def_var(this%ncid, "level", nf90_int, z_dimid, z_varid))
        call check(nf90_put_att(this%ncid, z_varid, "long_name", "Isobaric surface"))
        call check(nf90_put_att(this%ncid, z_varid, "units", "Pa"))
        this%z%varid = z_varid
        this%z%length = nz
      end if
    end if

    if (present(nt)) then
      dim_number = dim_number + 1
      if (this%t%attribute%size /= 0) then
        iter = hash_table_iterator(this%t%attribute)
        do while (.not. iter%ended())
          select type(value => iter%value)
          type is (character(*))
            if (iter%key == "name") then
              call check(nf90_def_dim(this%ncid, value, nt, t_dimid))
              call check(nf90_def_var(this%ncid, value, nf90_real, t_dimid, t_varid))
              is_t_dim_def = .true.
            else
              call check(nf90_put_att(this%ncid, t_varid, iter%key, value))
            end if
          end select
          call iter%next()
        end do
        if (.not. is_t_dim_def) stop "Error: input t_table have not t dimension name."
      else
        call check(nf90_def_dim(this%ncid, "time", nf90_unlimited, t_dimid))
        call check(nf90_def_var(this%ncid, "time", nf90_double, t_dimid, t_varid))
        call check(nf90_put_att(this%ncid, t_varid, "long_name", "Time"))
        call check(nf90_put_att(this%ncid, t_varid, "units", "seconds since 1970-01-01 00:00"))
        this%t%varid = t_varid
        this%t%length = nt
      end if
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


  subroutine netcdf_define_variable(this, variable_name, data_type, attribute)

    class(netcdf_type)   , intent(inout)             :: this
    character(*)         , intent(in)                :: variable_name
    integer              , intent(in)                :: data_type
    type(hash_table_type), optional     , intent(in) :: attribute   

    integer                                          :: varid
    type(hash_table_iterator_type)                   :: iter

    if (.not. allocated(this%iotype)) stop "Error: netcdf file have not open or create."
    if (this%iotype /= "write")       stop "Error: netcdf open model is not write model."
    if (.not. allocated(this%dimids)) stop "Error: netcdf file dimension was not define."

    call check(nf90_def_var(this%ncid, variable_name, data_type, this%dimids, varid))
    call this%variable_list%append(variable_name, varid)

    if(present(attribute)) then
      iter = hash_table_iterator(attribute)
      do while (.not. iter%ended())
        select type (value => iter%value)
        type is (character(*))
          call check(nf90_put_att(this%ncid, varid, iter%key, value))
        type is (integer)
          call check(nf90_put_att(this%ncid, varid, iter%key, value))
        type is (real)
          call check(nf90_put_att(this%ncid, varid, iter%key, value))
        end select
        call iter%next()
      end do
    end if
      
  end subroutine netcdf_define_variable

  
  subroutine netcdf_define_global(this, attribute)

    class(netcdf_type)             , intent(inout) :: this
    type(hash_table_type), optional, intent(in)    :: attribute
   
    type(hash_table_iterator_type)                 :: iter 
    type(datetime_type)                            :: datetime

    call datetime%init()

    call check(nf90_put_att(this%ncid, nf90_global, "creation_date", trim(datetime%isoformat())))
    
    if(present(attribute)) then
      iter = hash_table_iterator(attribute)
      do while (.not. iter%ended())
        select type (value => iter%value)
        type is (character(*))
          call check(nf90_put_att(this%ncid, nf90_global, iter%key, value))
        type is (integer)
          call check(nf90_put_att(this%ncid, nf90_global, iter%key, value))
        type is (real)
          call check(nf90_put_att(this%ncid, nf90_global, iter%key, value))
        end select
        call iter%next()
      end do
    end if

    call check(nf90_enddef(this%ncid))

    this%define_end = .true.

  end subroutine netcdf_define_global


  subroutine netcdf_write_coordinate(this, lon, lat, lev, time)

    class(netcdf_type), intent(in)           :: this
    real              , intent(in), optional :: lon(:)
    real              , intent(in), optional :: lat(:)
    integer           , intent(in), optional :: lev(:)
    real              , intent(in), optional :: time(:)

    if (.not. allocated(this%iotype)) stop "Error: netcdf file have not open or create."
    if (this%iotype /= "write")       stop "Error: netcdf open model is not write model."
    if (.not. allocated(this%dimids)) stop "Error: netcdf file dimension was not define."
    if (.not. this%define_end)        stop "Error: netcdf file define process is not end."

    if (present(lon))  call check(nf90_put_var(this%ncid, this%x%varid, lon))
    if (present(lat))  call check(nf90_put_var(this%ncid, this%y%varid, lat))
    if (present(lev))  call check(nf90_put_var(this%ncid, this%z%varid, lev))
    if (present(time)) call check(nf90_put_var(this%ncid, this%t%varid, time))

  end subroutine netcdf_write_coordinate


  subroutine netcdf_write_data_short(this, variable_name, data)

    class(netcdf_type), intent(in) :: this
    integer(2)        , intent(in) :: data(this%x%length, this%y%length, this%z%length, this%t%length)
    character(*)      , intent(in) :: variable_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(variable_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_short


  subroutine netcdf_write_data_int(this, variable_name, data)

    class(netcdf_type), intent(in) :: this
    integer(4)        , intent(in) :: data(this%x%length, this%y%length, this%z%length, this%t%length)
    character(*)      , intent(in) :: variable_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(variable_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_int


    subroutine netcdf_write_data_int_3d(this, variable_name, data)

    class(netcdf_type), intent(in) :: this
    integer(4)        , intent(in) :: data(this%x%length, this%y%length, this%z%length)
    character(*)      , intent(in) :: variable_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is notend."

    select type (value => this%variable_list%value(variable_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))

  end subroutine netcdf_write_data_int_3d


  subroutine netcdf_write_data_long(this, variable_name, data)

    class(netcdf_type), intent(in) :: this
    integer(8)        , intent(in) :: data(this%x%length, this%y%length, this%z%length, this%t%length)
    character(*)      , intent(in) :: variable_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(variable_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_long


  subroutine netcdf_write_data_float(this, variable_name, data)

    class(netcdf_type), intent(in) :: this
    real(4)           , intent(in) :: data(this%x%length, this%y%length, this%z%length, this%t%length)
    character(*)      , intent(in) :: variable_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(variable_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_float


  subroutine netcdf_write_data_double(this, variable_name, data)

    class(netcdf_type), intent(in) :: this
    real(8)           , intent(in) :: data(this%x%length, this%y%length, this%z%length, this%t%length)
    character(*)      , intent(in) :: variable_name

    integer                       :: varid

    if (.not. this%define_end) stop "Error: netcdf file define process is not end."

    select type (value => this%variable_list%value(variable_name))
    type is (integer)
      varid = value
    end select
    call check(nf90_put_var(this%ncid, varid, data))
    
  end subroutine netcdf_write_data_double


  subroutine netcdf_close(this)

    class(netcdf_type), intent(in) :: this

    call check(nf90_close(this%ncid))

  end subroutine netcdf_close


end module netcdf_io_mod

