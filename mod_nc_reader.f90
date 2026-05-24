!======================================================================!
! Module: mod_nc_reader
! Author: Yan Chen
! Date:   2026-05-24
! Purpose: Read NetCDF data for WRF intermediate format conversion
!======================================================================!

module mod_nc_reader

    use netcdf
    implicit none

    ! Type definitions
    type :: tp_nc_meta
        character(len=200) :: file_name
        character(len=50)  :: var_name
        character(len=50)  :: lat_name
        character(len=50)  :: lon_name
        character(len=50)  :: lev_name
        character(len=50)  :: time_name
    end type tp_nc_meta

    type :: tp_nc_data
        integer :: nx, ny, nz, nt
        real, allocatable :: slab(:,:,:)  ! (lon, lat, time)
        real :: startlat, startlon
        real :: deltalat, deltalon
        character(len=50) :: units
        character(len=100) :: hdate
    end type tp_nc_data

contains

!======================================================================!
! Subroutine: nc_read_data
! Purpose: Read variable data from NetCDF file
!======================================================================!

subroutine nc_read_data(nc_data, nc_meta)

    implicit none
    type(tp_nc_data), intent(out) :: nc_data
    type(tp_nc_meta), intent(in)  :: nc_meta
    
    ! Local variables
    integer :: ncid, varid, status
    integer :: latid, lonid, timeid
    integer :: ndims, nvars, ngatts, unlimdimid
    integer :: lat_dimid, lon_dimid, time_dimid
    integer :: dimids(3)
    integer :: start(3), count(3)
    integer :: i, j, t
    real, allocatable :: lats(:), lons(:), times(:)
    real, allocatable :: data_2d(:,:), data_3d(:,:,:)
    character(len=100) :: time_units
    character(len=20) :: calendar
    integer :: year, month, day, hour, minute, second
    integer :: days_since
    
    ! Open NetCDF file
    status = nf90_open(trim(nc_meta%file_name), nf90_nowrite, ncid)
    call check_nc_status(status, "Opening file: " // trim(nc_meta%file_name))
    
    ! Get dimensions
    status = nf90_inq_dimid(ncid, trim(nc_meta%lon_name), lon_dimid)
    call check_nc_status(status, "Getting longitude dimension ID")
    
    status = nf90_inq_dimid(ncid, trim(nc_meta%lat_name), lat_dimid)
    call check_nc_status(status, "Getting latitude dimension ID")
    
    status = nf90_inq_dimid(ncid, trim(nc_meta%time_name), time_dimid)
    if (status /= nf90_noerr) then
        ! Try alternative time dimension names
        status = nf90_inq_dimid(ncid, "Time", time_dimid)
        if (status /= nf90_noerr) then
            status = nf90_inq_dimid(ncid, "t", time_dimid)
        end if
    end if
    call check_nc_status(status, "Getting time dimension ID")
    
    ! Get dimension sizes
    status = nf90_inquire_dimension(ncid, lon_dimid, len=nc_data%nx)
    call check_nc_status(status, "Getting longitude dimension size")
    
    status = nf90_inquire_dimension(ncid, lat_dimid, len=nc_data%ny)
    call check_nc_status(status, "Getting latitude dimension size")
    
    status = nf90_inquire_dimension(ncid, time_dimid, len=nc_data%nt)
    call check_nc_status(status, "Getting time dimension size")
    
    print*, "  Grid dimensions: ", nc_data%nx, "x", nc_data%ny
    print*, "  Time steps: ", nc_data%nt
    
    ! Read latitude and longitude arrays
    allocate(lats(nc_data%ny))
    allocate(lons(nc_data%nx))
    
    status = nf90_inq_varid(ncid, trim(nc_meta%lat_name), latid)
    call check_nc_status(status, "Getting latitude variable ID")
    
    status = nf90_inq_varid(ncid, trim(nc_meta%lon_name), lonid)
    call check_nc_status(status, "Getting longitude variable ID")
    
    status = nf90_get_var(ncid, latid, lats)
    call check_nc_status(status, "Reading latitude data")
    
    status = nf90_get_var(ncid, lonid, lons)
    call check_nc_status(status, "Reading longitude data")
    
    ! Calculate grid spacing and starting points
    nc_data%startlat = lats(1)
    nc_data%startlon = lons(1)
    nc_data%deltalat = lats(2) - lats(1)
    nc_data%deltalon = lons(2) - lons(1)
    
    ! Get variable ID
    status = nf90_inq_varid(ncid, trim(nc_meta%var_name), varid)
    call check_nc_status(status, "Getting variable ID: " // trim(nc_meta%var_name))
    
    ! Get variable attributes
    status = nf90_get_att(ncid, varid, "units", nc_data%units)
    if (status /= nf90_noerr) then
        nc_data%units = "unknown"
    end if
    
    ! Check variable dimensions
    status = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
    call check_nc_status(status, "Inquiring variable dimensions")
    
    ! Allocate slab array (lon, lat, time)
    allocate(nc_data%slab(nc_data%nx, nc_data%ny, nc_data%nt))
    
    ! Read data based on dimensionality
    if (ndims == 3) then
        ! Read all time steps at once
        status = nf90_get_var(ncid, varid, nc_data%slab, &
                              start=(/1, 1, 1/), &
                              count=(/nc_data%nx, nc_data%ny, nc_data%nt/))
        call check_nc_status(status, "Reading 3D variable data")
        
    else if (ndims == 2) then
        ! 2D variable (no time dimension) - replicate across time
        allocate(data_2d(nc_data%nx, nc_data%ny))
        status = nf90_get_var(ncid, varid, data_2d)
        call check_nc_status(status, "Reading 2D variable data")
        
        do t = 1, nc_data%nt
            nc_data%slab(:,:,t) = data_2d(:,:)
        end do
        deallocate(data_2d)
        
    else
        print*, "Error: Unsupported number of dimensions: ", ndims
        stop
    end if
    
    ! Read time information (if available)
    status = nf90_inq_varid(ncid, trim(nc_meta%time_name), timeid)
    if (status == nf90_noerr) then
        allocate(times(nc_data%nt))
        status = nf90_get_var(ncid, timeid, times)
        
        ! Get time units
        status = nf90_get_att(ncid, timeid, "units", time_units)
        if (status == nf90_noerr) then
            ! Parse time units and set hdate for first time step
            call parse_time_units(time_units, times(1), nc_data%hdate)
        else
            nc_data%hdate = "1979-01-01_00:00:00"
        end if
        deallocate(times)
    else
        nc_data%hdate = "1979-01-01_00:00:00"
    end if
    
    ! Close NetCDF file
    status = nf90_close(ncid)
    call check_nc_status(status, "Closing file")
    
    deallocate(lats, lons)
    
    print*, "  Successfully read: ", trim(nc_meta%var_name)
    print*, "  Units: ", trim(nc_data%units)
    
end subroutine nc_read_data

!======================================================================!
! Subroutine: get_var
! Purpose: Get variable IDs for all surface fields
!======================================================================!

subroutine get_var(varid)

    implicit none
    character(len=9), dimension(1:18), intent(out) :: varid
    
    varid(1)='sst'
    varid(2)='sst'
    varid(3)='swvl1'
    varid(4)='swvl2'
    varid(5)='swvl3'
    varid(6)='swvl4'
    varid(7)='stl1'
    varid(8)='stl2'
    varid(9)='stl3'
    varid(10)='stl4'
    varid(11)='msl'
    varid(12)='sd'
    varid(13)='skt'
    varid(14)='sp'
    varid(15)='t2m'
    varid(16)='d2m'
    varid(17)='u10'
    varid(18)='v10'
    
end subroutine get_var

!======================================================================!
! Subroutine: get_time
! Purpose: Generate time identifiers for each day of the year
!======================================================================!

subroutine get_time(timeid)

    implicit none
    character(len=19), dimension(1:365), intent(out) :: timeid
    integer :: year, month, day, doy
    integer :: days_in_month(12)
    integer :: i, cum_days
    
    year = 1979
    days_in_month = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
    
    cum_days = 0
    do month = 1, 12
        do day = 1, days_in_month(month)
            cum_days = cum_days + 1
            write(timeid(cum_days), '(I4.4,"-",I2.2,"-",I2.2,"_00:00:00")') &
                  year, month, day
        end do
    end do
    
end subroutine get_time

!======================================================================!
! Subroutine: set_resolution
! Purpose: Set grid resolution in WRF data structure
!======================================================================!

subroutine set_resolution(wrf_data, nx, ny)

    use mod_wrf_writer, only: tp_wrf_data
    implicit none
    type(tp_wrf_data), intent(inout) :: wrf_data
    integer, intent(in) :: nx, ny
    
    wrf_data%nx = nx
    wrf_data%ny = ny
    wrf_data%nxl = 1
    wrf_data%nyl = 1
    
end subroutine set_resolution

!======================================================================!
! Subroutine: set_slab
! Purpose: Set data slab in WRF data structure
!======================================================================!

subroutine set_slab(wrf_data, slab)

    use mod_wrf_writer, only: tp_wrf_data
    implicit none
    type(tp_wrf_data), intent(inout) :: wrf_data
    real, dimension(:,:), intent(in) :: slab
    
    integer :: nx, ny, i, j
    
    nx = size(slab, 1)
    ny = size(slab, 2)
    
    if (allocated(wrf_data%slab)) deallocate(wrf_data%slab)
    allocate(wrf_data%slab(nx, ny))
    
    wrf_data%slab = slab
    
end subroutine set_slab

!======================================================================!
! Subroutine: parse_time_units
! Purpose: Parse NetCDF time units to WRF date format
!======================================================================!

subroutine parse_time_units(time_units, time_value, hdate)

    implicit none
    character(len=*), intent(in) :: time_units
    real, intent(in) :: time_value
    character(len=*), intent(out) :: hdate
    
    integer :: year, month, day, hour, minute, second
    integer :: ref_year, ref_month, ref_day, ref_hour
    integer :: days_offset, seconds_offset
    character(len=20) :: time_unit_type
    integer :: pos, status
    
    ! Default value
    hdate = "1979-01-01_00:00:00"
    
    ! Find reference date in time_units string
    ! Format example: "hours since 1979-01-01 00:00:00"
    pos = index(time_units, "since")
    if (pos > 0) then
        read(time_units(pos+6:), *, iostat=status) ref_year, ref_month, ref_day
        ref_hour = 0
        hdate = "1979-01-01_00:00:00" ! Will be updated with actual date
    end if
    
end subroutine parse_time_units

!======================================================================!
! Subroutine: check_nc_status
! Purpose: Check NetCDF status and handle errors
!======================================================================!

subroutine check_nc_status(status, message)

    use netcdf
    implicit none
    integer, intent(in) :: status
    character(len=*), intent(in) :: message
    
    if (status /= nf90_noerr) then
        print*, "Error: ", trim(message)
        print*, "NetCDF error: ", trim(nf90_strerror(status))
        stop
    end if
    
end subroutine check_nc_status

end module mod_nc_reader
