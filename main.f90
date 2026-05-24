!======================================================================!
! Program: wrf_intermediate_sfc
! Author:  Yan Chen
! Date:    2019-02-09
! Purpose: Convert ECMWF ERA-Interim NetCDF surface data 
!          to WRF intermediate format
!======================================================================!

program wrf_intermediate_sfc

    use mod_nc_reader
    use mod_wrf_writer

    implicit none
    
    ! Program parameters
    character(len=*), parameter :: VERSION = "1.0"
    character(len=*), parameter :: AUTHOR  = "Yan Chen"
    
    ! Data structures
    type(tp_wrf_data) :: wrf_data
    type(tp_nc_data) :: nc_data
    type(tp_nc_meta) :: nc_meta
    
    ! File and directory variables
    character(len=100) :: dir, fn, out_ti, out_file_name
    
    ! Variable lists
    character(len=12), dimension(1:18) :: field_i
    character(len=19), dimension(1:365) :: timeid
    character(len=9), dimension(1:18) :: varid
    
    ! Loop counters
    integer :: nv, nt, ios
    
    ! Print header
    print*, "==========================================="
    print*, "WRF Intermediate Surface Data Converter"
    print*, "Version: ", VERSION
    print*, "Author:  ", AUTHOR
    print*, "==========================================="
    print*, ""
    
    ! Initialize field names (WRF standard names)
    field_i(1)='SEAICE';   field_i(7)='ST000007';  field_i(13)='SKINTEMP'
    field_i(2)='SST';      field_i(8)='ST007028';  field_i(14)='PSFC'
    field_i(3)='SM000007'; field_i(9)='ST028100';  field_i(15)='TT'
    field_i(4)='SM007028'; field_i(10)='ST100289'; field_i(16)='DEWPT'
    field_i(5)='SM028100'; field_i(11)='PMSL';     field_i(17)='UU'
    field_i(6)='SM100289'; field_i(12)='SNOW_EC';  field_i(18)='VV'

    ! Input file configuration (MODIFY THESE PATHS)
    dir = "/path/to/your/input/data/"   ! <-- CHANGE THIS
    fn  = "srf.1979.00.nc"               ! <-- CHANGE THIS
    out_ti = "SFC:"

    ! Setup NetCDF metadata
    nc_meta % file_name = TRIM(dir)//TRIM(fn)
    print*, "Input file: ", TRIM(nc_meta % file_name)
    
    nc_meta % lat_name = "latitude"
    nc_meta % lon_name = "longitude"
    nc_meta % time_name = "time"

    ! Get variable IDs and time identifiers
    call get_var(varid)
    call get_time(timeid)
    
    print*, ""
    print*, "Processing 18 variables for 365 days..."
    print*, "Total output files expected: ", 18 * 365
    print*, ""

    ! Main processing loops
    do nv = 1, 18
       
       print*, "Processing variable: ", TRIM(varid(nv)), " -> ", TRIM(field_i(nv))
       
       nc_meta%var_name = varid(nv)
       call nc_read_data(nc_data, nc_meta)

       do nt = 1, 365
          
          ! Optional: print min/max values for validation
          ! print *, "Day ", nt, " min: ", minval(nc_data%slab(:,:,nt))
          ! print *, "Day ", nt, " max: ", maxval(nc_data%slab(:,:,nt))

          ! Setup WRF metadata
          ! See: http://www2.mmm.ucar.edu/wrf/users/docs/user_guide/users_guide_chap3.html#_Writing_Meteorological_Data
          
          wrf_data%version = 5
          wrf_data%field  = field_i(nv)

          call set_resolution(wrf_data, nc_data%nx, nc_data%ny)

          wrf_data%iproj = 3               ! Lambert projection
          wrf_data%nlats = 121
          wrf_data%xfcst = 0.0
          wrf_data%xlvl = 0.0
          wrf_data%startlat = nc_data%startlat
          wrf_data%startlon = nc_data%startlon
          wrf_data%deltalat = nc_data%deltalat
          wrf_data%deltalon = nc_data%deltalon
          wrf_data%dx = 25.0               ! Grid resolution (km)
          wrf_data%dy = 25.0               ! Grid resolution (km)
          wrf_data%xlonc = 95.0            ! Central longitude
          wrf_data%truelat1 = 30.0         ! First standard latitude
          wrf_data%truelat2 = 60.0         ! Second standard latitude
          wrf_data%earth_radius = 6371.0   ! Earth radius (km)

          call set_slab(wrf_data, nc_data%slab(:,:,nt))

          wrf_data%is_wind_grid_rel = .FALSE.
          wrf_data%startloc = "SWCORNER"
          wrf_data%units = nc_data%units
          wrf_data%map_source = "European Center for Medium-Range Weather Forecasts (RSMC)"
          wrf_data%desc = ""
   
          wrf_data%hdate = timeid(nt)
          out_file_name  = TRIM(out_ti)//wrf_data%hdate(1:13)
   
          ! Write output file
          call wrf_write(out_file_name, wrf_data)
          
          ! Progress indicator
          if (mod(nt, 30) == 0) then
             print*, "  Progress: ", nt, "/365 days completed for this variable"
          end if
          
       end do
       
       print*, "Completed variable: ", TRIM(field_i(nv))
       print*, ""
       
    end do

    ! Completion message
    print*, "==========================================="
    print*, "Conversion completed successfully!"
    print*, "Total files created: ", 18 * 365
    print*, "==========================================="

end program wrf_intermediate_sfc
