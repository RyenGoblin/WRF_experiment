subroutine da_read_obs_hdf5mwts2d (iv, infile_tb)
   !--------------------------------------------------------
   !  Purpose: read in FY3D MWTS2 Level-1 data in HDF5 format
   !           and form innovation structure
   !
   !   METHOD: use F90 sequential data structure to avoid reading the file twice
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2023/-3/25 - Creation         Fang Di
   !------------------------------------------------------------------------------

   implicit none

   character(len=*), intent(in)    :: infile_tb
   type(iv_type),    intent(inout) :: iv

#if defined(HDF5)
! fixed parameter values
   integer,parameter::max_scan=2000     ! Maximum allowed NumberOfScans 总扫描线数 maybe1250
   integer,parameter::lo_rez_fov=90    ! low  resolution pixel width 
   integer,parameter::time_dims=6       ! Time dimension  maybe 3 (nchanl*nscans*pixel)
   integer,parameter::nfile_max = 8     ! each hdf file contains ~100min of data
                                        ! at most 6 files for a 6-h time window
   integer,parameter  :: num_low_freq_chan=13
! interface variable
   integer iret                         ! return status
   integer(HID_T) fhnd1, fhnd2, fhnd3   ! file handle
   integer(HID_T) ahnd1, ahnd2, ahnd3   ! attribute handle
   integer(HID_T) dhnd1, dhnd2, dhnd3   ! dataset handle
   integer(HID_T) shnd1, shnd2, shnd3   ! dataspace handle
   integer(HSIZE_T) sz1(3)              ! array size 1
   integer(HSIZE_T) sz2(3)              ! array size 2

   integer(4) :: nscan                       ! NumberOfScans
   real(4)    :: slp,itp                         ! ScaleFactor

   integer :: yr,mt,dy,hr,mn,sc
   integer :: tess,tesp,allocstat
   integer :: scnday(max_scan),scntim(max_scan)
   real(8) :: ju
   integer,allocatable     :: date(:,:)
   real(4),allocatable     :: tbb(:,:,:)
   integer(4),allocatable  :: tbb0(:,:,:)  !!!notice:integer 

! array data
   real(4) :: latlr(lo_rez_fov,max_scan)   ! lat for low resolution
   real(4) :: lonlr(lo_rez_fov,max_scan)   ! lon for low resolution

!   real(4) :: tbb(90,1213,13)

   integer(4) :: loflo(lo_rez_fov,max_scan) ! land ocean flag for low
   integer(4) :: elev(lo_rez_fov,max_scan)  ! elevation

   integer    :: satzen0(lo_rez_fov,max_scan)  ! sensor zenith
   integer    :: satazi0(lo_rez_fov,max_scan)  ! sensor azimuth
   integer    :: solzen0(lo_rez_fov,max_scan)  ! solar zenith
   integer    :: solazi0(lo_rez_fov,max_scan)  ! solar azimuth

   real    :: satzen(lo_rez_fov,max_scan)  ! sensor zenith
   real    :: satazi(lo_rez_fov,max_scan)  ! sensor azimuth
   real    :: solzen(lo_rez_fov,max_scan)  ! solar zenith
   real    :: solazi(lo_rez_fov,max_scan)  ! solar azimuth

   
   real(r_kind)            :: R90    = 90.0_r_kind
   real(r_kind),parameter  :: tbmin  = 50._r_kind   ! 极值检验 50 < tb <500
   real(r_kind),parameter  :: tbmax  = 550._r_kind


   real(kind=8)                   :: obs_time
   type (datalink_type),pointer   :: head, p, current, prev
   type(info_type)                :: info
   type(model_loc_type)           :: loc

   integer(i_kind)    :: idate5(6)

   integer(i_kind)   :: inst,platform_id,satellite_id,sensor_id
   real(r_kind)      :: tb, crit
   integer(i_kind)   :: ifgat, iout, iobs
   logical           :: outside, outside_all, iuse

   integer           :: i,j,k,l,m,n, ifile, landsea_mask
   logical           :: found, head_found, head_allocated

! Other work variables
   real(r_kind)     :: dlon_earth,dlat_earth
   integer(i_kind)  :: num_mwts2d_local, num_mwts2d_global, num_mwts2d_used, num_mwts2d_thinned
   integer(i_kind)  :: num_mwts2d_used_tmp, num_mwts2d_file
   integer(i_kind)  :: num_mwts2d_local_local, num_mwts2d_global_local, num_mwts2d_file_local
   integer(i_kind)  :: itx, itt
   character(80)    :: filename1, filename2
   integer          :: nchan,ifov,iscan,ichannels
   integer          :: nfile
   character(80)    :: fname_tb(nfile_max)
   logical          :: fexist

! Allocatable arrays
   integer(i_kind),allocatable  :: ptotal(:)
   real,allocatable             :: in(:), out(:)
   
   if (trace_use) call da_trace_entry("da_read_obs_hdf5mwts2d")

!  0.0  Initialize variables
!-----------------------------------
   head_allocated = .false.
   platform_id  = 23  ! Table-2 Col 1 corresponding to 'fy3'
   satellite_id = 4   ! Table-2 Col 3 corresponding to 'D'
   sensor_id    = 72  ! Table-3 Col 2 corresponding to 'mwts2d'

   allocate(ptotal(0:num_fgat_time))
   ptotal(0:num_fgat_time) = 0
   iobs = 0                 ! for thinning, argument is inout
   num_mwts2d_file    = 0
   num_mwts2d_local   = 0
   num_mwts2d_global  = 0
   num_mwts2d_used    = 0
   num_mwts2d_thinned = 0

   do i = 1, rtminit_nsensor
      if (platform_id  == rtminit_platform(i) &
          .and. satellite_id == rtminit_satid(i)    &
          .and. sensor_id    == rtminit_sensor(i)) then
         inst = i
         exit
      end if
   end do
   if (inst == 0) then
      call da_warning(__FILE__,__LINE__, &
          (/"The combination of Satellite_Id and Sensor_Id for MWTS-2 is not found"/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5mwts2d")
      return
   end if

! Initialize HDF5 library and Fortran90 interface
   call H5open_f(iret)
   if(iret.lt.0)then
      call da_warning(__FILE__,__LINE__, &
           (/"Problems in Initializing HDF5 library. Can not read MWTS-2d HDF data. "/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5mwts2d")
      return
   endif

   nchan = iv%instid(inst)%nchan
   write(unit=stdout,fmt=*)'MWTS2d nchan: ',nchan

! 1.0 Assign file names and prepare to read mwhs2 files
!-------------------------------------------------------------------------
   nfile       = 0  !initialize
   fname_tb(:) = '' !initialize
   ! first check if L1SGRTBR.h5 is available
   filename1 = trim(infile_tb)//'.hdf'
   inquire (file=filename1, exist=fexist)

   if ( fexist ) then
      nfile = 1
      fname_tb(nfile)  = filename1
   else
      ! check if L1SGRTBR-0x.h5 is available for multiple input files
      ! here 0x is the input file sequence number  
      ! do not confuse it with fgat time slot index
      ! 将文件重命名为 mwts2d-01.hdf, mwts2d-02.hdf...
      do i = 1, nfile_max
         write(filename1,fmt='(A,A,I2.2,A)') trim(infile_tb),'-',i,'.hdf'
         inquire (file=filename1, exist=fexist)
         if ( fexist ) then
            nfile = nfile + 1
            fname_tb(nfile)  = filename1
         else
            exit
         end if
      end do
   end if

   if ( nfile == 0 ) then
      call da_warning(__FILE__,__LINE__, &
         (/"No valid mwts2d .hdf file found."/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5mwts2d")
      return
   end if

!!   ! Check to see if leap second file exists for graceful failure
!!     inquire( file='leapsec.dat', exist=fexist )
!!      if (.not. fexist) call da_error(__FILE__,__LINE__, &
!!           (/'Can not find leapsec.dat for MWHS2 data: copy or link from WRFDA/var/run'/))

   infile_loop:  do ifile = 1, nfile
      num_mwts2d_file_local    = 0
      num_mwts2d_local_local   = 0
      num_mwts2d_global_local  = 0

   ! open HDF5 file for read
      call H5Fopen_f(fname_tb(ifile),H5F_ACC_RDONLY_F,fhnd1,iret,H5P_DEFAULT_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
              (/"Cannot open HDF5 file "//trim(fname_tb(ifile))/))
         cycle infile_loop
      endif

   ! calculate NumberOfScans from array size and OverlapScans
      call h5dopen_f(fhnd1,"/Geolocation/Latitude",dhnd1,iret)
      call h5dget_space_f(dhnd1,shnd1,iret)
      call h5sget_simple_extent_dims_f(shnd1,sz1,sz2,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read problem for: Latitude"/))
      endif
      call H5Sclose_f(shnd1,iret)
      call H5Dclose_f(dhnd1,iret)
      nscan=sz1(2)

      write(unit=stdout,fmt=*)'NumberOfScans(RETRIEVE BY ARRAY SIZE): ',nscan

   ! check limit
      if(nscan.gt.max_scan)then
         write(unit=stdout,fmt=*)'limit of NumberOfScans = ',max_scan
         call da_warning(__FILE__,__LINE__, &
              (/"HDF5 lmit error for: max_scan"/))
      endif

   ! read array: scantime
   ! read
      sz1(1)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/Scnlin_daycnt",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,scnday,sz1,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Scan day Scnlin_daycnt"/))
      endif
      call h5dclose_f(dhnd1, iret)
      do j=nscan+1,max_scan
	      scnday(j)=0
      end do

      call h5dopen_f(fhnd1,"/Geolocation/Scnlin_mscnt",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,scntim,sz1,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Scan Time Scnlin_mscnt"/))
      endif
      call h5dclose_f(dhnd1,iret)

      do j=nscan+1,max_scan
	      scntim(j)=0
      end do

      allocate(date(nscan,6))
      do j=1,nscan
          ju=scnday(j)+dfloat(scntim(j))/86400000.
          call mjd2cal2td(ju,yr,mt,dy,hr,mn,sc)
          date(j,1)=yr
          date(j,2)=mt
          date(j,3)=dy
          date(j,4)=hr
          date(j,5)=mn
          date(j,6)=sc
      end do

	tess = 1000 !????? 
	tesp = 10   !????? 
   ! sample display
      write(unit=stdout,fmt=*)'time=',tesp,tess,date(tess,:)


   ! read array: tb
   ! read
   
   !   sz1(1)=90
   !   sz1(2)=1213
   !   sz1(3)=13

      sz1(1)=lo_rez_fov
      sz1(2)=nscan
      sz1(3)=num_low_freq_chan
      allocate(tbb0(sz1(1),sz1(2),sz1(3)))
      allocate(tbb(sz1(1),sz1(2),sz1(3)))
	   print*,'newthree',lo_rez_fov,nscan,num_low_freq_chan

      ! call h5dopen_f(fhnd1,"/Data/Earth_Obs_BT",dhnd1,iret)
      ! call h5dread_f(dhnd1,H5T_NATIVE_REAL,tbb,sz1,iret)
      ! if(iret.lt.0)then
      !    call da_warning(__FILE__,__LINE__, &
      !       (/"HDF5 read error for: Brightness Temperature"/))
      ! endif
      ! call h5dclose_f(dhnd1, iret)

      call h5dopen_f(fhnd1,"/Data/Earth_Obs_BT",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,tbb0,sz1,iret)       !!! 变量声明时为integer !!H5T_NATIVE_INTEGER
      call h5aopen_f(dhnd1,"Slope",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,slp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      call h5aopen_f(dhnd1,"Intercept",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,itp,sz1,iret)
      call h5aclose_f(ahnd1,iret)    
      if (iret.lt.0) then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Data/Earth_Obs_BT"/))
      endif
      call h5dclose_f(dhnd1,iret)
      tbb = tbb0*slp+itp       ! change scale        !!!FillValue???
      deallocate(tbb0)


   ! read array: latlon
   ! read lat
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/Latitude",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_REAL,latlr,sz1,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Latitude"/))
      endif
      call h5dclose_f(dhnd1, iret)

   ! read lon
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/Longitude",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_REAL,lonlr,sz1,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Longitude"/))
             call da_trace_exit("da_read_obs_hdf5mwts2d")
      endif
      call h5dclose_f(dhnd1, iret)

   ! cutoff overlap
      do i=1,lo_rez_fov
        do j=1,nscan
           if(lonlr(i,j).lt.0.0) lonlr(i,j)=lonlr(i,j)+360.0
        enddo
      enddo
      do j=nscan+1,max_scan
         latlr(:,j)=0
         lonlr(:,j)=0
      enddo
   ! sample display


   ! read array: elevation
   ! read
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/DEM",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,elev,sz1,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: DEM"/))
      endif
      call h5dclose_f(dhnd1,iret)
   ! cutoff overlap
      do j=nscan+1,max_scan
         elev(:,j)=0
      enddo
   ! sample display
      write(unit=stdout,fmt=*)'elev',elev(tesp,tess)

   ! read array: land ocean flag for low
   ! read
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/LandSeaMask",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,loflo,sz1,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Land_OceanMask"/))
      endif
      call h5dclose_f(dhnd1,iret)
   ! cutoff overlap
      do j=nscan+1,max_scan
         loflo(:,j)=0
      enddo

   ! read array: satellite zenith
   ! read
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/SensorZenith",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,satzen0,sz1,iret)
      call h5aopen_f(dhnd1,"Slope",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,slp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      call h5aopen_f(dhnd1,"Intercept",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,itp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      if(iret.lt.0)then
          call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Satellite Zenith"/))
      endif
      call h5dclose_f(dhnd1,iret)
      satzen=satzen0*slp+itp
   ! cutoff overlap & change scale
      do j=nscan+1,max_scan
         satzen(:,j)=0
      enddo

   ! read array: satellite azimuth
   ! read
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/SensorAzimuth",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,satazi0,sz1,iret)
      call h5aopen_f(dhnd1,"Slope",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,slp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      call h5aopen_f(dhnd1,"Intercept",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,itp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      if(iret.lt.0)then
          call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Satellite Azimuth"/))
      endif
      call h5dclose_f(dhnd1,iret)
      satazi=satazi0*slp+itp
   ! cutoff overlap & change scale
       do i=1,lo_rez_fov
         do j=1,nscan
            if(satazi(i,j).lt.0.0) satazi(i,j)=satazi(i,j)+360.0
         enddo
       enddo
       do j=nscan+1,max_scan
          satazi(:,j)=0
       enddo


   ! read array: solar zenith
   ! read
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/SolarZenith",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,solzen0,sz1,iret)
      call h5aopen_f(dhnd1,"Slope",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,slp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      call h5aopen_f(dhnd1,"Intercept",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,itp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      if(iret.lt.0)then
          call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Solar Zenith"/))
      endif
      call h5dclose_f(dhnd1,iret)
      solzen=solzen0*slp+itp
   ! cutoff overlap & change scale
      do j=nscan+1,max_scan
         solzen(:,j)=0
      enddo

   ! read array: solar azimuth
   ! read
      sz1(1)=lo_rez_fov
      sz1(2)=max_scan
      call h5dopen_f(fhnd1,"/Geolocation/SolarAzimuth",dhnd1,iret)
      call h5dread_f(dhnd1,H5T_NATIVE_INTEGER,solazi0,sz1,iret)
      call h5aopen_f(dhnd1,"Slope",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,slp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      call h5aopen_f(dhnd1,"Intercept",ahnd1,iret)
      call h5aread_f(ahnd1,H5T_NATIVE_REAL,itp,sz1,iret)
      call h5aclose_f(ahnd1,iret)
      if(iret.lt.0)then
          call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Solar Azimuth"/))
      endif
      call h5dclose_f(dhnd1,iret)
      solazi=solazi0*slp+itp

   ! cutoff overlap & change scale
      do i=1,lo_rez_fov
        do j=1,nscan
           if(solazi(i,j).lt.0.0) solazi(i,j)=solazi(i,j)+360.0
        enddo
      enddo
      do j=nscan+1,max_scan
         solazi(:,j)=0
      enddo


   ! close file and HDF5
      call h5fclose_f(fhnd1,iret)

! 2.0 Loop to read hdf file and assign information to a sequential structure
!-------------------------------------------------------------------------

   ! Allocate arrays to hold data
      if ( .not. head_allocated ) then
         allocate(head)
         nullify( head % next )
         p => head
         head_allocated = .true.
      end if
      
   ! start scan_loop
      scan_loop:     do iscan=1, nscan
         do i = 1, 6
            idate5(i)=date(iscan,i)
         end do
         call da_get_julian_time(idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),obs_time)
         if ( obs_time < time_slots(0) .or.  &
            obs_time >= time_slots(num_fgat_time) ) cycle scan_loop
         do ifgat=1,num_fgat_time
            if ( obs_time >= time_slots(ifgat-1) .and.  &
               obs_time  < time_slots(ifgat) ) exit
         end do

      ! start fov_loop
         fov_loop:      do ifov=1, lo_rez_fov
            num_mwts2d_file       = num_mwts2d_file + 1
            num_mwts2d_file_local = num_mwts2d_file_local + 1
            info%lat  =  latlr(ifov,iscan)
            info%lon  =  lonlr(ifov,iscan)

            call da_llxy (info, loc, outside, outside_all)
            if (outside_all) cycle fov_loop

            num_mwts2d_global       = num_mwts2d_global + 1
            num_mwts2d_global_local = num_mwts2d_global_local + 1
            ptotal(ifgat) = ptotal(ifgat) + 1
            if (outside) cycle fov_loop   ! No good for this PE
         ! Discard data over Land (landmask =0 -->Land =1 -->Sea)
            landsea_mask = 0
            if(loflo(ifov,iscan).eq.3) landsea_mask = 1
!!!            if( landsea_mask == 0 ) cycle fov_loop

            num_mwts2d_local       = num_mwts2d_local + 1
            num_mwts2d_local_local = num_mwts2d_local_local + 1
            write(unit=info%date_char, &
            fmt='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
               idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
               ':', idate5(5), ':', idate5(6)
            info%elv = elev(ifov,iscan)

! 3.0  Make Thinning
! Map obs to thinning grid
!-------------------------------------------------------------------
            if (thinning) then
               dlat_earth = info%lat !degree
               dlon_earth = info%lon
               if (dlon_earth<zero)  dlon_earth = dlon_earth+r360
               if (dlon_earth>=r360) dlon_earth = dlon_earth-r360
               dlat_earth = dlat_earth*deg2rad !radian
               dlon_earth = dlon_earth*deg2rad
               crit = 1.
               call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
               if (.not. iuse) then
                  num_mwts2d_thinned = num_mwts2d_thinned+1
                  cycle fov_loop
               end if
            end if

            num_mwts2d_used = num_mwts2d_used + 1


! 4.0 assign information to sequential radiance structure
!--------------------------------------------------------------------------
            allocate(p%tb_inv(1:nchan))
            p%info             = info
            p%loc              = loc
            p%landsea_mask     = landsea_mask
            p%scanpos          = ifov
            p%satzen           = satzen(ifov,iscan)
            p%satazi           = satazi(ifov,iscan)
            p%solzen           = solzen(ifov,iscan)
            p%solazi           = solazi(ifov,iscan)
            p%tb_inv(1:nchan)  = tbb(ifov,iscan,1:nchan)
            p%sensor_index     = inst
            p%ifgat            = ifgat

            allocate(p%next)   ! add next data
            p => p%next
            nullify (p%next)
         end do fov_loop
      end do scan_loop

   ! Dellocate arrays
   deallocate(date)
   deallocate(tbb)

	print*,'nscan=',nscan
	print*,'nfov=',lo_rez_fov
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_mwts2d_file    : ',num_mwts2d_file_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_mwts2d_global  : ',num_mwts2d_global_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_mwts2d_local   : ',num_mwts2d_local_local
   end do infile_loop

   call H5close_f(iret)


   if (thinning .and. num_mwts2d_global > 0 ) then
#ifdef DM_PARALLEL
   ! Get minimum crit and associated processor index.
      j = 0
      do ifgat = 1, num_fgat_time
         j = j + thinning_grid(inst,ifgat)%itxmax
      end do 

      allocate(in(j))
      allocate(out(j))
      j = 0
      do ifgat = 1, num_fgat_time
         do i = 1, thinning_grid(inst,ifgat)%itxmax
            j = j + 1
            in(j) = thinning_grid(inst,ifgat)%score_crit(i)
         end do
      end do
      call mpi_reduce(in, out, j, true_mpi_real, mpi_min, root, comm, ierr)

      call wrf_dm_bcast_real (out, j)

      j = 0
      do ifgat = 1, num_fgat_time
         do i = 1, thinning_grid(inst,ifgat)%itxmax
            j = j + 1
            if ( ABS(out(j)-thinning_grid(inst,ifgat)%score_crit(i)) > 1.0E-10 ) &
            thinning_grid(inst,ifgat)%ibest_obs(i) = 0
         end do
      end do

      deallocate( in  )
      deallocate( out )

#endif

   ! Delete the nodes which being thinning out
      p => head
      prev => head
      head_found = .false.
      num_mwts2d_used_tmp = num_mwts2d_used
      do j = 1, num_mwts2d_used_tmp
         n = p%sensor_index
         ifgat = p%ifgat
         found = .false.

         do i = 1, thinning_grid(n,ifgat)%itxmax
            if ( thinning_grid(n,ifgat)%ibest_obs(i) == j .and. thinning_grid(n,ifgat)%score_crit(i) < 9.99e6_r_kind ) then
               found = .true.
               exit
            end if
         end do

      ! free current data
         if ( .not. found ) then
            current => p
            p => p%next
            if ( head_found ) then
               prev%next => p
            else
               head => p
               prev => p
            end if
            deallocate( current%tb_inv )
            deallocate( current )
            num_mwts2d_thinned = num_mwts2d_thinned + 1
            num_mwts2d_used = num_mwts2d_used - 1
            continue
         end if

         if ( found .and. head_found ) then
            prev => p
            p => p%next
            continue
         end if

         if ( found .and. .not. head_found ) then
            head_found = .true.
            head => p
            prev => p
            p => p%next
         end if

      end do

   end if  ! End of thinning

   iv%total_rad_pixel   = iv%total_rad_pixel + num_mwts2d_used
   iv%total_rad_channel = iv%total_rad_channel + num_mwts2d_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_mwts2d_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_mwts2d_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'MWTS2d data counts: '
   write(stdout,fmt='(a,i7)') ' In file: ',num_mwts2d_file
   write(stdout,fmt='(a,i7)') ' Global : ',num_mwts2d_global
   write(stdout,fmt='(a,i7)') ' Local  : ',num_mwts2d_local
   write(stdout,fmt='(a,i7)') ' Used   : ',num_mwts2d_used
   write(stdout,fmt='(a,i7)') ' Thinned: ',num_mwts2d_thinned

!  5.0 allocate innovation radiance structure
!----------------------------------------------------------------

   if (num_mwts2d_used > 0) then
      iv%instid(inst)%num_rad  = num_mwts2d_used
      iv%instid(inst)%info%nlocal = num_mwts2d_used
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
         'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      call da_allocate_rad_iv (inst, nchan, iv)
   end if

!  6.0 assign sequential structure to innovation structure
!-------------------------------------------------------------
   p => head

   do n = 1, num_mwts2d_used
      i = p%sensor_index 
      call da_initialize_rad_iv (i, n, iv, p)
      current => p
      p => p%next
   ! free current data
      deallocate( current % tb_inv )
      deallocate( current )
   end do
   deallocate( p )
   deallocate(ptotal)

   if (trace_use) call da_trace_exit("da_read_obs_hdf5mwts2d")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with HDF5 library"/))
#endif
end subroutine da_read_obs_hdf5mwts2d

subroutine mjd2cal2td(ju,yr,mt,dy,hr,mn,sc)
  implicit none
  real(kind=8)::ju,j0
  integer::yr,mt,dy,mn,yr0,sc
  real(kind=4)::bc,dd,n1,n2,n3,sc0
  integer::hr

  ju=ju+2451545.0

  if (ju.lt.1721423.5) then
    bc=1
  else
    bc=0
  end if

  if (ju.lt.2299160.5) then
    j0=floor(ju+0.5)
    dd=ju+0.5-j0
  else
    n1=floor((ju-2342031.5)/36524.25/4)+1
    n2=floor((ju-2378555.5)/36524.25/4)+1
    n3=floor((ju-2415079.5)/36524.25/4)+1
    j0=n1+n2+n3+ju+10
    dd=j0+0.5-floor(j0+0.5)
    j0=floor(j0+0.5)
  end if

  j0=j0+32083
  yr0=ceiling(j0/365.25)-1
  yr=yr0-4800
  dy=j0-floor(yr0*365.25)
  mt=floor((dy-0.6)/30.6)+3
  dy=dy-nint((mt-3)*30.6)

  if (mt.gt.12) then
    mt=mt-12
    yr=yr+1
  end if

  yr=yr-bc
  sc0=nint(dd*86400)


  hr=floor(sc0/3600)
  sc0=sc0-hr*3600
  mn=floor(sc0/60)
  sc=int(sc0-mn*60)

end subroutine mjd2cal2td