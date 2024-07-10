subroutine da_rttov_scatt_init(iv,ob,nsensor,nchan)

   !Purpose: interface to the initialization subroutine of RTTOV SCATT.

   implicit none 
   type (iv_type), intent (inout) :: iv
   type (y_type) , intent (inout) :: ob
   integer ,       intent (in)    :: nsensor
   integer ,       intent (in)    :: nchan(nsensor)

   !local arguments
   !------------------- 
   integer   :: i, j, n

   !  input parameters of RTTOV_SCATT_SETUP
   !----------------------------------
   integer :: err_unit        ! Logical error unit (<0 for default)
   integer :: verbosity_level ! (<0 for default)
   integer, allocatable  :: sensor(:,:) ! instrument id
   integer, allocatable  :: coefs_channels (:,:)
   character(len=256)    :: fmt_string
   character(len=256)    :: file_path, file_suffix,mietable_path
   character(len=256)    :: coef_prefix, scaer_prefix, sccld_prefix
   character(len=256)    :: coef_filename, scaer_filename, sccld_filename,mietable_filename

   !  output parameters of RTTOV_SCATT_SETUP
   !-------------------------------------
   integer :: errorstatus  ! return code

   ! local variables
   !----------------
   integer               :: mxchn
   integer(jpim)         :: id_sensor
   
   allocate (coefs(nsensor))
   allocate (opts(nsensor))
   allocate (opts_rt_ir(nsensor))      
   allocate (sensor(3,nsensor))
   allocate (coefs_channels(mxchn,nsensor))
   allocate (coefs_scatt(nsensor))
   allocate (opts_scatt(nsensor))
   
   sensor (1,1:nsensor)  = rtminit_platform (1:nsensor) 
   sensor (2,1:nsensor)  = rtminit_satid    (1:nsensor)
   sensor (3,1:nsensor)  = rtminit_sensor   (1:nsensor)
   
   coefs_channels(:,:) = 0
   do n = 1, nsensor
      coefs_channels(1:nchan(n),n) = iv%instid(n)%ichan(1:nchan(n))
   end do
   
   if (print_detail_rad) then
      write(unit=message(1),fmt='(A,I5)') 'err_unit             = ', err_unit
      write(unit=message(2),fmt='(A,I5)') 'verbosity_level      = ', verbosity_level
      write(unit=message(3),fmt='(A,I5)') 'nsensor              = ', nsensor
      write(unit=message(4),fmt='(A,10I5)') 'sensor (1,1:nsensor) = ', sensor (1,1:nsensor)
      write(unit=message(5),fmt='(A,10I5)') 'sensor (2,1:nsensor) = ', sensor (2,1:nsensor)
      write(unit=message(6),fmt='(A,10I5)') 'sensor (3,1:nsensor) = ', sensor (3,1:nsensor)
      call da_message(message(1:6))
   end if

   !read and initialize coefficients
   !rttov_read_coefs and rttov_init_coefs are called for each instrument

   file_path    = 'rttov_coeffs/'
   file_suffix  = '.dat'
   coef_prefix  = 'rtcoef_'
   mietable_path = 'mietable/'

   write(unit=message(1),fmt='(a)') 'Read in the RTTOV coef files for the following sensors'
   call da_message(message(1:1))
   
   do n = 1, nsensor
   !rttov_option只需要保持默认状态即可
      opts(n) % rt_all % switchrad  = .true.   ! 用于设置ad/k的输入扰动，若为true会使用波长大于3µm的通道，false则使用所有通道brightness temperature radiance%bt is the input perturbation
      opts(n) % rt_all % addrefrac  = .false.  ! refraction in path calc
      opts(n) % interpolation % addinterp  = .true.  
	  opts % interpolation % interp_mode = 1
      opts(n) % rt_mw % clw_data = .false.
      opts_rt_ir(n) % addsolar   = .false.  ! reflected solar
      opts_rt_ir(n) % addclouds  = .false.  ! cloud effect
      opts_rt_ir(n) % addaerosl  = .false.  ! aerosol effect
      opts_rt_ir(n) % ozone_data = .false.
      opts_rt_ir(n) % co2_data   = .false.
      opts_rt_ir(n) % n2o_data   = .false.
      opts_rt_ir(n) % ch4_data   = .false.
      opts_rt_ir(n) % co_data    = .false.
   !rttov_scatt_option
      opts_scatt(n) % interp_mode = 1                    ! 设置插值模式
      opts_scatt(n) % config % verbose = .TRUE.          ! 如果设置为false，只有出现fatal error时才会报错
	  
   !construct the full path name to the coef file
      coef_filename = trim(file_path)//trim(coef_prefix)//trim(iv%instid(n)%rttovid_string_coef)//trim(file_suffix)
	  !增加mietable系数文件路径,暂时设置为fy3_mwhs2的mietable文件
	  mietable_filename= trim(file_path)//trim(mietable_path)//'mietable_fy3_mwhs2.dat' 
	  
	  write(unit=fmt_string, fmt='(a,i3,a)') '(2a,2x,a,', nchan(n), 'i5)'
      write(unit=message(2),fmt=trim(fmt_string)) "   ", &
      !write(unit=message(2),fmt='(2a,2x,a,(30i5))') "   ", &
      trim(iv%instid(n)%rttovid_string), 'nchan = ', coefs_channels(1:nchan(n),n)
      call da_message(message(2:2))
	  
	  call rttov_read_coefs( &
        & errorstatus,       &   ! out
        & coefs(n),          &   ! out
        & opts(n),           &   ! in
      ! & channels     = coefs_channels(1:nchan(n),n),  &  !如果使用scatt要求读取所有通道的信息，所以channels选项不应该被使用
        & file_coef    = trim(coef_filename),  &
      ! & file_scaer   = trim(scaer_filename), &
      ! & file_sccld   = trim(sccld_filename) )
      if ( errorstatus /= errorstatus_success ) then
         call da_error(__FILE__,__LINE__,(/"rttov_read_coefs fatal error"/))
      end if
	  
      !Read the RTTOV-SCATT Mietable file
	  call rttov_read_scattcoeffs(&
	    & errorstatus,            &
		& coefs(n),               &
		& coefs_scatt(n)          &
		& opts_scatt(n),          &
		& file_coef = trim(mietable_filename))
	  if ( errorstatus /= errorstatus_success ) then
         call da_error(__FILE__,__LINE__,(/"rttov_read_scattcoeffs fatal error"/))
      end if
	  
	  iv%instid(n)%nlevels = coefs(n)%coef%nlevels
	        if ( rttov_emis_atlas_ir > 0 .or. rttov_emis_atlas_mw > 0 ) then
         id_sensor = coefs(n)%coef%id_sensor
         atlas_type(n) = 0
         if( id_sensor == sensor_id_ir .OR. id_sensor == sensor_id_hi ) then
            atlas_type(n) = atlas_type_ir
!            atlas_id(n) = uwiremis_atlas_id    !(Previous WRFDA default)
            atlas_id(n) = rttov_emis_atlas_ir !(namelist variable, can either be 1=uwiremis or 2=camel)
         end if
         if( id_sensor == sensor_id_mw .OR. id_sensor == sensor_id_po ) then
            atlas_type(n) = atlas_type_mw
            atlas_id(n) = rttov_emis_atlas_mw !(namelist variable, can either be 1=TELSEM2 or 2=CNRW)
         end if
      end if
   end do

   deallocate (sensor)
   deallocate (coefs_channels)