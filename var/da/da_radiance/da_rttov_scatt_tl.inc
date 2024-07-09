#ifdef RTTOV_SCATT
subroutine da_rttov_scatt_tl(inst, nchanl, nprofiles, con_vars, aux_vars,con_cld_vars &
                      con_vars_tl, aux_vars_tl,con_cld_vars_tl,tb)

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the tangent subroutine of RTTOV_SCATT
   !---------------------------------------------------------------------------
   implicit none
   
   !调用切线性模式需要额外定义tangent变量
   integer             ,  intent (in) :: inst, nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)            
   type (con_vars_type),  intent (in) :: con_vars_tl (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars_tl (nprofiles)
   type (con_cld_vars_type),intent (in) :: con_cld_vars (nprofiles)      
   type (con_cld_vars_type),intent (in) :: con_cld_vars_tl (nprofiles) !加入额外的水凝物切线性变量
   type (rttov_profile_cloud),intent (in):: cld_profiles(:),cld_profiles_tl(:)
   real                , intent (out) :: tb(nchanl,nprofiles)

   ! local variables
   integer             :: n, j, asw, nlevels, nchanprof,iprof,ilev
   integer             :: alloc_status(11)
   logical             :: use_totalice, mmr_snowrain
   
   type (rttov_chanprof), allocatable  :: chanprof(:)
   type (rttov_profile),   allocatable :: profiles(:), profiles_tl(:) 
   logical,               allocatable  :: calcemis(:)
   type (rttov_emissivity), allocatable :: emissivity(:), emissivity_tl(:)
   integer(KIND=jpim), pointer :: frequencies(:) => NULL()   !定义频率

   ! RTTOV out parameters
   integer                            :: errorstatus

   ! RTTOV inout parameters
   type (rttov_radiance)               :: radiance, radiance_tl
   

   call da_trace_entry("da_rttov_scatt_tl") 
   nlevels = con_vars(1) % nlevels
   nchanprof = nchanl*nprofiles   
   !------------------------生成通道、廓线和频率索引-------------------
   !确定哪些指定通道用于模拟
   !为channel_list赋值,channel_list是一个由1到nchannel的数字列表
   channel_list=[(i,i=1,nchanl)]
   allocate(use_chan(nprofiles,coefs%coef%fmv_chn), frequencies(nchanprof))
   ! Set use_chan to .TRUE. only for required channels
   use_chan(:,:) = .FALSE.
   do j = 1, nprof
     use_chan(j,channel_list(1:nchannels)) = .TRUE.  
   end do

    call rttov_scatt_setupindex ( &
       nprofiles,          &
       nchanl,             &
       coefs(inst),        &
       nchanprof,          &
       chanprof,           &
       frequencies,        &
       use_chan)
   
   !---------------------------为变量分配内存-----------------------------------
   alloc_status(:) = 0  
   
   ! allocate input profile arrays with the number of levels
   allocate (profiles(nprofiles), stat=alloc_status(1))
   call rttov_alloc_prof(        &
      & errorstatus,             &
      & nprofiles,               &
      & profiles,                &
      & nlevels,                 &
      & opts(inst),              &
      & asw=1,                   &  ! Switch (1=allocate; 0=deallocate)
      & coefs = coefs(inst),     &  
      & init = .true.            )  ! additionally initialize profiles structure
   if ( errorstatus /= errorstatus_success .or. alloc_status(1) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"profile allocation error"/))
   end if
   
   allocate (profiles_tl(nprofiles), stat=alloc_status(2))
   call rttov_alloc_prof(        &
      & errorstatus,             &
      & nprofiles,               &
      & profiles_tl,             &
      & nlevels,                 &
      & opts(inst),              &
      & asw=1,                   &
      & coefs = coefs(inst),     &  
      & init = .true.            )  ! additionally initialize profiles structure
   if ( errorstatus /= errorstatus_success .or. alloc_status(2) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"profile TL allocation error"/))
   end if
   
   ! allocate input cld_profile arrays with the number of levels
   allocate (cld_profiles(nprofiles),stat=alloc_status(3))
   call rttov_alloc_scatt_prof(
        errorstatus,             &
        nprofiles,               &
        cld_profiles,            &
        nlevels,                 &
        use_totalice=.false.,    &    ! false => separate ciw and snow; true => totalice
        asw=1,                   &     
        init = .true.,           &
		mmr_snowrain=.false.)
   if ( errorstatus /= errorstatus_success .or. alloc_status(3) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"r cld_profile allocation error"/))
   end if

   allocate (cld_profiles_tl(nprofiles),stat=alloc_status(4))
   call rttov_alloc_scatt_prof(
        errorstatus,             &
        nprofiles,               &
        cld_profiles_tl,         &
        nlevels,                 &
        use_totalice=.false.,    &   
        asw=1,                   &    
        init = .true.,           &
		mmr_snowrain=.false.) 
   if ( errorstatus /= errorstatus_success .or. alloc_status(4) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"cld_profile_TL allocation error"/))
   end if  
   
   ! allocate input radiance array    
   call rttov_alloc_rad(
        err,                     &
		nchanprof,               &
		radiance,                &
		nlevels,                 & 
		asw=1,                   &
		init=.true.)
   if ( errorstatus /= errorstatus_success) then
     call da_error(__FILE__,__LINE__, &
       (/"radiance allocation error"/))
   end if
   
   call rttov_alloc_rad(
        err,                     &
		nchanprof,               &
		radiance_tl,             &
		nlevels,                 & 
		asw=1,                   &
		init=.true.)
   if ( errorstatus /= errorstatus_success) then
     call da_error(__FILE__,__LINE__, &
       (/" radiance_TL allocation error/))
	   
   allocate (emissivity(nchanprof), stat=alloc_status(5))
   allocate (emissivity_tl(nchanprof), stat=alloc_status(6))
   allocate (calcemis(nchanprof), stat=alloc_status(7))
   if ( any( alloc_status /= 0 ) ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for emissivity or calcemis arrays"/))
   end if
   if ( coefs(inst)%coef%id_sensor == sensor_id_mw .or. &
             coefs(inst)%coef%id_sensor == sensor_id_po ) then ! microwave sensor
      do n = 1, nprofiles
         if ( profiles(n) % skin % surftype == 1 ) then  ! sea  
            calcemis((n-1)*nchanl+1:n*nchanl) = .true.
            emissivity((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
            emissivity_tl((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
         else                                            ! 0:land ; 2:sea-ice
            calcemis((n-1)*nchanl+1:n*nchanl) = .false.
            emissivity((n-1)*nchanl+1:n*nchanl)%emis_in = 0.9
            emissivity_tl((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0 !如果使用FASTEM, TESSEM2 or IREMIS，发射率扰动会在rttov内部自行计算，并在emis_out给出
         end if
      end do
   end if
   !-------------------------------------读取廓线数据-----------------------------------

   do iprof = 1, nprofiles
     do ilev = 1,nlevels
         if (use_totalice = .true.) then !使用totalice
	         profiles(iprof)     % p   (ilev)     = coefs(inst)%coef%ref_prfl_p(:)       (ilev) ! full level pressure (hPa)
		     cld_profiles(iprof) % ph  (ilev)     = con_cld_vars(iprof)%ph      (ilev) ! half level pressure (hPa)
		     profiles(iprof)     % t   (ilev)     = con_cld_vars(iprof)%t       (ilev) ! temperature (K)
		     profiles(iprof)     % q   (ilev)     = con_vars(iprof)%q           (ilev) ! specific humility
		     cld_profiles(iprof) % cc  (ilev)     = con_cld_vars(iprof)%cc      (ilev) ! cloud cover (0-1)
             cld_profiles(iprof) % clw (ilev)     = con_cld_vars(iprof)%clw     (ilev) ! liquid water (kg/kg)
             cld_profiles(iprof) % totalice(ilev) = con_cld_vars(iprof)%totalice(ilev) ! combined ice water and snow (kg/kg)
             cld_profiles(iprof) % rain(ilev)     = con_cld_vars(iprof)%rain    (ilev) ! rain (kg/kg)
	     else                           !不使用totalice
	         profiles(iprof)     % p   (ilev)     = con_cld_vars(iprof)%p       (ilev) ! full level pressure (hPa)
		     cld_profiles(iprof) % ph  (ilev)     = con_cld_vars(iprof)%ph      (ilev) ! half level pressure (hPa)
		     profiles(iprof)     % t   (ilev)     = con_cld_vars(iprof)%t       (ilev) ! temperature (K)
		     profiles(iprof)     % q   (ilev)     = con_vars(iprof)%q           (ilev) ! specific humility
		     cld_profiles(iprof) % cc  (ilev)     = con_cld_vars(iprof)%cc      (ilev) ! cloud cover (0-1)
             cld_profiles(iprof) % clw (ilev)     = con_cld_vars(iprof)%clw     (ilev) ! liquid water (kg/kg)
             cld_profiles(iprof) % ciw (ilev)     = con_cld_vars(iprof)%ciw     (ilev) ! ice water (kg/kg)
		     cld_profiles(iprof) % rain(ilev)     = con_cld_vars(iprof)%rain    (ilev) ! rain (kg/kg)
		     cld_profiles(iprof) % sp  (ilev)     = con_cld_vars(iprof)%sp      (ilev) ! kg/m2/s
         end if
     end do
	 
	 ! read 2 meter air variables
	  profiles(iprof) % s2m % t   = aux_vars (iprof) % t2m
      profiles(iprof) % s2m % q   = aux_vars (iprof) % q2m
      profiles(iprof) % s2m % p   = con_vars (iprof) % ps
      profiles(iprof) % s2m % u   = aux_vars (iprof) % u10
      profiles(iprof) % s2m % v   = aux_vars (iprof) % v10
	
	 ! The bottom-most half pressure level is taken as the 2m pressure
      cld_profiles(iprof) % ph(nlevels+1) = profiles(iprof) % s2m % p
	
	 ! Read surface variables
      profiles(iprof) % skin % t        = aux_vars (iprof) % surft
	  profiles(iprof) % skin % surftype = aux_vars(iprof) % surftype        !land = 0, sea = 1,seaice = 2
	  profiles(iprof) % skin % salinity = aux_vars(iprof) % salinity       !FASTEM4-6需要的变量，原始wrfda中没有此变量，需要额外定义
	 

      if ( coefs(inst)%coef%id_sensor == sensor_id_mw .or. &
           coefs(inst)%coef%id_sensor == sensor_id_po ) then
         if ( profiles(iprof) % skin % surftype == 2 ) then
            profiles(iprof) % skin % fastem (1) = 2.2
            profiles(iprof) % skin % fastem (2) = 3.7
            profiles(iprof) % skin % fastem (3) = 122.0
            profiles(iprof) % skin % fastem (4) = 0.0
            profiles(iprof) % skin % fastem (5) = 0.15
         else if ( profiles(iprof) % skin % surftype == 0 ) then
            profiles(iprof) % skin % fastem (1) = 3.0
            profiles(iprof) % skin % fastem (2) = 5.0
            profiles(iprof) % skin % fastem (3) = 15.0
            profiles(iprof) % skin % fastem (4) = 0.1
            profiles(iprof) % skin % fastem (5) = 0.3
         end if
      end if
	 ! 读取卫星天顶/方位角、高程、经纬度
	  
	  profiles(iprof) % azangle   = aux_vars (iprof) % satazi
      profiles(iprof) % zenangle  = aux_vars (iprof) % satzen
	  profiles(iprof) % elevation = 0.001* aux_vars(iprof) % elevation   ! km
	  profiles(iprof) % latitude  = aux_vars(iprof) % rlat
	  profiles(iprof) % longitude = aux_vars(iprof) % rlon
	  
      profiles(iprof) % Be          = 0.0   ! optional, for zeeman effect for ssmis and amsua
      profiles(iprof) % cosbk       = 0.0    ! optional, for zeeman effect for ssmis and amsua

      profiles(iprof) % ctp         = 500.0  ! hPa, optional, for simple cloud
      profiles(iprof) % cfraction   = 0.0    ! 0-1, optional, for simple cloud
     !---------------------------读取profile_TL 这里需要计算扰动------------------------------
      profiles_tl(iprof) % t(:)       = con_vars_tl(iprof)%t(:)
      profiles_tl(iprof) % q(:)       = con_vars_tl(iprof)%q(:)
      profiles_tl(iprof) % s2m % p    = con_vars_tl(iprof)%ps
	 !还需要引入哪些水凝物变量的扰动/con_cld_vars_tl？
   end do
	
	call  rttov_scatt_tl(          &
      & errorstatus,        & ! out
	  & opts_scatt          & !
	  & nlevels             & ! in
      & chanprof,           & ! in      chanprof(nchanprof)
	  & frequencies,        & !
      & profiles,           & ! in      profiles(nprof)
      & profiles_tl,        & ! inout   profiles_tl(nprof)
	  & cld_profiles,       & ! inout   
	  & cld_profiles_tl,    & ! inout
      & coefs(inst),        & ! in
	  & coefs_scatt(inst),  & ! in
      & radiance,           & ! inout
      & radiance_tl,        & ! inout
      & calcemis,           & ! in,    optional   calcemis(nchanprof)
      & emissivity,         & ! inout, optional   emissivity(nchanprof)
      & emissivity_tl)        ! inout, optional   emissivity_tl(nchanprof)

   if ( print_detail_rad .or. errorstatus /= errorstatus_success ) then
       write (message(1),*)  'rttov_tl error code     = ', errorstatus
       write (message(2),*)  'nchanl                  = ', nchanl
       write (message(3),*)  'nprofiles               = ', nprofiles
       write (message(4),*)  'calcemis                = ', calcemis
       write (message(5),*)  'profiles%s2m            = ', profiles(1)%s2m
       write (message(6),*)  'profiles%skin           = ', profiles(1)%skin
       write (message(7),*)  'profiles%zenangle       = ', profiles(1)%zenangle
       write (message(8),*)  'profiles%azangle        = ', profiles(1)%azangle
       write (message(9),*)  'profiles%p              = ', profiles(1)%p
       write (message(10),*) 'profiles%t              = ', profiles(1)%t
       write (message(11),*) 'profiles%q              = ', profiles(1)%q
       write (message(12),*) 'emissivity_out          = ', emissivity(:)%emis_out
       write (message(13),*) 'radiance                = ', radiance%bt
       write (message(14),*) 'profiles_tl%s2m         = ', profiles_tl(1)%s2m
       write (message(15),*) 'profiles_tl%skin        = ', profiles_tl(1)%skin
       write (message(16),*) 'profiles_tl%zenangle    = ', profiles_tl(1)%zenangle
       write (message(17),*) 'profiles_tl%azangle     = ', profiles_tl(1)%azangle
       write (message(18),*) 'profiles_tl%p           = ', profiles_tl(1)%p 
       write (message(19),*) 'profiles_tl%t           = ', profiles_tl(1)%t 
       write (message(20),*) 'profiles_tl%q           = ', profiles_tl(1)%q 
       write (message(21),*) 'emissivity_out_tl       = ', emissivity_tl(:)%emis_out    
       write (message(22),*) 'radiance_tl             = ', radiance_tl%bt
       if ( errorstatus /= errorstatus_success ) call da_error(__FILE__,__LINE__,message(1:22))
   end if
   
   do n = 1, nprofiles
     tb(1:nchanl,n) = radiance_tl % bt((n-1)*nchanl+1:n*nchanl)
   end do

   deallocate (emissivity)
   deallocate (emissivity_tl)
   deallocate (calcemis)
   deallocate (chanprof)

   asw = 0 ! deallocation
   ! deallocate radiance arrays
   call rttov_alloc_rad (errorstatus,nchanprof,radiance,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance deallocation error"/))
   end if
   call rttov_alloc_rad (errorstatus,nchanprof,radiance_tl,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance TL deallocation error"/))
   end if

   ! deallocate profile arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles,nlevels,opts(inst),asw=0)
   deallocate (profiles, stat=alloc_status(8))
   if ( errorstatus /= errorstatus_success .or. alloc_status(8) /=0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile deallocation error"/))
   end if
   call rttov_alloc_prof (errorstatus,nprofiles,profiles_tl,nlevels,opts(inst),asw=0)
   deallocate (profiles_tl, stat=alloc_status(9))
   if ( errorstatus /= errorstatus_success .or. alloc_status(9) /=0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile TL deallocation error"/))
   end if
   ! deallocate cld_pofile arrays
   call rttov_alloc_scatt_prof(errorstatus,nprofiles,cld_profiles,nlevel,use_totalice,asw=0,init,mmr_snowrain)
   deallocate (cld_profiles, stat=alloc_status(10))
   if ( errorstatus /= errorstatus_success .or. alloc_status(10) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile  deallocation error"/))
   end if
   call rttov_alloc_scatt_prof(errorstatus,nprofiles,cld_profiles_tl,nlevel,use_totalice,asw=0,init,mmr_snowrain)
   deallocate(cld_profiles_tl,stat=alloc_status(11))
   if ( errorstatus /= errorstatus_success .or. alloc_status(11) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile TL deallocation error"/))
   end if
   call da_trace_exit("da_rttov_scatt_tl")

end subroutine da_rttov_scatt_tl
#endif
