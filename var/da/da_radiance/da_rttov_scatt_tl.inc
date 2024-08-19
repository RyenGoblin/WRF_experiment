#ifdef RTTOV
subroutine da_rttov_scatt_tl(inst, nchanl, nprofiles, con_vars, aux_vars,con_cld_vars, &
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
   real                , intent (out) :: tb(nchanl,nprofiles)
   logical,allocatable :: use_chan(:,:)
   integer,allocatable :: channel_list(:)

   ! local variables
   integer             :: n, i, j, asw, nlevels, nchanprof,iprof
   integer             :: alloc_status(18)
   logical             :: use_totalice, mmr_snowrain
   
   type (rttov_chanprof), allocatable  :: chanprof(:)
   type (rttov_profile),   allocatable :: profiles(:), profiles_tl(:) 
   type (rttov_profile_cloud), allocatable:: cld_profiles(:),cld_profiles_tl(:)
   logical,               allocatable  :: calcemis(:)
   type (rttov_emissivity), allocatable :: emissivity(:), emissivity_tl(:)
   integer,allocatable :: frequencies(:)    !定义频率

   ! RTTOV out parameters
   integer                            :: errorstatus

   ! RTTOV inout parameters
   type (rttov_radiance)               :: radiance, radiance_tl
   

   call da_trace_entry("da_rttov_scatt_tl") 
   nlevels = con_vars(1) % nlevels
   nchanprof = nchanl*nprofiles   

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
   call rttov_alloc_scatt_prof(  &
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
        (/" cld_profile allocation error"/))
   end if

   allocate (cld_profiles_tl(nprofiles),stat=alloc_status(4))
   call rttov_alloc_scatt_prof(  &
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
   call rttov_alloc_rad(         &
        errorstatus,             &
		nchanprof,               &
		radiance,                &
		nlevels,                 & 
		asw=1,                   &
		init=.true.)
   if ( errorstatus /= errorstatus_success) then
     call da_error(__FILE__,__LINE__, &
       (/"radiance allocation error"/))
   end if

   call rttov_alloc_rad(         &
        errorstatus,             &
		nchanprof,               &
		radiance_tl,             &
		nlevels,                 & 
		asw=1,                   &
		init=.true.)
   if ( errorstatus /= errorstatus_success) then
     call da_error(__FILE__,__LINE__, &
       (/"radiance_TL allocation error"/))
   end if
   
   !allocate chanprof structure
   allocate (chanprof(nchanprof), stat=alloc_status(5))
   if (alloc_status(5) /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"chanprof allocation error"/))
   end if

   ! allocate emissivity structure   
   allocate (emissivity(nchanprof), stat=alloc_status(6))
   if (alloc_status(6) /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"emissivity allocation error"/))
   end if

   !allocate emissivity_tl structure   
   allocate (emissivity_tl(nchanprof), stat=alloc_status(7))
   if (alloc_status(7) /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"emissivity_tl allocation error"/))
   end if 

   !allocate calcemis structure
   allocate (calcemis(nchanprof), stat=alloc_status(8))
   if (alloc_status(8) /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"calcemis allocation error"/))
   end if

   !allocate frequencies structure
   allocate(frequencies(nchanprof), stat=alloc_status(9))
   if (alloc_status(9) /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"frequencies allocation error"/))
   end if
   
   !------------------------生成通道、廓线和频率索引-------------------
   !确定哪些指定通道用于模拟
   !为channel_list赋值,channel_list是一个由1到nchannel的数字列表
   channel_list=[(i,i=1,nchanl)]
   allocate(use_chan(nprofiles,nchanl)) 

   ! Set use_chan to .TRUE. only for required channels
   use_chan(:,:) = .FALSE.
   do j = 1, nprofiles
     use_chan(j,channel_list(1:nchanl)) = .TRUE.  
   end do

    call rttov_scatt_setupindex ( &
       nprofiles,          &
       nchanl,             &
       coefs(inst),        &
       nchanprof,          &
       chanprof,           &
       frequencies,        &
       use_chan)
    
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
   
         if (use_totalice .eqv. .true.) then !使用totalice
	          profiles(iprof)     % p   (:)     = coefs(inst)%coef%ref_prfl_p (:) ! full level pressure (hPa)
		      cld_profiles(iprof) % ph  (:)     = con_cld_vars(iprof)%ph      (:) ! half level pressure (hPa)
		      profiles(iprof)     % t   (:)     = con_vars(iprof)%t           (:) ! temperature (K)
		      profiles(iprof)     % q   (:)     = con_vars(iprof)%q           (:) ! specific humility
		      cld_profiles(iprof) % cc  (:)     = con_cld_vars(iprof)%cc      (:) ! cloud cover (0-1)
              cld_profiles(iprof) % clw (:)     = con_cld_vars(iprof)%clw     (:) ! liquid water (kg/kg)
             !cld_profiles(iprof) % totalice(:) = con_cld_vars(iprof)%totalice(:) ! combined ice water and snow (kg/kg)
              cld_profiles(iprof) % rain(:)     = con_cld_vars(iprof)%rain    (:) ! rain (kg/kg)
	     else                           !不使用totalice
	          profiles(iprof)     % p   (:)     = coefs(inst)%coef%ref_prfl_p (:) ! full level pressure (hPa)
		      cld_profiles(iprof) % ph  (:)     = con_cld_vars(iprof)%ph      (:) ! half level pressure (hPa)
		      profiles(iprof)     % t   (:)     = con_vars(iprof)%t           (:) ! temperature (K)
		      profiles(iprof)     % q   (:)     = con_vars(iprof)%q           (:) ! specific humility
		      cld_profiles(iprof) % cc  (:)     = con_cld_vars(iprof)%cc      (:) ! cloud cover (0-1)
              cld_profiles(iprof) % clw (:)     = con_cld_vars(iprof)%clw     (:) ! liquid water (kg/kg)
              cld_profiles(iprof) % ciw (:)     = con_cld_vars(iprof)%ciw     (:) ! ice water (kg/kg)
		      cld_profiles(iprof) % rain(:)     = con_cld_vars(iprof)%rain    (:) ! rain (kg/kg)
		     !cld_profiles(iprof) % sp  (:)     = con_cld_vars(iprof)%sp      (:) ! kg/m2/s
         end if
     
	 
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
	 !profiles(iprof) % skin % salinity = aux_vars(iprof) % salinity       !FASTEM4-6需要的变量，原始wrfda中没有此变量，需要额外定义
	 

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
	  
      profiles(iprof) % Be          = 0.0   ! optional, for zeeman effect for ssmis and amsua
      profiles(iprof) % cosbk       = 0.0    ! optional, for zeeman effect for ssmis and amsua

      profiles(iprof) % ctp         = 500.0  ! hPa, optional, for simple cloud
      profiles(iprof) % cfraction   = 0.0    ! 0-1, optional, for simple cloud
     !---------------------------读取profile_TL ------------------------------
	  profiles_tl(iprof) % p(:)       = con_cld_vars_tl(iprof)%p(:)
      profiles_tl(iprof) % t(:)       = con_vars_tl(iprof)%t(:)
      profiles_tl(iprof) % q(:)       = con_vars_tl(iprof)%q(:)
      profiles_tl(iprof) % s2m % p    = con_vars_tl(iprof)%ps
	 !从da_transform_xtoy_rttov_scatt获取cld_profile_tl
	  cld_profiles_tl(iprof) % ph(:)     = con_cld_vars_tl(iprof)%ph(:)
	  cld_profiles_tl(iprof) % clw(:)    = con_cld_vars_tl(iprof)%clw(:)
	  cld_profiles_tl(iprof) % ciw(:)    = con_cld_vars_tl(iprof)%ciw(:)
	  cld_profiles_tl(iprof) % rain(:)   = con_cld_vars_tl(iprof)%rain(:)
	  cld_profiles_tl(iprof) % cc(:)     = con_cld_vars_tl(iprof)%cc(:)
	  !cld_profiles_tl(iprof)% sp(:)     = con_cld_vars_tl(iprof)%sp(:)
   end do
	
	call  rttov_scatt_tl(   &
      & errorstatus,        & ! out
	  & opts_scatt(inst),   & !
	  & nlevels,            & ! in
      & chanprof,           & ! in      chanprof(nchanprof)
	  & frequencies,        & !
      & profiles,           & ! in      profiles(nprof)
	  & cld_profiles,       & ! inout 
	  & coefs(inst),        & ! in
	  & coefs_scatt(inst),  & ! in
	  & calcemis,           & ! in,    optional   calcemis(nchanprof)
      & emissivity,         & ! inout, optional   emissivity(nchanprof)	  
      & profiles_tl,        & ! inout   profiles_tl(nprof)
	  & cld_profiles_tl,    & ! inout
      & emissivity_tl,      &
      & radiance,           & ! inout
      & radiance_tl)          ! inout   
	  
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
	   write (message(12),*) 'cld_profiles%ph         = ', cld_profiles(1)%ph
	   write (message(13),*) 'cld_profiles%cc         = ', cld_profiles(1)%cc
	   write (message(14),*) 'cld_profiles%clw        = ', cld_profiles(1)%clw
	   write (message(15),*) 'cld_profiles%ciw        = ', cld_profiles(1)%ciw
	   write (message(16),*) 'cld_profiles%rain       = ', cld_profiles(1)%rain
	  !write (message(16),*) 'cld_profiles%sp         = ', cld_profiles(1)%sp
       write (message(17),*) 'emissivity_out          = ', emissivity(:)%emis_out
       write (message(18),*) 'radiance                = ', radiance%bt
       write (message(19),*) 'profiles_tl%s2m         = ', profiles_tl(1)%s2m
       write (message(20),*) 'profiles_tl%skin        = ', profiles_tl(1)%skin
       write (message(21),*) 'profiles_tl%zenangle    = ', profiles_tl(1)%zenangle
       write (message(22),*) 'profiles_tl%azangle     = ', profiles_tl(1)%azangle
       write (message(23),*) 'profiles_tl%p           = ', profiles_tl(1)%p 
       write (message(24),*) 'profiles_tl%t           = ', profiles_tl(1)%t 
       write (message(25),*) 'profiles_tl%q           = ', profiles_tl(1)%q
       write (message(12),*) 'cld_profiles_tl%ph      = ', cld_profiles_tl(1)%ph
	   write (message(13),*) 'cld_profiles_tl%cc      = ', cld_profiles_tl(1)%cc
	   write (message(14),*) 'cld_profiles_tl%clw     = ', cld_profiles_tl(1)%clw
	   write (message(15),*) 'cld_profiles_tl%ciw     = ', cld_profiles_tl(1)%ciw
	   write (message(16),*) 'cld_profiles_tl%rain    = ', cld_profiles_tl(1)%rain
      !write (message(16),*) 'cld_profiles_tl%sp      = ', cld_profiles_tl(1)%sp	   
       write (message(26),*) 'emissivity_out_tl       = ', emissivity_tl(:)%emis_out    
       write (message(27),*) 'radiance_tl             = ', radiance_tl%bt
       if ( errorstatus /= errorstatus_success ) call da_error(__FILE__,__LINE__,message(1:22))
   end if
   
   do n = 1, nprofiles
     tb(1:nchanl,n) = radiance_tl % bt((n-1)*nchanl+1:n*nchanl)
   end do
   !--------------------------释放变量内存------------------------
   ! deallocate profile arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles,nlevels,opts(inst),asw=0)
   deallocate (profiles, stat=alloc_status(10))
   if ( errorstatus /= errorstatus_success .or. alloc_status(10) /=0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile deallocation error"/))
   end if
   
   ! deallocate profiles_tl arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles_tl,nlevels,opts(inst),asw=0)
   deallocate (profiles_tl, stat=alloc_status(11))
   if ( errorstatus /= errorstatus_success .or. alloc_status(11) /=0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile TL deallocation error"/))
   end if
   
   ! deallocate cld_pofile arrays
   call rttov_alloc_scatt_prof(errorstatus,nprofiles,cld_profiles,nlevels,.false.,asw=0)
   deallocate (cld_profiles, stat=alloc_status(12))
   if ( errorstatus /= errorstatus_success .or. alloc_status(12) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile  deallocation error"/))
   end if
   
   ! deallocate cld_pofile_tl arrays
   call rttov_alloc_scatt_prof(errorstatus,nprofiles,cld_profiles_tl,nlevels,.false.,asw=0)
   deallocate(cld_profiles_tl,stat=alloc_status(13))
   if ( errorstatus /= errorstatus_success .or. alloc_status(13) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile TL deallocation error"/))
   end if
   
   ! deallocate radiance arrays
   call rttov_alloc_rad (errorstatus,nchanprof,radiance,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance deallocation error"/))
   end if
   
   ! deallocate radiance_tl arrays
   call rttov_alloc_rad (errorstatus,nchanprof,radiance_tl,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance TL deallocation error"/))
   end if

   ! deallocate emissivity arrays   
   deallocate (emissivity, stat=alloc_status(14))
   if ( alloc_status(14) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"emissivity deallocation error"/))
   end if

   ! deallocate emissivity_tl arrays   
   deallocate (emissivity_tl, stat=alloc_status(15))
   if ( alloc_status(15) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"emissivity_tl deallocation error"/))
   end if

   ! deallocate calcemis array   
   deallocate (calcemis,stat = alloc_status(16))
   if ( alloc_status(16) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"calcemis deallocation error"/))
   end if

   !deallocate chanprof array
   deallocate (chanprof,stat = alloc_status(17))
   if ( alloc_status(17) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"chanporf deallocation error"/))
   end if   
   
   !deallocate frequencies array
   deallocate (frequencies,stat = alloc_status(18))
   if ( alloc_status(18) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"frequencies deallocation error"/))
   end if  
   
   deallocate(use_chan)
   call da_trace_exit("da_rttov_scatt_tl")

#endif
end subroutine da_rttov_scatt_tl