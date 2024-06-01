#ifdef RTTOV_SCATT
subroutine da_rttov_scatt_ad( inst, nchanl, nprofiles, con_vars, &
                      aux_vars, con_vars_ad,con_cld_vars,con_cld_vars_ad, tb )

!---------------------------------------------------------------------------
   ! PURPOSE: interface to the adjoint subroutine of RTTOV_SCATT
   !---------------------------------------------------------------------------
   !-----------------定义输入输出变量、参数------------------------------------
   implicit none
   
   integer             ,  intent (in) :: inst, nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (con_vars_type),  intent (inout) :: con_vars_ad (nprofiles)
   type (con_cld_vars_type),intent (in) :: con_cld_vars (nprofiles)
   type (con_cld_vars_type),intent (in) :: con_cld_var_ad (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   real                ,  intent (in) :: tb(nchanl,nprofiles)

   ! local variables
   integer             :: n, j, asw, nlevels, nchanprof
   Integer             :: alloc_status(11)
   
   ! RTTOV input parameters
   type (rttov_chanprof), allocatable  :: chanprof(:)
   type (rttov_profile),   allocatable :: profiles(:), profiles_ad(:)
   type (rttov_profile_cloud),  allocatable :: cld_profiles(:),cld_profiles_ad(:)   
   type (rttov_emissivity), allocatable :: emissivity(:), emissivity_ad(:)
   type (rttov_chanprof),      allocatable :: chanprof(:)    !定义要模拟的通道/廓线索引
   logical,               allocatable  :: calcemis(:)
   integer(KIND=jpim), pointer         :: frequencies(:) => NULL()   !定义频率
   logical                     :: use_totalice, mmr_snowrain
   
   ! RTTOV out parameters
   integer                            :: errorstatus

   ! RTTOV inout parameters
   type (rttov_radiance)               :: radiance, radiance_ad
   call da_trace_entry("da_rttov_scatt_ad")

   nchanprof = nchanl*nprofiles
   nlevels = con_vars(1) % nlevels
   
   !------------------------生成通道、廓线和频率索引-------------------
   ! generate the chanprof array
   !确定哪些指定通道用于模拟
   !为channel_list赋值,channel_list是一个由1到nchannel的数字列表
   channel_list=[(i,i=1,nchanl)]
   allocate(use_chan(nprofiles,coefs%coef%fmv_chn), frequencies(nchanprof))
   ! Set use_chan to .TRUE. only for required channels
   use_chan(:,:) = .FALSE.
   do j = 1, nprof
     use_chan(j,channel_list(1:nchannels)) = .TRUE. 
   end do
   
   !---------------------------给数据分配内存---------------------------
   call rttov_scatt_setupindex ( &
      nprofiles,          &
      nchanl,             &
      coefs(inst),        &
      nchanprof,          &
      chanprof,           &
      frequencies,        &
      use_chan)

   alloc_status (:) = 0
   
   allocate ( profiles(nprofiles), stat= alloc_status(1))
   call rttov_alloc_prof(        &
      & errorstatus,             &
      & nprofiles,               &
      & profiles,                &
      & nlevels,                 &
      & opts(inst),              &
      & asw=1,                   &  ! Switch (1=allocate; 0=deallocate)
      & coefs = coefs(inst),     &  ! mandatory if either opts%addclouds or opts%addaerosl is true
      & init = .true.            )  ! additionally initialize profiles structure
   if ( errorstatus /= errorstatus_success .or. alloc_status(1) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for profile arrays"/))
   end if
   
   allocate ( profiles_ad(nprofiles), stat= alloc_status(2))
   call rttov_alloc_prof(        &
      & errorstatus,             &
      & nprofiles,               &
      & profiles_ad,             &
      & nlevels,                 &
      & opts(inst),              &
      & asw=1,                   &
      & coefs = coefs(inst),     &  ! mandatory if either opts%addclouds or opts%addaerosl is true
      & init = .true.            )  ! additionally initialize profiles structure
   if ( errorstatus /= errorstatus_success .or. alloc_status(2) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for profile AD arrays"/))
   end if   
   
   allocate (cld_profiles(nprofiles),stat=alloc_status(3))
   call rttov_alloc_scatt_prof(  &
      & errorstatus,             &
      & nprof,                   &
      & cld_profiles,            &
      & nlevels,                 &
	  & use_totalice =.false.    & ! false => separate ciw and snow; true => totalice
      & asw=1,                   & ! Switch (1=allocate; 0=deallocate)
      & init = .true.,           & 
	  & mmr_snowrain =.false.)      ! snow/rain input units: false => kg/m2/s; true => kg/kg
   if ( errorstatus /= errorstatus_success .or. alloc_status(3) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for scatt profile arrays"/))
   end if	  
   
   allocate (cld_profiles_ad(nprofiles),stat=alloc_status(4))
   call rttov_alloc_scatt_prof(  &
      & errorstatus,             &
      & nprof,                   &
      & cld_profiles_ad,         &
      & nlevels,                 &
	  & use_totalice =.false.    & ! false => separate ciw and snow; true => totalice
      & asw=1,                   & 
      & init = .true.,           & 
	  & mmr_snowrain =.false.)
   if ( errorstatus /= errorstatus_success .or. alloc_status(4) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for scatt profile AD arrays"/))
   end if	   
 
 call rttov_alloc_rad( &
      & errorstatus,     &
      & nchanprof,       &
      & radiance,        &
      & nlevels,         &
      & asw=1,           &
      & init=.true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for radiance arrays"/))
   end if

   call rttov_alloc_rad( &
      & errorstatus,     &
      & nchanprof,       &
      & radiance_ad,     &
      & nlevels,         &
      & asw=1,           &
      & init=.true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for radiance AD arrays"/))
   end if
   
   !--------------------------------------读取廓线数据--------------------------------------------------------
   do iprof = 1, nprofiles
      do ilev = 1,nlevels
	     if (use_totalice = .true.) then !使用totalice
	         profiles(iprof)     % p   (ilev)     = con_cld_vars(iprof)%p       (ilev) ! full level pressure (hPa)
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
		     cld_profiles(iprof) % sp  (ilev)     = con_cld_vars(iprof)%sp      (ilev) ! frozen precip. (kg/kg)
         end if
      end do    
   
      profiles(iprof) % skin % t          = aux_vars (iprof) % surft 
      profiles(iprof) % skin % surftype   = aux_vars (iprof) % surftype
      if ( profiles(iprof)% skin % surftype == 1 ) then
         if ( opts_rt_ir(inst) % addsolar ) then
            ! if refelcted solar radiation is to be included in the SWIR channels, then
            ! specification of fresh or salt water needs to be provided
            profiles(iprof) % skin % watertype = 1
         end if
      end if

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
      profiles(iprof) % s2m  % t    = aux_vars (n) % t2m
      profiles(iprof) % s2m  % q    = aux_vars (n) % q2m
      profiles(iprof) % s2m  % o    = 0.0 !aux_vars (n) % o3
      profiles(iprof) % s2m  % p    = con_vars (n) % ps
      profiles(iprof) % s2m  % u    = aux_vars (n) % u10
      profiles(iprof) % s2m  % v    = aux_vars (iprof) % v10

      profiles(iprof) % zenangle  = aux_vars (iprof) % satzen
	  profiles(iprof) % azangle   = aux_vars (iprof) % satazi
      profiles(iprof) % elevation = 0.001* aux_vars(iprof) % elevation   ! km
      profiles(iprof) % latitude  = aux_vars(iprof) % rlat
	  profiles(iprof) % longitude = aux_vars(iprof) % rlon
	  if ( opts_rt_ir(inst) % addsolar ) then
         profiles(iprof) % azangle     = aux_vars (iprof) % satazi
         profiles(iprof) % sunzenangle = aux_vars (iprof) % solzen     !50.0
         profiles(iprof) % sunazangle  = aux_vars (iprof) % solazi     !86.0
         profiles(iprof) % s2m % wfetc = 100000.0  ! m
      end if

      profiles(iprof) % Be          = 0.0   ! optional, for zeeman effect for ssmis and amsua
      profiles(iprof) % cosbk       = 0.0    ! optional, for zeeman effect for ssmis and amsua

      profiles(iprof) % ctp         = 500.0  ! hPa, optional, for simple cloud
      profiles(iprof) % cfraction   = 0.0    ! 0-1, optional, for simple cloud

   end do
   
   allocate (emissivity(nchanprof), stat=alloc_status(5))
   allocate (emissivity_ad(nchanprof), stat=alloc_status(6))
   allocate (calcemis(nchanprof), stat=alloc_status(7))   
   if ( any( alloc_status /= 0 ) ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for emissivity or calcemis arrays"/))
   end if
   !在调用rttov_scatt_ad前，所有AD的输入和输出结构都必须初始化为0
    call rttov_init_prof(profiles_ad)
    call rttov_init_scatt_prof(cld_profiles_ad)
	call rttov_init_rad(radiance_ad)
	
   !计算辐射扰动
   do n = 1, nprofiles
       radiance_ad % bt ((n-1)*nchanl+1:n*nchanl) = tb(1:nchanl,n)
       radiance_ad % total ((n-1)*nchanl+1:n*nchanl) = 0.0
   end do  

   else if ( coefs(inst)%coef%id_sensor == sensor_id_mw .or. &
             coefs(inst)%coef%id_sensor == sensor_id_po ) then ! microwave sensor
      do n = 1, nprofiles
         if ( profiles(n) % skin % surftype == 1) then  ! sea  
            calcemis((n-1)*nchanl+1:n*nchanl) = .true.
            emissivity((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
            emissivity_ad((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
         else                                           ! 0:land ; 2:sea-ice
            calcemis((n-1)*nchanl+1:n*nchanl) = .false.
            emissivity((n-1)*nchanl+1:n*nchanl)%emis_in = 0.9
            emissivity_ad((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
         end if
      end do
   end if
	
   call  rttov_scatt_ad(          &
      & errorstatus,       & ! out
      & chanprof,          & ! in
      & opts_scatt(inst),  & ! in
	  & frequencies,       & ! in
      & profiles,          & ! in 
      & profiles_ad,       & ! inout
	  & cld_profiles,      & ! in
	  & cld_profiles_ad,   & ! in
      & coefs(inst),       & ! in
	  & coefs_scatt(inst), & ! in
      & radiance,          & ! inout
      & radiance_ad,       & ! inout
      & calcemis,          & ! in,    optional
      & emissivity,        & ! inout, optional
      & emissivity_ad)       ! inout, optional

   if ( print_detail_rad .or. errorstatus /= errorstatus_success ) then
       write (message( 1),*) 'rttov_ad error code    = ', errorstatus
       write (message( 2),*) 'nchanl                 = ', nchanl
       write (message( 3),*) 'nprofiles              = ', nprofiles
       write (message( 4),*) 'calcemis               = ', calcemis
       write (message( 5),*) 'profiles%s2m           = ', profiles(1)%s2m
       write (message( 6),*) 'profiles%skin          = ', profiles(1)%skin
       write (message( 7),*) 'profiles%zenangle      = ', profiles(1)%zenangle
       write (message( 8),*) 'profiles%azangle       = ', profiles(1)%azangle
       write (message( 9),*) 'profiles%p             = ', profiles(1)%p
       write (message(10),*) 'profiles%t             = ', profiles(1)%t
       write (message(11),*) 'profiles%q             = ', profiles(1)%q
       write (message(12),*) 'emissivity_out         = ', emissivity(:)%emis_out
       write (message(13),*) 'radiance               = ', radiance%bt_clear
       write (message(14),*) 'profiles_ad%s2m        = ', profiles_ad(1)%s2m
       write (message(15),*) 'profiles_ad%skin       = ', profiles_ad(1)%skin
       write (message(16),*) 'profiles_ad%zenangle   = ', profiles_ad(1)%zenangle
       write (message(17),*) 'profiles_ad%azangle    = ', profiles_ad(1)%azangle
       write (message(18),*) 'profiles_ad%p          = ', profiles_ad(1)%p
       write (message(19),*) 'profiles_ad%t          = ', profiles_ad(1)%t
       write (message(20),*) 'profiles_ad%q          = ', profiles_ad(1)%q
       write (message(21),*) 'emissivity_out_ad      = ', emissivity_ad(:)%emis_out
       write (message(22),*) 'radiance_ad            = ', radiance_ad%bt
   do n = 1, nprofiles
      con_vars_ad(n)%q(:)         = profiles_ad(n) % q(:)
      con_vars_ad(n)%ps           = profiles_ad(n) % s2m % p
	  con_cld_vars_ad(n)%p(:)     = cld_profiles_ad(n) % p(:)
	  con_cld_vars_ad(n)%ph(:)    = cld_profiles_ad(n) % ph(:)
	  con_cld_vars_ad(n)%t(:)     = cld_profiles_ad(n) % t(:)
	  con_cld_vars_ad(n)%cc(:)    = cld_profiles_ad(n) % cc(:)
	  con_cld_vars_ad(n)%clw(:)   = cld_profiles_ad(n) % clw(:)
	  con_cld_vars_ad(n)%rain(:)  = cld_profiles_ad(n) % rain(:)
	  if (use_totalice = .true.) then
		  con_cld_vars_ad(n)%totalice(:) = cld_profiles_ad(n) % totalice(:)
      else
          con_cld_vars_ad(n)%ciw(:)   = cld_profiles_ad(n) % ciw(:)	  
		  con_cld_vars_ad(n)%sp(:)    = cld_profiles_ad(n) % sp(:)
   end do

   deallocate (emissivity)
   deallocate (emissivity_ad)
   deallocate (calcemis)
   deallocate (chanprof)
   
   ! deallocate radiance arrays
   call rttov_alloc_rad (errorstatus,nchanprof,radiance,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance deallocation error"/))
   end if
   call rttov_alloc_rad (errorstatus,nchanprof,radiance_ad,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance AD deallocation error"/))
   end if

   ! deallocate profile arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles,nlevels,opts(inst),asw=0)
   deallocate(profiles,stat=alloc_status(8))
   if ( errorstatus /= errorstatus_success .or. alloc_status(8) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile deallocation error"/))
   end if
   call rttov_alloc_prof (errorstatus,nprofiles,profiles_ad,nlevels,opts(inst),asw=0)
   deallocate(profiles_ad,stat=alloc_status(9))
   if ( errorstatus /= errorstatus_success .or. alloc_status(9) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile AD deallocation error"/))
   end if

   ! deallocate cld_profile arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles,nlevels,opts(inst),asw=0)
   deallocate(cld_profiles,stat=alloc_status(10))
   if ( errorstatus /= errorstatus_success .or. alloc_status(10) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile deallocation error"/))
   end if
   call rttov_alloc_prof (errorstatus,nprofiles,profiles_ad,nlevels,opts(inst),asw=0)
   deallocate(cld_profiles_ad,stat=alloc_status(11))
   if ( errorstatus /= errorstatus_success .or. alloc_status(11) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile AD deallocation error"/))
   end if
   call da_trace_exit("da_rttov_scatt_ad")
end subroutine da_rttov_scatt_ad
#endif