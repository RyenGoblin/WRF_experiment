#ifdef RTTOV
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
   type (con_cld_vars_type),intent (inout) :: con_cld_vars_ad (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   real                ,  intent (in) :: tb(nchanl,nprofiles)

   ! local variables
   integer             :: n, j, i, asw, nlevels, nchanprof,iprof,unit1,unit2,unit3,unit4,lv
   Integer             :: alloc_status(18)
   
   ! RTTOV input parameters
   type (rttov_chanprof), allocatable  :: chanprof(:)
   type (rttov_profile),   allocatable :: profiles(:), profiles_ad(:)
   type (rttov_profile_cloud),  allocatable :: cld_profiles(:),cld_profiles_ad(:)   
   type (rttov_emissivity), allocatable :: emissivity(:), emissivity_ad(:)
   logical,               allocatable  :: calcemis(:)
   logical,allocatable                 :: use_chan(:,:)
   integer,allocatable                 :: channel_list(:)
   integer,allocatable                 :: frequencies(:)    !定义频率
   logical                     :: use_totalice, mmr_snowrain
   
   ! RTTOV out parameters
   integer                            :: errorstatus

   ! RTTOV inout parameters
   type (rttov_radiance)               :: radiance, radiance_ad
   call da_trace_entry("da_rttov_scatt_ad")

   nchanprof = nchanl*nprofiles
   nlevels = con_vars(1) % nlevels
   
   !---------------------------给数据分配内存---------------------------
   alloc_status (:) = 0
   ! allocate profiles structure
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
   
   !allocate profiles_ad structure
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

   !allocate cld_profiles structure
   allocate (cld_profiles(nprofiles),stat=alloc_status(3))
   call rttov_alloc_scatt_prof(  &
      & errorstatus,             &
      & nprofiles,               &
      & cld_profiles,            &
      & nlevels,                 &
	   & use_totalice =.false.,   & ! false => separate ciw and snow; true => totalice
      & asw=1,                   & ! Switch (1=allocate; 0=deallocate)
      & init = .true.,           & 
	  & mmr_snowrain =.false.)      ! snow/rain input units: false => kg/m2/s; true => kg/kg
   if ( errorstatus /= errorstatus_success .or. alloc_status(3) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for scatt profile arrays"/))
   end if
   
   !allocate cld_profiles_ad structure
   allocate (cld_profiles_ad(nprofiles),stat=alloc_status(4))
   call rttov_alloc_scatt_prof(  &
      & errorstatus,             &
      & nprofiles,               &
      & cld_profiles_ad,         &
      & nlevels,                 &
	   & use_totalice =.false.,   & ! false => separate ciw and snow; true => totalice
      & asw=1,                   & 
      & init = .true.,           & 
	  & mmr_snowrain =.false.)
   if ( errorstatus /= errorstatus_success .or. alloc_status(4) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for scatt profile AD arrays"/))
   end if 

   !allocate radiance structure
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

   !allocate emissivity structure
   allocate (emissivity(nchanprof), stat=alloc_status(5)) 
   if (alloc_status(5) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for emissivity arrays"/))
   end if

   !allocate emissivity_ad structure
   allocate (emissivity_ad(nchanprof), stat=alloc_status(6))
   if (alloc_status(6) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for emissivity_ad arrays"/))
   end if

   !allocate calcemis structure
   allocate (calcemis(nchanprof), stat=alloc_status(7))
   if (alloc_status(7) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for calcemis arrays"/))
   end if
   
   !allocate frequencies structure
   allocate (frequencies(nchanprof), stat=alloc_status(8))
   if (alloc_status(8) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for frequencies arrays"/))
   end if  

   !allocate chanprof structure
   allocate (chanprof(nchanprof), stat=alloc_status(9))
   if (alloc_status(9) /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"chanprof allocation error"/))
   end if
   
   !------------------------生成通道、廓线和频率索引-------------------
   ! generate the chanprof array
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
 
   !--------------------------------------读取廓线数据--------------------------------------------------------
   !do iprof = 1, nprofiles
   iprof = 1 
      
	   if (use_totalice .eqv. .true.) then !使用totalice
	      profiles(iprof)     % p   (:)     = coefs(inst)%coef%ref_prfl_p (:) ! full level pressure (hPa)
		   cld_profiles(iprof) % ph  (:)     = con_cld_vars(iprof)%ph      (:) ! half level pressure (hPa)
		   profiles(iprof)     % t   (:)     = con_vars(iprof)%t           (:) ! temperature (K)
		   profiles(iprof)     % q   (:)     = con_vars(iprof)%q           (:) ! specific humility
		   cld_profiles(iprof) % cc  (:)     = con_cld_vars(iprof)%cc      (:) ! cloud cover (0-1)
         cld_profiles(iprof) % clw (:)     = con_cld_vars(iprof)%clw     (:) ! liquid water (kg/kg)
         cld_profiles(iprof) % totalice(:) = con_cld_vars(iprof)%ciw     (:)  !combined ice water and snow (kg/kg)
         !cld_profiles(iprof) % rain(:)     = con_cld_vars(iprof)%rain    (:) ! rain (kg/kg)
         cld_profiles(iprof) % rain(:)     = 0.00
	   else                           !不使用totalice
	      profiles(iprof)     % p   (:)     = coefs(inst)%coef%ref_prfl_p (:)  ! full level pressure (hPa)
		   cld_profiles(iprof) % ph  (:)     = con_cld_vars(iprof)%ph      (:) ! half level pressure (hPa)
		   profiles(iprof)     % t   (:)     = con_vars(iprof)%t       (:) ! temperature (K)
		   profiles(iprof)     % q   (:)     = con_vars(iprof)%q           (:) ! specific humility
		   cld_profiles(iprof) % cc  (:)     = con_cld_vars(iprof)%cc      (:) ! cloud cover (0-1)
         cld_profiles(iprof) % clw (:)     = con_cld_vars(iprof)%clw     (:) ! liquid water (kg/kg)
         cld_profiles(iprof) % ciw (:)     = con_cld_vars(iprof)%ciw     (:) ! ice water (kg/kg)
		   cld_profiles(iprof) % rain(:)     = con_cld_vars(iprof)%rain    (:) ! rain (kg/kg)
		   !cld_profiles(iprof) % sp  (:)     = con_cld_vars(iprof)%sp     (:) ! frozen precip. (kg/kg)
      end if
   
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

      profiles(iprof) % s2m  % t    = aux_vars (iprof) % t2m
      profiles(iprof) % s2m  % q    = aux_vars (iprof) % q2m
      profiles(iprof) % s2m  % o    = 0.0 !aux_vars (iprof) % o3
      profiles(iprof) % s2m  % p    = con_vars (iprof) % ps
      !profiles(iprof) % s2m  % p    = 1050.00
      profiles(iprof) % s2m  % u    = aux_vars (iprof) % u10
      profiles(iprof) % s2m  % v    = aux_vars (iprof) % v10
      profiles(iprof) % zenangle  = aux_vars (iprof) % satzen
	   profiles(iprof) % azangle   = aux_vars (iprof) % satazi
      profiles(iprof) % elevation = 0.001* aux_vars(iprof) % elevation   ! km
      profiles(iprof) % latitude  = aux_vars(iprof) % rlat

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
   !end do

      

   !计算辐射扰动
   do n = 1, nprofiles
       radiance_ad % bt ((n-1)*nchanl+1:n*nchanl) = tb(1:nchanl,n)
       radiance_ad % total ((n-1)*nchanl+1:n*nchanl) = 0.0
   end do  

   if ( coefs(inst)%coef%id_sensor == sensor_id_ir .or. &
        coefs(inst)%coef%id_sensor == sensor_id_hi ) then  ! infrared sensor 
      calcemis(1:nchanprof)   = .true.
      emissivity(1:nchanprof)%emis_in = 0.0
      emissivity_ad(1:nchanprof)%emis_in = 0.0
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
      & opts_scatt(inst),  & ! in
	  & nlevels,           & ! in
      & chanprof,          & ! in
	  & frequencies,       & ! in
      & profiles,          & ! in 
	  & cld_profiles,      & ! in
	  & coefs(inst),       & ! in
	  & coefs_scatt(inst), & ! in
      & calcemis,          & ! in,    optional
      & emissivity,        & ! inout, optional	  
      & profiles_ad,       & ! inout
	  & cld_profiles_ad,   & ! in
      & emissivity_ad,     & ! inout, optional
      & radiance,          & ! inout
      & radiance_ad)         ! inout
	  
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
	   write (message(12),*) 'cld_profiles%ph         = ', cld_profiles(1)%ph
	   write (message(13),*) 'cld_profiles%cc         = ', cld_profiles(1)%cc
	   write (message(14),*) 'cld_profiles%clw        = ', cld_profiles(1)%clw
	   write (message(15),*) 'cld_profiles%ciw        = ', cld_profiles(1)%ciw
	   write (message(16),*) 'cld_profiles%rain       = ', cld_profiles(1)%rain
	  !write (message(16),*) 'cld_profiles%sp         = ', cld_profiles(1)%sp
       write (message(12),*) 'emissivity_out         = ', emissivity(:)%emis_out
       write (message(13),*) 'radiance               = ', radiance%bt
       write (message(14),*) 'profiles_ad%s2m        = ', profiles_ad(1)%s2m
       write (message(15),*) 'profiles_ad%skin       = ', profiles_ad(1)%skin
       write (message(16),*) 'profiles_ad%zenangle   = ', profiles_ad(1)%zenangle
       write (message(17),*) 'profiles_ad%azangle    = ', profiles_ad(1)%azangle
       write (message(18),*) 'profiles_ad%p          = ', profiles_ad(1)%p
       write (message(19),*) 'profiles_ad%t          = ', profiles_ad(1)%t
       write (message(20),*) 'profiles_ad%q          = ', profiles_ad(1)%q
	   write (message(12),*) 'cld_profiles_ad%ph      = ', cld_profiles_ad(1)%ph
	   write (message(13),*) 'cld_profiles_ad%cc      = ', cld_profiles_ad(1)%cc
	   write (message(14),*) 'cld_profiles_ad%clw     = ', cld_profiles_ad(1)%clw
	   write (message(15),*) 'cld_profiles_ad%ciw     = ', cld_profiles_ad(1)%ciw
	   write (message(16),*) 'cld_profiles_ad%rain    = ', cld_profiles_ad(1)%rain
      !write (message(16),*) 'cld_profiles_ad%sp      = ', cld_profiles_ad(1)%sp
       write (message(21),*) 'emissivity_out_ad      = ', emissivity_ad(:)%emis_out
       write (message(22),*) 'radiance_ad            = ', radiance_ad%bt
  end if
  !------------------------将得到的扰动传出-------------------------
  do n = 1, nprofiles
      con_vars_ad(n)%t(:)         = profiles_ad(n) % t(:)
      con_vars_ad(n)%q(:)         = profiles_ad(n) % q(:)
		con_cld_vars_ad(n)%p(:)     = profiles_ad(n) % p(:)
      con_vars_ad(n)%ps           = profiles_ad(n) % s2m % p
	   con_cld_vars_ad(n)%ph(:)    = cld_profiles_ad(n) % ph(:)
	   con_cld_vars_ad(n)%cc(:)    = cld_profiles_ad(n) % cc(:)
	   con_cld_vars_ad(n)%clw(:)   = cld_profiles_ad(n) % clw(:)
      con_cld_vars_ad(n)%ciw(:)   = cld_profiles_ad(n) % ciw(:)
	   con_cld_vars_ad(n)%rain(:)  = cld_profiles_ad(n) % rain(:)
	   !con_cld_vars_ad(n)%sp(:)    = cld_profiles_ad(n) % sp(:) 
	end do
 
   !--------------------------释放变量内存------------------------
   ! deallocate profile arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles,nlevels,opts(inst),asw=0)
   deallocate(profiles,stat=alloc_status(10))
   if ( errorstatus /= errorstatus_success .or. alloc_status(10) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile deallocation error"/))
   end if
   
   ! deallocate profile_ad arrays   
   call rttov_alloc_prof (errorstatus,nprofiles,profiles_ad,nlevels,opts(inst),asw=0)
   deallocate(profiles_ad,stat=alloc_status(11))
   if ( errorstatus /= errorstatus_success .or. alloc_status(11) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile AD deallocation error"/))
   end if

   ! deallocate cld_profile arrays
   call rttov_alloc_scatt_prof (errorstatus,nprofiles,cld_profiles,nlevels,.false.,asw=0)
   deallocate(cld_profiles,stat=alloc_status(12))
   if ( errorstatus /= errorstatus_success .or. alloc_status(12) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile deallocation error"/))
   end if
   
   ! deallocate cld_profile_ad arrays
   call rttov_alloc_scatt_prof (errorstatus,nprofiles,cld_profiles_ad,nlevels,.false.,asw=0)
   deallocate(cld_profiles_ad,stat=alloc_status(13))
   if ( errorstatus /= errorstatus_success .or. alloc_status(13) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"cld_profile AD deallocation error"/))
   end if

   ! deallocate radiance arrays
   call rttov_alloc_rad (errorstatus,nchanprof,radiance,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance deallocation error"/))
   end if
   
   ! deallocate radiance_ad arrays   
   call rttov_alloc_rad (errorstatus,nchanprof,radiance_ad,nlevels,asw=0)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance AD deallocation error"/))
   end if

   !deallocate emissivity structure
   deallocate (emissivity, stat=alloc_status(14)) 
   if (alloc_status(14) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for emissivity arrays"/))
   end if

   !deallocate emissivity_ad structure
   deallocate (emissivity_ad, stat=alloc_status(15))
   if (alloc_status(15) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for emissivity_ad arrays"/))
   end if

   !allocate calcemis structure
   deallocate (calcemis, stat=alloc_status(16))
   if (alloc_status(16) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for calcemis arrays"/))
   end if
   
   !allocate frequencies structure
   deallocate (frequencies, stat=alloc_status(17))
   if (alloc_status(17) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for frequencies arrays"/))
   end if  

   !allocate chanprof structure
   deallocate (chanprof, stat=alloc_status(18))
   if (alloc_status(18) /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"chanprof allocation error"/))
   end if

   deallocate(use_chan)
   call da_trace_exit("da_rttov_scatt_ad")
   
#endif
end subroutine da_rttov_scatt_ad
