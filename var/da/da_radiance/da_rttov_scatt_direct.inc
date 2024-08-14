#ifdef RTTOV
subroutine da_rttov_scatt_direct(inst, nchanl, nprofiles, nlevels, &
                          con_vars, aux_vars, con_cld_vars, &
                          tb, rad_xb, rad_ovc, emissivity)! 增加了额外的云雨背景场变量con_cld_vars

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the forward subroutine of RTTOV_SCATT
   !---------------------------------------------------------------------------
   implicit none

   integer             , intent (in) :: inst, nchanl, nprofiles, nlevels
   type (con_vars_type), intent (in) :: con_vars(nprofiles)         !相当于RTTOV内的rttov_profile
   type (aux_vars_type), intent (in) :: aux_vars(nprofiles)         !提供2m变量
   type (con_cld_vars_type), intent (in) :: con_cld_vars(nprofiles) !相当于RTTOV内的rttov_profile_cloud,con_cld_vars_type来自da_radiance1.f90
   real                , intent (inout) :: tb(nchanl,nprofiles)
   real                , intent (inout) :: rad_xb(nchanl,nprofiles)
   real                , intent (inout) :: rad_ovc(nchanl,nlevels-1,nprofiles)
   type (rttov_emissivity), intent (inout) :: emissivity(nchanl*nprofiles)

   ! local variables
   integer             :: nchanprof, asw
   integer             :: n, i, j, k,iprof              !由于scatt_forward需要读取每条廓线的压力层数据(full\half pressure level)，额外定义iprof
   integer             :: alloc_status(3)
   integer,allocatable :: frequencies(:)    !定义频率
   logical             :: use_totalice, mmr_snowrain
   ! RTTOV input parameters
   type (rttov_chanprof),      allocatable :: chanprof(:)    !定义要模拟的通道/廓线索引
   type (rttov_profile),       allocatable :: profiles(:)
   type (rttov_profile_cloud), allocatable :: cld_profiles(:)!定义scatt模拟需要的额外廓线
   logical, allocatable               :: calcemis(:)
   logical,allocatable :: use_chan(:,:)
   integer,allocatable :: channel_list(:)
   ! RTTOV out parameters
   integer             :: errorstatus

   ! RTTOV inout parameters
   type (rttov_radiance)     :: radiance

   call da_trace_entry("da_rttov_scatt_direct")
   alloc_status (:) = 0
   nchanprof = nchanl*nprofiles
   !Allocate the RTTOV-SCATT cloud profiles structure
   allocate(cld_profiles(nprofiles), stat=alloc_status(1))
   ! 将原本的rttov_alloc_prof替换成了对应的rttov_alloc_scatt_prof并增加对应变量
  
   call rttov_alloc_scatt_prof(  &
      & errorstatus,             &
      & nprofiles,               &
      & cld_profiles,            &
      & nlevels,                 &
	  & use_totalice=.false.,    & ! false => separate ciw and snow; true => totalice
      & asw=1,                   & ! Switch (1=allocate; 0=deallocate)
      & init =.true.,           &
	  & mmr_snowrain=.false.)    ! snow/rain input units: false => kg/m2/s; true => kg/kg
   if ( errorstatus /= errorstatus_success .or. alloc_status(1) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"scatt_profile allocation error"/))
   end if

   !------------------------生成通道、廓线和频率索引-------------------
   !确定哪些指定通道用于模拟
   !为channel_list赋值,channel_list是一个由1到nchannel的数字列表
   channel_list=[(i,i=1,nchanl)]

   allocate(use_chan(nprofiles,nchanl)) 
   allocate(frequencies(nchanprof))
   !Set use_chan to .TRUE. only for required channels
   use_chan(:,:) = .FALSE.
   do j = 1, nprofiles
     use_chan(j,channel_list(1:nchanl)) = .TRUE.
   end do

    call rttov_scatt_setupindex ( &
       nprofiles,          &
       nchanl,             &           !读取通道数
       coefs(inst),        &
       nchanprof,          &
       chanprof,           &
       frequencies,        &
       use_chan)

   !---------------------------为SCATT_forward模式中的参数的分配内存--------------------
   !调用call rttov_alloc_direct内包含对profiles,rad的allocate，因此不需要在额外为这些数据分配内存
   call rttov_alloc_direct(      &
        errorstatus,             &
		1,                       &
        nprofiles,               &
        nchanprof,               &
        nlevels,                 &
        chanprof,                &
        opts(inst),              &
        profiles,                &
        coefs(inst),             &
        radiance = radiance,     &
        calcemis = calcemis,     &
        emissivity = emissivity, &
        init = .TRUE.)
    if (errorstatus /= errorstatus_success) THEN
         call da_error(__FILE__,__LINE__, &
        (/"alloc direct model error"/))
    end if
   !------------------------------廓线数据读取-----------------------------


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
	   else                           
	     !不使用totalice
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

	 !Surface variables
	  profiles(iprof) % skin % t            = aux_vars (iprof) % surft
	  profiles(iprof) % skin % surftype = aux_vars(iprof) % surftype       !land = 0, sea = 1,seaice = 2
	 !profiles(iprof) % skin % salinity = aux_vars(iprof) % salinity       !FASTEM4-6需要的变量，原始wrfda中没有此变量，需要额外定义

	 ! for microwave channels, land/sea-ce emissivity is computed
     ! from coefs in prof%skin%fastem, if calcemis = True
      if ( coefs(inst)%coef%id_sensor == sensor_id_mw .or. &
           coefs(inst)%coef%id_sensor == sensor_id_po ) then
         if ( profiles(iprof) % skin % surftype == 2 ) then  ! sea-ice
            profiles(iprof) % skin % fastem (1) = 2.2
            profiles(iprof) % skin % fastem (2) = 3.7
            profiles(iprof) % skin % fastem (3) = 122.0
            profiles(iprof) % skin % fastem (4) = 0.0
            profiles(iprof) % skin % fastem (5) = 0.15
         else if ( profiles(iprof) % skin % surftype == 0 ) then  ! land
            profiles(iprof) % skin % fastem (1) = 3.0
            profiles(iprof) % skin % fastem (2) = 5.0
            profiles(iprof) % skin % fastem (3) = 15.0
            profiles(iprof) % skin % fastem (4) = 0.1
            profiles(iprof) % skin % fastem (5) = 0.3
         end if
      end if

	 ! Elevation, latitude and longitude
      profiles(iprof) % elevation = 0.001* aux_vars(iprof) % elevation   ! km
      profiles(iprof) % latitude  = aux_vars(iprof) % rlat

      profiles(iprof) % zenangle  = aux_vars (iprof) % satzen
      profiles(iprof) % azangle   = aux_vars (iprof) % satazi

   end do



   allocate (calcemis(nchanprof), stat = alloc_status(2))
   if (alloc_status(2) /= 0) then
         call da_error(__FILE__,__LINE__, &
        (/"calemis allocation error"/))
   end if
   !当输入的emissivity值小于等于0时，计算相应的发射率值，在opt%rt_mw%fastem_version中可以选定发射率计算模型，默认为FASTEM6
   calcemis(:) = emissivity(:)%emis_in <= 0

   !-----------------------------------
   !  calling RTTOV-SCATT forward model
   !----------------------------------

   call rttov_scatt(       &
      & errorstatus,       &   ! out
	  & opts_scatt(inst),  &   ! in
	  & nlevels,           &   ! in
      & chanprof,          &   ! in     channel and profile index structure
	  & frequencies,       &   ! in     channel indexes for Mietable lookup
      & profiles,          &   ! in     profiles(nprofiles)
	  & cld_profiles,      &   ! in     cloud/hydrometeor profile array
      & coefs(inst),       &   ! in
	  & coefs_scatt(inst), &   ! in     Mietable structure
      & calcemis=calcemis, &   ! in,    optional   calcemis(nchanprof)
      & emissivity=emissivity,&! inout  optional   emissivity(nchanprof)
	  & radiance=radiance)              ! inout  computed radiances
   if (errorstatus /= errorstatus_success) then
       call da_error(__FILE__,__LINE__, &
           (/"rttov_scatt error"/))
   end if

   if ( print_detail_rad .or. errorstatus /= errorstatus_success ) then
      WRITE (UNIT=stderr,FMT=*) 'rttov_scatt error code = ', errorstatus
      WRITE (UNIT=stderr,FMT=*) 'nchanprof    = ', nchanprof
      WRITE (UNIT=stderr,FMT=*) 'nprofiles    = ', nprofiles
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%t = ', profiles(1)%s2m%t
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%q = ', profiles(1)%s2m%q
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%o = ', profiles(1)%s2m%o
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%p = ', profiles(1)%s2m%p
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%u = ', profiles(1)%s2m%u
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%v = ', profiles(1)%s2m%v
      WRITE (UNIT=stderr,FMT=*) 'profiles%skin%surftype = ', profiles(1)%skin%surftype
      WRITE (UNIT=stderr,FMT=*) 'profiles%skin%t        = ', profiles(1)%skin%t
      WRITE (UNIT=stderr,FMT=*) 'profiles%skin%fastem   = ', profiles(1)%skin%fastem
      WRITE (UNIT=stderr,FMT=*) 'profiles%zenangle = ', profiles(1)%zenangle
      WRITE (UNIT=stderr,FMT=*) 'profiles%azangle = ', profiles(1)%azangle
      WRITE (UNIT=stderr,FMT=*) 'profiles%p   = ', profiles(1)%p
      WRITE (UNIT=stderr,FMT=*) 'profiles%t   = ', profiles(1)%t
      WRITE (UNIT=stderr,FMT=*) 'profiles%q   = ', profiles(1)%q
	  WRITE (UNIT=stderr,FMT=*) 'cld_profiles%ph =', cld_profiles(1)%ph
	  WRITE (UNIT=stderr,FMT=*) 'cld_profiles%cc =', cld_profiles(1)%cc
	  WRITE (UNIT=stderr,FMT=*) 'cld_profiles%clw =', cld_profiles(1)%clw
	  WRITE (UNIT=stderr,FMT=*) 'cld_profiles%ciw =', cld_profiles(1)%ciw
	  WRITE (UNIT=stderr,FMT=*) 'cld_profiles%rain =', cld_profiles(1)%rain
	 !WRITE (UNIT=stderr,FMT=*) 'cld_profiles%sp =', cld_profiles(1)%sp
      WRITE (UNIT=stderr,FMT=*) 'calcemis     = ', calcemis
      WRITE (UNIT=stderr,FMT=*) 'emissivity   = ', emissivity(:)%emis_in
      WRITE (UNIT=stderr,FMT=*) 'emissivity_out = ', emissivity(:)%emis_out
      WRITE (UNIT=stderr,FMT=*) 'radiance = ', radiance%bt
      if ( errorstatus /= errorstatus_success) call da_error(__FILE__,__LINE__,(/"Problem in rttov_scatt_fwd"/))
   end if

   do n = 1, nprofiles
      tb(1:nchanl,n) = radiance % bt((n-1)*nchanl+1:n*nchanl)
      rad_xb(1:nchanl,n) = radiance % total((n-1)*nchanl+1:n*nchanl)
      do k = 1, nlevels-1
         rad_ovc(1:nchanl,k,n) = radiance % overcast(k,(n-1)*nchanl+1:n*nchanl)
      end do
   end do

   ! deallocate profile arrays
   deallocate (frequencies, stat=alloc_status(3))
   if (alloc_status(3) /= 0) then
      call da_error(__FILE__,__LINE__, &
        (/"frequencies deallocation error"/))
   end if
   call rttov_alloc_scatt_prof (errorstatus,nprofiles,cld_profiles,nlevels,.false.,asw)
   deallocate (cld_profiles, stat=alloc_status(4))
   if ( errorstatus /= errorstatus_success .or. alloc_status(4) /= 0) then
      call da_error(__FILE__,__LINE__, &
        (/"scatt_profile deallocation error"/))
   end if
   ! Deallocate structures for RTTOV direct model
   asw=0
   call rttov_alloc_direct( &
        errorstatus,           &
        asw,                 &  ! 0 => deallocate
        nprofiles,             &
        nchanprof,             &
        nlevels,               &
        chanprof,              &
        opts,                  &
        profiles,              &
        coefs,                 &
        radiance = radiance,   &
        calcemis = calcemis,   &
        emissivity = emissivity)
   if (errorstatus /= errorstatus_success) then
     WRITE(*,*) 'deallocation error for rttov_direct structures'
   end if
#endif

end subroutine da_rttov_scatt_direct
