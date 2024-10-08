subroutine da_get_innov_vector_rttov_scatt (it, grid, ob, iv)

   !---------------------------------------------------------------------------
   !  Purpose: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate grid%xb to obs location
   !       2. call forward RTM to get simulated bright temperature
   !       3. obs BT - simulated BT
   !---------------------------------------------------------------------------

   implicit none

   integer,           intent(in)    :: it       ! External iteration.
   type (domain),     intent(in)    :: grid     ! first guess state.
   type (y_type),     intent(inout) :: ob       ! Observation structure.
   type (iv_type),    intent(inout) :: iv       ! O-B structure.

#if defined(RTTOV)

   integer  :: n        ! Loop counter.
   integer  :: i, j, k  ! Index dimension.
   integer  :: nlevels  ! Number of obs levels.
   integer  :: nchanprof, errorstatus
   integer, pointer :: frequencies(:) => NULL()   !定义频率

   character(len=256) :: atlas_path
   real*8                           :: seap, icep, lndp, snop
   real, allocatable :: v_p(:,:)

   integer :: inst, nchan
   real,    allocatable   :: pres(:)

   integer :: n1,n2,n1n2

! FIX?
real,allocatable :: temp(:), temp2(:), temp3(:,:)

   type(rttov_emissivity), allocatable :: emissivity(:)

   type(con_vars_type),  allocatable :: con_vars(:)
   type(aux_vars_type),  allocatable :: aux_vars(:)
   type(con_cld_vars_type),  allocatable :: con_cld_vars(:) !添加水凝物变量

   type(rttov_chanprof), allocatable :: chanprof(:)
   type(rttov_profile), allocatable   :: profiles(:)

   ! variables for computing clwp
   real, allocatable :: dpf(:,:), clw(:,:), pf(:,:)

   real, allocatable :: em_mspps(:)    ! emissivity caluclated using MSPPS algorithm
   real              :: ts_mspps       ! surface temperature calcualted using MSPPS algorithm

   !For Zhuge and Zou cloud detection
   real, allocatable :: geoht_full(:,:,:)
   real              :: geoht_pixel(kts:min(kte,kme-1))
   real              :: tt_pixel(kts:min(kte,kme-1))
   real              :: pp_pixel(kts:min(kte,kme-1))

   if (trace_use) call da_trace_entry("da_get_innov_vector_rttov_scatt")

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------

   if ( use_clddet_zz ) then
      allocate ( geoht_full(ims:ime,jms:jme,kms:kme-1) )
      do k = kms, kme-1
         do j = jms, jme
            do i = ims, ime
               geoht_full(i,j,k) = 0.5 * ( grid%ph_2(i,j,k)   + grid%phb(i,j,k) + &
                                           grid%ph_2(i,j,k+1) + grid%phb(i,j,k+1) ) / gravity
            end do
         end do
      end do
   end if

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      nlevels  = iv%instid(inst)%nlevels
      nchan    = iv%instid(inst)%nchan

      if (iv%instid(inst)%info%n2 < iv%instid(inst)%info%n1) cycle
      n1 = iv%instid(inst)%info%n1
      n2 = iv%instid(inst)%info%n2

      n1n2=n2-n1+1

      allocate (pres(1:nlevels))
      allocate (con_vars(n1:n2))
      allocate (aux_vars(n1:n2))
	  allocate (con_cld_vars(n1:n2))

      pres(1:nlevels) = coefs(inst) % coef % ref_prfl_p(1:nlevels)

      allocate(v_p(kms:kme,n1:n2))
      v_p(:,:)=0.0
      allocate(clw(kms:kme,n1:n2))
      allocate(dpf(kms:kme,n1:n2))
      allocate(pf(kms:kme+1,n1:n2))

      ! horizontal interpolate grid%xb pressure to ob position for every grid%xb levels
      do n=n1,n2
         do k=kts,kte
            ! convert to mb
            v_p(k,n) = 0.01*(iv%instid(inst)%info%dym(k,n)*( &
               iv%instid(inst)%info%dxm(k,n)*grid%xb%p(iv%instid(inst)%info%i(k,n),  iv%instid(inst)%info%j(k,n),k) + &
               iv%instid(inst)%info%dx(k,n) *grid%xb%p(iv%instid(inst)%info%i(k,n)+1,iv%instid(inst)%info%j(k,n),k)) + &
               iv%instid(inst)%info%dy(k,n) *( &
               iv%instid(inst)%info%dxm(k,n)*grid%xb%p(iv%instid(inst)%info%i(k,n),  iv%instid(inst)%info%j(k,n)+1,k) + &
               iv%instid(inst)%info%dx(k,n) *grid%xb%p(iv%instid(inst)%info%i(k,n)+1,iv%instid(inst)%info%j(k,n)+1,k)))
         end do
      end do

      call da_to_zk_new(pres, v_p(:,n1:n2), v_interp_p, n1n2,nlevels,iv%instid(inst)%info%zk(:,n1:n2))

      call da_convert_zk (iv%instid(inst)%info)

      ! [1.2] Interpolate horizontally to ob:
      call da_interp_lin_3d (grid%xb%t, iv%instid(inst)%info, iv%instid(inst)%t (:,n1:n2))
      call da_interp_lin_3d (grid%xb%q, iv%instid(inst)%info, iv%instid(inst)%mr(:,n1:n2))
	  

      do n= n1,n2
         do k=1, nlevels
            if (iv%instid(inst)%info%zk(k,n) <= 0.0) then
               iv%instid(inst)%t(k,n) = coefs(inst) % coef % ref_prfl_t(k,gas_id_watervapour)  ! outside model level
               iv%instid(inst)%mr(k,n) = coefs(inst) % coef % ref_prfl_mr(k,gas_id_watervapour)
            else
               iv%instid(inst)%mr(k,n) = iv%instid(inst)%mr(k,n) * q2ppmv
            end if
            if (pres(k) < 100.0) iv%instid(inst)%mr(k,n) = coefs(inst) % coef % ref_prfl_mr(k,gas_id_watervapour)
         end do

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp( grid%xb%snow, grid%xb%xice, grid%xb%landmask,  &
            grid%xb%ivgtyp, grid%xb%isltyp, &
            ims, ime, jms, jme, &
            iv%instid(inst)%info%i(1,n), iv%instid(inst)%info%j(1,n), &
            iv%instid(inst)%info%dx(1,n), iv%instid(inst)%info%dy(1,n), &
            iv%instid(inst)%info%dxm(1,n), iv%instid(inst)%info%dym(1,n), &
            iv%instid(inst)%isflg(n),iv%instid(inst)%vegtyp(n), iv%instid(inst)%soiltyp(n), &
            seap, icep, lndp, snop )

         iv%instid(inst)%snow_frac(n) = snop   ! snow coverage fraction 0-1

         if ( iv%instid(inst)%isflg(n) == 0 .or. iv%instid(inst)%isflg(n) == 4 ) then       ! sea
            iv%instid(inst)%surftype(n) = 1
         else if ( iv%instid(inst)%isflg(n) == 1 .or. iv%instid(inst)%isflg(n) == 5 ) then  ! sea-ice with snow
            iv%instid(inst)%surftype(n) = 2
         else
            iv%instid(inst)%surftype(n) = 0
         end if

         if ( use_clddet_zz ) then
            ! Find tropopause temperature for Zhuge and Zou Cloud Detection
            do k = kts, min(kte,kme-1)
               call da_interp_2d_partial ( grid%xb%t(:,:,k), iv%instid(inst)%info, k, n, n, tt_pixel(k) )
               call da_interp_2d_partial ( grid%xb%p(:,:,k), iv%instid(inst)%info, k, n, n, pp_pixel(k) )
               call da_interp_2d_partial ( geoht_full(:,:,k), iv%instid(inst)%info, k, n, n, geoht_pixel(k) )

!               call da_interp_lin_2d ( grid%xb%t(:,:,k), iv%instid(inst)%info, k, n, n, tt_pixel(k) )
!               call da_interp_lin_2d ( grid%xb%p(:,:,k), iv%instid(inst)%info, k, n, n, pp_pixel(k) )
!               call da_interp_lin_2d ( geoht_full(:,:,k), iv%instid(inst)%info, k, n, n, geoht_pixel(k) )
            end do
            call da_trop_wmo ( tt_pixel, geoht_pixel, pp_pixel, (min(kte,kme-1)-kts+1), tropt = iv%instid(inst)%tropt(n) )
         end if
      end do

      call da_interp_lin_2d (grid%xb % u10,  iv%instid(inst)%info, 1, iv%instid(inst)%u10(n1:n2))
      call da_interp_lin_2d (grid%xb % v10,  iv%instid(inst)%info, 1, iv%instid(inst)%v10(n1:n2))
      call da_interp_lin_2d (grid%xb % t2,   iv%instid(inst)%info, 1, iv%instid(inst)%t2m(n1:n2))
      call da_interp_lin_2d (grid%xb % q2,   iv%instid(inst)%info, 1, iv%instid(inst)%q2m(n1:n2))
      call da_interp_lin_2d (grid%xb % psfc, iv%instid(inst)%info, 1, iv%instid(inst)%ps (n1:n2))
      call da_interp_lin_2d (grid%xb % tsk,  iv%instid(inst)%info, 1, iv%instid(inst)%ts (n1:n2))
      call da_interp_lin_2d (grid%xb % terr, iv%instid(inst)%info, 1, iv%instid(inst)%elevation(n1:n2))

      if ( use_mspps_ts(inst) ) then
         ! only for AMSU-A over land
         if ( trim(rttov_inst_name(rtminit_sensor(inst))) == 'amsua' ) then
            do n = n1, n2
               if ( iv%instid(inst)%surftype(n) == 0 ) then
                  call da_mspps_ts(ob%instid(inst)%tb(1:nchan,n), nchan,  &
                                   iv%instid(inst)%satzen(n), ts_mspps)
                  ! ts_mspps is initilaized as negative values in the
                  ! da_mspps_ts subroutine.  Apply only valid values here.
                  if ( ts_mspps > 0.0 ) then
                     iv%instid(inst)%ts(n) = ts_mspps
                  end if
               end if
            end do
         end if
      end if

      ! variables for calculation of cloud affected radiance
      !-------------------------------------------------------
      do k=kts,kte
         call da_interp_lin_2d (grid%xb%t  (:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%tm(k,:))
         call da_interp_lin_2d (grid%xb%q  (:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qm(k,:))
         call da_interp_lin_2d (grid%xb%qrn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qrn(k,:))
         call da_interp_lin_2d (grid%xb%qcw(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qcw(k,:))
         call da_interp_lin_2d (grid%xb%qci(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qci(k,:))
       ! call da_interp_lin_2d (grid%xb%qsn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qsn(k,:))
       ! call da_interp_lin_2d (grid%xb%qgr(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qgr(k,:))
	     call da_interp_lin_2d (grid%xb%rh (:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%rh(k,:))!将背景场相对湿度插值到观测场中
      end do
      iv%instid(inst)%pm(:,n1:n2)  = v_p(:,n1:n2)

      iv%instid(inst)%ps(n1:n2) = 0.01 * iv%instid(inst)%ps(n1:n2)       ! hPa
      iv%instid(inst)%mr2m(n1:n2) = iv%instid(inst)%q2m(n1:n2) * q2ppmv  ! ppmv
	  
	  ! 计算RTTOV-SCATT需要的额外的水凝物变量,计算半层压力
      call da_calc_hydrotable(grid,iv,iv%instid(inst)%rain,iv%instid(inst)%cc,iv%instid(inst)%sp,iv%instid(inst)%phm)!得到降雨通量、云覆盖
	  
      ! ADD for computing cloud liquid water path (mm) from guess

      pf(kts,n1:n2) = 100.0*iv%instid(inst)%ps(n1:n2)
      do k = 2,kte
         pf(k,n1:n2) = 50.0*(v_p(k-1,n1:n2)+v_p(k,n1:n2))
      end do
      pf(kte+1,n1:n2)= 50.0*v_p(kte,n1:n2)

      iv%instid(inst)%clwp(n1:n2) = 0.0
      do k = kts,kte
         dpf(k,n1:n2) = pf(k,n1:n2) - pf(k+1,n1:n2)
         clw(k,n1:n2) = iv%instid(inst)%qcw(k,n1:n2)*dpf(k,n1:n2)/gravity
         where (v_p(k,n1:n2)<100.0) clw (k,n1:n2) = 0.0
         iv%instid(inst)%clwp(n1:n2)  =iv%instid(inst)%clwp(n1:n2) + clw(k,n1:n2)
      end do

      ! surface emissivity
      !-------------------------------------------

      nchanprof = nchan*n1n2
      allocate(emissivity(nchanprof))
      emissivity(:)%emis_in = 0.0

      if ( rttov_emis_atlas_ir > 0 .or. rttov_emis_atlas_mw > 0 ) then
         ! set up emissivity atlas
         atlas_path = 'emis_data/'
         write(unit=message(1),fmt='(A,A)')  &
            'Setting up emissivity atlas for instrument ', trim(iv%instid(inst)%rttovid_string)
         call da_message(message(1:1))
         call rttov_setup_emis_atlas(                      &
            errorstatus,                                   & ! out
            opts(inst),                                    & ! in
            grid%start_month,                              & ! in
            atlas_type(inst),                              & ! in
            atlas,                                         & ! inout
            atlas_id(inst),                                & ! in, optional
            path = trim(atlas_path),                       & ! in, optional
            coefs = coefs(inst))                             ! in

         if ( errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__, &
               (/"failure in setting up emissivity atlas"/))
         end if

         ! Generate the chanprof array
         allocate(chanprof(nchanprof),frequencies(nchanprof))
		 call rttov_scatt_setupindex ( &
         n1n2,               &
         nchan,              &           !读取通道数
         coefs(inst),        &
         nchanprof,          &
         chanprof,           &
         frequencies        )

         allocate(profiles(n2-n1+1))
         do n = n1, n2
            ! latitude, longitude, surftype are used for retreiving emis from atlas
            ! zenangle is used by MW emmisivity atlas
            ! snow_frac is used only by IR emmisivity atlas
            profiles(n-n1+1)%latitude           = iv%instid(inst)%info%lat(1,n)
            profiles(n-n1+1)%longitude          = iv%instid(inst)%info%lon(1,n)
            profiles(n-n1+1)%zenangle           = iv%instid(inst)%satzen(n)
            profiles(n-n1+1)%skin%surftype      = iv%instid(inst)%surftype(n)
            profiles(n-n1+1)%skin%snow_fraction = iv%instid(inst)%snow_frac(n)
         end do

         ! Retrieve values from atlas
         call rttov_get_emis(                   &
            errorstatus,                        & ! out
            opts(inst),                         & ! in
            chanprof,                           & ! in
            profiles,                           & ! in
            coefs(inst),                        & ! in
            atlas,                              &! in
            emissivity=emissivity(:)%emis_in )    ! out
         if ( errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__, &
               (/"failure in retrieving emissivity values"/))
         end if
         deallocate (profiles)
         deallocate (chanprof)
      end if

      if ( use_mspps_emis(inst) ) then
         ! Only for AMSU-A over land
         if ( trim(rttov_inst_name(rtminit_sensor(inst))) == 'amsua' ) then
            allocate(em_mspps(nchan))
            do n = n1, n2
               if ( iv%instid(inst)%surftype(n) == 0 ) then
                  call da_mspps_emis(ob%instid(inst)%tb(1:nchan,n), nchan, em_mspps)
                  do k = 1, nchan
                     if ( emissivity((n-n1)*nchan+k)%emis_in  < 0.01 ) then
                        emissivity((n-n1)*nchan+k)%emis_in  = em_mspps(k)
                     end if
                  end do
               end if
            end do
            deallocate(em_mspps)
         end if
      end if

      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( n, temp, temp2, temp3 )
      do n=n1,n2
         con_vars(n) % nlevels = nlevels
         allocate (con_vars(n) % t(nlevels))
         allocate (con_vars(n) % q(nlevels))
		 allocate (con_cld_vars(n) % ph(nlevels+1))
		 allocate (con_cld_vars(n) % cc(nlevels))
		 allocate (con_cld_vars(n) % clw(nlevels))
		 allocate (con_cld_vars(n) % ciw(nlevels))
		 allocate (con_cld_vars(n) % rain(nlevels))
		!allocate (con_cld_vars(n) % sp(nlevels))
         if ( use_rttov_kmatrix ) then
            allocate (con_vars(n) % t_jac(nchan,nlevels))
            allocate (con_vars(n) % q_jac(nchan,nlevels))
            allocate (con_vars(n) % ps_jac(nchan))
            con_vars(n) % t_jac(:,:) = 0.0
            con_vars(n) % q_jac(:,:) = 0.0
            con_vars(n) % ps_jac(:)  = 0.0
         end if
		 
         con_vars(n) % t(1:nlevels) = iv%instid(inst)%t(1:nlevels,n)
         con_vars(n) % q(1:nlevels) = iv%instid(inst)%mr(1:nlevels,n)

         aux_vars(n) % t2m      = iv%instid(inst)%t2m(n)
         aux_vars(n) % q2m      = iv%instid(inst)%mr2m(n)
         aux_vars(n) % u10      = iv%instid(inst)%u10(n)
         aux_vars(n) % v10      = iv%instid(inst)%v10(n)
         aux_vars(n) % surftype = iv%instid(inst)%surftype(n)
         aux_vars(n) % surft    = iv%instid(inst)%ts(n)
         aux_vars(n) % satzen   = iv%instid(inst)%satzen(n)
         aux_vars(n) % satazi   = iv%instid(inst)%satazi(n)
         aux_vars(n) % solzen   = iv%instid(inst)%solzen(n)
         aux_vars(n) % solazi   = iv%instid(inst)%solazi(n)
         aux_vars(n) % elevation = iv%instid(inst)%elevation(n) !iv%instid(inst)%info%elv(n)
         aux_vars(n) % rlat     = iv%instid(inst)%info%lat(1,n)

         !加入con_cld_vars并为其赋值
		 con_cld_vars(n) % ph(1:nlevels+1)       = iv%instid(inst)%phm(1:nlevels+1,n)!半层压力层数要比nlevels多一层
		 con_cld_vars(n) % cc(1:nlevels)       = iv%instid(inst)%cc(1:nlevels,n)
		 con_cld_vars(n) % clw(1:nlevels)      = iv%instid(inst)%qcw(1:nlevels,n)
		 con_cld_vars(n) % ciw(1:nlevels)      = iv%instid(inst)%qci(1:nlevels,n)
         con_cld_vars(n) % rain(1:nlevels)     = iv%instid(inst)%rain(1:nlevels,n)
		!con_cld_vars(n) % sp(1:nlevels)       = iv%instid(inst)%sp(1:nlevels,n) !固体降水暂无计算公式，不作为输入



         ! [1.3] Call RTM forward model
         ! da_rttov_direct nominally an array version, but doesn't handle arrays
         ! of surface flags properly
         ! da_rttov_k or da_rttov_direct is used one profile per call
         allocate(temp(nchan),temp2(nchan),temp3(nchan,nlevels-1))
         if ( use_rttov_kmatrix ) then
            call da_rttov_k (inst, nchan, 1, nlevels, &
               con_vars(n:n), aux_vars(n:n), &
               temp, temp2, temp3, emissivity((n-n1)*nchan+1:(n-n1+1)*nchan))
            iv%instid(inst)%emiss(:,n) = emissivity((n-n1)*nchan+1:(n-n1+1)*nchan)%emis_out
         else
		    !调用RTTOV-SCATT正向模式
            call da_rttov_scatt_direct (inst, nchan, 1, nlevels, &
               con_vars(n:n), aux_vars(n:n),con_cld_vars(n:n), &
               temp, temp2, temp3, emissivity((n-n1)*nchan+1:(n-n1+1)*nchan))
            iv%instid(inst)%emiss(:,n) = emissivity((n-n1)*nchan+1:(n-n1+1)*nchan)%emis_out
         end if
         iv%instid(inst)%tb_xb(:,n)=temp(:)
         iv%instid(inst)%rad_xb(:,n)=temp2(:)
         ! Overcast Radiances for AIRS Cloud Detection(MMR)
         iv%instid(inst)%rad_ovc(:,1:nlevels-1,n)=temp3(:,:) ! overcast nlayers=nlevels-1
         deallocate(temp,temp2,temp3)

      end do
      !$OMP END PARALLEL DO

      if ( use_rttov_kmatrix ) then
         do n = n1, n2
            do k = 1, nlevels
               iv%instid(inst)%t_jacobian(:,k,n) = con_vars(n)%t_jac(:,k)
               iv%instid(inst)%q_jacobian(:,k,n) = con_vars(n)%q_jac(:,k)
            end do
            iv%instid(inst)%ps_jacobian(:,n) = con_vars(n)%ps_jac(:)
         end do
         do n = n1, n2
            deallocate (con_vars(n) % t_jac)
            deallocate (con_vars(n) % q_jac)
            deallocate (con_vars(n) % ps_jac)
         end do
      end if

      do n=n1,n2
         deallocate (con_vars(n) % t)
         deallocate (con_vars(n) % q)
		 deallocate (con_cld_vars(n) % ph)
		 deallocate (con_cld_vars(n) % cc)
		 deallocate (con_cld_vars(n) % clw)
		 deallocate (con_cld_vars(n) % ciw)
		 deallocate (con_cld_vars(n) % rain)
		!deallocate (con_cld_vars(n) % sp)
      end do

      !----------------------------------------------------------------
      ! [2.0] calculate components of innovation vector:
      !----------------------------------------------------------------

      do n=n1,n2
         do k=1,nchan
            if (iv%instid(inst)%tb_inv(k,n) > missing_r) then
               iv%instid(inst)%tb_inv(k,n) = ob%instid(inst)%tb(k,n) - iv%instid(inst)%tb_xb(k,n)
            else
               iv%instid(inst)%tb_inv(k,n)   = missing_r
               iv%instid(inst)%tb_qc(k,n)    = qc_bad
            end if
         end do
      end do

      deallocate (v_p)
      deallocate (clw)
      deallocate (dpf)
      deallocate (pf)
      deallocate (pres)
      deallocate (con_vars)
	  deallocate (con_cld_vars)
      deallocate (aux_vars)
      deallocate (emissivity)

      if ( rttov_emis_atlas_ir > 0 .or. rttov_emis_atlas_mw > 0 ) then
         call rttov_deallocate_emis_atlas(atlas)
      end if

   end do        ! end loop for sensor

   if ( use_clddet_zz ) deallocate ( geoht_full )

   if (trace_use) call da_trace_exit("da_get_innov_vector_rttov")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV_SCATT option for radiances"/))
#endif

end subroutine da_get_innov_vector_rttov_scatt

