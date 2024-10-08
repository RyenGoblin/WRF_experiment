subroutine da_get_innov_vector_radiance (it, grid, ob, iv)

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x) - bc
   !       1. obs BT - simulated BT
   !       2. Bias correction
   !       3. Radiances Quality Control
   !
   !  HISTORY: 10/24/2007 - Creation from da_get_innov_vector_crtm  Tom Auligne
   !---------------------------------------------------------------------------
   
   implicit none
   
   integer, intent(in)            :: it       ! External iteration.
   type(domain),   intent(in)     :: grid
   type (y_type),  intent(inout)  :: ob       ! Observation structure.
   type (iv_type), intent(inout)  :: iv       ! O-B structure.

   integer                        :: inst

   if(trace_use) call  da_trace_entry("da_get_innov_vector_radiance")

   iv%instid(:)%info%n1 = iv%instid(:)%info%plocal(iv%time-1) + 1
   iv%instid(:)%info%n2 = iv%instid(:)%info%plocal(iv%time)

   !------------------------------------------------------------------------
   ! [1.0] calculate components of innovation vector
   !------------------------------------------------------------------------
   if (rtm_option == rtm_option_rttov) then
#ifdef RTTOV
      if (rttov_scatt .eqv. .true.) then
	      call da_get_innov_vector_rttov_scatt(it, grid, ob, iv )
      else		  
          call da_get_innov_vector_rttov( it, grid, ob, iv )
	  end if 
#else
      call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#endif
   elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
      call da_get_innov_vector_crtm (it, grid, ob, iv )
#else
      call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#endif
   else
      call da_warning(__FILE__,__LINE__,(/"Unknown Radiative Transfer Model"/))
   endif

   !------------------------------------------------------------------------
   ! [2.0] Perform (Variational) bias correction
   !------------------------------------------------------------------------
   if (use_varbc .or. freeze_varbc) then
      call da_varbc_pred(iv)
      !varbc coldstart can not be done here when num_fgat_time>1
      !because da_varbc_coldstart uses all obs from all time slots
   else if (biascorr) then
      do inst = 1, iv%num_inst                 ! loop for sensor
         write(unit=stdout,fmt='(A,A)') 'Performing bias correction for ', &
            trim(iv%instid(inst)%rttovid_string)
         call da_biascorr(inst,ob,iv)
      end do                                   ! end loop for sensor
   end if

   if(trace_use) call  da_trace_exit("da_get_innov_vector_radiance")

end subroutine da_get_innov_vector_radiance
