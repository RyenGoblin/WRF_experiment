subroutine da_qc_mwhs2c (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for fy3c mwhs2 data.
   ! Wei Sun modified from da_qc_mwhs 2017/10/15
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.

   ! local variables
   integer   :: n,scanpos,k,isflg,ios,fgat_rad_unit,sfchgt,ii,nn
   logical   :: lmix
   real      :: si,Lindex,Tnorm,Stdd,SUMM,summ1,Aver
   ! real     :: satzen
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),      &
                nrej_mixsurface,nrej_windowchanl, nrej_si,    &
                nrej_clw,nrej_topo, num_proc_domain,  &
                nrej_limb,nrej_sfchgt,nrej_Lindex

   character(len=30)  :: filename

   if (trace_use) call da_trace_entry("da_qc_mwhs2c")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_mixsurface = 0
   nrej_windowchanl= 0
   nrej_si         = 0
   nrej_clw        = 0
   nrej_topo       = 0
   nrej_limb       = 0
   nrej_sfchgt     = 0
   num_proc_domain = 0
   nrej_Lindex     = 0

      do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2

         if (iv%instid(i)%info%proc_domain(1,n)) &
               num_proc_domain = num_proc_domain + 1

         ! 0.0  initialise QC flags by assuming good obs
         !---------------------------------------------
         iv%instid(i)%tb_qc(:,n) = qc_good

         ! a.  reject all channels over mixture surface type
         !------------------------------------------------------
         isflg = iv%instid(i)%isflg(n)
         lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
         if (lmix) then 
            iv%instid(i)%tb_qc(:,n)  =  qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_mixsurface = nrej_mixsurface + 1
         end if

         !  b.  reject channels 1 and 10 over land/sea-ice/snow
         !------------------------------------------------------
         if (isflg > 0) then
            iv%instid(i)%tb_qc(1,n)     = qc_bad
            iv%instid(i)%tb_qc(10,n)    = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_windowchanl = nrej_windowchanl + 1
            if (only_sea_rad) iv%instid(i)%tb_qc(:,n)    = qc_bad
	      end if

         ! c. reject limb obs 
         !------------------------------------------------------
         scanpos = iv%instid(i)%scanpos(n)
         if (scanpos <= 10 .or. scanpos >= 89) then
             iv%instid(i)%tb_qc(:,n)  =  qc_bad
             if (iv%instid(i)%info%proc_domain(1,n)) &
                 nrej_limb = nrej_limb + 1
         end if

         ! satzen  = rad%satzen
         ! if (abs(satzen) > 45.0) &
         !    iv%instid(i)%tb_qc(:,n)  =  qc_bad

         !  d. check cloud/precipitation
         !-----------------------------------------------------------
         if (ob%instid(i)%tb(1,n) > 0.0 .and. ob%instid(i)%tb(10,n) > 0.0 ) then
            si = ob%instid(i)%tb(1,n)-ob%instid(i)%tb(10,n)
            if (si >= 4.0) then
               iv%instid(i)%tb_qc(:,n) = qc_bad
               iv%instid(i)%cloud_flag(:,n) = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_si = nrej_si + 1
            end if
         end if

         !!!!! modified based on Qin Zhengkun et al. by Fangdi ！！！！！！！
         SUMM=0.0
         Aver=0.0
         do ii=1,15
            SUMM=SUMM+ob%instid(i)%tb(ii,n)
         end do 
         Aver=SUMM/15
         
         Stdd=0.0
         summ1=0.0
         do nn=1,15
            summ1=summ1+(ob%instid(i)%tb(nn,n)-Aver)**2
         end do   
         Stdd=sqrt(summ1/15)  
         Tnorm=(ob%instid(i)%tb(1,n)-Aver)/Stdd
         Lindex=Tnorm/(((ob%instid(i)%tb(10,n)/100-1)**3)/2)    

         if (Lindex > 0.35) then
            iv%instid(i)%tb_qc(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               ! nrej_Lindex = nrej_Lindex + 1
               nrej_clw = nrej_clw + 1
         end if

         if (iv%instid(i)%clwp(n) >= 0.02) then
            iv%instid(i)%tb_qc(:,n) = qc_bad
            iv%instid(i)%cloud_flag(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw = nrej_clw + 1
         end if

         !  e. check surface height/pressure
         !------------------------------------------------------
         sfchgt = iv%instid(i)%info%elv(n)
         if (sfchgt >=1500) then
            iv%instid(i)%tb_qc(11,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_sfchgt = nrej_sfchgt + 1
         end if
         if (sfchgt >=1000) then
            iv%instid(i)%tb_qc(12,n) = qc_bad
            iv%instid(i)%tb_qc(13,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_sfchgt = nrej_sfchgt + 1
         end if
         if (sfchgt >=800) then
            iv%instid(i)%tb_qc(14,n) = qc_bad
            iv%instid(i)%tb_qc(15,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_sfchgt = nrej_sfchgt + 1
         end if

	!!!!! chanel 5 when ps ???? !!!!!
         ! if ((isflg .ne. 0) .and. (iv%instid(i)%ps(n) < 800.0)) then
         !    iv%instid(i)%tb_qc(15,n)  = qc_bad
         !    if (iv%instid(i)%info%proc_domain(1,n)) &
         !       nrej_topo = nrej_topo + 1
         ! end if

         !  g. check iuse (pre-rejected channels by .info files)
         !------------------------------------------------------
         do k = 1, nchan
           if (satinfo(i)%iuse(k) .eq. -1) &
                iv%instid(i)%tb_qc(k,n) = qc_bad
         end do

         !  f. check innovation
         !-----------------------------------------------------------
         do k = 1, nchan
          ! absolute departure check
            if (abs(iv%instid(i)%tb_inv(k,n)) > 5.0) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if

          ! relative departure check
            if (use_error_factor_rad) then
                 iv%instid(i)%tb_error(k,n) =  &
                    satinfo(i)%error_std(k)*satinfo(i)%error_factor(k)
            else
                 iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
            end if

            if (abs(iv%instid(i)%tb_inv(k,n)) > 3.0*iv%instid(i)%tb_error(k,n)) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_omb_std(k) = nrej_omb_std(k) + 1
            end if

           ! final QC decision
            if (iv%instid(i)%tb_qc(k,n) == qc_bad) then
               iv%instid(i)%tb_error(k,n) = 500.0
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej(k) = nrej(k) + 1
            else
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  ngood(k) = ngood(k) + 1
            end if
         end do ! chan
      end do ! end loop pixel

   ! Do inter-processor communication to gather statistics.

   call da_proc_sum_int (num_proc_domain)
   call da_proc_sum_int (nrej_mixsurface)
   call da_proc_sum_int (nrej_windowchanl)
   call da_proc_sum_int (nrej_si)
   call da_proc_sum_int (nrej_clw)
   call da_proc_sum_int (nrej_topo)
   call da_proc_sum_int (nrej_limb)
   call da_proc_sum_ints (nrej_omb_abs(:))
   call da_proc_sum_ints (nrej_omb_std(:))
   call da_proc_sum_ints (nrej(:))
   call da_proc_sum_ints (ngood(:))

   if (rootproc) then
      if (num_fgat_time > 1) then
         write(filename,'(i2.2,a,i2.2)') it, '_qcstat_'//trim(iv%instid(i)%rttovid_string)//'_',iv%time
      else
         write(filename,'(i2.2,a)') it, '_qcstat_'//trim(iv%instid(i)%rttovid_string)
      end if
      call da_get_unit(fgat_rad_unit)
      open(fgat_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) then
         write(unit=message(1),fmt='(A,A)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      write(fgat_rad_unit, fmt='(/a/)') &
         'Quality Control Statistics for '//iv%instid(i)%rttovid_string
      write(fgat_rad_unit,'(a20,i7)') ' num_proc_domain  = ', num_proc_domain
      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_windowchanl = ', nrej_windowchanl
      write(fgat_rad_unit,'(a20,i7)') ' nrej_si          = ', nrej_si
      write(fgat_rad_unit,'(a20,i7)') ' nrej_clw         = ', nrej_clw
      write(fgat_rad_unit,'(a20,i7)') ' nrej_topo        = ', nrej_topo
      write(fgat_rad_unit,'(a20,i7)') ' nrej_limb        = ', nrej_limb
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_abs(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_abs(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_std(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_std(:)
      write(fgat_rad_unit,'(a20)')    ' nrej(:)          = '
      write(fgat_rad_unit,'(10i7)')     nrej(:)
      write(fgat_rad_unit,'(a20)')    ' ngood(:)         = '
      write(fgat_rad_unit,'(10i7)')     ngood(:)

      close(fgat_rad_unit)
      call da_free_unit(fgat_rad_unit)
   end if

   if (trace_use) call da_trace_exit("da_qc_mwhs2c")

end subroutine da_qc_mwhs2c
