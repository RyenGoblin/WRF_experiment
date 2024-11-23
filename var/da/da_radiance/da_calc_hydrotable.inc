subroutine da_calc_hydrotable (nlevels,n1,n2,grid,iv,rain,fi,sp,phm)
  implicit none

  type (domain) , intent(in)     :: grid     !first guess
  type (iv_type), intent(in)  :: iv       ! O-B structure.
  real,   intent(out)  :: fi(nlevels,n1:n2)! Output variable
  real,   intent(out)  :: rain(nlevels,n1:n2)! Output variable
  real,   intent(out)  :: sp(nlevels,n1:n2)  ! Output variable
  real,   intent(out)  :: phm(nlevels+1,n1:n2) ! Output variable
  real               :: rhc        !相对湿度阈值
  real, allocatable :: iv_pb(:,:)
  real, allocatable :: a(:,:),Vt(:,:),rho(:,:)
  integer :: n,k,inst,nlevels,n1,n2,unit1,unit2,unit3,unit4,unit5,i,l
  
  unit1=10
  unit2=11
  unit3=12
  unit4=14
  write(*,*)"in da_calc_hydrotable the nlevels is ",nlevels

  allocate(iv_pb(nlevels,n1:n2))
  allocate(a(nlevels,n1:n2))
  allocate(Vt(nlevels,n1:n2))
  allocate(rho(nlevels,n1:n2))


  sp(:,:) = 0

  !对基态压力做水平插值
  do inst = 1,iv%num_inst

       do k = 1,nlevels
  !计算雨水流量  
          rain(k,n1:n2) = 0.00
          !write(*,*)"rain calculate success!"
  !计算云覆盖
          do n=n1,n2
             if (coefs(inst)%coef%ref_prfl_p (k) < 450) then
                 rhc = 80
             else if (450<=coefs(inst)%coef%ref_prfl_p (k) .and. coefs(inst)%coef%ref_prfl_p (k)<800) then
                 rhc = 60
             else
                 rhc = 40
             end if
             if (iv%instid(inst)%rh(k,n)<rhc) then
                 fi(k,n) = 0.00
             else
                 fi(k,n) = ((iv%instid(inst)%rh(k,n)-rhc)/(100-rhc))**2
             end if
            end do
          !write(*,*)"cloud cover calculate success!"
       end do
       !phm(nlevels+1,n1:n2) = iv%instid(inst)%ps (n1:n2) !半层压力的最顶层为地表气压
       phm(nlevels+1,n1:n2) = 1050.00
       phm(1,n1:n2)   = 0.00                           !半层压力的底层为模式顶层气压
           
       do k=2,nlevels
           phm(k,n1:n2) = 0.5 * (coefs(inst)%coef%ref_prfl_p (k-1) + coefs(inst)%coef%ref_prfl_p (k)) !其他半层气压采用对数插值方案
       end do
       write(*,*)"phm calculate success!"
       write(*,*)"iv_pb array size",shape(iv_pb)
       write(*,*)"phm array size",shape(phm)
       write(*,*)"rain array size",shape(rain)
       write(*,*)"cc array size",shape(fi)


   
   end do
   !输出计算出的水凝物到文件内
    !open(unit=unit2,file='rain.txt', status='unknown')
    !do l = 1,nlevels
    !    write(unit2,*)(rain(l,n),n=n1,n2)    
    !end do
    !close(unit2)
    !write(*,*)"rain.txt output success!"
    !open(unit=unit3,file='cc.txt', status='unknown')
    !do l = 1,nlevels
    !    write(unit3,*)(fi(l,n),n=n1,n2)
    !end do
    !close(unit3)
    !write(*,*)"cloud_cover.txt output success!"
    !open(unit=unit4,file='ph.txt', status='unknown')
    !do l = 1,nlevels+1
    !    write(unit4,*)(phm(l,n),n=n1,n2)
    !end do
    !close(unit4)
   ! write(*,*)"half_pressure.txt output success!"   




  deallocate(iv_pb)
  deallocate(a)
  deallocate(Vt)
  deallocate(rho)
  write(*,*)"hydrotable calc success!"
end subroutine da_calc_hydrotable


