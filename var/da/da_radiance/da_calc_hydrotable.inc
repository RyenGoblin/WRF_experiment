subroutine da_calc_hydrotable (grid,iv,rain,fi,sp)
  implicit none

  type (domain) , intent(in)     :: grid     !first guess
  type (iv_type), intent(inout)  :: iv       ! O-B structure.
  type(infa_type), intent(in)    :: info
  real, allocatable  intent(inout)  :: fi(:)! Output variable
  real, allocatable  intent(inout)  :: rain(:)! Output variable
  real, allocatable  intent(inout)  :: sp(:)  ! Output variable
  real               :: rhc        !相对湿度阈值
  real, allocatable :: iv_pb(:)
  real, allocatable :: a(:),Vt(:),rho(:)
  integer, :: n,k

  allocate(fi(kts:kte,n1:n2))
  allocate(rain(kts:kte,n1:n2))
  allocate(rhc(kts:kte,n1:n2))
  allocate(iv_pb(kts:kte,n1:n2))
  allocate(a(kts:kte,n1:n2))
  allocate(rho(kts:kte,n1:n2))
  allocate(sp(kts:kte,n1:n2))

  sp(:,:) = 0

  !对基态压力做水平插值
  allocate iv_pb()
  do n=n1,n2
         do k=kts,kte !是用kms还是kts？
            iv_pb(k,n) = 0.01*(iv%instid(inst)%info%dym(k,n)*( &
               iv%instid(inst)%info%dxm(k,n)*grid%pb(iv%instid(inst)%info%i(k,n),  iv%instid(inst)%info%j(k,n),k) + &
               iv%instid(inst)%info%dx(k,n) *grid%pb(iv%instid(inst)%info%i(k,n)+1,iv%instid(inst)%info%j(k,n),k)) + &
               iv%instid(inst)%info%dy(k,n) *( &
               iv%instid(inst)%info%dxm(k,n)*grid%pb(iv%instid(inst)%info%i(k,n),  iv%instid(inst)%info%j(k,n)+1,k) + &
               iv%instid(inst)%info%dx(k,n) *grid%pb(iv%instid(inst)%info%i(k,n)+1,iv%instid(inst)%info%j(k,n)+1,k)))
         end do
      end do
  do k = kts,kte
  !计算雨水流量
      a (k,n1:n2) = (iv%instid(inst)%ps(n1:n2)/iv_pb(k,n1:n2))**0.4          !计算订正因子
	  Vt(k,n1:n2) = 5.40*a(k,n1:n2)*(iv%instid(inst)%qcw(k,n1:n2))**0.125  !计算雨滴末速度
	  rho(k,n1:n2) = iv%instid(inst)%ps(k,n1:n2)/(287.05*iv%instid(inst)%t(k:,n1:n2)) !计算每层的空气总密度
	  rain(k,n1:n2) = rho(k,n1:n2)*Vt(k,n1:n2)*(iv%instid(inst)%qrn(k,n1:n2)/iv%instid(inst)%qrn(k,n1:n2)+1)

  !计算云覆盖
      do n=n1:n2
        if iv%instid(inst)%ps(k,n)/100 < 450 then
            rhc = 0.80
        elseif 450<=iv%instid(inst)%ps(k,n)/100 .and. iv%instid(inst)%ps(k,n)/100<800 then
            rhc = 0.60
        else
            rhc = 0.40
        end if
        if rh<rhc then
            fi(k,n) = 0
        else
            fi(k,n) = (iv%instid(inst)rh(k,n)-rhc/1-rhc)**2
        end if

      end do



