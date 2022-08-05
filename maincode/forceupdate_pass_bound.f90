!for jj=1: dt*(rhs of particle-equations without noise part)
!for jj=2: dt*(rhs of particle-equations without noise part)/2
!kk prints output in arrays Fx(:,kk) and Fy(:,kk)

subroutine forceupdate_pass_bound(dx,dy,ii)
  use parameters
  implicit none
  integer ii,kk,jj
  double precision rxy2,irxy2,irxy4,irxy6,irxy8,fxdum,fydum,dx,dy,rxy7mrxy4


    rxy2 = dx*dx + dy*dy

     irxy2=1.d0/rxy2
     irxy4 = irxy2*irxy2
     irxy6 = irxy4*irxy2
     irxy8 = irxy4*irxy4
     !irxy14 =rxy8*rxy4*irxy2

     !rxy7mrxy4=rxy7 - rxy4
     fxdum = (24*epsilon_sp*dx*sigma_pb**6)*irxy8*(2*irxy6*sigma_pb**6-1)  !dx*rxy7mrxy4
     fydum = (24*epsilon_sp*dy*sigma_pb**6)*irxy8*(2*irxy6*sigma_pb**6-1)   !dy*rxy7mrxy4
     Fxps(ii) = Fxps(ii) - fxdum
     Fyps(ii) = Fyps(ii) - fydum
     !print*,fxas(ii),fyas(ii),"pb"
	
end subroutine forceupdate_pass_bound
