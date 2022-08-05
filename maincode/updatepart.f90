!subroutine to update active particle positions, velocities, orientation and angular velocity
subroutine updatepart
  use parameters
  implicit none
  integer kk,jj,i,j,seed
  double precision etax,etay,gau2,gau1,gau3,itrel2,itrel3,itrel4

  !call random_seed(seed)
  itrel2 = 1/trel2
  itrel3 = 1/trel3
  itrel4 = 1/trel4
	

    do j=1,na
    !update positions of the particles (we will add the D_perp part later)

    !call random_number(gau1)!0<=r1<1
    !call random_number(gau2)
    !call random_number(gau3)

        call gauss(gau1,gau2)
        !if(j==5)then
        !print*,act_ples(j,3),Fxa(j),gau1
        !endif
        !1 is for active particle
        act_ples(j,1)=act_ples(j,1)+act_ples(j,3)*deltat  !xposition
        act_ples(j,2)=act_ples(j,2)+act_ples(j,4)*deltat  !yposition

        act_ples(j,3)=act_ples(j,3)*(1-deltat)+(vas*cos(act_ples(j,5)) + Fxas(j))*deltat+sqrt(2*Dts_a*deltat)*gau1  !xvelocity
        act_ples(j,4)=act_ples(j,4)*(1-deltat)+(vas*sin(act_ples(j,5)) + Fyas(j))*deltat+sqrt(2*Dts_a*deltat)*gau2  !yvelocity

	
	!print*,"gau1", j, gau1, gau2,Fxas(j),Fyas(j),deltat,act_ples(j,5),act_ples(j,6),itrel2,Dts

        !need to check about fsp
        call gauss(gau1,gau2)

        act_ples(j,5)=act_ples(j,5)+(act_ples(j,6))*deltat  !orientation
        act_ples(j,6)=act_ples(j,6)-act_ples(j,6)*itrel2*deltat+itrel2*(taus0+tauap(j))*deltat +itrel2*sqrt(2*Drs*deltat)*gau1 !angular_velocity

	!print*,"gau2", j, gau1, gau2,Fxas(j),Fyas(j),deltat,act_ples(j,5),act_ples(j,6),itrel2,gau1,Drs,taus0,tauap(j)
	!if(act_ples(j,5).ne.act_ples(j,5))then
	!stop
	!endif

        !vx(i,j,1) = vx(i-1,j,1)+(va*cos(phi(i-1,j,1) + Fx(j)+sqrt2d*gau(1))*deltat  !rg
        !vy(i,j,1) = vy(i-1,j,1)+(va*sin(phi(i-1,j,1) + Fy(j)+sqrt2d*gau(2))*deltat  !rg

        !x(i,j,1) = x(i-1,j,1) + vx(i-1,j,1)*deltat !Fx(i,kk)+sqrtD*gau(1) !rg
        !y(i,j,1) = y(i-1,j,1) + vy(i-1,j,1)*deltat !Fy(i,kk)+sqrtD*gau(2)  !rg

        !w(i,j,1) = w(i-1,j,1) + (js*taus0 + js*taus(i-1,1) + sqrt2d*gau(3))*deltat  !rg
        !phi(i,j,1) = phi(i-1,j,1) + w(i-1,j,1)*deltat      !rg

      enddo


  do j=1,np

        !call random_number(gau1)!0<=r1<1
        !call random_number(gau2)
        !call random_number(gau3)

        call gauss(gau1,gau2)

    pass_ples(j,1)=pass_ples(j,1)+pass_ples(j,3)*deltat  !xposition
    pass_ples(j,2)=pass_ples(j,2)+pass_ples(j,4)*deltat  !yposition

    pass_ples(j,3)=pass_ples(j,3)*(1-itrel3*deltat)+itrel3*Fxps(j)*deltat+itrel3*sqrt(2*Dts_p*deltat)*gau1  !xvelocity
    pass_ples(j,4)=pass_ples(j,4)*(1-itrel3*deltat)+itrel3*Fyps(j)*deltat+itrel3*sqrt(2*Dts_p*deltat)*gau2  !yvelocity

    call gauss(gau1,gau2)

    pass_ples(j,5)=pass_ples(j,5)+(pass_ples(j,6))*deltat  !orientation
    pass_ples(j,6)=pass_ples(j,6)-pass_ples(j,6)*itrel4*deltat+itrel4*taupp(j)*deltat+itrel4*sqrt(2*Drs*deltat)*gau1 !angular_velocity



        !2 is for Passive particle
        !vx(i,j,2) = vx(i-1,j,2) + (Fx(j)+sqrt2d*gau(1))*deltat  !rg
        !vy(i,j,2) = vy(i-1,j,2) + (Fy(j)+sqrt2d*gau(2))*deltat  !rg

         !x(i,j,2) = x(i-1,j,2) + vx(i-1,j,2)*deltat !Fx(i,kk)+sqrtD*gau(1) !rg
         !y(i,j,2) = y(i-1,j,2) + vy(i-1,j,2)*deltat !Fy(i,kk)+sqrtD*gau(2)  !rg

        !w(i,j,2) = w(i-1,j,2) + (js*taus(i-1,2) + sqrt2d*gau(3))*deltat  !rg
        !phi(i,j,2) = phi(i-1,j,2) + w(i-1,j,2)*deltat      !rg
  enddo

end subroutine updatepart
