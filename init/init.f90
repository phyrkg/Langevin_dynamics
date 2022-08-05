!code to initialize the system
	program init
	use parameters
	!use bound_mod
	implicit none

	integer seed,i,j,k,l
	double precision r1,r2,r3,theta
	double precision minrelpos,dx,dy,dr,rel_disx, rel_disy, reldis
	!integer

	pi=4.d0*atan(1.d0)
	print*,"na=",na,"np=",np

	open(10,file="initstat.dat",status="unknown")
	allocate(act_ples(na,6))
	allocate(pass_ples(np,6))
	!1-xpos, 2-ypos, 3-xvelocity, 4-yvelocity, 5-orientation, 6-z_angularvelocity


pass_ples=0
call random_seed(seed)
if(np>=1)then
i=1
2000 do
	call random_number(r1)!0<=r1<1
	call random_number(r2)
	call random_number(r3)
	!theta = r1*2*pi
	pass_ples(i,1)=0.d0	!(sysrad*r2)*cos(theta)
	pass_ples(i,2)=0.d0	!(sysrad*r2)*sin(theta)
	pass_ples(i,5)=r3*2*pi			!orientation of the particle


	!dx=pass_ples(i,1)-centr
	!dy=pass_ples(i,2)-centr
	!dr =sqrt(dx*dx+dy*dy)

	!if(dr>(sysrad-1.2*rp))then
	!go to 2000
	!endif
	
	

	!check for passive-passive particles collisions
	!do j=1,i
	!	rel_disx=pass_ples(i,1)-pass_ples(j,2)
	!	rel_disy=pass_ples(i,2)-pass_ples(j,2)
	!	reldis = sqrt(rel_disx*rel_disx + rel_disy*rel_disy)

	!	minrelpos=rp*2.d0
	!	if(reldis>0.and.(reldis-minrelpos)<0)then
	!	goto 2000
	!	endif
	!enddo

		i=i+1
		write(*,*)i-1
		if(i>=(np+1))then
		exit
		endif
enddo
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



act_ples=0
call random_seed(seed)
if (na >= 1)then
i=1
1000 do
	call random_number(r1)!0<=r1<1
	call random_number(r2)
	call random_number(r3)
	!print*,r1,r2,r3
	theta = r1*2*pi
	act_ples(i,1)=(sysrad*r2)*cos(theta)
	act_ples(i,2)=(sysrad*r2)*sin(theta)
	act_ples(i,5)=r3*2*pi			!orientation of the particle

	!print*,"act_ples",act_ples(i,1),act_ples(i,2)


	dx=act_ples(i,1)-centr
	dy=act_ples(i,2)-centr
	dr= sqrt(dx*dx+dy*dy)

	!print*,"dr",dr

	if(dr>(sysrad-2*ra))then
	go to 1000
	endif

!check for active particles and passive particles collisions
minrelpos=ra+rp
	do j=1,i
		rel_disx=pass_ples(1,1)-act_ples(j,1)
		rel_disy=pass_ples(1,2)-act_ples(j,2)
		reldis = sqrt(rel_disx*rel_disx + rel_disy*rel_disy)

		if(reldis>0.and.(reldis-minrelpos)<0)then
		goto 1000
		endif
	enddo

!check for active particle-particle collisions
minrelpos=2.d0*ra
	do j=1,i-1
		rel_disx=act_ples(i,1)-act_ples(j,1)
		rel_disy=act_ples(i,2)-act_ples(j,2)
		reldis = sqrt(rel_disx*rel_disx + rel_disy*rel_disy)

		print*,"reldis",reldis

		if((reldis-minrelpos)<0.d0)then
		go to 1000
		endif
	enddo

		i=i+1
		write(*,*)i-1
		if(i>=(na+1))then
		exit
		endif
enddo
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




	open(10,file='configs_a.in',status='new')
	write(10,*)0.d0
	do i=1,na
	write(10,*)i,(act_ples(i,j),j=1,6)
	enddo
	close(10)
	open(10,file='configs_p.in',status='new')
	write(10,*)0.d0
	do  i=1,np
	write(10,*)i,(pass_ples(i,j),j=1,6)
	enddo
	close(10)
	end program init
