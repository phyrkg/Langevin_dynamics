!detect particle and circular boundary collisions
	subroutine plebound_col
	use parameters
	implicit none


	double precision dx,dy,dr,dr1,ld1,ld2,ld3,ld4,relpos
	double precision relvel,langle,theta,rmin
	integer k,i,liangle,liangle1,j

	rmin = 2**0.1666*sigma_ab
	rmin = cutoff*rmin
!for active particle
do j=1,na
	dx=act_ples(j,1)-centr
	dy=act_ples(j,2)-centr
	dr=sqrt(dx*dx+dy*dy)
	dr = sysrad+ra-dr				!actual dr, considering boundary to be made of same type of particles


	if(dr<rmin)then
		!print*,"hello ", "collision"
		theta =atan2(dy,dx)
		if(dy<0)then
		theta = 2*pi +theta
		endif
		dx = dr*cos(theta)	!considering boundary to be made of same type of particles
		dy = dr*sin(theta)
		!print*,"fxs", fxas(j),dx,dy,j
		call forceupdate_act_bound(dx,dy,j)
		!print*,"fxs", fxas(j),j
	endif

enddo

rmin = 2**0.1666*sigma_pb
rmin = cutoff*rmin

!for passive particle
do j=1,np
	dx=pass_ples(j,1)-centr
	dy=pass_ples(j,2)-centr
	dr=sqrt(dx*dx+dy*dy)
	dr = sysrad+rp-dr

	if(dr<rmin)then !to stop the code when particle hit the boundary
	!print*,dr
	stop
	endif

	if(dr<rmin)then
		theta =atan2(dy,dx)
		if(dy<0)then
		theta = 2*pi +theta
		endif
		dx = dr*cos(theta)	!considering boundary to be made of same type of particles
		dy = dr*sin(theta)
		call forceupdate_pass_bound(dx,dy,j)
	endif
enddo


!if((dr+0.5*rmin)>sysrad)then
	!theta =atan2(dy,dx)
	!if(dy<0)then
	!theta = 2*pi +theta
	!endif
	!dx = (sysrad+rmin-dr)*cos(theta)	!considering boundary to be made of same type of particles
	!dy = (sysrad+rmin-dr)*sin(theta)

end subroutine plebound_col
