!particle-particle collision
	subroutine pleple_col
	use parameters
	implicit none

	integer i,j,k
	double precision dx,dy,dr,rmin

	rmin = 2**0.1666*sigma_aa
	rmin = cutoff*rmin

	!for active-active particle collisions
		do j=1,na-1
			do k=j+1,na		!because we do not want to count collision multiple times
				dx=act_ples(j,1)-act_ples(k,1)
				dy=act_ples(j,2)-act_ples(k,2)
				dr=sqrt(dx*dx+dy*dy)
				if(dr<rmin)then
					call forceupdate_aa(dx,dy,j,k)
					!print*,"dr",dr,j,k,act_ples(1,j),act_ples(1,k)

				endif
			enddo
		enddo

		rmin = 2**0.1666*sigma_ap
		rmin = cutoff*rmin

	!for active-passive particle collisions
	do j=1,na
		do k=1,np
			dx=act_ples(j,1)-pass_ples(k,1)
			dy=act_ples(j,2)-pass_ples(k,2)
			dr=sqrt(dx*dx+dy*dy)
			if(dr<rmin)then
				call forceupdate_ap(dx,dy,j,k)
			endif
		enddo
	enddo

	!for passive-passive particles collision not considerd because we have one passive particle only
end subroutine pleple_col
