!Calculating velocity auto correlation function.
	program vel_cor
	use parameters
	implicit none

	integer noofsteps,i,j,k,locint,locwhtm,preil_p,ndt,count,averaging_windows,gap
	!integer, allocatable, dimension(:) :: preil_1,preils!,presx,presy
	double precision dt,pret(6),vx0,vy0
	real pre1(3)

	double precision, allocatable, dimension(:) :: tcor_p,vc
	double precision, allocatable, dimension(:,:) :: press


	pi=4.d0*atan(1.d0)


	write(*,*)"how namy steps?"
	read(*,*)noofsteps


	write(*,*)"If you want to print output file for i_th run, print i"
	read(*,*)locwhtm
	write(swhtm,'(i8)')locwhtm

	open(10,file='velcor'//trim(adjustl(swhtm))//'.dat',status='replace')
	open(30,file='../traject/trajects_p'//trim(adjustl(swhtm))//'.dat',status='old')
	!open(40,file='../traject/trajects_p_nopbc'//trim(adjustl(swhtm))//'.dat',status='old')


	allocate(tcor_p(noofsteps))
	allocate(press(noofsteps,2))



	do i=1,noofsteps
	read(30,*)tcor_p(i)
	read(30,*)preil_p,(pret(j),j=1,6)
		press(i,1)=pret(3)		!vx velocity
		press(i,2)=pret(4)		!vy velocity
	enddo
	close(30)


!velcor=0.d0
!ncount=0
ndt=200        !max tau time
gap=200	        !differance between two trajectroy cut for averaging
!when gap = ndt menas you are taking non overlapping trajectory cut
averaging_windows=int((noofsteps-ndt)/gap)-1

allocate(vc(ndt))
count=0
do i=1,averaging_windows		!tau or delta t

	vx0 = press(gap*count+1,1)
	vy0 = press(gap*count+1,2)

	do j=2,ndt
	vc(j-1) = vx0*press(gap*count+j,1) + vy0*press(gap*count+j,2)
	enddo

	count = count+1
enddo


dt=tcor_p(2)-tcor_p(1)
do i=1,ndt-1
	 vc(i) = vc(i)/averaging_windows
	write(10,*)i*dt*t1, vc*l1*l1/(t1*t1)	
enddo


	!do i=1,noofsteps-1
	!	
	!	if(mod(i,10)==0)then
                !ncount = ncount+1

	 !       vc=0.d0
	  !      do j=1,noofsteps-i
          !      vc = vc + press(j,1)*press(j+i,1) + press(j,2)*press(j+i,2) 
         !       enddo
                !velcor(ncount) = velcor(ncount)+vc/(noofsteps-i)
         !   	vc = vc/(noofsteps-i)
	!	dt=tcor_p(i+1)-tcor_p(1)
                
	!	write(10,*)dt*t1, vc*l1*l1/(t1*t1)
        !        endif
	!enddo
	

end program vel_cor
