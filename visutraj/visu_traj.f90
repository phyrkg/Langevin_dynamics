!to create the trajectory of center of the particle
!to create the trajectory of center of the particle
	program visu_traj
	use parameters
	use visu_mod
	implicit none

	integer i,j,k,locwhtm,boundary_ple,noofsteps
	integer, allocatable, dimension(:) :: preil_a,preil_p
	double precision tcor_a,tcor_p,pret(6),theta
	real pre1(3)

	write(*,*)"how namy steps?"
	read(*,*)noofsteps

	pi=4.d0*atan(1.d0)
	boundary_ple = int(pi*lbox/0.4d0)
	allocate(preil_a(na),preil_p(np))
	write(*,*)"If you want to make .xyz file for i_th run, print i. (i=1 for initially generated config)"
	read(*,*)locwhtm
	write(swhtm,'(i8)')locwhtm

	open(10,file='trajectout'//trim(adjustl(swhtm))//'.xyz',status='new')

	!open(20,file='../traject/trajects_a'//trim(adjustl(swhtm))//'.dat',status='old',form = 'unformatted')
	!open(30,file='../traject/trajects_p'//trim(adjustl(swhtm))//'.dat',status='old',form = 'unformatted')

	open(20,file='../traject/trajects_a'//trim(adjustl(swhtm))//'.dat',status='old',form='unformatted')
	open(30,file='../traject/trajects_p'//trim(adjustl(swhtm))//'.dat',status='old',form='unformatted')

	if(na>0)then
	allocate(act_ples(na,6))
	endif
	if(np>0)then
	allocate(pass_ples(np,6))
	endif


do k=1,noofsteps
	write(10,*)na+np+boundary_ple
	write(10,*)"atoms"

		!print*,"hello2"

			read(20)tcor_a
			do  i=1,na
				read(20)preil_a(i),(pret(j),j=1,6)
				print*,tcor_a,preil_a(i)

				!print*,"hello3"

				do j=1,6
					act_ples(i,j)=pret(j)
				enddo
			enddo
			do  i=1,na
				pre1(1)=amplifier*act_ples(i,1)
				pre1(2)=amplifier*act_ples(i,2)
				pre1(3)=0.0d0
				write(10,*)1,(pre1(j),j=1,3)
			enddo

	print*,"hello4"



			read(30)tcor_p
			do  i=1,np
				read(30)preil_p(i),(pret(j),j=1,6)
				do j=1,6
					pass_ples(i,j)=pret(j)
				enddo
			enddo
			do  i=1,np
				pre1(1)=amplifier*pass_ples(i,1)
				pre1(2)=amplifier*pass_ples(i,2)
				pre1(3)=0.0d0
				write(10,*)2,(pre1(j),j=1,3)
			enddo


		!boundary particle
		theta = 2*pi/boundary_ple
		do  i=1,boundary_ple
		pre1(1)=(lbox/2+ra)*cos(i*theta)
		pre1(2)=(lbox/2+ra)*sin(i*theta)
		pre1(3)=0.0d0
		write(10,*)3,(pre1(j),j=1,3)
		enddo


enddo

!creat script file for vmd
	open(40,file='vmdfmt.tcl',status='unknown')
	if(na>0)then
	write(40,*)'set sel [atomselect top "','atomicnumber',1,'"]'
	write(40,*)"$sel set radius", ra
	write(40,*)"$sel set name",1
	write(40,*)"color Name ",1,"silver"
	endif
	if(np>0)then
	write(40,*)'set sel [atomselect top "','atomicnumber',2,'"]'
	write(40,*)"$sel set radius",rp
	write(40,*)"$sel set name",2
	write(40,*)"color Name ",2,"red"
	endif
	write(40,*)'set sel [atomselect top "','atomicnumber',3,'"]'
	write(40,*)"$sel set radius",ra/2.d0
	write(40,*)"$sel set name",3
	write(40,*)"color Name ",3,"white"
	close(40)
end program visu_traj
