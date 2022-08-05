!Calculating mean square displacement(msd) by averaging from many trajectories
	program msd_analysis
	use parameters
	implicit none

	integer nruns,i,j,k,locint,locwhtm,preil_p,ndt,nn_max,nn_min
	integer, allocatable, dimension(:) :: nn,count	!,preil_1,preils!,presx,presy
	double precision dx,dy,dr2,msd,dt,pret(6)
	real pre1(3)

	double precision, allocatable, dimension(:) :: tcor_p,dx2_avg,dy2_avg,dr2_avg
	double precision, allocatable, dimension(:,:) :: press


	pi=4.d0*atan(1.d0)

	write(*,*)"total number of runs"
	read(*,*)nruns
	allocate(nn(nruns))

	nn_min=10000
	nn_max=0
	open(40,file='noofsteps.dat',status='old')

	do i=1,nruns
	read(40,*)nn(i)
	if(nn_min>nn(i))then
	nn_min=nn(i)
	endif
	if(nn_max<nn(i))then
	nn_max=nn(i)
	endif	
	enddo

	allocate(count(nn_max))
	allocate(tcor_p(nn_max))
	allocate(press(nn_max,nn_max))
	allocate(dx2_avg(nn_max),dy2_avg(nn_max),dr2_avg(nn_max))
	

	open(10,file='msd_final.dat',status='replace')
	!open(20,file='../traject/trajects_a'//trim(adjustl(swhtm))//'.dat',status='old',form = 'unformatted')

	

	!dx2_avg=0.0
	!dy2_avg=0.0
	dr2_avg=0.0
	
count=0	
do i=1,nruns
	locwhtm=i
	write(swhtm,'(i8)')locwhtm

	!open(20,file='../traject/trajects_a'//trim(adjustl(swhtm))//'.dat',status='old')
	open(30,file='../traject/trajects_p'//trim(adjustl(swhtm))//'.dat',status='old',form='unformatted') 


	
	read(30)tcor_p(1)
	read(30)preil_p,(pret(j),j=1,6)
	press(1,1)=pret(1)
	press(1,2)=pret(2)
	
	do j=2,nn(i)
	read(30)tcor_p(j)
	read(30)preil_p,(pret(k),k=1,6)
	dx = pret(1)-press(1,1)
	dy = pret(2)-press(1,2)

	!dx2_avg(i)=dx2_avg(i)+dx*dx
	!dy2_avg(i)=dy2_avg(i)+dy*dy
	dr2_avg(j) = dr2_avg(j)+dx*dx+dy*dy 
	count(j) = count(j)+1
	enddo

enddo
	

	do i=2,nn_max
	dt=tcor_p(i)-tcor_p(1)
	msd = dr2_avg(i)/count(i)
	write(10,*)dt*t1, msd*l1*l1	!,dx2_avg-dx_avg**2 +dy2_avg-dy_avg**2
	enddo

end program msd_analysis
