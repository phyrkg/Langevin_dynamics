!Code for simulating the dynamics of inertial passive particle in a medium of inertial active particles
!Method; particles are simulated using langevin dynamics with torque equation
program maincode
  use parameters
  implicit none

  integer nsteps,i,j,k,pleid,tb,ts
  double precision :: pret(6),clocktime1,clocktime2

  !calculate nsteps
	nsteps=int(totaltime/deltat)
	print*,"what is the value of whtm"
	read*,whtm

  write(swhtm,'(i8)')whtm

	
  !print*,"hello",swhtm

  !reading values of whtm and Tfin from the input file
  !Mersenne random number generator settings
  call mersenne

  !Memory allocation
  call allocate
  tauap=0.d0
  taupp=0.d0

  !open input files and read
  open(30,file='../init/configs_a.in',status='unknown')
  open(40,file='../init/configs_p.in',status='unknown')

  read(30,*)time_a
  do i=1,na
	read(30,*)pleid,(pret(j),j=1,6)
   do j=1,6
   act_ples(pleid,j)=pret(j)
   enddo
  enddo

  read(40,*)time_p
  do i=1,np
	read(40,*)pleid,(pret(j),j=1,6)
   do j=1,6
   pass_ples(pleid,j)=pret(j)
   enddo
  enddo

  !To print parameters to file
  open(10,file='parameters.dat',status='unknown')
  write(10,*)"na=",na
  write(10,*)"va=",va,"mm/sec"
  write(10,*)"ma=",ma,"g"
  write(10,*)"Ia=",Ia,"gmm^2"
  write(10,*)"np=",np
  write(10,*)"mp=",mp,"g"
  write(10,*)"Ip=",Ip,"gmm^2"
  write(10,*)"GAMMAT_a=",GAMMAT_a,"g/sec"
  write(10,*)"GAMMAT_p=",GAMMAT_p,"g/sec"
  write(10,*)"GAMMAR=",GAMMAR,"gmm^2/sec"

  write(10,*),""
  write(10,*)"l1=",l1,"mm"
  write(10,*)"t1=",t1,"sec"
  write(10,*)"t2=",t2,"sec"
  write(10,*)"t3=",t3,"sec"
  write(10,*)"t4=",t4,"sec"

  write(10,*),""
  write(10,*),"values in scaled unit"
  write(10,*)"vas=",vas
  write(10,*)"Dts_a=",Dts_a
  write(10,*)"Dts_p=",Dts_p
  write(10,*)"Drs=",Drs
  write(10,*)"lbox=",lbox
  write(10,*)"ra=",ra
  write(10,*)"rp=",rp
  write(10,*)"epsilon_sa=",epsilon_sa
  write(10,*)"epsilon_sp=",epsilon_sp
 close(10)

  !merged output file
  open(50,file='../traject/trajects_a'//trim(adjustl(swhtm))//'.dat',status='unknown',form='unformatted') 
  open(60,file='../traject/trajects_p'//trim(adjustl(swhtm))//'.dat',status='unknown',form='unformatted') 
  open(70,file='../traject/totaltimesteps'//trim(adjustl(swhtm))//'.dat',status='unknown')


  !open(50,file='../traject/trajects_a'//trim(adjustl(swhtm))//'.dat',status='unknown',form = 'unformatted')!//trim(adjustl(ctime))
  !open(60,file='../traject/trajects_p'//trim(adjustl(swhtm))//'.dat',status='unknown',form = 'unformatted')!//trim(adjustl(ctime))
  !end!merged output file

!interaction potential parameters
  sigma_aa = 2*ra
  sigma_ab = 2*ra
  sigma_ap = 2*sqrt(ra*rp)
  sigma_pb = 2*rp

      CALL CPU_TIME(clocktime1)

  !Print*,"hello"
  t_cor=0.0d0
  numtp=int(totaltime/print_ts)!timstp is time step for printing the data
  numtb=int(print_ts/deltat)

	do 200 tb=1,numtp
	   do 210 i=1,numtb

     	   

        !set values of Fx and Fy to zero.
        Fxas = 0.d0
        Fyas = 0.d0
        Fxps = 0.d0
        Fyps = 0.d0

        !time evolution of the system in deltat time and collision with each other and boundary
        call plebound_col
        call pleple_col
        call updatepart
        !do k=1,na
        !write(*,*)t_cor,(act_ples(k,j),j=1,6),k,"act"
        !enddo
        !write(*,*)t_cor,(pass_ples(k,j),j=1,6),k,"pass"

	!if(fxas(1)>0)then
	!print*,fxas(1),fyas(1)
	!endif

	
	!t_cor=t_cor+deltat
      	210 enddo

	t_cor=t_cor+deltat*numtb !to prevent error from accumulating keeping it outside 210 loop

     !Print*,"hello"

    !Write data to file
    write(50)t_cor
    write(60)t_cor

     do i=1,na
      !write(50)t_cor,(act_ples(i,j),j=1,6)
      write(50)i,(act_ples(i,j),j=1,6)
     enddo

     do i=1,np
      !write(60)t_cor,(pass_ples(i,j),j=1,6)
      write(60)i,(pass_ples(i,j),j=1,6)
     enddo
      !call printdata
      !Print*,"hello"
     write(70,*)tb
     !print*,"Print time step", tb

  200 enddo

      CALL CPU_TIME(clocktime2)
      print*,"Total run time=", clocktime2-clocktime1, "sec."

end program maincode
