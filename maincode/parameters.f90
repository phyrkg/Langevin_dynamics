	module parameters
	implicit none

!total no of time coordinates
	integer tottimcor

!length is scaled by the length scale l1 = V_a*M_a/GAMMAT = 13.6236539mm
!time is scaled by the time scale t1 = M_a/GAMMAT = 0.1702956743sec

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!All the parameters are in cgs unit
	!Experimental parameters
  	double precision, parameter :: GAMMAT_a = 4.873876	!(In g/sec)(also called \xi)
  	double precision, parameter :: GAMMAT_p = 45.73876	!(In g/sec)(also called \xi)
  	double precision, parameter :: GAMMAR = 183.651829	!(In gmm^2/sec)(also called \xi_r)
  	double precision, parameter :: DIFT_a = 78.709824 	!(In mm^2/sec)
  	double precision, parameter :: DIFT_p = 1.4      	!(In mm^2/sec)
  	double precision, parameter :: DIFR = 1.92103193	!(In rad^2/sec)
  	double precision, parameter :: epsilon = 158.41 	!(In gm/sec^2)

  	double precision, parameter :: va = 75			!(In mm/sec)
  	double precision, parameter :: ma = 0.83		!(In g)
  	double precision, parameter :: Ia = 31  		!(In gmm^2)
!type p1
  	double precision, parameter :: mp = 3.25		!(In g)
  	double precision, parameter :: Ip = 341 		!(In gmm^2)
!type p4
  	!double precision, parameter :: mp = 2.26		!(In g)
  	!double precision, parameter :: Ip = 259		!(In gmm^2)
!type p5
  	!double precision, parameter :: mp = 1.60		!(In g)
  	!double precision, parameter :: Ip = 232		!(In gmm^2)





	!one unit length in mm
	double precision, parameter::  l1=(va*ma/GAMMAT_a) 		!In mm, scaling length(l1 = V_a*M_a/GAMMAT)

  	double precision, parameter :: t1 = (ma/GAMMAT_a)		!(scaling time scale, t1 = ma/GAMMAT, in sec )	
  	double precision, parameter :: t2 = Ia/GAMMAR			!(time scale t2 = Ia/GAMMAR, in sec )	
  	double precision, parameter :: t3 = (mp/GAMMAT_p)		!(time scale t3 = mp/GAMMAT, in sec )	
  	double precision, parameter :: t4 = Ip/GAMMAR			!(time scale t4 = Ip/GAMMAR, in sec )	

	double precision, parameter :: trel2 = t2/t1       		!ratio of t2 and t1(t2/t1)
	double precision, parameter :: trel3 = t3/t1       		!ratio of t3 and t1(t3/t1)
	double precision, parameter :: trel4 = t4/t1       		!ratio of t4 and t1(t4/t1)




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!Parameters in scaled unit(length is scaled by l1 and time by t1)
	double precision, parameter :: vas = va*t1/l1       		!scaled active velocity(because vas = va*t1/l1)
	double precision, parameter :: Dts_a = DIFT_a*t1/l1**2     		!scaled trans diffusion constant (Dts = DIFT*t1/l1^2)
	double precision, parameter :: Dts_p = DIFT_p*t1/l1**2     		!scaled trans diffusion constant (Dts = DIFT*t1/l1^2)
	double precision, parameter :: Drs = DIFR*t1        		!scaled rot diffusion constant (Drs = DIFR*t1)
	!double precision, parameter :: fis=fi*fsp		     	!scaled interaction force(fsp=t1/(\xi*l1))
	!do not need this as force is directly calculated in scale unit.

        !epsilon_scaled = epsilon*t1/(l1*Gammat)
	double precision, parameter :: epsilon_sa = epsilon*t1/(l1*GAMMAT_a)       		!LJ energy constant in scaled unit
	double precision, parameter :: epsilon_sp = epsilon*t1/(l1*GAMMAT_p)       		!LJ energy constant in scaled unit
	double precision :: sigma_aa,sigma_ap, sigma_ab, sigma_pb


	double precision, parameter :: taus0 = 0.0d0       		!scaled active torque
	!double precision, parameter :: taui = 0.0d0       		!scaled interaction torque
	!double precision, parameter :: tauis=taui*jsp		     	!scaled interaction torque(jsp=t1^2/J) again don't need this

	!double precision, parameter :: etat_s =etat*sqrt(t1)   	!scaled trans noise
	!double precision, parameter :: etar_s = etar*sqrt(t1)      	!scaled rot noise
	double precision, parameter :: sqrtt1 =  sqrt(t1)




	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 	double precision, parameter ::  lbox=300/l1 !In simulation unit, 300mm

	!time step for the simulation
	double precision, parameter ::  deltat=0.000500000000d0	!trial basis

	!time step to write the trajectory in the output file
	double precision, parameter ::  print_ts=0.24d0

	!total time duration for simulation
	double precision, parameter ::totaltime=100.0d0


	!pi radian pi=4*atan(1.d0)
	double precision ::  pi

	!presenttime time of present time
	double precision t_cor


	!some characters
	character xcor*10,ycor*10,ctime*10

	!which time you are running the job
	integer whtm1,intag!whtm+1
	character swhtm*20,swhtm1*20,ctaskid*20

	!to be modified before each run
	integer :: whtm



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!Parameters for active Particle
	!Total number of active particles
		integer :: na=114
		double precision :: ra=7.5d0/l1		!In scale unit l1, value in real unit=7.5mm
		double precision :: time_a
		double precision, allocatable, dimension(:,:) :: act_ples
		double precision, allocatable, dimension(:) :: Fxas,Fyas,tauap
		!double precision, allocatable, dimension(:) :: taua
		integer, allocatable, dimension(:,:) :: preis_a
	!area fraction for the particle _1
		double precision :: area_a=0.020d0
		double precision :: part_area_a!are of active particle
	!area state
		integer, parameter :: area_state_a=2!area_state_a=2 if want to take specified area fractions instead of no of particles



	!Parameters for Passive Particle
	!Total number of passive spherical particles
		integer :: np=1!, parameter
		double precision :: rp=15.d0/l1		!In scale unit l1,value in real unit=15mm
		double precision :: time_p
		double precision, allocatable, dimension(:,:) :: pass_ples
		double precision, allocatable, dimension(:) :: Fxps,Fyps,taupp
		integer, allocatable, dimension(:,:) :: preis_p
	!area fraction for the beads
		double precision,parameter :: area_p=0.02d0
		double precision :: part_area_p!  area of passive beads
	!area state
		integer, parameter :: area_state_p=2!area_states=2 if want to take specified area fractions instead of no of particles

	!density ratio of passive to active particle
	!double precision, parameter :: densrtos=1.0d0



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!common parameters
	integer maxpart,numtp,numtb
	double precision, parameter :: cutoff=0.85d0 	!Force cut-off value for ple_ple collision
	double precision, parameter :: centr=0.0d0
	double precision, parameter ::sysrad = lbox/2.d0
	double precision, allocatable, dimension(:,:,:)::ples
	!double precision,parameter :: dtp=0.02d0


end module parameters
