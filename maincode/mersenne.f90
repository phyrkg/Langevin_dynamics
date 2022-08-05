!Mersenne random number generator settings
subroutine mersenne
  use parameters
  use mt_stream
  implicit none
  integer i,ok
  integer :: iseeda(4) = (/ Z'123', Z'234', Z'345', Z'456' /)!in general one can use this array seed for random no generator
  ALLOCATE(mts(0:1), STAT = ok)!some no which tells state of the streams of the random numbers. For the first stream id is 0 and nprocs-1 for the nprocs_th stream.

  call set_mt19937! dont think of this
  call new(mts(0))! dont think of this
  !call init(mts,iseed)  ! init by scalar one can use this also i dont know about it
  call init(mts(0),iseeda)  ! init by array
  !call print(mts(0))
  !do i=1,nprocs-1
     call create_stream(mts(0),mts(1),whtm)! to generate state of different streams
     print*,"whtm",whtm
  !enddo

end subroutine mersenne
