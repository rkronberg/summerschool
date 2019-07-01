program vectorsum
  use omp_lib
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i,omp_rank

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,*) 'Arithmetic sum formula (exact):                  ', sumex

  sum = 0
  ! TODO: Parallelize the computation
  !$omp parallel do shared(vecA) private(i) reduction(+:sum)
  do i = 1, nx
     sum = sum + vecA(i)
  end do
  !$omp end parallel do

  write(*,*) 'Sum: ', sum

  sum = 0
  ! TODO: Parallelize the computation (critical)
  !$omp parallel shared(sum,vecA) private(i,psum,omp_rank)
  omp_rank=omp_get_thread_num()
  !$omp do
  psum=0
  do i = 1, nx
     psum = psum + vecA(i)
  end do
  !$omp end do

  write(*,*) 'Partial sum, thread', omp_rank, ':' , psum

  !$omp critical
  sum=sum+psum
  !$omp end critical
  !$omp end parallel

  write(*,*) 'Sum: ', sum

end program vectorsum
