program hello
  use omp_lib
  use mpi_f08
  implicit none
  integer :: my_id, tid, rc
  integer :: provided, required=MPI_THREAD_FUNNELED

  ! TODO: Initialize MPI with thread support.

  call mpi_init_thread(required,provided,rc)
  call mpi_comm_rank(mpi_comm_world,my_id)
  
  !$omp parallel private(tid)
  tid=omp_get_thread_num()
  write(*,*) 'Im thread ', tid, 'in process ', my_id
  !$omp end parallel

  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.

  ! TODO: Investigate the provided thread support level.

  call MPI_Finalize(rc)
end program hello
