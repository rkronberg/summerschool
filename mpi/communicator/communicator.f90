program commu
    use mpi_f08
    implicit none

    integer :: ntasks, rank, ierr, i, color
    integer, parameter :: n_mpi_tasks = 4
    integer, dimension(2*n_mpi_tasks) :: sendbuf, recvbuf
    integer, dimension(2*n_mpi_tasks**2) :: printbuf
    type(mpi_comm) :: subcomm

    call mpi_init(ierr)
    call mpi_comm_size(MPI_COMM_WORLD, ntasks, ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

    call init_buffers
    call print_buffers(sendbuf)

    if (rank < 2) then
        color=1
    else
        color=2
    end if

    call mpi_comm_split(MPI_COMM_WORLD,color,rank,subcomm)
    call mpi_comm_rank(subcomm,color)

    call mpi_reduce(sendbuf,recvbuf,2*n_mpi_tasks,MPI_INTEGER,mpi_sum,0,subcomm)

    call print_buffers(recvbuf)

    call mpi_finalize(ierr)

    contains
    
    subroutine init_buffers
      implicit none
      integer :: i
          do i = 1, 2*n_mpi_tasks
         recvbuf(i) = -1
         sendbuf(i) = i + 2*n_mpi_tasks * rank - 1
      end do
    end subroutine init_buffers

    subroutine print_buffers(buffer)
      implicit none
      integer, dimension(:), intent(in) :: buffer
      integer, parameter :: bufsize = 2*n_mpi_tasks
      integer :: i
      character(len=40) :: pformat
  
      write(pformat,'(A,I3,A)') '(A4,I2,":",', bufsize, 'I3)'
  
      call mpi_gather(buffer, bufsize, MPI_INTEGER, &
           & printbuf, bufsize, MPI_INTEGER, &
           & 0, MPI_COMM_WORLD, ierr)
  
      if (rank == 0) then
         do i = 1, ntasks
            write(*,pformat) 'Task', i - 1, printbuf((i-1)*bufsize+1:i*bufsize)
         end do
         print *
      end if
    end subroutine print_buffers

end program commu
