program pario
  use mpi_f08
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
     write(error_unit, *) 'Maximum number of tasks is 64!'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
     write(error_unit, *) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_reader()

  call ordered_print()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine single_reader()
    implicit none

    ! TODO: Implement a function that will read the data from a file so that
    !       a single process does the file io. Use rank WRITER_ID as the io rank

    open(10,file='spokesman.dat',access='stream')
    if (my_id == writer_id) then
      read(10) fullvector
      do i=1,ntasks-1
        call mpi_send(fullvector(i*localsize+1:(i+1)*localsize),localsize,mpi_real, &
          i,i,mpi_comm_world,rc)
      end do
    else
      call mpi_recv(localvector,localsize,mpi_real,0,my_id,mpi_comm_world, &
        mpi_status_ignore,rc)
    end if

    close(10)

  end subroutine single_reader

  subroutine ordered_print
    implicit none
    integer :: task

    do task = 0, ntasks-1
       if (my_id == task) then
          write(output_unit, '(A,I0,A)', advance='no') 'Task ', &
               & my_id, ' received:'
          do i = 1, localsize
             write(output_unit, '(I3)', advance='no') localvector(i)
          end do
          write(output_unit,*) ' '
       end if
       call mpi_barrier(MPI_COMM_WORLD, rc)
    end do

  end subroutine ordered_print


end program pario
