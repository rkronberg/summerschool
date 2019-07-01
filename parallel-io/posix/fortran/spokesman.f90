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
     write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_writer()

  deallocate(localvector)

  call mpi_finalize(rc)

contains

  subroutine single_writer()
    implicit none

    character (len=18) :: filename

    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io

!    if (my_id == writer_id) then
!      fullvector(1:localsize) = localvector
!      do i=1,ntasks-1
!        call mpi_recv(fullvector(i*localsize+1),localsize,mpi_real,i, &
!          i,mpi_comm_world,mpi_status_ignore,rc)
!      end do
!
!      open(11,file='spokesman.dat',access='stream')
!      write(11) fullvector
!      close(11)
!
!    else
!      call mpi_send(localvector,localsize,mpi_real,0,my_id,mpi_comm_world,rc)
!    end if

!    b)

    write(filename,'(a,i2.2,a)') 'spokesman_id', my_id, '.dat'
    write(*,*) filename
    open(11,file=filename,access='stream')
    write(11) localvector
    close(11)

  end subroutine single_writer

end program pario
