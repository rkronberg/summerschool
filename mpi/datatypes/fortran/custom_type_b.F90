program datatype1
  use mpi_f08
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for datatype
  type(mpi_datatype) :: indexedtype
  integer :: i, j
  type(mpi_status) :: status
  integer, dimension(4) :: displs,blocklens


  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  !TODO: create datatype describing one row, use mpi_type_vector

  displs=[0,17,34,51]
  blocklens=[1,2,3,4]

  call mpi_type_indexed(4,blocklens,displs,mpi_integer,indexedtype,ierr)
  call mpi_type_commit(indexedtype,ierr)

  !TODO: send first row of matrix from rank 0 to 1

  if (rank == 0) then
    call mpi_send(array,1,indexedtype,rank+1,rank+1,MPI_COMM_WORLD,ierr)
  else
    call mpi_recv(array,1,indexedtype,rank-1,rank,MPI_COMM_WORLD,status,ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype

  call mpi_type_free(indexedtype,ierr)

  call mpi_finalize(ierr)

end program datatype1
