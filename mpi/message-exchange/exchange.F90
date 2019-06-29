program exchange
  use mpi_f08
  implicit none
  integer, parameter :: msgsize = 100000
  integer :: rc, myid, ntasks
  type(mpi_status) :: status
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)

  call mpi_init(rc)
  call mpi_comm_rank(mpi_comm_world, myid, rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)

  message = myid

  ! TODO: Implement sending and receiving as defined in the assignment

  if (myid == 0) then
    call mpi_send(message,msgsize,mpi_integer,1,10,mpi_comm_world,rc)
    call mpi_recv(receiveBuffer,msgsize,mpi_integer,1,11,mpi_comm_world,status,rc)
    write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  else if (myid == 1) then
    call mpi_recv(receiveBuffer,msgsize,mpi_integer,0,10,mpi_comm_world,status,rc)
    call mpi_send(message,msgsize,mpi_integer,0,11,mpi_comm_world,rc)
    write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  end if

  call mpi_finalize(rc)

end program exchange
