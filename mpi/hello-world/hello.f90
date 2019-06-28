program hello
    use mpi_f08
    implicit none
    integer :: rank, size, rc

    call mpi_init(rc)
    call mpi_comm_rank(mpi_comm_world,rank,rc)
    call mpi_comm_size(mpi_comm_world,size,rc)

    if ( rank == 0 ) then
        write(*,*) 'Hello world from ', rank, 'There are', size, 'of us.'
    else
        write(*,*) 'Hello world from ', rank
    end if

    call mpi_finalize()

end program hello