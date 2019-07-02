program multiple
    
    use omp_lib
    use mpi_f08
    implicit none

    integer :: omp_rank, mpi_rank, nthreads, nproc, buf, i, ttag, ierr
    integer :: provided, required=MPI_THREAD_MULTIPLE

    call mpi_init_thread(required,provided,ierr)
    call mpi_comm_rank(mpi_comm_world,mpi_rank,ierr)
    call mpi_comm_size(mpi_comm_world,nproc,ierr)

    !$omp parallel default(shared) private(omp_rank,ttag,i,buf)
    omp_rank = omp_get_thread_num()
    nthreads = omp_get_num_threads()
    ttag=2**10+omp_rank

    if (mpi_rank == 0) then
        do i=1,nproc-1
            call mpi_send(omp_rank,1,mpi_integer,i,ttag, &
                mpi_comm_world,ierr)
        end do
    else
        call mpi_recv(buf,1,mpi_integer,0,ttag, &
            mpi_comm_world,mpi_status_ignore,ierr)
        write(*,*) 'Rank ', mpi_rank, ', thread ', omp_rank, &
          & ' received ', buf
    end if
    !$omp end parallel

    call mpi_finalize(ierr)

end program multiple