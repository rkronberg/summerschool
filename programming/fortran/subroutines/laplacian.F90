module laplacian_mod
  implicit none
  real, parameter :: dx = 0.05, dy = 0.05
  integer :: i,j
  real :: x,y


contains

  subroutine initialize(field0)
    ! TODO: implement a subroutine that initializes the input array
    real :: field0(:,:)
    y=0.0
    do j=1,size(field0,2)
    x=0.0
      do i=1,size(field0,1)
        field0(i,j)=x**2+y**2
        x=x+dx
      end do
      y=y+dy
    end do
  end subroutine initialize

  subroutine laplacian(curr, prev)
    real :: curr(:,:), prev(:,:)
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
    do j=2,size(curr,2)-1
      do i=2,size(curr,1)-1
        curr(i,j)=(prev(i-1,j)-2*prev(i,j)+prev(i+1,j))/(dx**2)+(prev(i,j-1)-2*prev(i,j)+prev(i,j+1))/(dy**2)
      end do
    end do
  end subroutine laplacian

  subroutine write_field(array)
    real :: array(:,:)
    ! TODO: write a subroutine that prints "array" on screen
    write(*,*) 'Array is:'
    do i=1,size(array,1)
      write(*, '(12F6.2)') array(i,:)
    end do
  end subroutine write_field

end module laplacian_mod
