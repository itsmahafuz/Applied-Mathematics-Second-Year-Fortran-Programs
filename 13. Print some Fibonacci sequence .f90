! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

! Write a Fortran program to print some Fibonacci sequence (i.e. n < 20)

program fibonacci
    implicit none
    integer, allocatable, dimension(:) :: fib  ! Array to hold the Fibonacci series
    integer :: i, n  ! Loop index and number of terms

    ! Prompt user for the value of n
    print *, "Enter the value of n:"
    read *, n

    ! Allocate the array to hold the Fibonacci series
    allocate(fib(n))

    ! Initialize the first two terms
    fib(1) = 0
    fib(2) = 1

    ! Generate the Fibonacci series
    do i = 3, n
        fib(i) = fib(i-1) + fib(i-2)
    end do

    ! Print the Fibonacci series
    print *, "Fibonacci series is:"
    do i = 1, n
        print *, fib(i)
    end do

    ! Deallocate the array
    deallocate(fib)
end program fibonacci
