!Write a Fortran program to calculate the value of y from the following using function.

! This program calculates the value of y based on the input x.
! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program finvalue
    implicit none
    real :: y, x
    integer :: si

    do
        ! Prompt user to enter serial number
        print*, "Serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the value of x
        print*, "Enter the value of x:"
        read*, x

        ! Calculate and print the value of y
        print 5, y(x)
        5 format(2x, "The value of y is:", f8.3)
    end do
end program

! Function to calculate the value of y based on x
real function y(x)
    implicit none
    real, intent(in) :: x

    if (x < 2) then
        y = 2 * x**2 + 3 * x + 4
    else if (x == 2) then
        y = 0
    else
        y = 2 * x**2 + 3 * x - 4
    end if
end function
