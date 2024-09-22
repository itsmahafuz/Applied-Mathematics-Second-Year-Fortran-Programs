! Program to calculate the sum of the series (1 + x)^-1 using its Taylor series expansion

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sumseries
    implicit none
    real :: sm, x, s
    integer :: si, n

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of x and n
        print*, "Enter the value of x and n:"
        read*, x, n

        ! Check if x is within the valid range
        if (abs(x) <= 1) then
            ! Calculate the sum of the series
            s = sm(x, n)

            ! Print the result
            print 5, s
            5 format(2x, "The sum of the series is:", f8.3)
        else
            print*, "Undefined."
        end if
    end do
end program

! Function to calculate the sum of the series (1 + x)^-1 using its Taylor series expansion
real function sm(x, n)
    implicit none
    real, intent(in) :: x
    integer, intent(in) :: n
    integer :: i
    sm = 0.0

    ! Loop to calculate the sum
    do i = 1, n
        sm = sm + x**(i - 1)
    end do
end function
