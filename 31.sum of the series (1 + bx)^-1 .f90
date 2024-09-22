! This program calculates the sum of the series (1 + bx)^-1 using its Taylor series expansion.
! The series is valid when |x| <= 1/|b|.
! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sumseries
    implicit none
    real :: sm, x, s, b, y
    integer :: si, n

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of x, b, and n
        print*, "Enter the value of x, b, and n:"
        read*, x, b, n

        ! Calculate the threshold value for x
        y = 1 / abs(b)
        if (abs(x) <= y) then
            ! Calculate the sum of the series
            s = sm(x, b, n)

            ! Print the result
            print 5, s
            5 format(2x, "The sum of the series is:", f8.3)
        else
            print*, "Undefined."
        end if
    end do
end program

! Function to calculate the sum of the series (1 + bx)^-1 using its Taylor series expansion
real function sm(x, b, n)
    implicit none
    real, intent(in) :: x, b
    integer, intent(in) :: n
    integer :: i
    sm = 0.0

    ! Loop to calculate the sum
    do i = 1, n
        sm = sm + (-1)**(i + 1) * (b * x)**(i - 1)
    end do
end function
