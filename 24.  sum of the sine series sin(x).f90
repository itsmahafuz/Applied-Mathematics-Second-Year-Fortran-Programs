! Program to calculate the sum of the sine series sin(x) using its Taylor series expansion

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sumsin
    implicit none
    real :: s, sm, x
    real, parameter :: pi = 3.1416
    integer :: si, n

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the value of x in degrees
        print*, "Enter the value of x (in degrees):"
        read*, x
        x = x * (pi / 180)  ! Convert degrees to radians

        ! Prompt user to enter the value of n
        print*, "Enter the value of n:"
        read*, n

        ! Calculate the sum of the sine series
        s = sm(x, n)

        ! Print the result
        print 5, s
        5 format(2x, "The sum of the sin(x) series is:", f12.5)
    end do
end program

! Function to calculate the sum of the sine series sin(x) using its Taylor series expansion
real function sm(x, n)
    implicit none
    real, intent(in) :: x
    integer, intent(in) :: n
    integer :: i
    real :: term, factorial
    sm = x
    factorial = 1.0

    ! Loop to calculate the sum
    do i = 1, n-1
        factorial = factorial * (2 * i) * (2 * i + 1)  ! Calculate factorial
        term = ((-1)**i * x**(2 * i + 1)) / factorial  ! Calculate term
        sm = sm + term  ! Add term to sum
    end do
end function
