! Program to calculate the sum of the cosine series cos(x) using its Taylor series expansion

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program cosxx
    implicit none
    real :: x, sm
    integer :: si, n

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the value of x in degrees and n
        print*, "Enter the value of x (in degrees) and n:"
        read*, x, n
        x = 3.1416 * (x / 180)  ! Convert degrees to radians

        ! Calculate the sum of the cosine series
        print 5, sm(x, n)
        5 format(2x, "The sum of the cos(x) series is:", f12.3)
    end do
end program

! Function to calculate the sum of the cosine series cos(x) using its Taylor series expansion
real function sm(x, n)
    implicit none
    real, intent(in) :: x
    real :: ft
    integer :: i, n
    n = n - 1
    sm = 1.0
    ft = 1.0

    ! Loop to calculate the sum
    do i = 1, n
        ft = ft * (2 * i) * (2 * i - 1)  ! Calculate factorial
        sm = sm + ((-1)**i * x**(2 * i)) / ft  ! Add term to sum
    end do
end function

