! Program to calculate the sum of the tangent series tan(x) using its Taylor series expansion

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program tanxx
    implicit none
    real :: tas, ci, x, si
    integer :: n, i

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, i
        if (i == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of x and n
        print*, "Enter the value of x (in degrees) and n:"
        read*, x, n
        x = 3.1416 * (x / 180)  ! Convert degrees to radians
        n = n - 1  ! Adjust n for the loop

        ! Check if n is valid
        if (n <= 0) then
            print*, "The sum of the series is = 0"
        else
            ! Calculate the sum of the tangent series
            tas = si(x, n) / ci(x, n)

            ! Print the result
            print 5, tas
5          format(2x, "The sum of the tan(x) series is:", f18.6)
        end if
    end do
end program

! Function to calculate the sum of the sine series sin(x) using its Taylor series expansion
real function si(x, n)
    implicit none
    real, intent(in) :: x
    real :: ft
    integer, intent(in) :: n
    integer :: i
    ft = 1.0
    si = x

    ! Loop to calculate the sum
    do i = 1, n
        ft = ft * (2 * i + 1) * (2 * i)  ! Calculate factorial
        si = si + ((-1)**i * x**(2 * i + 1)) / ft  ! Add term to sum
    end do
end function

! Function to calculate the sum of the cosine series cos(x) using its Taylor series expansion
real function ci(x, n)
    implicit none
    real, intent(in) :: x
    real :: ft
    integer, intent(in) :: n
    integer :: i
    ft = 1.0
    ci = 1.0

    ! Loop to calculate the sum
    do i = 1, n
        ft = ft * (2 * i) * (2 * i - 1)  ! Calculate factorial
        ci = ci + ((-1)**i * x**(2 * i)) / ft  ! Add term to sum
    end do
end function

