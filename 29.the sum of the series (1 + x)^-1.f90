! Program to calculate the sum of the series (1 + x)^-1 using its Taylor series expansion
! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program inverse_series
    implicit none
    real :: x, sum
    integer :: n, si

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of x and n
        print*, "Enter the value of x and n:"
        read*, x, n

        ! Check if x is within the valid range
        if (abs(x) <= 1.0) then
            ! Calculate the sum of the series
            sum = series_sum(x, n)

            ! Print the result
            print 5, sum
          5 format(2x, "The sum of the (1 + x)^-1 series is:", f8.3)
        else
            print*, "Undefined."
        end if
    end do
end program

! Function to calculate the sum of the series (1 + x)^-1 using its Taylor series expansion
real function series_sum(x, n)
    implicit none
    real, intent(in) :: x
    integer, intent(in) :: n
    integer :: i
    series_sum = 0.0

    ! Loop to calculate the sum
    do i = 0, n-1
        series_sum = series_sum + (-1.0)**i * x**i
    end do
end function
