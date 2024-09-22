! Program to calculate the sum of the series (1 - 2x)^-1 for |x| <= 1

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sum_series
    implicit none
    real :: x, sum
    integer :: n, i, si

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
            sum = 0.0  ! Initialize sum to 0

            ! Loop to calculate the sum of the series
            do i = 0, n-1
                sum = sum + (2*x)**i
            end do

            ! Print the result
            print 5, sum
5          format(2x, "The sum of the series is:", f8.3)
        else
            print*, "Undefined."
        end if
    end do
end program
