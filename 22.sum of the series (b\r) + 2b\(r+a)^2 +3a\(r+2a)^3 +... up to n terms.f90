! Program to calculate the sum of the series (b/r) + 2b/(r+a)^2 + 3b/(r+2a)^3 + ... up to n terms

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sum_series
    implicit none
    real :: a, b, r, sum
    integer :: n, i, si

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of a, b, r, and n
        print*, "Enter the value of a, b, r and n:"
        read*, a, b, r, n

        ! Check for undefined conditions
        if ((a == 0 .and. r == 0) .or. (r == (1 - n) * a)) then
            print*, "Undefined."
        else
            sum = 0.0  ! Initialize sum to 0

            ! Loop to calculate the sum of the series
            do i = 1, n
                sum = sum + (i * b) / ((r + (i - 1) * a)**i)
            end do

            ! Print the result
            print 5, sum
            5 format(2x, "The sum of the series is:", f8.3)
        end if
    end do
end program
