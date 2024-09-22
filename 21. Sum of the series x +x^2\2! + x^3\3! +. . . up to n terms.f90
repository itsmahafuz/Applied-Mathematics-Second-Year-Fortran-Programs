! Program to calculate the sum of the series x + x^2/2! + x^3/3! + ... up to n terms
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
        print*, "Enter the value of x and n > 0:"
        read*, x, n

        ! Calculate the sum of the series
        sum = sm(x, n)

        ! Print the result
        print 5, sum
        5 format(2x, "Sum of the series is:", f8.3)
    end do
end program

! Function to calculate the sum of the series x + x^2/2! + x^3/3! + ... up to n terms
real function sm(x, n)
    implicit none
    real, intent(in) :: x
    integer, intent(in) :: n
    integer :: i
    real :: factorial
    sm = 0.0
    factorial = 1.0

    ! Loop to calculate the sum
    do i = 1, n
        factorial = factorial * i  ! Calculate factorial
        sm = sm + x**i / factorial  ! Add term to sum
    end do
end function
