! Program to calculate the sum of the series log(1+x) using its Taylor series expansion

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program logseries
    implicit none
    real :: l, lm, x
    integer :: si, n

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of x and n
        print*, "Enter the value of x and n:"
        read*, x, n

        ! Calculate the sum of the log(1+x) series
        l = lm(x, n)

        ! Print the result
        print 5, l
        5 format(2x, "The sum of log(1+x) series is:", f8.2)
    end do
end program

! Function to calculate the sum of the series log(1+x) using its Taylor series expansion
real function lm(x, n)
    implicit none
    real, intent(in) :: x
    integer, intent(in) :: n
    integer :: i
    lm = 0.0

    ! Loop to calculate the sum
    do i = 1, n
        lm = lm + ((-1)**(i + 1) * x**i) / i
    end do
end function
