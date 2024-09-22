! Program to calculate the sum of the series e^x using its Taylor series expansion

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program exseries
    implicit none
    real :: em, x
    integer :: si, n

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of x and n
        print*, "Enter the value of x and n:"
        read*, x, n
        n = n - 1  ! Adjust n for the loop

        ! Print initial sum
        print*, "Sum = 0"

        ! Calculate the sum of the e^x series
        print 5, em(x, n)
        5 format(2x, "The sum of the e^x series is:", f8.2)
    end do
end program

! Function to calculate the sum of the series e^x using its Taylor series expansion
real function em(x, n)
    implicit none
    real, intent(in) :: x
    integer, intent(in) :: n
    real :: ft
    integer :: i
    em = 1.0
    ft = 1.0

    ! Loop to calculate the sum
    do i = 1, n
        ft = ft * i  ! Calculate factorial
        em = em + (x**i) / ft  ! Add term to sum
    end do
end function
