! Program to calculate the product of the series (1/(a+b))(2/(a+2b))(3/(a+3b))... up to n terms

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program product_series
    implicit none
    real :: a, b, product
    integer :: n, i, si

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the values of a and b
        print*, "Enter the value of a and b:"
        read*, a, b

        ! Prompt user to enter the value of n
        print*, "Enter the value of n:"
        read*, n

        product = 1.0  ! Initialize product to 1

        ! Loop to calculate the product of the series
        do i = 1, n
            product = product * (i / (a + i * b))
        end do

        ! Print the result
        print 5, product
5       format(2x, "The product of the expression is:", e12.3)
    end do
end program
