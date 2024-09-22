! Program to calculate the product of the series (1/2)(3/4)(5/6)... up to n terms

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program product_series
    implicit none
    real :: product, term
    integer :: n, i, si

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the value of n
        print*, "Enter the value of n:"
        read*, n

        product = 1.0  ! Initialize product to 1

        ! Loop to calculate the product of the series
        do i = 1, n
            term = (2.0*i - 1) / (2.0*i)  ! Calculate each term
            product = product * term  ! Multiply the term to the product
        end do

        ! Print the result
        print 5, product
        5 format(2x, "The product of the expression is:", f8.3)
    end do
end program

