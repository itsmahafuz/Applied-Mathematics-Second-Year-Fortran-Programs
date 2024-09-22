! Program to find the sum of digits of a number (at least 7 digits)

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sumdih
    implicit none
    integer:: si, n, d1, d2, d3, d4, d5, d6, d7, sum

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter a 7-digit integer number
        print*, "Enter an integer number of 7 digits:"
        read*, n

        ! Extract each digit from the number
        d1 = mod(n, 10)
        n = n / 10
        d2 = mod(n, 10)
        n = n / 10
        d3 = mod(n, 10)
        n = n / 10
        d4 = mod(n, 10)
        n = n / 10
        d5 = mod(n, 10)
        n = n / 10
        d6 = mod(n, 10)
        n = n / 10
        d7 = n

        ! Calculate the sum of the digits
        sum = d1 + d2 + d3 + d4 + d5 + d6 + d7

        ! Print the sum of the digits
        print 5, sum5
        format(2x, "The sum is:", I7)
    end do
end program
