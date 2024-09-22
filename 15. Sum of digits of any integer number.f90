! Program to find the sum of digits of a number

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sumdih
    implicit none
    integer:: n, sum, digit, si

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter an integer number
        print*, "Enter an integer number:"
        read*, n

        sum = 0  ! Initialize sum to 0

        ! Loop to calculate the sum of digits
        do
            if (n == 0) exit  ! Exit loop if number is 0
            digit = mod(n, 10)  ! Get the last digit
            sum = sum + digit  ! Add the digit to sum
            n = n / 10  ! Remove the last digit
        end do

        ! Print the sum of the digits
        print 5, sum
        5 format(2x, "Sum of the digits is:", i6)
    end do
end program
