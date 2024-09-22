! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz
! 
! Write a Fortran program to print prime numbers and count how many prime numbers are between a specified range (m, n; m < n).

program prime
    implicit none
    integer :: m, n, i, j, ct, flag, si  ! Variables for range, loop control, prime count, and flag

    do
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:"
        read *, si
        if (si == 0) exit

        ! Input values for the range (m, n)
        print *, "Enter the value of m and n:"
        read *, m, n

        ! Initialize the prime count
        ct = 0

        ! Loop through each number in the range
        do i = m, n
            if (i <= 1) cycle  ! Skip numbers less than or equal to 1

            ! Assume the number is prime
            flag = 1

            ! Check if the number is prime
            do j = 2, i-1
                if (mod(i, j) == 0) then
                    flag = 0
                    exit
                end if
            end do

            ! If the number is prime, print it and increment the count
            if (flag == 1) then
                print *, i
                ct = ct + 1
            end if
        end do

        ! Print the total count of prime numbers
        print *, "Number of prime numbers is:", ct
    end do

end program prime
