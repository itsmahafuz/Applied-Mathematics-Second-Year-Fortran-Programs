!Write a Fortran program, which reads two matrices and print their sum, subtraction and multiplication in matrix form.

! This program calculates the sum, difference, and product of two matrices A and B.

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sumsubmul
    implicit none
    integer, allocatable, dimension(:,:) :: A, B, C, D, S
    integer :: m, n, p, q, i, j, k, si

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the dimensions of matrix A
        print*, "Enter the value of m and n:"
        read*, m, n

        ! Prompt user to enter the dimensions of matrix B
        print*, "Enter the value of p and q:"
        read*, p, q

        ! Allocate memory for matrices A, B, C, S, and D
        allocate(a(m, n), b(p, q), c(m, n), s(m, n), d(m, q))

        ! Prompt user to enter the elements of matrix A row-wise
        print*, "Enter the matrix A row-wise:"
        read*, ((a(i, j), j = 1, n), i = 1, m)

        ! Prompt user to enter the elements of matrix B row-wise
        print*, "Enter the matrix B row-wise:"
        read*, ((b(i, j), j = 1, q), i = 1, p)

        ! Print matrix A
        print*, "The matrix A is:"
        do i = 1, m
            print*, (a(i, j), j = 1, n)
        end do

        ! Print matrix B
        print*, "The matrix B is:"
        do i = 1, p
            print*, (b(i, j), j = 1, q)
        end do

        ! Check if the dimensions of A and B are the same for addition and subtraction
        if ((m == p) .and. (n == q)) then
            ! Calculate the sum of matrices A and B
            do i = 1, m
                do j = 1, n
                    c(i, j) = a(i, j) + b(i, j)
                end do
            end do

            ! Print the sum of the two matrices
            print*, "The sum of A and B is:"
            do i = 1, m
                print*, (c(i, j), j = 1, n)
            end do

            ! Calculate the difference of matrices A and B
            do i = 1, m
                do j = 1, n
                    s(i, j) = a(i, j) - b(i, j)
                end do
            end do

            ! Print the difference of the two matrices
            print*, "The subtraction of A and B is:"
            do i = 1, m
                print*, (s(i, j), j = 1, n)
            end do
        else
            print*, "Summation and subtraction are not possible."
        end if

        ! Check if the number of columns in A is equal to the number of rows in B for multiplication
        if (n == p) then
            ! Initialize matrix D to zero
            d = 0

            ! Calculate the product of matrices A and B
            do i = 1, m
                do j = 1, q
                    do k = 1, n
                        d(i, j) = d(i, j) + a(i, k) * b(k, j)
                    end do
                end do
            end do

            ! Print the product of the two matrices
            print*, "The multiplication of A and B is:"
            do i = 1, m
                print*, (d(i, j), j = 1, q)
            end do
        else
            print*, "Multiplication is not possible."
        end if

        ! Deallocate memory
        deallocate(a)
        deallocate(b)
        deallocate(c)
        deallocate(d)
        deallocate(s)
    end do
end program
