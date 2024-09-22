!Write a Fortran program, which reads two matrices and print their sum in matrix form.

! This program calculates the sum of two matrices A and B.

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program summatrix
    implicit none
    integer, allocatable, dimension(:,:) :: a, b, c
    integer :: m, n, i, j, si, p, q

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

        ! Allocate memory for matrices A, B, and C
        allocate(a(m, n), b(p, q), c(m, n))

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

        ! Check if the dimensions of A and B are the same
        if ((m == p) .and. (n == q)) then
            ! Calculate the sum of matrices A and B
            do i = 1, m
                do j = 1, n
                    c(i, j) = a(i, j) + b(i, j)
                end do
            end do

            ! Print the sum of the two matrices
            print*, "The sum of the two matrices is:"
            do i = 1, m
                print*, (c(i, j), j = 1, n)
            end do
        else
            print*, "Matrix summation is not possible."
        end if

        ! Deallocate memory
        deallocate(a)
        deallocate(b)
        deallocate(c)
    end do
end program
