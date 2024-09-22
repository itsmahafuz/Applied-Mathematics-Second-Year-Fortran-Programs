!Write a Fortran program, which reads two matrices and print their multiplication in matrix form.

! This program calculates the product of two matrices A and B.

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program matmul
    implicit none
    integer, allocatable, dimension(:,:) :: A, B, C
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

        ! Allocate memory for matrices A, B, and C
        allocate(a(m, n), b(p, q), c(m, q))

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

        ! Check if the number of columns in A is equal to the number of rows in B
        if (n == p) then
            ! Initialize matrix C to zero
            c = 0

            ! Calculate the product of matrices A and B
            do i = 1, m
                do j = 1, q
                    do k = 1, n
                        c(i, j) = c(i, j) + a(i, k) * b(k, j)
                    end do
                end do
            end do

            ! Print the product of the two matrices
            print*, "The multiplication of A and B is:"
            do i = 1, m
                print*, (c(i, j), j = 1, q)
            end do
        else
            print*, "Multiplication is not possible."
        end if

        ! Deallocate memory
        deallocate(a)
        deallocate(b)
        deallocate(c)
    end do
end program
