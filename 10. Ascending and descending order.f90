! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz
! 
! Write a Fortran program to read n elements in an array and print them in ascending and descending order.

program asdes
    implicit none
    real, allocatable, dimension(:) :: a  ! Array to hold the elements
    integer :: n, i, j, term              ! Variables for number of elements and loop control

    ! Prompt user for the number of elements
    print *, "Enter the value of n:"
    read *, n

    ! Allocate memory for the array
    allocate(a(n))

    ! Read the elements into the array
    print *, "Enter array elements:"
    read *, (a(i), i = 1, n)

    ! Sort the array in ascending order using bubble sort
    do i = 1, n-1
        do j = i+1, n
            if (a(i) > a(j)) then
                term = a(i)
                a(i) = a(j)
                a(j) = term
            end if
        end do
    end do

    ! Print the array in ascending order
    print *, "The ascending order is:"
    do i = 1, n
        print *, a(i)
    end do

    ! Sort the array in descending order using bubble sort
    do i = 1, n-1
        do j = i+1, n
            if (a(i) < a(j)) then
                term = a(i)
                a(i) = a(j)
                a(j) = term
            end if
        end do
    end do

    ! Print the array in descending order
    print *, "The descending order is:"
    do i = 1, n
        print *, a(i)
    end do

    ! Deallocate the array
    deallocate(a)

end program asdes
