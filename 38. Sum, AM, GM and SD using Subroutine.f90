!Write a Fortran program ,which reads n elements and write some Subroutine  subprogram to print  the values of Sum, AM, GM and SD.

! This program reads n elements and uses subroutines to print the values of Sum, AM, GM, and SD.

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sagsd
    implicit none
    real, allocatable, dimension(:) :: a
    integer :: si, n, i
    real :: s, am, gm, sd

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the number of observations
        print*, "Enter number of observations:"
        read*, n

        ! Allocate memory for the observations array
        allocate(a(n))

        ! Prompt user to enter the observations
        print*, "Enter the observations:"
        read*, (a(i), i = 1, n)

        ! Call subroutines to calculate and print the results
        call calculate_sum(a, n, s)
        call calculate_am(a, n, am)
        call calculate_gm(a, n, gm)
        call calculate_sd(a, n, sd)

        ! Print the results
        print 5, s
        5 format(2x, "The sum of the observations is:", f9.3)
        print 6, am
        6 format(2x, "The arithmetic mean of the observations is:", f9.3)
        print 51, gm
        51 format(2x, "The geometric mean of the observations is:", f9.3)
        print 55, sd
        55 format(2x, "The standard deviation of the observations is:", f9.3)

        ! Deallocate memory
        deallocate(a)
    end do
end program

! Subroutine to calculate the sum of the observations
subroutine calculate_sum(a, n, s)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    real, intent(out) :: s
    integer :: i
    s = 0
    do i = 1, n
        s = s + a(i)
    end do
end subroutine

! Subroutine to calculate the arithmetic mean of the observations
subroutine calculate_am(a, n, am)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    real, intent(out) :: am
    integer :: i
    real :: s
    s = 0
    do i = 1, n
        s = s + a(i)
    end do
    am = s / n
end subroutine

! Subroutine to calculate the geometric mean of the observations
subroutine calculate_gm(a, n, gm)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    real, intent(out) :: gm
    integer :: i
    real :: ft
    ft = 1
    do i = 1, n
        ft = ft * a(i)
    end do
    gm = ft**(1.0 / n)
end subroutine

! Subroutine to calculate the standard deviation of the observations
subroutine calculate_sd(a, n, sd)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    real, intent(out) :: sd
    integer :: i
    real :: am, s, fd
    s = 0
    do i = 1, n
        s = s + a(i)
    end do
    am = s / n
    fd = 0
    do i = 1, n
        fd = fd + (a(i) - am)**2
    end do
    sd = sqrt(fd / n)
end subroutine
