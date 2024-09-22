!Write a Fortran program ,which reads n elements and write some Functions subprogram to print  the values of Sum, AM, GM and SD.

! This program calculates the sum, arithmetic mean, geometric mean, and standard deviation of a set of observations.

! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program sagsd
    implicit none
    real, allocatable, dimension(:) :: a
    integer :: si, n, i
    real :: s, as, g, sd, sm, am, gm, sdm

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

        ! Calculate the sum, arithmetic mean, geometric mean, and standard deviation
        s = sm(a, n)
        as = am(a, n)
        g = gm(a, n)
        sd = sdm(a, n)

        ! Print the results
        print 5, s
        5 format(2x, "The sum of the observations is:", f9.3)
        print 6, as
        6 format(2x, "The arithmetic mean of the observations is:", f9.3)
        print 51, g
        51 format(2x, "The geometric mean of the observations is:", f9.3)
        print 55, sd
        55 format(2x, "The standard deviation of the observations is:", f9.3)

        ! Deallocate memory
        deallocate(a)
    end do
end program

! Function to calculate the sum of the observations
real function sm(a, n)
    implicit none
    integer :: i, n
    real :: a(n)
    sm = 0
    do i = 1, n
        sm = sm + a(i)
    end do
end function

! Function to calculate the arithmetic mean of the observations
real function am(a, n)
    implicit none
    integer :: n, i
    real :: a(n), sm
    sm = 0
    do i = 1, n
        sm = sm + a(i)
    end do
    am = sm / n
end function

! Function to calculate the geometric mean of the observations
real function gm(a, n)
    implicit none
    integer :: i, n
    real :: a(n), ft
    ft = 1
    do i = 1, n
        ft = ft * a(i)
    end do
    gm = ft**(1.0 / n)
end function

! Function to calculate the standard deviation of the observations
real function sdm(a, n)
    implicit none
    integer :: i, n
    real :: am, sm, fd, a(n)
    sm = 0
    do i = 1, n
        sm = sm + a(i)
    end do
    am = sm / n
    fd = 0
    do i = 1, n
        fd = fd + (a(i) - am)**2
    end do
    sdm = sqrt(fd / n)
end function
