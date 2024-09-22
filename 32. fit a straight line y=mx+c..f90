!Write a Fortran program ,which reads n elements in an array and Given a set of points (x1,y1),(x2,y2),(x3,y3)…….(xn,yn) to fit a straight line y=mx+c.

! This program calculates the linear regression line (Y = mx + c) for a given set of data points.

! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program stline
    implicit none
    integer :: si, n, i
    real :: sumx, sumy, sumxy, sumxsq, m, rl, c
    real, allocatable, dimension(:) :: x, y

    do
        ! Prompt user to enter serial number
        print*, "Enter serial no:"
        read*, si
        if (si == 0) exit  ! Exit loop if serial number is 0

        ! Prompt user to enter the value of n
        print*, "Enter the value of n:"
        read*, n

        ! Allocate arrays for x and y values
        allocate(x(n), y(n))

        ! Prompt user to enter n pairs of x, y values
        print*, "Type n pairs of x, y values:"
        read*, (x(i), y(i), i = 1, n)

        ! Initialize sums
        sumx = 0
        sumy = 0
        sumxy = 0
        sumxsq = 0

        ! Calculate sums
        do i = 1, n
            sumx = sumx + x(i)
            sumy = sumy + y(i)
            sumxy = sumxy + x(i) * y(i)
            sumxsq = sumxsq + x(i) * x(i)
        end do

        ! Calculate the number of data points
        rl = n

        ! Calculate the slope (m) and intercept (c) of the regression line
        m = (rl * sumxy - sumx * sumy) / (rl * sumxsq - (sumx)**2)
        c = (sumy - m * sumx) / rl

        ! Print the result
        print 6, m, c
        6 format(2x, "Y = ", f8.5, "x + ", f8.3)

        ! Deallocate arrays
        deallocate(x)
        deallocate(y)
    end do
end program
