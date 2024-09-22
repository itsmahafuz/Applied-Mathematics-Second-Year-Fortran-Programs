! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz
! 
! Write a Fortran program to solve a quadratic equation ax^2+bx^2+cx+d=0 and print the roots (real/complex) by normally.
program quadroot
    implicit none
    real :: a, b, c, d, x1, x2, x, dis  ! Variables for coefficients, roots, and discriminant
    integer :: si                       ! Serial number for input control

    do
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:"
        read *, si
        if (si == 0) exit

        ! Input values for a, b, c, and d
        print *, "Enter the value of a, b, c and d:"
        read *, a, b, c, d

        ! Check if the equation is not quadratic
        if ((a + b) == 0) then
            x = -d / c
            print 5, x
            5 format(2x, "This is not a quadratic equation and the root is:", f8.3)
        else
            ! Calculate the discriminant
            dis = c**2 - 4 * (a + b) * d

            ! Check the nature of the roots
            if (dis == 0) then
                x = (-c) / (2 * (a + b))
                print 6, x, x
                6 format(2x, "Root1 =", f8.3 / 2x, "Root2 =", f8.3)
            else if (dis > 0) then
                x1 = ((-c) + sqrt(dis)) / (2 * (a + b))
                x2 = ((-c) - sqrt(dis)) / (2.0 * (a + b))
                print 7, x1, x2
                7 format(2x, "Root1 =", f8.3 / 2x, "Root2 =", f8.3)
            else
                dis = -dis
                x1 = (-c) / (2 * (a + b))
                x2 = sqrt(dis) / (2 * (a + b))
                print 9, x1, x2
                9 format(2x, "Root1 =", f8.3, "+i", f8.3)
                print 10, x1, x2
                10 format(2x, "Root2 =", f8.3, "-i", f8.3)
            end if
        end if
    end do

end program quadroot
