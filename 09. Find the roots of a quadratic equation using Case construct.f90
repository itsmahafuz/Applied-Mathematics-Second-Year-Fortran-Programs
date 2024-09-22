! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz
! 
! Write a Fortran program to solve a quadratic equation ax^2+bx^2+cx+d=0 and print the roots (real/complex) by using case construct.

program quadroot
    implicit none
    real :: a, b, c, d, x1, x2, x, dis  ! Variables for coefficients, roots, and discriminant
    integer :: si, i                    ! Serial number for input control and case selector

    do
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:"
        read *, si
        if (si == 0) exit

        ! Input values for a, b, c, and d
        print *, "Enter the value of a, b, c and d:"
        read *, a, b, c, d

        ! Initialize case selector
        i = 0

        ! Check if the equation is not quadratic
        if ((a + b) == 0) then
            if (c /= 0) then
                i = 1  ! Linear equation case
            else
                print *, "Both coefficients a and b are zero; the equation is invalid."
                cycle
            end if
        else
            ! Calculate the discriminant
            dis = c**2 - 4.0 * (a + b) * d
            if (dis == 0) then
                i = 2  ! One real root
            else if (dis > 0) then
                i = 3  ! Two distinct real roots
            else
                i = 4  ! Complex roots
            end if
        end if

        ! Select case based on the value of i
        select case (i)
        case (1)
            ! Linear equation case
            x = -d / c
            print 5, x
            5 format(2x, "The root is:", f8.3)
        case (2)
            ! One real root case
            x = -c / (2.0 * (a + b))
            print 6, x, x
            6 format(2x, "Root1 =", f8.3 / 2x, "Root2 =", f8.3)
        case (3)
            ! Two distinct real roots case
            x1 = (-c + sqrt(dis)) / (2.0 * (a + b))
            x2 = (-c - sqrt(dis)) / (2.0 * (a + b))
            print 7, x1, x2
            7 format(2x, "Root1 =", f8.3 / 2x, "Root2 =", f8.3)
        case (4)
            ! Complex roots case
            x1 = -c / (2.0 * (a + b))
            x2 = sqrt(-dis) / (2.0 * (a + b))
            print 61, x1, x2
            61 format(2x, "Root1 =", f8.3, " + i", f8.3)
            print 1, x1, x2
            1 format(2x, "Root2 =", f8.3, " - i", f8.3)
        end select
    end do

end program quadroot
