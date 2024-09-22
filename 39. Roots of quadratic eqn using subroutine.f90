!Write a Fortran program to solve a quadratic equation ax^2+bx^2+cx+d=0 and print the roots (real/complex) by subroutine subprogram.

! This program solves a quadratic equation ax^2 + bx + c = 0 and prints the roots (real or complex).
! Author: MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz

program quadsubroutine
    implicit none
    integer :: serial_no, root_type
    real :: a, b, c, discriminant, root, root1, root2
    
    do
        print *, "Enter serial number (0 to exit):"
        read *, serial_no
        if (serial_no == 0) exit
        
        print *, "Enter the coefficients a, b, and c:"
        read *, a, b, c
        
        call roots(a, b, c, discriminant, root, root1, root2, root_type)
        
        select case (root_type)
            case (1)
                print 5, root
                5 format(2x, "The root of the equation is: ", f8.3)
            case (2)
                print 6, root, root
                6 format(2x, "R1 = ", f8.3, "  R2 = ", f8.3)
            case (3)
                print 7, root1, root2
                7 format(2x, "R1 = ", f8.3, "  R2 = ", f8.3)
            case (4)
                print 8, root1, root2
                8 format(2x, "R1 = ", f8.3, " + i", f8.3, "  R2 = ", f8.3, " - i", f8.3)
        end select
    end do
end program quadsubroutine

subroutine roots(a, b, c, discriminant, root, root1, root2, root_type)
    implicit none
    real :: root1, root2, discriminant, root, a, b, c
    integer :: root_type

    discriminant = b**2 - 4.0 * a * c

    if (discriminant > 0.0) then
        ! Real and distinct roots
        root1 = (-b + sqrt(discriminant)) / (2.0 * a)
        root2 = (-b - sqrt(discriminant)) / (2.0 * a)
        root_type = 3
    else if (discriminant == 0.0) then
        ! Real and equal roots
        root = -b / (2.0 * a)
        root_type = 2
    else
        ! Complex roots
        discriminant = -discriminant
        root1 = -b / (2.0 * a)
        root2 = sqrt(discriminant) / (2.0 * a)
        root_type = 4
    end if
end subroutine roots
