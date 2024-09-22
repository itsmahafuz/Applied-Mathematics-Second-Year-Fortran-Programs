! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn : https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub : https://github.com/itsmahafuz
! 
! Write a Fortran program to calculate the area and circumference of the 
! incircle of a triangle defined by three vertices.

program areacirintri 
    implicit none 
    real :: x1, x2, x3, y1, y2, y3        ! Coordinates of the triangle vertices
    real :: area, circum, ta, a, b, c, s, r ! Variables for area, circumference, side lengths, semi-perimeter, and radius
    real, parameter :: pi = 3.1416         ! Constant value of pi
    integer :: si                          ! Serial number for input control 

    do 
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:" 
        read *, si 
        if (si == 0) exit 
        
        ! Input coordinates of the triangle vertices
        print *, "Enter the value of x1 and y1:" 
        read *, x1, y1 
        print *, "Enter the value of x2 and y2:" 
        read *, x2, y2 
        print *, "Enter the value of x3 and y3:" 
        read *, x3, y3 
        
        ! Calculate lengths of the triangle sides
        a = sqrt((x2 - x1)**2 + (y2 - y1)**2)  ! Length between vertex 1 and 2
        b = sqrt((x3 - x2)**2 + (y3 - y2)**2)  ! Length between vertex 2 and 3
        c = sqrt((x3 - x1)**2 + (y3 - y1)**2)  ! Length between vertex 3 and 1

        ! Check if the sides can form a triangle using the triangle inequality theorem
        if ((a + b) > c .and. (b + c) > a .and. (a + c) > b) then 
            s = (a + b + c) / 2.0              ! Calculate semi-perimeter
            ta = sqrt(s * (s - a) * (s - b) * (s - c))  ! Area of the triangle using Heron's formula
            r = ta / s                          ! Radius of the incircle
            
            ! Calculate area and circumference of the incircle
            area = pi * r**2 
            circum = 2 * pi * r 
            
            ! Print the results
            print 5, area, circum 
            5 format(2x, "The area of the inner circle is: ", f8.3, / 2x, "The circumference of the inner circle is: ", f8.3) 
        else 
            print *, "Does not form a triangle."  ! Inform user if it does not form a triangle
        end if 
    end do 

end program areacirintri
