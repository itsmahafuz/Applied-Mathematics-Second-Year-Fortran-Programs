! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn : https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub : https://github.com/itsmahafuz
! 
! Write a Fortran program to calculate the area and circumference of the 
! circumcircle of a triangle defined by three vertices.

program outtr 
    implicit none 
    real :: x1, x2, x3, y1, y2, y3        ! Coordinates of the triangle vertices
    real :: area, ta, circum, s, r, a, b, c ! Variables for area, triangle area, circumference, semi-perimeter, radius, and side lengths
    integer :: si                          ! Serial number for input control 
    real, parameter :: pi = 3.1416         ! Constant value of pi 

    do 
        ! Prompt user for a serial number, exit if 0
        print *, "Serial no:" 
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
        a = sqrt((x1 - x2)**2 + (y1 - y2)**2)  ! Length between vertex 1 and 2
        b = sqrt((x3 - x2)**2 + (y3 - y2)**2)  ! Length between vertex 2 and 3
        c = sqrt((x1 - x3)**2 + (y1 - y3)**2)  ! Length between vertex 3 and 1
        
        ! Check if the sides can form a triangle using the triangle inequality theorem
        if ((a + b) > c .and. (b + c) > a .and. (a + c) > b) then 
            s = (a + b + c) / 2.0              ! Calculate semi-perimeter
            ta = sqrt(s * (s - a) * (s - b) * (s - c))  ! Area of the triangle using Heron's formula
            r = (a * b * c) / (4 * ta)         ! Radius of the circumcircle
            
            ! Calculate area and circumference of the circumcircle
            area = pi * r**2 
            circum = 2 * pi * r 
            
            ! Print the results
            print 5, area, circum 
            5 format(2x, "Area =", f8.3, / 2x, "Circumference =", f8.3) 
        else 
            print *, "Try again, these points do not form a triangle."  ! Inform user if it does not form a triangle
        end if 
    end do 

end program outtr
