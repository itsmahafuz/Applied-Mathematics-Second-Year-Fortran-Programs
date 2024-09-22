! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn : https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub : https://github.com/itsmahafuz
! 
! Write a Fortran program to print the area and perimeter of 
! rectangle when four vertices of the rectangle are given.

program rectangle 
    implicit none 
    real :: x1, y1, x2, y2, x3, y3, x4, y4  ! Coordinates of the rectangle vertices
    real :: area, peri, a, b, c, d, e, f     ! Variables for area, perimeter, and lengths of sides
    integer :: si                           ! Serial number for input control 

    do 
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:" 
        read *, si 
        if (si == 0) exit 
        
        ! Input coordinates of the rectangle's vertices
        print *, "Enter the value of x1, y1:" 
        read *, x1, y1 
        print *, "Enter the value of x2, y2:" 
        read *, x2, y2 
        print *, "Enter the value of x3, y3:" 
        read *, x3, y3 
        print *, "Enter the value of x4, y4:" 
        read *, x4, y4 
        
        ! Calculate lengths of the sides
        a = sqrt((x1 - x2)**2 + (y1 - y2)**2)  ! Length between vertex 1 and 2
        b = sqrt((x2 - x3)**2 + (y2 - y3)**2)  ! Length between vertex 2 and 3
        c = sqrt((x4 - x3)**2 + (y4 - y3)**2)  ! Length between vertex 3 and 4
        d = sqrt((x1 - x4)**2 + (y1 - y4)**2)  ! Length between vertex 4 and 1
        e = sqrt((x2 - x4)**2 + (y2 - y4)**2)  ! Length between vertex 4 and 2
        f = sqrt((x1 - x3)**2 + (y1 - y3)**2)  ! Length between vertex 1 and 3 (diagonal)

        ! Check if the points form a rectangle
        if ((a == c) .and. (b == d) .and. (e**2 == b**2 + c**2)) then 
            area = a * b                      ! Calculate area
            peri = 2.0 * (a + b)              ! Calculate perimeter
            
            ! Print the results
            print 5, area, peri 
            5 format(2x, "The area is: ", f8.3, / 2x, "The perimeter is: ", f8.3) 
        else 
            print *, "Does not form a rectangle."  ! Inform user if it does not form a rectangle
        end if 
    end do 

end program rectangle
