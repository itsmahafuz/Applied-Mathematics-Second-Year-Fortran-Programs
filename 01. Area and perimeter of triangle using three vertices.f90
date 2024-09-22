! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn : https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub : https://github.com/itsmahafuz
! 
! Write a Fortran program to print the area and perimeter of a triangle
! when three vertices of the triangle are given.

program triangle
    implicit none
    real :: x1, y1, x2, y2, x3, y3  ! Coordinates of the triangle vertices
    real :: area, peri, s, a, b, c    ! Variables for area, perimeter, and side lengths
    integer :: si                      ! Serial number for input control

    do 
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no (0 to exit):" 
        read *, si 
        if (si == 0) exit 
        
        ! Input coordinates of the triangle vertices
        print *, "Enter the value of x1, y1:" 
        read *, x1, y1 
        print *, "Enter the value of x2, y2:" 
        read *, x2, y2 
        print *, "Enter the value of x3, y3:" 
        read *, x3, y3 

        ! Calculate the lengths of the sides using the distance formula
        a = sqrt((x1 - x2)**2 + (y1 - y2)**2)  ! Length between vertex 1 and 2
        b = sqrt((x2 - x3)**2 + (y2 - y3)**2)  ! Length between vertex 2 and 3
        c = sqrt((x1 - x3)**2 + (y1 - y3)**2)  ! Length between vertex 3 and 1

        ! Check if the sides can form a triangle using the triangle inequality theorem
        if ((a + b) > c .and. (b + c) > a .and. (a + c) > b) then 
            peri = a + b + c  ! Calculate perimeter
            s = peri / 2.0     ! Calculate semi-perimeter
            area = sqrt(s * (s - a) * (s - b) * (s - c))  ! Calculate area using Heron's formula

            ! Print the results
            print 5, peri 
            5 format(2x, "The perimeter of the triangle is: ", f8.3) 
            print 10, area 
            10 format(2x, "The area of the triangle is: ", f8.3) 
        else 
            print *, "Does not form a triangle."  ! Inform user if a triangle cannot be formed
        end if 
    end do 

end program triangle
