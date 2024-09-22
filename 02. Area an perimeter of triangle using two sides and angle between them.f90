! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn : https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub : https://github.com/itsmahafuz
! 
! Write a Fortran program to calculate the area and perimeter of a triangle 
! given two sides and the angle between them.

program triangle
    implicit none
    real :: area, peri, a, b, c, th, rth  ! Variables for area, perimeter, sides, angle, and angle in radians
    integer :: si                          ! Serial number for input control

    do 
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:" 
        read *, si 
        if (si == 0) exit 
        
        ! Input the lengths of the two sides and the angle between them
        print *, "Enter the value of the two sides (a, b):" 
        read *, a, b 
        print *, "Enter the value of the angle between them (in degrees):" 
        read *, th 
        
        ! Convert angle from degrees to radians
        rth = (3.1416 * th) / 180.0 
        
        ! Calculate the length of the third side using the cosine rule
        c = sqrt(a**2 + b**2 - 2 * a * b * cos(rth)) 
        
        ! Check if the sides can form a triangle using the triangle inequality theorem
        if ((a + b) > c .and. (b + c) > a .and. (a + c) > b) then 
            ! Calculate area using the formula: Area = 0.5 * a * b * sin(angle)
            area = 0.5 * a * b * sin(rth) 
            peri = a + b + c  ! Calculate perimeter
            
            ! Print the results
            print 5, peri, area 
            5 format(2x, "The perimeter is: ", f8.3, / 2x, "The area is: ", f8.3) 
        else 
            print *, "Does not form a triangle."  ! Inform user if a triangle cannot be formed
        end if 
    end do 

end program triangle
