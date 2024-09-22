! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn : https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub : https://github.com/itsmahafuz
! 
! Write a Fortran program to calculate the area and circumference of a circle 
! given the endpoints of its diameter.

program circumx 
    implicit none 
    real :: x1, x2, y1, y2               ! Coordinates of the endpoints of the diameter
    real :: area, circum, d, r           ! Variables for area, circumference, diameter, and radius
    real, parameter :: pi = 3.1416        ! Constant value of pi
    integer :: si                         ! Serial number for input control 

    do 
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:" 
        read *, si 
        if (si == 0) exit 
        
        ! Input coordinates of the endpoints of the diameter
        print *, "Enter the coordinate of first end of the diameter:" 
        read *, x1, y1 
        print *, "Enter the coordinate of second end of the diameter:" 
        read *, x2, y2 
        
        ! Calculate the diameter and radius
        d = sqrt((x1 - x2)**2 + (y1 - y2)**2)  ! Diameter calculation
        r = d / 2.0                              ! Radius calculation
        
        ! Check if the radius is zero (point circle)
        if (r == 0) then 
            print *, "It is a point circle." 
        else 
            ! Calculate area and circumference
            area = pi * r**2 
            circum = 2 * pi * r 
            
            ! Print the results
            print 5, area, circum 
            5 format(2x, "The area is: ", f8.3, / 2x, "The circumference is: ", f8.3) 
        end if 
    end do 

end program circumx
