! 
! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn : https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub : https://github.com/itsmahafuz
! 
! Write a Fortran program to evaluate u,v of the expressions: u=|a+b^-1|/sin(r-d) ,v=(cu^-1 -u cosx)/b.

program uveva 
    implicit none 
    real :: u, v, a, b, c, x, r, d     ! Variables for calculations
    integer :: si                       ! Serial number for input control 

    do 
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:" 
        read *, si 
        if (si == 0) exit 
        
        ! Input values for a, b, r, and d
        print *, "Enter the value of a, b, r and d:" 
        read *, a, b, r, d 
        
        ! Input values for c and x
        print *, "Enter the value of c and x:" 
        read *, c, x 
        
        ! Check for undefined conditions
        if ((r - d) == 0 .and. b == 0) then 
            print *, "Undefined" 
        else 
            ! Calculate the value of u
            if (b /= 0) then
                u = (abs(a + 1/b)) / sin(r - d)  ! Ensure b is not zero
                print 5, u 
                5 format(2x, "The value of u is:", f8.3) 
                
                ! Check if u is zero to avoid division by zero
                if (u == 0) then 
                    print *, "v is infinite." 
                else 
                    ! Calculate the value of v
                    v = (c / u - u * cos(x)) / b 
                    print 6, v 
                    6 format(2x, "The value of v is:", f8.3) 
                end if 
            else
                print *, "b cannot be zero for this calculation." 
            end if
        end if 
    end do 

end program uveva
