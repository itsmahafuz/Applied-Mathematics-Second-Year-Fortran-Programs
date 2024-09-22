! MD MAHAFUZUR RAHMAN
! Roll: 2110428176
! Department of Applied Mathematics
! University Of Rajshahi.
! LinkedIn: https://www.linkedin.com/in/md-mahafuzur-rahman-07b80b1b7
! GitHub: https://github.com/itsmahafuz
! 
! Write a Fortran program to find the biggest number among u, v, and w.

program bigg
    implicit none
    real :: u, v, w  ! Variables to hold the three numbers
    integer :: si    ! Serial number for input control

    do
        ! Prompt user for a serial number, exit if 0
        print *, "Enter serial no:"
        read *, si
        if (si == 0) exit

        ! Input values for u, v, and w
        print *, "Enter three numbers:"
        read *, u, v, w

        ! Determine and print the biggest number
        if ((u > v) .and. (u > w)) then
            print 5, u
            5 format(2x, f8.3, 3x, "is the biggest number,")
        else if ((v > u) .and. (v > w)) then
            print 6, v
            6 format(2x, f8.3, 3x, "is the biggest number,")
        else
            print 8, w
            8 format(2x, f8.3, 3x, "is the biggest number,")
        end if
    end do

end program bigg
