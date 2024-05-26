program circles

    implicit none
    ! Define the kind of real number to use
    integer, parameter :: sp = selected_real_kind(p=6)  ! (4)  !kind(1.0)
    integer, parameter :: dp = selected_real_kind(p=15) ! (8)  !kind(1.0d0)
    integer, parameter :: qp = selected_real_kind(p=33) ! (16) !kind(1.0q0)

    real(dp) :: radius, pi, height
    character(len=100) :: input_line
    integer :: choice, io_status
    logical :: done = .false.

    pi = 4 * atan(1.0_dp)                               ! 3.14159265358979323846

    call clearscr()

    do while (.not. done)
        call menu()
        !read *, choice
        read(*, '(A)') input_line

        read (input_line, '(I10)', iostat=io_status) choice
    
        if (io_status == 0) then
            if (choice == 1) then
                print *, ""
                write (*, '(A)', advance='no') "   Enter the radius of the circle: "
                read *, radius
                
                print *, ""
                print *, "  The area of the circle is: ", area(radius, pi)
                
            else if (choice == 2) then
                print *, ""
                write (*, '(A)', advance='no') "   Enter the radius of the circle: "
                read *, radius

                print *, ""
                print *, "  The circumference of the circle is: ", circumference(radius, pi)
                
            else if (choice == 3) then
                print *, ""
                write (*, '(A)', advance='no') "   Enter the radius of the sphere: "
                read *, radius

                print *, ""
                print *, "  The volume of the sphere is: ", volumeofSphere(radius, pi)
            
            else if (choice == 4) then
                print *, ""
                write (*, '(A)', advance='no') "   Enter the radius of the cylinder: "
                read *, radius
                write (*, '(A)', advance='no') "   Enter the height of the cylinder: "
                read *, height

                print *, ""
                print *, "  The volume of the cylinder is: ", volumeofCylinder(radius, pi, height)

            else if (choice == 0) then
                print *, ""
                print *, "  Goodbye!"                
                done = .true.

            else
                print *, ""
                print *, "  Not yet implemented!"
            end if
        else
            print *, ""
            print *, "  Invalid choice!"
        end if
    end do

    contains

    ! Function to calculate the area of a circle
    function area(radius, pi) result(a)
        real(dp), intent(in) :: radius, pi
        real(dp) :: a

        a = pi * (radius**2)
    end function area

    ! Function to calculate the circumference of a circle
    function circumference(radius, pi) result(c)
        real(dp), intent(in) :: radius, pi
        real(dp) :: c

        c = 2 * pi * radius
    end function circumference

    ! Function to calculate the volume of a sphere
    function volumeofSphere(radius, pi) result(v)
        real(dp), intent(in) :: radius, pi
        real(dp) :: v

        v = (4.0 / 3.0) * pi * radius**3
    end function volumeofSphere

    ! Function to calculate the volume of a cylinder
    function volumeofCylinder(radius, pi, height) result(v)
        real(dp), intent(in) :: radius, pi, height
        real(dp) :: v

        v = height * area(radius, pi)        
    end function volumeofCylinder

    ! Subroutine to display the menu
    subroutine menu()
        print *, " "
        print *, "****************************************"
        print *, "*** Welcome to the world of Cirlces! ***"
        print *, "****************************************"
        print *, " "
        print *, "  Select from the below:"
        print *, "  1. Area of a circle"
        print *, "  2. Circumference of a circle"
        print *, "  3. Volume of a sphere"
        print *, "  4. Volume of a cylinder"
        print *, "  0. Exit"
        print *, ""
        write (*, '(A)', advance='no') "   Enter your choice (0-9) : "
    end subroutine menu

    ! Subroutine to clear the screen
    subroutine clearscr()
        integer :: i, limit

        limit = 100
        
        do i = 1, limit                                ! Clear the screen
            write (*,*)                                ! by printing 'limit' empty lines.
        end do                                         ! This ensures compatibility with all terminals.

        !write (*,*) char(27)//"[2J"                     ! Clear the screen but only workd on ANSI terminals.
    end subroutine clearscr
end program circles