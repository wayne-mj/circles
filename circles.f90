! Formuals referenced from Wikipedia
! https://en.wikipedia.org/wiki/Area
! https://en.wikipedia.org/wiki/List_of_formulas_in_elementary_geometry

program circles

    implicit none
    ! Define the kind of real number to use
    !integer, parameter :: sp = selected_real_kind(p=6)  ! (4)  !kind(1.0)
    !integer, parameter :: dp = selected_real_kind(p=15) ! (8)  !kind(1.0d0)
    !integer, parameter :: qp = selected_real_kind(p=33) ! (16) !kind(1.0q0)

    !real(dp) :: radius, pi, height
    real(8) :: radius, pi, height
    character(len=100) :: input_line
    integer :: choice, io_status
    logical :: done = .false.

    !pi = 4 * atan(1.0_dp)                               ! 3.14159265358979323846
    pi = 4 * atan(1.0)                                   ! 3.14159265358979323846

    call clearscr()

    do while (.not. done)
        call menu()
        !read *, choice
        read(*, '(A)') input_line

        read (input_line, '(I10)', iostat=io_status) choice
    
        if (io_status == 0) then
            select case(choice)
                case(1)
                    print *, ""
                    write (*, '(A)', advance='no') "   Enter the radius of the circle: "
                    read *, radius
                    
                    print *, ""
                    print *, "  The area of the circle is: ", area(radius, pi)
                    
                case(2)
                    print *, ""
                    write (*, '(A)', advance='no') "   Enter the radius of the circle: "
                    read *, radius

                    print *, ""
                    print *, "  The circumference of the circle is: ", circumference(radius, pi)
                    
                case(3)
                    print *, ""
                    write (*, '(A)', advance='no') "   Enter the radius of the sphere: "
                    read *, radius

                    print *, ""
                    print *, "  The volume of the sphere is: ", volumeofSphere(radius, pi)
                
                case(4)
                    print *, ""
                    write (*, '(A)', advance='no') "   Enter the radius of the cylinder: "
                    read *, radius
                    write (*, '(A)', advance='no') "   Enter the height of the cylinder: "
                    read *, height

                    print *, ""
                    print *, "  The volume of the cylinder is: ", volumeofCylinder(radius, height, pi)

                case(5)
                    print *, ""
                    write (*, '(A)', advance='no') "   Enter the radius of the cone: "
                    read *, radius
                    write (*, '(A)', advance='no') "   Enter the height of the cone: "
                    read *, height

                    print *, ""
                    print *, "  The volume of the cone is: ", volumeofCone(radius, height, pi)

                case(6)
                    print *, ""
                    write (*, '(A)', advance='no') "   Enter the radius of the cone: "
                    read *, radius
                    write (*, '(A)', advance='no') "   Enter the height of the cone: "
                    read *, height

                    print *, ""
                    print *, "  The surface area of the cone is: ", surfaceAreaCone(radius, height, pi)

                case(7)
                    print *, ""
                    write (*, '(A)', advance='no') "   Enter the radius 1 of the ellipse: "
                    read *, radius
                    write (*, '(A)', advance='no') "   Enter the radius 2 of the ellipse: "
                    read *, height

                    print *, ""
                    print *, "  The area of the ellipse is: ", areaofEllipse(radius, height, pi)

                case(0)
                    print *, ""
                    print *, "  Goodbye!"                
                    done = .true.

                case default
                    print *, ""
                    print *, "  Not yet implemented!"
            
            end select
        else
            print *, ""
            print *, "  Invalid choice!"
        end if
    end do

    contains

    ! Function to calculate the area of a circle
    function area(radius, pi) result(a)
        !real(dp), intent(in) :: radius, pi
        !real(dp) :: a
        real(8), intent(in) :: radius, pi
        real(8) :: a

        a = pi * (radius**2)
    end function area

    ! Function to calculate the circumference of a circle
    function circumference(radius, pi) result(c)
        !real(dp), intent(in) :: radius, pi
        !real(dp) :: c
        real(8), intent(in) :: radius, pi
        real(8) :: c

        c = 2 * pi * radius
    end function circumference

    ! Function to calculate the volume of a sphere
    function volumeofSphere(radius, pi) result(v)
        !real(dp), intent(in) :: radius, pi
        !real(dp) :: v
        real(8), intent(in) :: radius, pi
        real(8) :: v

        v = (4.0 / 3.0) * pi * radius**3
    end function volumeofSphere

    ! Function to calculate the volume of a cylinder
    function volumeofCylinder(radius, height, pi) result(v)
        !real(dp), intent(in) :: radius, pi, height
        !real(dp) :: v
        real(8), intent(in) :: radius, pi, height
        real(8) :: v

        v = height * area(radius, pi)        
    end function volumeofCylinder

    ! Function to calculate the volume of a cone
    function volumeofCone(radius, height, pi) result(v)
        !real(dp), intent(in) :: radius, pi, height
        !real(dp) :: v
        real(8), intent(in) :: radius, pi, height
        real(8) :: v

        v = (pi * (radius**2) * height) / 3.0
    end function volumeofCone

    ! Function to calculate the surface area of a cone
    function surfaceAreaCone(radius,height,pi) result(area)
        !real(dp), intent(in) :: radius, pi, height
        !real(dp) :: area
        real(8), intent(in) :: radius, pi, height
        real(8) :: area

        area = (pi * radius) * (radius + (sqrt(radius**2 + height**2)))
    end function surfaceAreaCone

    ! Function to calculate the area of an ellipse
    function areaofEllipse(a,b,pi) result(area)
        !real(dp), intent(in) :: a, b, pi
        !real(dp) :: area
        real(8), intent(in) :: a, b, pi
        real(8) :: area

        area = pi * a * b
    end function areaofEllipse

    ! Subroutine to display the menu
    subroutine menu()
        print *, " "
        print *, "****************************************"
        print *, "*** Welcome to the world of Cirlces! ***"
        print *, "****************************************"
        print *, " "
        print *, "  Select from the below:"
        print *, "  1. Area of a circle                "
        print *, "  2. Circumference of a circle       "
        print *, "  3. Volume of a sphere              "
        print *, "  4. Volume of a cylinder            "
        print *, "  5. Volume of a cone                "
        print *, "  6. Surface Area of a cone          "
        print *, "  7. Area of an ellipse              "
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
