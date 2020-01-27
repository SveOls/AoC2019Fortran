module day3
    implicit none

    contains
        subroutine day3timer()
            implicit none
            integer, dimension(2) :: temp
            temp = day3all()
        end subroutine day3timer

        function day3all()
            implicit none 
            integer, dimension(2)    :: day3all
            character(5), dimension(2, 5) :: inp
            integer :: col
            open (unit = 3, file = "input//day3.txt")
            read(3,*) (inp(1,col), col=1, 2)
            read(3,*) (inp(2,col), col=1, 2)
            close (3)
            print*, inp(1, 1)
            print*, inp(1, 2)
            print*, inp(1, 3)
            print*, inp(1, 4)
            print*, inp(1, 5)
            day3all(1) = day3a()
            day3all(2) = day3b()
        end function day3all

        function day3a()
            implicit none
            integer :: day3a
            day3a = 2
        end function day3a

        function day3b()
            implicit none
            integer :: day3b
            day3b = 2
        end function day3b
end module day3