module day6
    implicit none

    contains
        subroutine day6timer()
            implicit none

            integer :: temp(2)

            temp = day6all()
        end subroutine day6timer

        function day6all()
            implicit none

            integer                   :: day6all(2)
            character(3), allocatable :: orbits(:,:)

            call readf(orbits)
            day6all(1) = day6a()
            day6all(2) = day6b()
        end function day6all

        function day6a()
            implicit none

            integer :: day6a

            day6a = 1
        end function day6a

        function day6b()
            implicit none

            integer day6b

            day6b = 2
        end function day6b

        subroutine readf(orbits)
            implicit none

            character(3), allocatable, intent(out) :: orbits(:,:)

            integer      :: leng
            character(3) :: parent, child

            leng = 0
            open (unit = 3, file = "input//day6.txt")
            do
                read(3,*, end = 6010)
                leng = leng + 1
                continue
       6010     exit
            end do

            ! read(3,'(A)') parent, child
            close (3)
            print*, "size:", leng


        end subroutine
end module day6