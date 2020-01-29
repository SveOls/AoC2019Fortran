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

            day6all(1) = day6a(orbits)
            day6all(2) = day6b()

        end function day6all

        function day6a(orbits)
            implicit none

            ! type point_orbit
            !     integer, pointer :: val => null()
            !     type(point_orbit), pointer :: pee => null()
            ! end type


            character(3)         :: orbits(:,:)
            integer              :: day6a, i, j
            integer, allocatable :: list(:,:)

            allocate(list(size(orbits, 1), 2))

            do i = 1, size(orbits, 1)
                list(i, :) = (/ -1, 1 /)
                do j = 1, size(orbits, 1)
                    if (orbits(i, 1) == orbits(j, 2)) then
                        list(i, :) = (/ j, 0 /)
                        exit
                    end if
                end do
            end do

            do i = 1, size(list, 1)
                day6a = day6a + get_out(i, list)
            end do



        end function day6a

        recursive function get_out(i, list) result(ret)
            implicit none 
    
            integer :: i, ret

            integer, intent(inout) :: list(:,:)

            if (list(i, 2) == 0) then
                list(i, 2) = get_out(list(i, 1), list) + 1
            end if

            ret = list(i, 2)

        end function

        function day6b()
            implicit none

            integer day6b

            day6b = 2
        end function day6b

        subroutine readf(orbits)
            implicit none

            character(3), allocatable, intent(out) :: orbits(:,:)

            integer      :: leng, i
            character(3) :: parent, child

            leng = 0

            open (unit = 2019, file = "input//day6.txt")

            do
    6011        continue
                read(2019,*, end = 6010)
                leng = leng + 1
                goto 6011
    6010        exit
            end do

            allocate(orbits(leng, 2))
            rewind(2019)

            do i = 1, leng
                read(2019,*) parent, child
                orbits(i, 1) = parent
                orbits(i, 2) = child
                ! print*, orbits(i, 1), " is orbited by ", orbits(i, 2)
            end do

            close (2019)
        end subroutine
end module day6