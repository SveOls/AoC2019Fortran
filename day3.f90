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

            integer, dimension(:,:), allocatable :: cross

            character(7), allocatable   :: inp1(:), inp2(:)
            integer, dimension(2)       :: day3all
            character(len=2048)         :: temp1, temp2
            integer                     :: col, i, j

            open (unit = 3, file = "input//day3.txt")
            read(3,'(A)') temp1
            read(3,'(A)') temp2                                     ! '(A)' gjør at den leser hele jævla stringen
            close (3)

            i = count(transfer(temp1, 'a', len(temp1)) == ",") + 1  ! count teller antall ganger det inni er true
            j = count(transfer(temp2, 'a', len(temp2)) == ",") + 1
            allocate(inp1(i), inp2(j))
            
            read(temp1,*) (inp1(col), col=1, i)
            read(temp2,*) (inp2(col), col=1, j)

            cross = find_int(inp1, inp2)

            day3all(1) = day3a(cross)
            day3all(2) = day3b(cross, inp1, inp2)
        end function day3all

        function find_int(inp1, inp2) result(ret)
            implicit none
            
            integer, dimension(:,:), allocatable    :: ret
            character(7), allocatable               :: inp1(:), inp2(:)

            integer(2) :: pos = (0, 0)

            integer, dimension(2) :: test
            test = parser(inp1(1))




            allocate(ret(2,2))
        end function find_int

        function parser(inp) result(ret)
            implicit none
            character, dimension(7) :: inp
            integer, dimension(2) :: ret
            ret(1) = int(inp(1))
            ret(2) = 1

            ! select case inp(1)
            !     case default
            !         print*, "test"
            !     case ("U")
            !         ret(1) = 1
            !     case ("R")
            !         ret(1) = 2
            !     case ("D")
            !         ret(1) = 3
            !     case ("L")
            !         ret(1) = 4
            ! end select
        end function parser 

        function day3a(input)
            implicit none

            integer, dimension(:,:), allocatable    :: input

            integer :: day3a

            day3a = 2
        end function day3a

        function day3b(input, inp1, inp2)
            implicit none

            integer, dimension(:,:), allocatable    :: input
            character(7), allocatable               :: inp1(:), inp2(:)

            integer :: day3b

            day3b = 2
        end function day3b
end module day3