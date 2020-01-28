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

            deallocate(cross, inp1, inp2)
        end function day3all

        function find_int(inp1, inp2) result(ret)
            implicit none
            
            integer, dimension(:,:), allocatable :: ret, firs, sec
            character(len=7), allocatable        :: inp1(:), inp2(:)
            integer, dimension(1024, 2)          :: tempst

            integer, dimension(2) :: test, pos
            integer               :: i, j, k, l = 1
            character             :: a, b
            logical               :: c

            a = inp1(1)(1:1)
            b = inp2(1)(1:1)

            ! Gives c a value; if true, a and b start out perpendicular. else, parallell. 
            if (a == "U" .or. a == "D") then
                if (b == "U" .or. b == "D") then
                    c = .false.
                else
                    c = .true.
                end if 
            else
                if (b == "R" .or. b == "L") then
                    c = .false.
                else
                    c = .true.
                end if
            end if
            
            allocate(firs(size(inp1),3), sec(size(inp2),3))

            pos = (/0, 0/)
            do k = 1, size(inp1)
                test = parser(inp1(k))
                select case (test(1))
                    case (2)
                        firs(k, 1) = pos(2)
                        firs(k, 3) = pos(1)
                        pos(2) = pos(2) + test(2)
                        firs(k, 2) = pos(2)
                    case (1)
                        firs(k, 1) = pos(1)
                        firs(k, 3) = pos(2)
                        pos(1) = pos(1) + test(2)
                        firs(k, 2) = pos(1)
                end select
            end do

            pos = (/0, 0/)
            do k = 1, size(inp2)
                test = parser(inp2(k))
                select case (test(1))
                    case (2)
                        sec(k, 1) = pos(2)
                        sec(k, 3) = pos(1)
                        pos(2) = pos(2) + test(2)
                        sec(k, 2) = pos(2)
                    case (1)
                        sec(k, 1) = pos(1)
                        sec(k, 3) = pos(2)
                        pos(1) = pos(1) + test(2)
                        sec(k, 2) = pos(1)
                end select
            end do

            ! Summary: firs = start+end point for first wire, plus static x/y coordinate.
            ! Same for sec for the second. Then, c sees whether or not you can compare evens.
            ! This comes from the fact that each wire alternates between vertical and horizontal.
            ! If true, 1 compares with 1, 3, 5, etc. 
            do i = 1, size(inp1)
                if (xor(c, mod(i, 2) == 1)) then  ! when c = true: allow i = even. else, odd.
                    ! check 2-top
                    do j = 2, size(inp2), 2
                        if (check_cross(firs(i, :), sec(j, :))) then
                            tempst(l, :) = (/ firs(i, 3), sec(j, 3) /) 
                            l = l + 1
                        end if
                    end do
                else
                    ! check 1-top
                    do j = 1, size(inp2), 2
                        if (check_cross(firs, sec)) then
                            tempst(l, :) = (/ firs(i, 3), sec(j, 3) /) 
                            l = l + 1
                        end if
                    end do
                end if 
            end do

            allocate(ret(l - 1, 2))
            do i = 1, l - 1
                ret(i, :) = tempst(i, :)
            end do

            deallocate(firs, sec)
        end function find_int

        function check_cross(a, b) result(ret)
            implicit none

            logical               :: ret 
            integer, dimension(3) :: a, b

            if ((a(1) - b(3)) * (a(2) - b(3)) <= 0) then
                if ((b(1) - a(3)) * (b(2) - a(3)) <= 0) then
                    ret = .true.
                end if
            else 
                ret = .false.
            end if 

        end function check_cross

        function parser(inp) result(ret)
            implicit none

            character(len=7)      :: inp
            integer, dimension(2) :: ret

            read(inp(2:),*) ret(2)

            select case (inp(1:1))
                case default
                    print*, "ERROR ERROR"
                case ("U")
                    ret(1) = 2
                case ("R")
                    ret(1) = 1
                case ("D")
                    ret(1) = 2
                    ret(2) = -ret(2)
                case ("L")
                    ret(1) = 1
                    ret(2) = -ret(2)
            end select

            ! print*, ret
        end function parser

        function day3a(input)
            implicit none

            integer, dimension(:,:), allocatable :: input

            integer :: day3a, i, temp

            day3a = abs(input(i, 1)) + abs(input(i, 2))
            do i = 2, size(input) / 2
                temp = abs(input(i, 1)) + abs(input(i, 2))
                if (temp < day3a .and. temp /= 0) then
                    day3a = temp
                end if
            end do
        end function day3a

        function day3b(input, inp1, inp2)
            implicit none

            integer, dimension(:,:), allocatable    :: input
            character(7), allocatable               :: inp1(:), inp2(:)

            integer :: day3b

            day3b = 2
        end function day3b
end module day3