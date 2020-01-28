module day4
    implicit none 

    contains
        subroutine day4timer()
            implicit none

            integer, dimension(2) :: temp
            temp = day4all()
        end subroutine day4timer

        function day4all()
            implicit none
            
            integer, dimension(2)    :: day4all
            integer, dimension(5000) :: dats
            integer                  :: a = 123257, b = 647015
            dats = filter(a, b)

            day4all(1) = day4a()
            day4all(2) = day4b()
        end function day4all

        function day4a()
            implicit none
            
            integer :: day4a
            day4a = 2
        end function day4a

        function day4b()
            implicit none

            integer :: day4b
            day4b = 2
        end function day4b

        function filter(a, b)
            implicit none
            
            integer, dimension(6)    :: start, temp
            integer, dimension(5000) :: filter
            integer                  :: a, b, coun = 0, num, q, w, e, r, t, y, verytempvalue = 0
            
            num = a
            
            start(1) = (a - mod(a, 100000)) / 100000
            start(2) = (a - mod(a, 10000)  - 100000 * start(1)) / 10000
            start(3) = (a - mod(a, 1000)   - 100000 * start(1) - 10000 * start(2)) / 1000
            start(4) = (a - mod(a, 100)    - 100000 * start(1) - 10000 * start(2) - 1000 * start(3)) / 100
            start(5) = (a - mod(a, 10)     - 100000 * start(1) - 10000 * start(2) - 1000 * start(3) - 100 * start(4)) / 10
            start(6) = a                   - 100000 * start(1) - 10000 * start(2) - 1000 * start(3) - 100 * start(4) - 10 * start(5)

            outer: do q = start(1), 9
                do w = start(2), 9
                    do e = start(3), 9
                        do r = start(4), 9
                            do t = start(5), 9
                                do y = start(6), 9
                                    temp = (/ q,w,e,r,t,y /)
                                    if (check_number(temp)) then
                                        verytempvalue = verytempvalue + 1
                                        coun = coun + 1
                                        filter(coun) = 100000 * q + 10000 * w + 1000 * e + 100 * r + 10 * t + y
                                    end if
                                    
                                    if (num == b) exit outer
                                    num = num + 1
                                end do
                                start(6) = 0
                            end do
                            start(5) = 0
                        end do
                        start(4) = 0
                    end do
                    start(3) = 0
                end do
                start(2) = 0
            end do outer

            filter(1) = a
            filter(2) = b

        end function filter

        function check_number(inp)
            implicit none

            integer, dimension(6) :: inp
            logical :: check_number, rising = .true., equals = .false.
            integer :: prev, i

            rising = .true.
            equals = .false.

            prev = inp(1)
            do i = 2, 6
                ! print*, prev, inp(i), rising, equals
                if (inp(i) < prev)  rising = .false.
                if (inp(i) == prev) equals = .true.
                prev = inp(i)
            end do

            check_number = rising .and. equals
        end function check_number
end module day4