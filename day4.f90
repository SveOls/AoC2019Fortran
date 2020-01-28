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
            
            integer, dimension(6)    :: start
            integer, dimension(5000) :: filter
            integer                  :: a, b, coun = 0, q, w, e, r, t, y
            
            start(1) = a / 100000
            start(2) = a / 10000 - 10 * start(1)
            start(3) = a / 1000 - 10 * (start(2) + 10 * start(1))
            start(4) = a / 100 - 10 * (start(3) + 10 * (start(2) + 10 * start(1)))
            start(5) = a / 10 - 10 * (start(4) + 10 * (start(3) + 10 * (start(2) + 10 * start(1))))
            start(6) = a - 10 * (start(5) + 10 * (start(4) + 10 * (start(3) + 10 * (start(2) + 10 * start(1)))))

            print*, start

            do q = start(1), 9
                do w = start(2), 9
                    do e = start(3), 9
                        do r = start(4), 9
                            do t = start(5), 9
                                do y = start(6), 9
                                end do
                                start(6) = 1
                            end do
                            start(5) = 1
                        end do
                        start(4) = 1
                    end do
                    start(3) = 1
                end do
                start(2) = 1
                1111 exit
            end do

            filter(1) = a
            filter(2) = b

        end function filter
end module day4