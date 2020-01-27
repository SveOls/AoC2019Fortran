    module day1
    implicit none 

    contains
        subroutine day1timer()
            integer, dimension(2) :: temp
            temp = day1all()
        end subroutine day1timer

        function day1all()
            implicit none
            integer, dimension(2) :: day1all

            day1all(1) = day1a() 
            day1all(2) = day1b()

        end function day1all

        function day1a()
            implicit none
            integer :: reader, sum = 0, day1a
            open (unit = 1, file = "input//day1.txt")

            do
100             read(1,*, end = 110) reader
                sum = sum + sum_a(reader)
                go to 100
110             exit
            end do

            close(1)
            day1a = sum
        end function day1a

        function sum_a(inp)
            integer :: inp, sum_a
            sum_a = inp / 3 - 2
        end function sum_a


        function day1b()
            implicit none
            integer :: reader, fuel, sum = 0, day1b
            open (unit = 1, file = "input//day1.txt")

            do
120             read(1,*, end = 130) reader
                fuel = sum_a(reader)
                sum = sum + fuel

                do
                    fuel = sum_a(fuel)
                    if ( fuel < 1 ) then
                    exit
                    end if
                    sum = sum + fuel
                end do

                go to 120
130             exit
            end do

            close(1)
            day1b = sum
        end function day1b

end module day1