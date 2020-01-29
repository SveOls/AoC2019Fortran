program advent_of_code_2019
    use day1

    use day3
    use day4
    
    use day6
    implicit none
    integer :: sel, i, times
    real :: starts, stops
    print*, "select day, integer values 1-25"
    read *,sel
    print*, "select number of times you want it run, 1-intmax. Only 1 prints results."
    read *,times


    call cpu_time(starts)
    select case (times)
        case (:1)
            call do_once(sel)
        case default
            call do_it(times, sel)
    end select

    call cpu_time(stops)
    print*, "time: ", stops - starts, " seconds"

    contains 
        subroutine do_once(sel)
            integer, intent(in) :: sel
            select case (sel)
            case default
            print*, "default"
            case (1)
                print*, day1all()
            case (2)
                print*, "2"
            case (3)
                print*, day3all()
            case (4)
                print*, day4all()
            case (5)
                print*, "5"
            case (6)
                print*, day6all()
            end select
        end subroutine do_once
        subroutine do_it(inp, sel)
            integer, intent(in) :: inp, sel
            do i = 0, inp
            select case (sel)
                case default
                    print*, "default"
                case (1)
                    call day1timer()
                case (2)
                    print*, "2"
                case (3)
                    call day3timer()
                case (4)
                    call day4timer()
                case (5)
                    print*, "5"
                case (6)
                    call day6timer()
            end select
            end do
        end subroutine do_it
end program advent_of_code_2019
