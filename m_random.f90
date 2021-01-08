module m_random

    implicit none

contains

    subroutine init_random_seed()
        implicit none
        integer, allocatable :: seed(:)
        integer :: i, n, un, istat, dt(8), pid, t(2), s
        integer(8) :: count, tms

        call random_seed(size = n)
        allocate(seed(n))
        ! First try if the OS provides a random number generator
        open(newunit=un, file="/dev/urandom", access="stream", &
            form="unformatted", action="read", status="old", iostat=istat)
        if (istat == 0) then
            read(un) seed
            close(un)
        else
            ! Fallback to XOR:ing the current time and pid. The PID is
            ! useful in case one launches multiple instances of the same
            ! program in parallel.
            call system_clock(count)
            if (count /= 0) then
                t = transfer(count, t)
            else
                call date_and_time(values=dt)
                tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
                    + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
                    + dt(3) * 24 * 60 * 60 * 60 * 1000 &
                    + dt(5) * 60 * 60 * 1000 &
                    + dt(6) * 60 * 1000 + dt(7) * 1000 &
                    + dt(8)
                t = transfer(tms, t)
            end if
            s = ieor(t(1), t(2))
            pid = getpid() + 1099279 ! Add a prime
            s = ieor(s, pid)
            if (n >= 3) then
                seed(1) = t(1) + 36269
                seed(2) = t(2) + 72551
                seed(3) = pid
                if (n > 3) then
                    seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
                end if
            else
                seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
            end if
        end if
        call random_seed(put=seed)
    end subroutine init_random_seed

    subroutine shuffle(x)

        real(8) :: vec(6), temp
        real(8) :: x_temp
        real(8) :: x(6)
        integer :: bubble, lsup, j

        call random_number(vec)

        lsup = 6 !lsup is the size of the array to be used

        do while (lsup > 1)
            bubble = 0 !bubble in the greatest element out of order
            do j = 1, (lsup-1)
                if (vec(j) > vec(j+1)) then

                    temp = vec(j)
                    vec(j) = vec(j+1)
                    vec(j+1) = temp

                    x_temp = x(j)
                    x(j) = x(j+1)
                    x(j+1) = x_temp

                    bubble = j

                endif
            enddo
            lsup = bubble
        enddo

    end subroutine

    subroutine shuffle_letter(x)

        real(8) :: vec(10), temp
        character(1) :: x_temp
        character(1) :: x(10)
        integer :: bubble, lsup, j

        call random_number(vec)

        lsup = 10 !lsup is the size of the array to be used

        do while (lsup > 1)
            bubble = 0 !bubble in the greatest element out of order
            do j = 1, (lsup-1)
                if (vec(j) > vec(j+1)) then

                    temp = vec(j)
                    vec(j) = vec(j+1)
                    vec(j+1) = temp

                    x_temp = x(j)
                    x(j) = x(j+1)
                    x(j+1) = x_temp

                    bubble = j

                endif
            enddo
            lsup = bubble
        enddo

    end subroutine

end module
