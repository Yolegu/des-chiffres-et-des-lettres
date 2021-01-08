module m_chiffres_lettres
    use m_random
    implicit none

    integer, parameter :: n_tuiles_sac = 24
    integer, parameter :: n_tuiles_main = 6

    character(50), allocatable :: word(:)
    integer, allocatable :: word_len(:)
    integer, allocatable :: mot_occur(:,:)

contains

    subroutine tirage(tuiles_main, objectif)

        real(8) :: objectif
        real(8) :: tuiles_sac(n_tuiles_sac)
        real(8) :: tuiles_main(n_tuiles_main)

        real(8) :: r(n_tuiles_main)
        integer :: tuile_num(n_tuiles_main)

        integer :: i, j

        tuiles_sac(1) = 1.d0
        tuiles_sac(2) = 2.d0
        tuiles_sac(3) = 3.d0
        tuiles_sac(4) = 4.d0
        tuiles_sac(5) = 5.d0
        tuiles_sac(6) = 6.d0
        tuiles_sac(7) = 7.d0
        tuiles_sac(8) = 8.d0
        tuiles_sac(9) = 9.d0
        tuiles_sac(10) = 10.d0

        tuiles_sac(11) = 1.d0
        tuiles_sac(12) = 2.d0
        tuiles_sac(13) = 3.d0
        tuiles_sac(14) = 4.d0
        tuiles_sac(15) = 5.d0
        tuiles_sac(16) = 6.d0
        tuiles_sac(17) = 7.d0
        tuiles_sac(18) = 8.d0
        tuiles_sac(19) = 9.d0
        tuiles_sac(20) = 10.d0

        tuiles_sac(21) = 25.d0
        tuiles_sac(22) = 50.d0
        tuiles_sac(23) = 75.d0
        tuiles_sac(24) = 100.d0


        l_pioche: do

            call random_number(r)
            tuile_num = int(r * n_tuiles_sac)

            if (any(tuile_num == 0)) then
                cycle
            end if

            do i = 1, n_tuiles_main
                do j = 1, n_tuiles_main
                    if (j == i) then
                        cycle
                    elseif (tuile_num(i) == tuile_num(j)) then
                        cycle l_pioche
                    end if
                end do
            end do

            exit

        end do l_pioche

        do i = 1, n_tuiles_main
            tuiles_main(i) = tuiles_sac(tuile_num(i))
        end do

        call random_number(objectif)
        objectif = int((999.d0 - 101.d0) * objectif + 101.d0)

    end subroutine

    subroutine make_rand_calc(tuiles, x, seq)

        real(8) :: tuiles(n_tuiles_main)
        real(8) :: x
        real(8) :: r
        integer :: oper, n_oper
        integer :: i
        character(3) :: valeur
        character(200) :: seq

        call shuffle(tuiles)

        call random_number(r)
        n_oper = int(r * 5.d0 + 1.d0)

        seq = "= "//repeat("(", n_oper)
        x = tuiles(1)

        write(valeur, "(I0)")int(tuiles(1))
        seq = trim(seq)//trim(valeur)

        do i = 1, n_oper

            call random_number(r)
            oper = int(r * 4.d0 + 1.d0)

            write(valeur, "(I0)")int(tuiles(i+1))

            if (oper == 1) then
                x = x + tuiles(i+1)
                seq = trim(seq)//"+"
            elseif (oper == 2) then
                x = x - tuiles(i+1)
                seq = trim(seq)//"-"
            elseif (oper == 3) then
                x = x * tuiles(i+1)
                seq = trim(seq)//"*"
            elseif (oper == 4) then
                x = x / tuiles(i+1)
                seq = trim(seq)//"/"
            else
                stop "Unknown operation"
            end if

            if (x < 0.d0 .or. abs(x - int(x)) > 1.d-6) then
                x = -1.d0
                exit
            end if

            seq = trim(seq)//trim(valeur)//")"

        end do

    end subroutine

    subroutine load_dico(n_word)

        integer :: ioerr
        integer :: i, j, n_word

        open(20, file = "liste_francais.txt", status = "old")

        read(20,*)

        n_word = 0

        do

            read(20,*,iostat = ioerr)
            if (ioerr /= 0) then
                exit
            else
                n_word = n_word + 1
            end if

        end do

        rewind(20)

        allocate(word(n_word))
        allocate(word_len(n_word))
        allocate(mot_occur(n_word, 26))

        read(20,*)

        do i = 1, n_word

            read(20,*)word(i), word_len(i), (mot_occur(i, j), j = 1, 26)

        end do

        close(20)

    end subroutine

    subroutine tirage_lettres(lettre, tirage_occur)

        integer, parameter :: n_voyelles = 4
        integer, parameter :: n_lettres = 10
        integer, parameter :: n_consonnes = n_lettres - n_voyelles
        real(8) :: r_voyelles(n_voyelles)
        real(8) :: r_consonnes(n_consonnes)
        character(1) :: consonne(n_consonnes)
        character(1) :: voyelle(n_voyelles)
        character(1) :: lettre(n_lettres)
        integer :: i
        integer :: tirage_occur(26)

        tirage_occur = 0

        call random_number(r_voyelles)

        do i = 1, n_voyelles

            if (r_voyelles(i) < 0.3383d0) then
                voyelle(i) = "e"
                tirage_occur(5) = tirage_occur(5) + 1
            elseif (r_voyelles(i) < 0.5370d0) then
                voyelle(i) = "a"
                tirage_occur(1) = tirage_occur(1) + 1
            elseif (r_voyelles(i) < 0.7213d0) then
                voyelle(i) = "i"
                tirage_occur(9) = tirage_occur(9) + 1
            elseif (r_voyelles(i) < 0.8616d0) then
                voyelle(i) = "o"
                tirage_occur(15) = tirage_occur(15) + 1
            elseif (r_voyelles(i) < 0.9871d0) then
                voyelle(i) = "u"
                tirage_occur(21) = tirage_occur(21) + 1
            elseif (r_voyelles(i) < 1.d0) then
                voyelle(i) = "y"
                tirage_occur(25) = tirage_occur(25) + 1
            end if

        end do


        call random_number(r_consonnes)

        do i = 1, n_consonnes

            if (r_consonnes(i) < 0.1315d0) then
                consonne(i) = "s"
                tirage_occur(19) = tirage_occur(19) + 1
            elseif (r_consonnes(i) < 0.2607d0) then
                consonne(i) = "n"
                tirage_occur(14) = tirage_occur(14) + 1
            elseif (r_consonnes(i) < 0.3833d0) then
                consonne(i) = "r"
                tirage_occur(18) = tirage_occur(18) + 1
            elseif (r_consonnes(i) < 0.5029d0) then
                consonne(i) = "t"
                tirage_occur(20) = tirage_occur(20) + 1
            elseif (r_consonnes(i) < 0.6032d0) then
                consonne(i) = "l"
                tirage_occur(12) = tirage_occur(12) + 1
            elseif (r_consonnes(i) < 0.6773d0) then
                consonne(i) = "d"
                tirage_occur(4) = tirage_occur(4) + 1
            elseif (r_consonnes(i) < 0.7416d0) then
                consonne(i) = "c"
                tirage_occur(3) = tirage_occur(3) + 1
            elseif (r_consonnes(i) < 0.7945d0) then
                consonne(i) = "m"
                tirage_occur(13) = tirage_occur(13) + 1
            elseif (r_consonnes(i) < 0.8448d0) then
                consonne(i) = "p"
                tirage_occur(16) = tirage_occur(16) + 1
            elseif (r_consonnes(i) < 0.8697d0) then
                consonne(i) = "g"
                tirage_occur(7) = tirage_occur(7) + 1
            elseif (r_consonnes(i) < 0.8927d0) then
                consonne(i) = "b"
                tirage_occur(2) = tirage_occur(2) + 1
            elseif (r_consonnes(i) < 0.9151d0) then
                consonne(i) = "f"
                tirage_occur(6) = tirage_occur(6) + 1
            elseif (r_consonnes(i) < 0.9376d0) then
                consonne(i) = "h"
                tirage_occur(8) = tirage_occur(8) + 1
            elseif (r_consonnes(i) < 0.9600d0) then
                consonne(i) = "v"
                tirage_occur(22) = tirage_occur(22) + 1
            elseif (r_consonnes(i) < 0.9731d0) then
                consonne(i) = "q"
                tirage_occur(17) = tirage_occur(17) + 1
            elseif (r_consonnes(i) < 0.9808d0) then
                consonne(i) = "x"
                tirage_occur(24) = tirage_occur(24) + 1
            elseif (r_consonnes(i) < 0.9877d0) then
                consonne(i) = "j"
                tirage_occur(10) = tirage_occur(10) + 1
            elseif (r_consonnes(i) < 0.9935d0) then
                consonne(i) = "k"
                tirage_occur(11) = tirage_occur(11) + 1
            elseif (r_consonnes(i) < 0.9970d0) then
                consonne(i) = "w"
                tirage_occur(23) = tirage_occur(23) + 1
            elseif (r_consonnes(i) < 1.d0) then
                consonne(i) = "z"
                tirage_occur(26) = tirage_occur(26) + 1
            end if

        end do

        do i = 1, n_consonnes

            lettre(i) = consonne(i)

        end do

        do i = 1, n_voyelles

            lettre(n_consonnes + i) = voyelle(i)

        end do

        call shuffle_letter(lettre)

    end subroutine

end module

program main
    use m_chiffres_lettres
    implicit none

    real(8) :: tuiles(n_tuiles_main)
    real(8) :: obj
    real(8) :: x
    integer :: i, j
    character(200) :: seq
    character(1) :: lettre(10)
    integer :: n_word, n_sol, max_len
    integer :: tirage_occur(26)
    character(50), allocatable :: lettre_solutions(:), lettre_solutions_temp(:)
    character(1) :: cl
    character(100) :: sol
    integer :: valeur
    integer :: ioerr
    integer :: n_sol_print

    call load_dico(n_word)

    write(*,*)"---------------------------"
    write(*,*)"DES CHIFFRES ET DES LETTRES"
    write(*,*)"---------------------------"

    do

        write(*,*)
        write(*,"(A,$)")"Chiffres (C) ou Lettres (L) ? (X to Exit)"
        read(*,*)cl
        call execute_command_line("cls")

        selectcase(cl)

        case("l","L")

            write(*,*)"---------------------------"
            write(*,*)"DES CHIFFRES ET DES LETTRES"
            write(*,*)"---------------------------"

            call tirage_lettres(lettre, tirage_occur)
            max_len = 0

            if (allocated(lettre_solutions)) deallocate(lettre_solutions)

            write(*,*)
            write(*,*)"Tirage : ", lettre

            l_word: do i = 1, n_word

                l_letter: do j = 1, 26

                    if (tirage_occur(j) < mot_occur(i,j)) then

                        cycle l_word

                    end if

                end do l_letter

                if (allocated(lettre_solutions)) then

                    n_sol = size(lettre_solutions)

                    if (allocated(lettre_solutions_temp)) then
                        deallocate(lettre_solutions_temp)
                    end if

                    allocate(lettre_solutions_temp(n_sol))
                    lettre_solutions_temp = lettre_solutions

                    deallocate(lettre_solutions)

                    allocate(lettre_solutions(n_sol + 1))
                    lettre_solutions(1:n_sol) = lettre_solutions_temp

                else

                    n_sol = 0
                    allocate(lettre_solutions(n_sol + 1))

                end if

                lettre_solutions(n_sol + 1) = word(i)

            end do l_word

            max_len = maxval(len_trim(lettre_solutions))

            write(*,*)
            write(*,*)"Montrer les solution (Entree)"
            read(*,*)

            do i = 1, size(lettre_solutions)
                if (len_trim(lettre_solutions(i)) == (max_len)) then
                    write(*,*)trim(lettre_solutions(i)), len_trim(lettre_solutions(i))
                end if
            end do

            do i = 1, size(lettre_solutions)
                if (len_trim(lettre_solutions(i)) == (max_len-1)) then
                    write(*,*)trim(lettre_solutions(i)), len_trim(lettre_solutions(i))
                end if
            end do

        case("c", "C")

            write(*,*)"---------------------------"
            write(*,*)"DES CHIFFRES ET DES LETTRES"
            write(*,*)"---------------------------"

            call tirage(tuiles, obj)

            open(20, file = "res.dat")
            write(20,*)"Valeurs des tuiles : ", int(tuiles)
            write(20,*)"Objectif : ", int(obj)
            write(20,*)

            write(*,*)
            write(*,*)"Tirage : ", int(tuiles)
            write(*,*)"Objectif : ", int(obj)
            write(*,*)

            i = 0
            do
                call make_rand_calc(tuiles, x, seq)
                if (x > 100.d0 .and. x < 1000.d0 .and. abs(x - int(x)) < 1.d-6) then
                    write(20,"(I0, T5, 2A)")int(x), " ", trim(seq)
                    i = i + 1
                    if (i > 1e5) exit
                end if
            end do

            close(20)

            write(*,*)
            write(*,*)"Montrer les solution (Entree)"
            read(*,*)

            open(20, file = "res.dat", status = "old")

            read(20,*)
            read(20,*)
            read(20,*)

            n_sol_print = 0

            do

                read(20,*, iostat = ioerr)valeur

                if (ioerr /= 0) then
                    exit
                end if

                if (valeur == obj) then

                    n_sol_print = n_sol_print + 1
                    backspace(20)
                    read(20,"(A)")sol
                    write(*,*)trim(sol)
                    if (n_sol_print > 10) exit
                end if

            end do

            close(20)

        case("x","X")

            exit

        case default

        end select

    end do

end
