program plusOuMoins
    ! In future version, we should ask the user to enter the interval
    ! between the random number will be choose.
    implicit none
    integer, parameter::bornInf = 1, bornSup = 50, compteur_Max = 5
    integer::guess, generate, compteur

    ! Generate the number between 1 and 50
    generate = random()
    print*, "Deviner le numéro tiré au hasard"
    
    do compteur = bornInf, compteur_Max
        read*, guess
        if (guess == generate) then
            print*, "Vous avez réussi après ", compteur, " coups"
            exit
        else if (guess < generate) then
            print*, "Plus haut"
        else
            print*, "Plus bas"
        end if
        
        if (compteur == compteur_Max) then
            print*, "Vous avez échoué. Le numéro est : ", generate
        end if
    end do

    contains

    ! There is a big issue with the function.
    ! It's very difficult to generate a random number with fortran
    ! In future version, we should change the signature of the function
    ! It should takes as argument the min and the max of the interval
    function random()
        implicit none
        integer, allocatable :: seed(:)
        integer :: n, random
      
        call random_seed(size = n)
        allocate(seed(n))
        call random_seed(get=seed)
        call srand(seed(6))
        random = int(rand() * (50 -1)) + 1
      end function random
    
end program plusOuMoins
