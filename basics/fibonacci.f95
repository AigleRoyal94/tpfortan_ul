program main_fibonacci
    implicit none
    integer::n, output, rec

    print*, "Donner la valeur de n"
    read*, n
    output = fibonacci(n)
    print*, "La somme de fibonacci pour n = ", n, "est S = ", output
    rec = recursive_fibonacci(n)
    print*, "La somme de fibonacci pour n = ", n, "est S = ", rec

    contains
    function fibonacci(n)
        implicit none
        integer::fibonacci
        integer, intent(in)::n
        integer::i = 0, j = 1, k, l

        if (n < 0) then
            print*, "Donnée en entrée incorrect"
            fibonacci = -1
        else if (n == 0) then
            fibonacci = i
        else if (n == 1) then
            fibonacci = j
        else
            do k = 1, n
                l = i + j
                i = j
                j = l
            end do
            fibonacci = i
        end if
    end function fibonacci

    recursive function recursive_fibonacci(n) result(output)
        implicit none
        integer::output
        integer, intent(in)::n

        if (n == 0) then
            output = 0
        else if (n == 1) then
            output = 1
        else
            output = recursive_fibonacci(n-1) + recursive_fibonacci(n-2)
        end if
    end function recursive_fibonacci

    !will add something new
end program main_fibonacci
