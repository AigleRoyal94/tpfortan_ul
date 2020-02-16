program test
  implicit none
  double precision::resultat
  integer::cas
  integer, parameter::CARRE=0, RECTANGLE=1, PR=2, PL=3, LOSANGE=4

  print*, "Programme de calcul d'air d'une figure géométrique"
  print*, "Choisissez le numéro de la figure et entrer les valeurs demander"
  print*, '0 - Carré 1 - Rectangle 2 - Parallélépipède rectangle'
  print*, "3 - Parallélogramme, 4 - Losange"

  read*, cas

  select case(cas)
  case (CARRE)
    print*, "Calcul de l'aire d'un carré"
    call calculairecarre(resultat)
    print*, "Aire du carré = ", resultat
  case (RECTANGLE)
    print*, "Calcul de l'aire d'un rectangle"
    call calculAireRectangle(resultat)
    print*, "Aire du rectangle = ", resultat
  case (PR)
    print*, "Calcul de l'air d'un Parallélépipède rectangle"
  case (PL)
    print*, "Calcul de l'air d'un Parallélogramme"
  case (LOSANGE)
    print*, "Calcul de l'air d'un losange"
  case default
    print*, "default case"
  end select

  contains
  subroutine calculairecarre(surface)
    implicit none
    double precision::cote
    double precision, intent(out)::surface

    print*, "Entrer la valeur du coté du carré"
    read*, cote

    if (cote < 0) then
      print*, "Le côté d'un carré ne peut être négatif."
    else
      surface = cote * cote
    end if 
  end subroutine calculairecarre

  subroutine calculAireRectangle(surface)
    implicit none
    double precision::largeur, longueur
    double precision, intent(out)::surface

    write(*,*) "Entrer la valeur de la largeur >>"
    read*, largeur
    write(*,*) "Entrer la valeur de la longueur >>"
    read*, longueur

    if (largeur < 0 .or. longueur <0) then
      print*, "La valeur de la longueur ou de la largeur ne peut être négative"
    else
      surface = largeur * longueur
    end if
  end subroutine calculAireRectangle

end program test
