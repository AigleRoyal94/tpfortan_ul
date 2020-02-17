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
    print*, "L'aire du carré = ", resultat
  case (RECTANGLE)
    print*, "Calcul de l'aire d'un rectangle"
    call calculAireRectangle(resultat)
    print*, "L'aire du rectangle = ", resultat
  case (PR)
    print*, "Calcul de l'aire d'un Parallélépipède rectangle"
    call calculAirePR(resultat)
    print*, "L'aire du parallélépipède rectangle = ", resultat
  case (PL)
    print*, "Calcul de l'aire d'un Parallélogramme"
    call calculAirePL(resultat)
    print*, "L'aire du parallélograme = ", resultat
  case (LOSANGE)
    print*, "Calcul de l'air d'un losange"
    call calculAireLosange(resultat)
    print*, "L'aire du losange = ", resultat
  case default
    print*, "Vous n'êtes dans aucun cas concerné"
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

  subroutine calculAirePR(surface)
    implicit none
    double precision::largeur, longueur, hauteur
    double precision, intent(out):: surface
    print*, "Entrer la valeur de la largeur"
    read*, largeur
    print*, "Entrer la valeur de la longueur"
    read*, longueur
    print*, "Entrer la valeur de la hauteur"
    read*, hauteur

    if (largeur < 0 .or. longueur < 0 .or. hauteur < 0) then
      print*, "Ni la largeur ni la longueur ni la hauteur ne peut avoir une valeur négative"
    else
      surface = 2*hauteur*largeur + 2*hauteur*longueur + 2*largeur*longueur
    end if
  end subroutine calculAirePR

  subroutine calculAirePL(surface)
    implicit none
    double precision::base, hauteur
    double precision, intent(out)::surface
    print*, "Entrer la valeur de la base"
    read*, base
    print*, "Entrer la valeur de la hauteur"
    read*, hauteur
    if (base < 0 .or. hauteur < 0) then
      print*, "Ni la base ni la hauteur ne peut être négatif"
    else
      surface = base * hauteur
    end if
  end subroutine calculAirePL

  subroutine calculAireLosange(surface)
    implicit none
    double precision::diagonal_1, diagonal_2
    double precision, intent(out)::surface
    print*, "Entrer la valeur de la petite diagonale"
    read*, diagonal_1
    print*, "Entrer la valeur de la grande diagonale"
    read*, diagonal_2
    if (diagonal_1 < 0 .or. diagonal_2 < 0) then
      print*, "Aucune diagonale ne doit être négatif"
    else
      surface = (diagonal_1 * diagonal_2)/2
    end if
  end subroutine calculAireLosange
end program test
