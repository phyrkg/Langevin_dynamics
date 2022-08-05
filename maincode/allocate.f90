subroutine allocate
  use parameters
  implicit none
  !allocate(no(alcnox1:alcnox2,alcnoy1:alcnoy2),stat=ok)
  !allocate(partindx(alcnox1:alcnox2,alcnoy1:alcnoy2,1:npartcell),ipartindx(maxpart,3),stat=ok)

  maxpart=na+np

  allocate(act_ples(na,6))!,stat=ok)
  allocate(pass_ples(np,6))!,stat=ok)

  !allocate(vx(maxpart),vy(maxpart),stat=ok)

  !allocate(x(maxpart),y(maxpart),stat=ok)

  !allocate(w(maxpart),phi(maxpart),stat=ok)

  allocate(Fxas(na),Fyas(na))!,stat=ok)

  allocate(Fxps(np),Fyps(np))!,stat=ok)

  allocate(tauap(na),taupp(np))!,stat=ok)


end subroutine allocate
