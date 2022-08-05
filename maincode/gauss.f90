!The gaussian random number used all along the code!!!

subroutine gauss(gau1,gau2)
	use parameters
	use mt_stream
  implicit none
  double precision gau1,gau2,fac,gset,rsq,v1,v2
1 gau1=genrand_double1(mts(1))
  gau2=genrand_double1(mts(1))
  v1 = 2.d0*gau1 - 1.d0
  v2 = 2.d0*gau2 - 1.d0
	rsq = v1*v1 + v2*v2
	if(rsq.ge.1.d0.or.rsq.eq.0.d0) goto 1
  fac = sqrt(-2.d0*log(rsq)/rsq)
  gau1 = v1*fac
  gau2 = v2*fac
end subroutine gauss
