!=DE!K TRIG6MSTIF
!=PURPOSE Form stiffness of 6-node plane-stress triangle
!=AUTHOR !. A. Felippa, February 1967
!=VERSION May 1982 (Fortran 77)
!=EQUIPMENT Ma!hine independent
!=KEYWORDS plane stress six node triangle
!=KEYWORDS finite element stiffness matrix
!=BLOCK ABSTRACT
!
! TRIG6MSTIF forms the element stiffness matrix of a
! six-node triangle in plane stress.
!
!=END ABSTRACT
!=BLOCK USAGE
!
! The calling sequence is
!
! !ALL TRIG6MSTIF (OPT, X, Y, H, C, P, SM, M, STATUS)
!
! where the input arguments are
!
! OPT Option letter argument, presently ignored.
! X (3 x 1) array of x coordinates of triangle nodes
! Y (3 x 1) array of y coordinates of triangle nodes
! H (3 x 1) array of thicknesses at triangle nodes
! C (3 x 3) plane stress constitutive material matrix (not
! integrated thorugh the thickness)
! P Identifies quadrature rule by value and sign
! (see TRIGGAUSSQ)
! M First dimension of SM in calling program.
!
! The outputs are:
!
! SM (6 x 6) computed element stiffness matrix. The
! arrangement of rows and columns pertains to node
! displacements arranged in the order
! (vx1, vy1, vx2, ... vy6)
! This particular ordering is obtained by setting array
! LS to 1,3,5,7, ... 12 (see DATA statement below)
! STATUS Status character variable. Blank if no error detected.
!
!=END USAGE
!=BLOCK FORTRAN
subroutine TRIG6MSTIF(opt, x, y, h, c, p, sm, m, status)
!
! ARGUMENTS
!
character*(*) opt, status
integer p, m
double precision x(6), y(6), h(6), c(3,3)
double precision sm(m,m)
!
! LOCAL VARIABLES
!
double precision q(6), qx(6), qy(6)
double precision zeta1, zeta2, zeta3, det, w, weight
double precision c1x, c1y, c2x, c2y, c3x, c3y
integer i, ix, iy, j, jx, jy, k, ls(12)
data ls /1,3,5,7,9,11, 2,4,6,8,10,12/
!
! LOGIC
!
status = ' '
do 1200 j = 1,12
do 1100 i= 1,12
sm(i,j) = 0.0
1100 continue
1200 continue
!
do 3000 k = 1,abs(p)
call TRIGGAUSSQ (p, k, zeta1, zeta2, zeta3, weight)
call TRIG6SHAPE (' ', zeta1,zeta2,zeta3, x,y, q,qx,qy, det)
if (det .le. 0.0) then
status = 'Negative Jacobian determinant'
if (det .eq. 0.0) then
status = 'Zero Jacobian determinant'
end if
return
end if
w = weight *(0.5*det)* (h(1)*q(1)+h(2)*q(2)+h(3)*q(3)+h(4)*q(4)+h(5)*q(5)+h(6)*q(6))
!
do 2000 j = 1,6
jx = ls(j)
jy = ls(j+6)
c1x = (c(1,1)*qx(j)+c(1,3)*qy(j)) * w
c1y = (c(1,3)*qx(j)+c(1,2)*qy(j)) * w
c2x = (c(1,2)*qx(j)+c(2,3)*qy(j)) * w
c2y = (c(2,3)*qx(j)+c(2,2)*qy(j)) * w
c3x = (c(1,3)*qx(j)+c(3,3)*qy(j)) * w
c3y = (c(3,3)*qx(j)+c(2,3)*qy(j)) * w
do 1500 i= j,6
ix = ls(i)
iy = ls(i+6)
sm(ix,jx) = sm(ix,jx) + qx(i)*c1x + qy(i)*c3x
sm(jx,ix) = sm(ix,jx)
sm(iy,jy) = sm(iy,jy) + qx(i)*c3y + qy(i)*c2y
sm(jy,iy) = sm(iy,jy)
sm(ix,jy) = sm(ix,jy) + qx(i)*c1y + qy(i)*c3y
sm(iy,jx) = sm(iy,jx) + qx(i)*c3x + qy(i)*c2x
sm(jy,ix) = sm(ix,jy)
sm(jx,iy) = sm(iy,jx)
1500 continue
2000 continue
3000 continue
!
return
end
!=END FORTRAN