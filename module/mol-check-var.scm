#!/bin/sh
:;if which gosh > /dev/null 2>&1; then
:;   exec gosh -- "$0" "$@"
:;else
:;   exec ~/apl/gauche/bin/gosh -- "$0" "$@"
:;fi

(define-module mol-check-var
  (export *comment-out-strings*
          *atoms*))
(select-module mol-check-var)

(define *comment-out-strings* '(";" "#" "..." "<=" ">>" "~"))

(define *atoms*
  '(+ - H D He T Li Be B C N O F Ne
      Na Mg Al Si P S Cl Ar
      K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr
      Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe
      Cs Ba La Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn
      Fr Ra Ac Rf Db Sg Bh Hs Mt Uun Uuu Uub Uut
      Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu
      Ac Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No Lr))
