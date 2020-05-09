
Numerical Recipes Code in KDP-2
-------------------------------

The original KDP-2 software relies on several subroutines from
"Numerical Recipes in Fortran 77, 2nd Edition" (NR). In Koko, these
subroutines were either removed or re-implemented using open-source
software. The following table lists the subroutines providing the
functionality of the original NR subroutines.


 | KDP-2 (NR)   | Koko         | Function |
 | :----------- | :----------- | :------- |
 | caldat<br>julday<br>julday1 | removed     | Julian date |
 | sort<br>darray_sort         | sortdmat<br>dpsort    | Array sorting |
 | spline<br>splint      | splineint<br>cubicspline   | Spline interpolation |
 | svdcmp[ab]<br>svbksb[ab]<br>pythag  | svdsub<br>svd      | Solve linear equations |
 | ranran<br>gasdev |  random_number<br>[sd]rand_normal | Random number generation |