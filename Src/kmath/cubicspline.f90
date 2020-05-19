module cubicspline

  implicit none
  integer, parameter :: dp = kind(1.0d0)

contains

  subroutine spline_cubic_set ( n, t, y, ibcbeg, ybcbeg, ibcend, ybcend, ypp, ier )

    !*****************************************************************************
    !
    !! SPLINE_CUBIC_SET computes the second derivatives of a piecewise cubic spline.
    !
    !  Discussion:
    !
    !    For data interpolation, the user must call SPLINE_CUBIC_SET to
    !    determine the second derivative data, passing in the data to be
    !    interpolated, and the desired boundary conditions.
    !
    !    The data to be interpolated, plus the SPLINE_CUBIC_SET output,
    !    defines the spline.  The user may then call SPLINE_CUBIC_VAL to
    !    evaluate the spline at any point.
    !
    !    The cubic spline is a piecewise cubic polynomial.  The intervals
    !    are determined by the "knots" or abscissas of the data to be
    !    interpolated.  The cubic spline has continous first and second
    !    derivatives over the entire interval of interpolation.
    !
    !    For any point T in the interval T(IVAL), T(IVAL+1), the form of
    !    the spline is
    !
    !      SPL(T) = A(IVAL)
    !             + B(IVAL) * ( T - T(IVAL) )
    !             + C(IVAL) * ( T - T(IVAL) )^2
    !             + D(IVAL) * ( T - T(IVAL) )^3
    !
    !    If we assume that we know the values Y(*) and YPP(*), which represent
    !    the values and second derivatives of the spline at each knot, then
    !    the coefficients can be computed as:
    !
    !      A(IVAL) = Y(IVAL)
    !      B(IVAL) = ( Y(IVAL+1) - Y(IVAL) ) / ( T(IVAL+1) - T(IVAL) )
    !        - ( YPP(IVAL+1) + 2 * YPP(IVAL) ) * ( T(IVAL+1) - T(IVAL) ) / 6
    !      C(IVAL) = YPP(IVAL) / 2
    !      D(IVAL) = ( YPP(IVAL+1) - YPP(IVAL) ) / ( 6 * ( T(IVAL+1) - T(IVAL) ) )
    !
    !    Since the first derivative of the spline is
    !
    !      SPL'(T) =     B(IVAL)
    !              + 2 * C(IVAL) * ( T - T(IVAL) )
    !              + 3 * D(IVAL) * ( T - T(IVAL) )^2,
    !
    !    the requirement that the first derivative be continuous at interior
    !    knot I results in a total of N-2 equations, of the form:
    !
    !      B(IVAL-1) + 2 C(IVAL-1) * (T(IVAL)-T(IVAL-1))
    !      + 3 * D(IVAL-1) * (T(IVAL) - T(IVAL-1))^2 = B(IVAL)
    !
    !    or, setting H(IVAL) = T(IVAL+1) - T(IVAL)
    !
    !      ( Y(IVAL) - Y(IVAL-1) ) / H(IVAL-1)
    !      - ( YPP(IVAL) + 2 * YPP(IVAL-1) ) * H(IVAL-1) / 6
    !      + YPP(IVAL-1) * H(IVAL-1)
    !      + ( YPP(IVAL) - YPP(IVAL-1) ) * H(IVAL-1) / 2
    !      =
    !      ( Y(IVAL+1) - Y(IVAL) ) / H(IVAL)
    !      - ( YPP(IVAL+1) + 2 * YPP(IVAL) ) * H(IVAL) / 6
    !
    !    or
    !
    !      YPP(IVAL-1) * H(IVAL-1) + 2 * YPP(IVAL) * ( H(IVAL-1) + H(IVAL) )
    !      + YPP(IVAL) * H(IVAL)
    !      =
    !      6 * ( Y(IVAL+1) - Y(IVAL) ) / H(IVAL)
    !      - 6 * ( Y(IVAL) - Y(IVAL-1) ) / H(IVAL-1)
    !
    !    Boundary conditions must be applied at the first and last knots.
    !    The resulting tridiagonal system can be solved for the YPP values.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    07 June 2013
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Carl deBoor,
    !    A Practical Guide to Splines,
    !    Springer, 2001,
    !    ISBN: 0387953663.
    !
    !  Parameters:
    !
    !    Input, integer N, the number of data points; N must be
    !    at least 2.
    !
    !    Input, real ( kind = 8 ) T(N), the points where data is specified.
    !    The values should be distinct, and increasing.
    !
    !    Input, real ( kind = 8 ) Y(N), the data values to be interpolated.
    !
    !    Input, integer IBCBEG, the left boundary condition flag:
    !    0: the spline should be a quadratic over the first interval;
    !    1: the first derivative at the left endpoint should be YBCBEG;
    !    2: the second derivative at the left endpoint should be YBCBEG;
    !    3: Not-a-knot: the third derivative is continuous at T(2).
    !
    !    Input, real ( kind = 8 ) YBCBEG, the left boundary value, if needed.
    !
    !    Input, integer IBCEND, the right boundary condition flag:
    !    0: the spline should be a quadratic over the last interval;
    !    1: the first derivative at the right endpoint should be YBCEND;
    !    2: the second derivative at the right endpoint should be YBCEND;
    !    3: Not-a-knot: the third derivative is continuous at T(N-1).
    !
    !    Input, real ( kind = 8 ) YBCEND, the right boundary value, if needed.
    !
    !    Output, real ( kind = 8 ) YPP(N), the second derivatives of
    !    the cubic spline. ier == 0 : successful call; ier == 1: number of knots
    !    is less than 2; ier == 2: knots are not monotonic; ier == 3: invalid
    !    IBCBEG or IBCEND arguments.

    integer, intent(in)    :: n, ibcbeg, ibcend
    integer, intent(out)   :: ier
    real (dp), intent(in)  :: t(n), y(n), ybcbeg, ybcend
    real (dp), intent(out) :: ypp(n)

    integer   :: i
    real (dp) :: a1(n), a2(n), a3(n), a4(n), a5(n), b(n)
    
    !  Check input
    if ( n <= 1 ) then
       ier = 1
       return
    end if
    
    do i = 1, n - 1
       if ( t(i+1) <= t(i) ) then
          ier = 2
          return
       end if
    end do
    
    !  Zero out the matrix.
    a1(1:n) = 0.0_dp
    a2(1:n) = 0.0_dp
    a3(1:n) = 0.0_dp
    a4(1:n) = 0.0_dp
    a5(1:n) = 0.0_dp
  
    !  Set the first equation
    select case (ibcbeg)
       
    case ( 0 )
       b(1)  = 0.0_dp
       a3(1) =  1.0_dp
       a4(1) = -1.0_dp
       
    case ( 1 )
       b(1)  = ( y(2) - y(1) ) / ( t(2) - t(1) ) - ybcbeg
       a3(1) = ( t(2) - t(1) ) / 3.0_dp
       a4(1) = ( t(2) - t(1) ) / 6.0_dp
       
    case ( 2 )
       b(1)  = ybcbeg
       a3(1) = 1.0_dp
       a4(1) = 0.0_dp
       
    case ( 3 )
       b(1)  = 0.0_dp
       a3(1) = - ( t(3) - t(2) )
       a4(1) =   ( t(3)        - t(1) )
       a5(1) = - (        t(2) - t(1) )

    case default
       ier = 3
       return

    end select
    
    !  Set the intermediate equations.
    do i = 2, n - 1
       b(i)  = ( y(i+1) - y(i) ) / ( t(i+1) - t(i) ) &
              -( y(i) - y(i-1) ) / ( t(i) - t(i-1) )
       a2(i) = ( t(i+1) - t(i)   ) / 6.0_dp
       a3(i) = ( t(i+1) - t(i-1) ) / 3.0_dp
       a4(i) = ( t(i)   - t(i-1) ) / 6.0_dp
    end do

    !  Set the last equation.
    select case (ibcend)
     
    case ( 0 )
       b(n)  = 0.0_dp
       a2(n) = -1.0_dp
       a3(n) = 1.0_dp
       
    case ( 1 )
       b(n)  = ybcend - ( y(n) - y(n-1) ) / ( t(n) - t(n-1) )
       a2(n) = ( t(n) - t(n-1) ) / 6.0_dp
       a3(n) = ( t(n) - t(n-1) ) / 3.0_dp
       
    case ( 2 )
       b(n)  = ybcend
       a2(n) = 0.0_dp
       a3(n) = 1.0_dp
       
    case ( 3 )
       b(n)  = 0.0_dp
       a1(n) = - ( t(n) - t(n-1) )
       a2(n) =   ( t(n)          - t(n-2) )
       a3(n) = - (        t(n-1) - t(n-2) )
       
    case default
       ier = 3
       return
    end select

    !  Special case:
    !    N = 2, IBCBEG = IBCEND = 0.
    if ( n == 2 .and. ibcbeg == 0 .and. ibcend == 0 ) then
       ypp(1) = 0.0_dp
       ypp(2) = 0.0_dp
       
    ! Solve the linear system.
    else

       call penta ( n, a1, a2, a3, a4, a5, b, ypp )
       
    end if

    ! we got here without error
    ier = 0
    
  end subroutine spline_cubic_set


  subroutine penta ( n, a1, a2, a3, a4, a5, b, x )

    !*****************************************************************************80
    !
    !! PENTA solves a pentadiagonal system of linear equations.
    !
    !  Discussion:
    !
    !    The matrix A is pentadiagonal.  It is entirely zero, except for
    !    the main diagaonal, and the two immediate sub- and super-diagonals.
    !
    !    The entries of Row I are stored as:
    !
    !      A(I,I-2) -> A1(I)
    !      A(I,I-1) -> A2(I)
    !      A(I,I)   -> A3(I)
    !      A(I,I+1) -> A4(I)
    !      A(I,I-2) -> A5(I)
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    07 June 2013
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Cheney, Kincaid,
    !    Numerical Mathematics and Computing,
    !    1985, pages 233-236.
    !
    !  Parameters:
    !
    !    Input, integer N, the order of the matrix.
    !
    !    Input, real (dp) A1(N), A2(N), A3(N), A4(N), A5(N), the nonzero
    !    elements of the matrix.  Note that the data in A2, A3 and A4
    !    is overwritten by this routine during the solution process.
    !
    !    Input, real (dp) B(N), the right hand side of the linear system.
    !
    !    Output, real (dp) X(N), the solution of the linear system.
    !

    integer :: n

    real (dp) :: a1(n), a2(n), a3(n), a4(n), a5(n), b(n)
    integer :: i
    real (dp) :: x(n), xmult

    do i = 2, n - 1
       xmult = a2(i) / a3(i-1)
       a3(i) = a3(i) - xmult * a4(i-1)
       a4(i) = a4(i) - xmult * a5(i-1)
       b(i) = b(i) - xmult * b(i-1)
       xmult = a1(i+1) / a3(i-1)
       a2(i+1) = a2(i+1) - xmult * a4(i-1)
       a3(i+1) = a3(i+1) - xmult * a5(i-1)
       b(i+1) = b(i+1) - xmult * b(i-1)
    end do

    xmult = a2(n) / a3(n-1)
    a3(n) = a3(n) - xmult * a4(n-1)
    x(n) = ( b(n) - xmult * b(n-1) ) / a3(n)
    x(n-1) = ( b(n-1) - a4(n-1) * x(n) ) / a3(n-1)
    do i = n - 2, 1, -1
       x(i) = ( b(i) - a4(i) * x(i+1) - a5(i) * x(i+2) ) / a3(i)
    end do

  end subroutine penta


  subroutine spline_cubic_val ( n, t, y, ypp, tval, yval, ypval, yppval )

    !*****************************************************************************80
    !
    !! SPLINE_CUBIC_VAL evaluates a piecewise cubic spline at a point.
    !
    !  Discussion:
    !
    !    SPLINE_CUBIC_SET must have already been called to define the
    !    values of YPP.
    !
    !    For any point T in the interval T(IVAL), T(IVAL+1), the form of
    !    the spline is
    !
    !      SPL(T) = A
    !             + B * ( T - T(IVAL) )
    !             + C * ( T - T(IVAL) )^2
    !             + D * ( T - T(IVAL) )^3
    !
    !    Here:
    !      A = Y(IVAL)
    !      B = ( Y(IVAL+1) - Y(IVAL) ) / ( T(IVAL+1) - T(IVAL) )
    !        - ( YPP(IVAL+1) + 2 * YPP(IVAL) ) * ( T(IVAL+1) - T(IVAL) ) / 6
    !      C = YPP(IVAL) / 2
    !      D = ( YPP(IVAL+1) - YPP(IVAL) ) / ( 6 * ( T(IVAL+1) - T(IVAL) ) )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    20 November 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Carl deBoor,
    !    A Practical Guide to Splines,
    !    Springer, 2001,
    !    ISBN: 0387953663.
    !
    !  Parameters:
    !
    !    Input, integer N, the number of data values.
    !
    !    Input, real (dp) T(N), the knot values.
    !
    !    Input, real (dp) Y(N), the data values at the knots.
    !
    !    Input, real (dp) YPP(N), the second derivatives of the
    !    spline at the knots.
    !
    !    Input, real (dp) TVAL, a point, typically between T(1) and
    !    T(N), at which the spline is to be evalulated.  If TVAL lies outside
    !    this range, extrapolation is used.
    !
    !    Output, real (dp) YVAL, YPVAL, YPPVAL, the value of the spline, and
    !    its first two derivatives at TVAL.
    !

    integer :: n

    real (dp) :: dt, h
    integer :: left, right
    real (dp) :: t(n), y(n), ypp(n), tval, yppval, ypval, yval
    
    !  Determine the interval [T(LEFT), T(RIGHT)] that contains TVAL.
    !  Values below T(1) or above T(N) use extrapolation.
    call r8vec_bracket ( n, t, tval, left, right )
    
    !  Evaluate the polynomial.
    dt = tval - t(left)
    h = t(right) - t(left)
    
    yval = y(left) &
         + dt * ( ( y(right) - y(left) ) / h &
         - ( ypp(right) / 6.0_dp + ypp(left) / 3.0_dp ) * h &
         + dt * ( 0.5_dp * ypp(left) &
         + dt * ( ( ypp(right) - ypp(left) ) / ( 6.0_dp * h ) ) ) )

    ypval = ( y(right) - y(left) ) / h &
         - ( ypp(right) / 6.0_dp + ypp(left) / 3.0_dp ) * h &
         + dt * ( ypp(left) &
         + dt * ( 0.5_dp * ( ypp(right) - ypp(left) ) / h ) )
    
    yppval = ypp(left) + dt * ( ypp(right) - ypp(left) ) / h

  end subroutine spline_cubic_val


  subroutine r8vec_bracket ( n, x, xval, left, right )

    !*****************************************************************************80
    !
    !! R8VEC_BRACKET searches a sorted R8VEC for successive brackets of a value.
    !
    !  Discussion:
    !
    !    An R8VEC is an array of double precision real values.
    !
    !    If the values in the vector are thought of as defining intervals
    !    on the real line, then this routine searches for the interval
    !    nearest to or containing the given value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    06 April 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer N, length of input array.
    !
    !    Input, real (dp) X(N), an array sorted into ascending order.
    !
    !    Input, real (dp) XVAL, a value to be bracketed.
    !
    !    Output, integer LEFT, RIGHT, the results of the search.
    !    Either:
    !      XVAL < X(1), when LEFT = 1, RIGHT = 2;
    !      X(N) < XVAL, when LEFT = N-1, RIGHT = N;
    !    or
    !      X(LEFT) <= XVAL <= X(RIGHT).
    !

    integer :: n, i, left, right
    real (dp) :: x(n),xval
  
    do i = 2, n - 1
       
       if ( xval < x(i) ) then
          left = i - 1
          right = i
          return
       end if

    end do
   
    left = n - 1
    right = n

  end subroutine r8vec_bracket

end module cubicspline
