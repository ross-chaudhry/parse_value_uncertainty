! ------------------------------------------------------------------------------
! -- Constructs string for a given value and uncertainty
!
! -- For example, 1.23456d-5 +- 7.89d-7 produces 1.23(8) x 10^-5
! -- Default output is for display with latex via siunitx
!
! -- Output type and some options are configurable.
! -- Self test is included, primarily for development.
!
! -- Works via the following procedure:
!     ~ Determine the number of digits and SI exponent for uncertainty, ie 3e-5
!       Number of digits is typically 1, unless uncertainty is 10-19 (default)
!     ~ Least significant digit of value is the LSD of uncertainty
!     ~ RSCFIX
!
! Public Subroutines
! ------------------
!    pvu_config      - Set configuration options
!    pvu_gen_str     - Generate string from value and uncertainty
!    pvu_self_test   - Check things are working, primarily a dev tool
!
! Private Subroutines
! -------------------
!    pvu_uncert      - Compute string of uncertainty, and LSD
!
! Configuration Options
! ---------------------
! -- Set configuration options using pvu_config, using the option as
! -- an input varible, eg.
! -- pvu_config(output_format=OUTPUT_FORMAT_SIUNITX_PLUSMINUS, &
!       threshold_sn_pos=5)
!
!    output_format      Format of output string.
!                       Strings passed to siunitx will be parsed by its rules
!                       Latex output should be used to check siunitx
!                       Possible values:
!
!        OUTPUT_FORMAT_SIUNITX_PLUSMINUS
!        Latex with sinutix, with plus-minus, eg '\num{1.23 +- 0.08 e-5}'
!
!        OUTPUT_FORMAT_SIUNITX_PARENTHESIS
!        Latex with siunitx, with parenthesis, eg '\num{1.23(8) e-5}'
!
!        OUTPUT_FORMAT_LATEX_PLUSMINUS
!        Latex vanilla with pm, eg '1.23 \pm 0.08 \times 10^{-5}'
!
!        OUTPUT_FORMAT_LATEX_PARENTHESIS
!        Latex vanilla with parenthesis, eg '1.23(8) \times 10^{-5}'
!
!        OUTPUT_FORMAT_DEFAULT      (is OUTPUT_FORMAT_SIUNITX_PLUSMINUS)
!
!    threshold_sn_pos   Minimum positive exponent that requires scientific not.
!                       Default value is 3
!                       Eg, by default, 1234 +- 1 is 1.234(1) e3
!                       but             123  +- 1 is 123(1)
!                       If the uncertainty is too large, revert to SN
!                       regardless, eg 123 +- 35 is 1.2(4) e2
!                       Possible values:
!
!        integer value from 1 to 10
!
!        THRESHOLD_SN_POS_DEFAULT   (is 3)
!
!
!    threshold_sn_neg   Minimum negative exponent that requires scientific not.
!                       Default value is 3
!                       Eg, by default, 0.0043 +- 0.0005 is 4.3(5) e-3
!                       but             0.043  +- 0.005  is 0.043(5)
!                       Possible values:
!
!        integer value from 1 to 10    (positive int corresponds to neg. exp)
!
!        THRESHOLD_SN_NEG_DEFAULT   (is 3)
!
!    mode_uncert_sf     Method to compute the number of significant figures
!                       in the uncertainty.
!
!        MODE_UNCERT_SF_ONE
!        Always one significant figure (0.12 > 0.1)
!
!        MODE_UNCERT_SF_FIFTEEN
!        If one sig fig. yields a '1', add an extra digit.
!        This is the recommendation of Taylor, RSCFIX
!        Called 'fifteen' because that's the largest uncertainty mag achievable
!
!        MODE_UNCERT_SF_NINETEEN
!        If two sig fig. yields 10 to 19, two digits.
!        This is what RSC was taught, he thinks.
!        Called 'nineteen' for same reason as 'fifteen'
!
!        MODE_UNCERT_SF_DEFAULT     (is MODE_UNCERT_SF_FIFTEEN)
!
!    local_debug        Whether verbose debugging output is on, true/false
!
!
! ------------------------------------------------------------------------------

module parse_value_uncertainty
   use ISO_FORTRAN_ENV, only : OUTPUT_UNIT
   implicit none

   ! ===== Base parameters
   ! -- Type of real, should be IEEE double
   integer, parameter :: wp = selected_real_kind(15,307)

   ! -- Output unit for debugging, test output, and errors
   ! -- Could make config parameter, but don't bother yet
   integer, parameter :: olun = OUTPUT_UNIT

   ! -- Default string length. All but input dummy argument should be this
   integer, parameter :: strlen = 200

   ! ===== Configuration Options
   ! -- Possible values and defaults, as parameters
   integer, parameter :: OUTPUT_FORMAT_SIUNITX_PLUSMINUS     = 1001
   integer, parameter :: OUTPUT_FORMAT_SIUNITX_PARENTHESIS   = 1002
   integer, parameter :: OUTPUT_FORMAT_LATEX_PLUSMINUS       = 1003
   integer, parameter :: OUTPUT_FORMAT_LATEX_PARENTHESIS     = 1004
   integer, parameter :: OUTPUT_FORMAT_DEFAULT = OUTPUT_FORMAT_SIUNITX_PLUSMINUS

   integer, parameter :: THRESHOLD_SN_POS_DEFAULT            = 3
   integer, parameter :: THRESHOLD_SN_NEG_DEFAULT            = 3

   integer, parameter :: MODE_UNCERT_SF_ONE = 2001
   integer, parameter :: MODE_UNCERT_SF_FIFTEEN = 2002
   integer, parameter :: MODE_UNCERT_SF_NINETEEN = 2003
   integer, parameter :: MODE_UNCERT_SF_DEFAULT = MODE_UNCERT_SF_FIFTEEN

   ! -- Current value of config options, set to default so no init requied
   integer :: cur_output_format = OUTPUT_FORMAT_DEFAULT
   integer :: cur_threshold_sn_pos = THRESHOLD_SN_POS_DEFAULT
   integer :: cur_threshold_sn_neg = THRESHOLD_SN_NEG_DEFAULT
   integer :: cur_mode_uncert_sf = MODE_UNCERT_SF_DEFAULT

   logical :: ld = .true.     ! -- Local Debugging

   ! -- List of numbers that are allowed to have 0 uncertainty
   integer, parameter :: n_allowed_no_uncert = 3
   real(wp), dimension(n_allowed_no_uncert), parameter :: &
      list_allowed_no_uncert = [ 0._wp, 1._wp, 100._wp ]

contains

! ------------------------------------------------------------------------------
! -- The main purpose of this module, generate a string from value and uncert
! ------------------------------------------------------------------------------
subroutine pvu_gen_str( real_val, real_err, str_out, ier)
   real(wp), intent(IN) :: real_val, real_err
   character(len=*), intent(OUT) :: str_out     ! -- Let this string be any size
   integer, intent(OUT) :: ier
   
   ier = 0

   str_out = ''

!    return
! 99 write(olun,*) '** Error in subroutine pvu_gen_str'
!    ier = 1
end subroutine pvu_gen_str

! ------------------------------------------------------------------------------
! -- Process an uncertainty value, real_err. Yields:
!     str_err     The string of uncertainty amount (goes inside parenthesis)
!     sf_err      Number of significant figures in uncertainty
!     lsd_exp_err SI exponent for last significant digit in uncertainty
! ------------------------------------------------------------------------------
subroutine pvu_process_uncertainty( real_err, str_err, sf_err, lsd_exp_err, ier)
   real(wp), intent(IN) :: real_err
   character(strlen), intent(OUT) :: str_err
   integer, intent(OUT) :: sf_err, lsd_exp_err, ier
   real(wp) :: normalized
   integer :: exp_tmp, one_digit, two_digit

   ier = 0

   if (real_err<=0._wp) then
      write(olun,*) '** Internal error, uncertainty is non-positive here'
      goto 99
   endif

   ! -- First, get exponent and normalize, eg 1.23, 1,49, 1.79, 1.95, 4.67, 9.87, 9.98
   exp_tmp = FLOOR(log10(real_err))
   normalized = real_err * 10._wp**(-1*exp_tmp)

   ! -- Our first special case, if the number is 9.5 or more, it will round to 10
   ! -- when rounding to one digit, so divde out an extra 10
   if (normalized>=9.5_wp) then
      exp_tmp = exp_tmp + 1
      normalized = real_err * 10._wp**(-1*exp_tmp)
   endif

   ! -- exp_tmp now refers to the most significant digit of uncertainty

                                                       ! has 1/10  \/
   ! -- Round to two digits and one digit                          *    *
   one_digit = NINT(normalized)           ! eg 1,   1,  2,  2,  5, 1,  1
   two_digit = NINT(normalized*10._wp)    ! eg 12, 15, 18, 20, 47, 10, 10

   ! -- Silly checks, necessary?
   if ( (one_digit<1) .OR. (one_digit>9) ) then
      write(olun,*) '** Internal error, one digit invalid: ', one_digit
      goto 99
   endif
   if ( (two_digit<10) .OR. (two_digit>99) ) then
      write(olun,*) '** Internal error, two digit invalid: ', two_digit
      goto 99
   endif

   ! -- Now, apply heuristics based on method
   ! -- This select case *only* determines sf_err
   ! -- Each mode described more in header
   select case (cur_mode_uncert_sf)
   case (MODE_UNCERT_SF_ONE)
      ! -- Only one digit of uncertainty always
      sf_err = 1
   case (MODE_UNCERT_SF_FIFTEEN)
      ! -- If the single digit is one, then we add an extra digit
      ! -- (fifteen isn't actually the cutoff, because 14.8 > 15 but 15.1 > 2)
      ! -- This is the recommendation from Taylor, p. RSCFIX
      if (one_digit==1) then
         sf_err = 2
      else
         sf_err = 1
      endif
   case (MODE_UNCERT_SF_NINETEEN)
      ! -- If the first digit of two_digit is one, two digits
      ! -- This is what RSC was taught
      if (two_digit<20) then
         sf_err = 2
      else
         sf_err = 1
      endif
   case default
      write(olun,*) '** Error, invalid cur_mode_uncert_sf: ', cur_mode_uncert_sf
      goto 99
   end select

   ! -- Compute least significant digit from most significant digit
   lsd_exp_err = exp_tmp - (sf_err - 1)

   ! -- For string, shift digits to least lsd at ones and write
   write(str_err,'(i4)') NINT( real_err * 10._wp**(-1*lsd_exp_err) )
   str_err = adjustl(str_err)

   if (ld) write(olun,'(a,2x,es14.8,2x,a5,2i4)') ' == Processed uncertainty: ',&
      real_err, '"'//trim(str_err)//'"', sf_err, lsd_exp_err

   if (sf_err/=len_trim(str_err)) then
      write(olun,*) '** Internal logic error; sf_err, len: ', &
         sf_err, len_trim(str_err)
      goto 99
   endif

   return
99 write(olun,*) '** Error in subroutine pvu_process_uncertainty'
   ier = 1
end subroutine pvu_process_uncertainty

! ------------------------------------------------------------------------------
! -- Test process_uncertainty by giving a number and the expected output
! -- For now, a failed test is catastrophic, ie causes full stop
! ------------------------------------------------------------------------------
subroutine pvu_test_process_uncertainty( real_err, expected_str, expected_sf, &
      expected_lsd_exp, ier)
   real(wp), intent(IN) :: real_err
   character(*), intent(IN) :: expected_str     ! -- Wildcard length
   integer, intent(IN) :: expected_sf, expected_lsd_exp
   integer, intent(OUT) :: ier
   integer :: received_sf, received_lsd_exp
   character(strlen) :: received_str

   call pvu_process_uncertainty( real_err, received_str, received_sf, &
      received_lsd_exp, ier )
   if (ier/=0) goto 99

   if ( (received_str/=expected_str) .OR. (received_sf/=expected_sf) .OR. &
         (received_lsd_exp/=expected_lsd_exp) ) then
      write(olun,*) '** Failed test for uncertainty: ', real_err
      write(olun,*) '** str:     "'//trim(expected_str)//'" "'//trim(received_str)//'"'
      write(olun,*) '** sf:      ', expected_sf, received_sf
      write(olun,*) '** lsd_exp: ', expected_lsd_exp, received_lsd_exp
      goto 99
   endif

   return
99 write(olun,*) '** Error in subroutine pvu_test_process_uncertainty'
   ier = 1
end subroutine pvu_test_process_uncertainty

! ------------------------------------------------------------------------------
! -- Perform self test, primarily to aid development. Only works with defaults.
! -- On success, PASSED is printed with ier=0
! -- On failure, FAILED is printed with ier=1
! ------------------------------------------------------------------------------
subroutine pvu_self_test(ier)
   integer, intent(OUT) :: ier
   integer :: jer

   ier = 0

   call pvu_test_process_uncertainty(0.0123_wp, '12', 2, -3, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0149_wp, '15', 2, -3, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0150_wp, '2', 1, -2, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0179_wp, '2', 1, -2, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0195_wp, '2', 1, -2, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0467_wp, '5', 1, -2, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0987_wp, '10', 2, -2, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0998_wp, '10', 2, -2, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(123._wp, '12', 2, 1, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(4e5_wp, '4', 1, 5, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.095_wp, '10', 2, -2, jer)
   if (jer/=0) ier = 2

   if (ier==0) then
      write(olun,*) '-- Testing parse_value_uncertainty module ===== PASSED ====='
   else
      write(olun,*) '-- Testing parse_value_uncertainty module ===== FAILED ====='
   endif

   return
! 99 write(olun,*) '** Error in subroutine pvu_self_test'
!    write(olun,*) '** Testing parse_value_uncertainty module ===== FAILED ====='
!    ier = 1
end subroutine pvu_self_test

end module parse_value_uncertainty
