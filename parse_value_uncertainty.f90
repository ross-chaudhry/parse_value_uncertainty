! ------------------------------------------------------------------------------
! -- Parse Value Uncertainty
! -- Constructs a string for a given value and uncertainty
! -- Author: Ross S. Chaudhry, August 2018
!
! -- Sample usage:
!        use parse_value_uncertainty, only pvu_gen_str
!        ...
!        call pvu_gen_str( 1.23456, 0.087, str_out, ier)
! -- str_out now contains '\num{1.23 +- 0.09}', suitable for siunitx in latex
!
! -- Designed for REAQCT (Univ. of Minnesota's Quasi-Classical Trajectory code),
! -- this module was adapted for general use.
! -- Hosted on the author's personal github with MIT license:
! -- https://github.com/ross-chaudhry
!
! -- Output type, positive and negative threshold to use scientific notation,
! -- and uncertainty significant figure behavior are configurable.
!
! -- A good reference is Taylor, "An Introduction to Error Analysis", 1982.
! -- Also Taylor and Kuyatt, "Guidelines for Evaluating and Expressing the
! -- Uncertainty of NIST Measurement Results", 1994
!
! Public Subroutines
! ------------------
!    pvu_config                     - Set configuration options
!    pvu_gen_str                    - Generate string from value and uncertainty
!    pvu_self_test                  - Check things are working
!
! Private Subroutines
! -------------------
!    pvu_process_uncertainty        - Compute string of uncertainty, and LSD
!    pvu_test_process_uncertainty   - Check known uncertainty vs computed
!    pvu_test_gen_str               - Check known val/err pair vs computed
!    pvu_test_tex_write             - Write single number to latex output
!    pvu_test_tex_init              - Open and init latex output
!    pvu_test_tex_finalize          - Close and finalize latex output
!
! Configuration Options
! ---------------------
! -- pvu_config sets configuration options using optional input variable, eg.
! pvu_config(output_format=OUTPUT_FORMAT_SIUNITX_PLUSMINUS, threshold_sn_pos=5)
!
!    output_format      Format of output string.
!                       Strings passed to siunitx will be parsed by its rules
!                       Latex output should be used to check siunitx
!        Possible values:
!
!        OUTPUT_FORMAT_SIUNITX_PLUSMINUS
!        Latex with sinutix, with plus-minus, eg '\num{1.23 \pm 0.08 e-5}'
!
!        OUTPUT_FORMAT_SIUNITX_PARENTHESIS
!        Latex with siunitx, with parenthesis, eg '\num{1.23(8) e-5}'
!
!        OUTPUT_FORMAT_SCOLUMN_PLUSMINUS
!        Latex with siunitx, inside S column, eg '1.23 \pm 0.08 e-5'
!
!        OUTPUT_FORMAT_SCOLUMN_PARENTHESIS
!        Latex with siunitx, inside S column, eg '1.23(8) e-5'
!
!         * Note that siunitx will accept either () or \pm uncertainty format
!           format and may be configured to give various outputs.
!
!        OUTPUT_FORMAT_LATEX_PLUSMINUS
!        Latex vanilla with pm, eg '$1.23 \pm 0.08 \times 10^{-5}$'
!
!        OUTPUT_FORMAT_LATEX_PARENTHESIS
!        Latex vanilla with parenthesis, eg '$1.23(8) \times 10^{-5}$'
!
!        OUTPUT_FORMAT_DEFAULT      set to OUTPUT_FORMAT_SIUNITX_PLUSMINUS
!
!    threshold_sn_pos   Minimum positive exponent that requires scientific not.
!                       Default value is 3
!                       Eg, by default, 1234 \pm 1 is 1.234(1) e3
!                       but             123  \pm 1 is 123(1)
!                       If the uncertainty is too large, revert to SN
!                       regardless, eg 123 \pm 35 is 1.2(4) e2
!        Possible values:
!
!        integer value from 1 to 10
!
!        THRESHOLD_SN_POS_DEFAULT   set to 3
!
!    threshold_sn_neg   Minimum negative exponent that requires scientific not.
!                       Default value is 3
!                       Eg, by default, 0.0043 \pm 0.0005 is 4.3(5) e-3
!                       but             0.043  \pm 0.005  is 0.043(5)
!        Possible values:
!
!        integer value from 1 to 10    (positive int corresponds to neg. exp)
!
!        THRESHOLD_SN_NEG_DEFAULT   set to 3
!
!    mode_uncert_sf     Method to compute the number of significant figures
!                       in the uncertainty.
!        Possible values:
!
!        MODE_UNCERT_SF_ONE
!        Always one significant figure (0.12 > 0.1)
!
!        MODE_UNCERT_SF_FIFTEEN
!        If one sig fig. yields a '1', add an extra digit.
!        This is one interpretation of the recommendation of Taylor (p. 16)
!        Called 'fifteen' because that's the largest uncertainty mag achievable
!
!        MODE_UNCERT_SF_NINETEEN
!        If two sig fig. yields 10 to 19, use two digits.
!        This is the other interpretation of the recommendation of Taylor,
!        and the one this author perfers.
!        Called 'nineteen' for same reason as 'fifteen'
!        This means 'uncertainty of uncertainty' is largest with 3 (3.5/2.5=40%)
!        which is not terrible.
!        Without this, largest is with 2 (2.5/1.5=67%), which is maybe too much.
!        Note also that 2 has largest 'uncertainty of uncertainty' even with
!        MODE_UNCERT_SF_ONE because 15/9.5=58%.
!
!        MODE_UNCERT_SF_DEFAULT     set to MODE_UNCERT_SF_NINETEEN
!
!    ld                 Print verbose output (Local Debugging), true/false
!
! Issues
! ------
!    The string \num{3.} is parsed to 3.0, so trailing decimals are stripped.
!    This has the side-effect of fixing a bug in siunitx where \num{3. \pm 3.}
!    becomes 3.0(3), the bug is mentioned here:
!    https://tex.stackexchange.com/questions/446074/wrong-value-with-decimals-and-uncertainty-in-siunitx
!
!    Zero uncertainty is causes a siunitx crash using the \pm input format,
!    so parenthesis format is used instead for siunitx when err=0
!    Bug report is here: https://github.com/josephwright/siunitx/issues/349
!
! ------------------------------------------------------------------------------

module parse_value_uncertainty
   use ISO_FORTRAN_ENV, only : OUTPUT_UNIT
   implicit none

   ! -- Everything here is private by default, proper OOP
   ! -- Exceptions are public subroutines (listed here) and CONFIG_PARAMS
   private
   public :: pvu_config, pvu_gen_str, pvu_self_test

   ! ===== Base parameters
   ! -- Type of real, should be IEEE double
   integer, parameter :: wp = selected_real_kind(15,307)

   ! -- Output unit for debugging, test output, and errors
   ! -- Could make config parameter, but don't bother yet
   integer, parameter :: olun = OUTPUT_UNIT

   ! -- Default string length. All but input dummy argument should be this
   integer, parameter :: strlen = 200

   ! -- newunit isn't supported by some versions of gfortran I want
   integer, parameter :: fid_tt = 101
   character(len=*), parameter :: fname_tex_test = 'pvu_test.tex'

   ! ===== Configuration Options
   ! -- Possible values and defaults, as parameters
   integer, parameter, public :: OUTPUT_FORMAT_SIUNITX_PLUSMINUS     = 1001
   integer, parameter, public :: OUTPUT_FORMAT_SIUNITX_PARENTHESIS   = 1002
   integer, parameter, public :: OUTPUT_FORMAT_SCOLUMN_PLUSMINUS     = 1003
   integer, parameter, public :: OUTPUT_FORMAT_SCOLUMN_PARENTHESIS   = 1004
   integer, parameter, public :: OUTPUT_FORMAT_LATEX_PLUSMINUS       = 1005
   integer, parameter, public :: OUTPUT_FORMAT_LATEX_PARENTHESIS     = 1006
   integer, parameter, public :: OUTPUT_FORMAT_DEFAULT = OUTPUT_FORMAT_SIUNITX_PLUSMINUS

   integer, parameter, public :: THRESHOLD_SN_POS_DEFAULT            = 3
   integer, parameter, public :: THRESHOLD_SN_NEG_DEFAULT            = 3

   integer, parameter, public :: MODE_UNCERT_SF_ONE = 2001
   integer, parameter, public :: MODE_UNCERT_SF_FIFTEEN = 2002
   integer, parameter, public :: MODE_UNCERT_SF_NINETEEN = 2003
   integer, parameter, public :: MODE_UNCERT_SF_DEFAULT = MODE_UNCERT_SF_NINETEEN

   ! -- Current value of config options, set to default so no init requied
   integer :: cur_output_format = OUTPUT_FORMAT_DEFAULT
   integer :: cur_threshold_sn_pos = THRESHOLD_SN_POS_DEFAULT
   integer :: cur_threshold_sn_neg = THRESHOLD_SN_NEG_DEFAULT
   integer :: cur_mode_uncert_sf = MODE_UNCERT_SF_DEFAULT

   logical :: ld = .false.       ! -- Local debugging, short special name

   ! -- List of numbers that are allowed to have 0 uncertainty
   integer, parameter :: n_allowed_no_uncert = 3
   real(wp), dimension(n_allowed_no_uncert), parameter :: &
      list_allowed_no_uncert = [ 0._wp, 1._wp, 100._wp ]

contains

! ------------------------------------------------------------------------------
! -- Set internal configuration variables for this module
! -- See the top of this module for details
! -- The notation is, for each variable $var, there are possible
! -- values $VAR_DESCRIPT and set with pvu_config(ier, $var=$VAR_DESCRIPT)
! ------------------------------------------------------------------------------
subroutine pvu_config( ier, output_format, threshold_sn_pos, threshold_sn_neg, &
      mode_uncert_sf, ld_in)
   integer, intent(OUT) :: ier
   integer, intent(IN), optional :: output_format, threshold_sn_pos, &
      threshold_sn_neg, mode_uncert_sf
   logical, intent(IN), optional :: ld_in

   ier = 0

   if (present(ld_in)) then
      ld = ld_in
      if (ld) write(olun,*) '== PVU debugging is on'
   endif

   if (present(output_format)) then
      if ( (output_format<OUTPUT_FORMAT_SIUNITX_PLUSMINUS) .OR. &
            (output_format>OUTPUT_FORMAT_LATEX_PARENTHESIS) ) then
         write(olun,*) '** Error, invalid output format: ', output_format
         goto 99
      else
         cur_output_format = output_format
      endif
   endif

   if (present(threshold_sn_pos)) then
      if ( (threshold_sn_pos<1) .OR. (threshold_sn_pos>9) ) then
         write(olun,*) '** Error, invalid threshold_sn_pos:', threshold_sn_pos
         write(olun,*) '** Should be between 1 and 9 (inclusive)'
         goto 99
      else
         cur_threshold_sn_pos = threshold_sn_pos
      endif
   endif

   if (present(threshold_sn_neg)) then
      if ( (threshold_sn_neg<1) .OR. (threshold_sn_neg>9) ) then
         write(olun,*) '** Error, invalid threshold_sn_neg:', threshold_sn_neg
         write(olun,*) '** Should be between 1 and 9 (inclusive)'
         goto 99
      else
         cur_threshold_sn_neg = threshold_sn_neg
      endif
   endif

   if (present(mode_uncert_sf)) then
      if ( (mode_uncert_sf<MODE_UNCERT_SF_ONE) .OR. &
            (mode_uncert_sf>MODE_UNCERT_SF_NINETEEN) ) then
         write(olun,*) '** Error, invalid mode_uncert_sf: ', mode_uncert_sf
         goto 99
      else
         cur_mode_uncert_sf = mode_uncert_sf
      endif
   endif

   return
99 write(olun,*) '** Error in subroutine pvu_config'
   ier = 1
end subroutine pvu_config

! ------------------------------------------------------------------------------
! -- The main purpose of this module, generate a string from value and uncert
! ------------------------------------------------------------------------------
subroutine pvu_gen_str( real_val, real_err, str_out, ier)
   real(wp), intent(IN) :: real_val, real_err
   character(len=*), intent(OUT) :: str_out     ! -- Let this string be any size
   integer, intent(OUT) :: ier
   character(len=strlen) :: str_err_paren, str_err_pm, str_val, fltfmt
   character(len=strlen) :: str_exp, str_exp_times, str_exp_e
   integer :: sf_err, lsd_exp_err, exp_both, tmp_val, dig, l
   real(wp) :: shifted_val, shifted_err

   ier = 0

   ! -- Some checks to start
   if (real_err<0._wp) then
      write(olun,*) '** Error, uncertainty is not allowed to be negative'
      goto 99
   endif
   if (real_err==0._wp) then
      ! -- This is allowed only under specific cases
      ! -- Use tolerance here? Could be 1.0+epsilon, right? Hm.
      if (.not.ANY(real_val==list_allowed_no_uncert)) then
         write(olun,*) '** Error, uncertainty is identically 0'
         write(olun,*) '** This is sometimes allowed, but real_val=', real_val
         goto 99
      else
         ! -- Could print warning, nah
      endif
   endif

   if (real_err==0._wp) then
      ! -- Should display error as '0', and use ones as lsd
      str_err_paren = '0'
      sf_err = 1
      lsd_exp_err = 0
   else
      call pvu_process_uncertainty( real_err, str_err_paren, sf_err, lsd_exp_err, ier)
   endif

   ! -- Determine SN exponent
   ! -- Place decimal at lsd_exp_err and round
   tmp_val = abs(NINT( real_val * 10._wp**(-1*lsd_exp_err) ))

   if (tmp_val==0) then
      ! -- If 0, err is larger than val (probably by a lot), so use err value
      ! -- Error should have lsd in single, so it can be 0(12) e1
      exp_both = lsd_exp_err
   else
      ! -- Determine exponent based on val, and correct for the shift
      exp_both = FLOOR(log10(real(tmp_val,wp)))
      exp_both = exp_both + lsd_exp_err
   endif

   ! -- Now, check if exp_both is close to 0 so we may revert to no SN
   if ( (exp_both<0) .AND. (exp_both>(-1*cur_threshold_sn_neg)) ) then
      ! -- Our exponent would be negative and not as small as -threshold
      exp_both = 0
   elseif ( (exp_both>0) .AND. (exp_both<cur_threshold_sn_pos) ) then
      ! -- Our exponent would be positive and not as large as threshold
      ! -- Can revert if uncertainty is small enough
      if (lsd_exp_err<=0) then
         exp_both = 0
      endif
   endif

   ! -- How many digits should we print after the decimal?
   ! -- This is the difference between lsd_exp_err and exp_both
   dig = exp_both - lsd_exp_err

   if ( (dig>10) .OR. (dig<0) ) then
      write(olun,*) '** Error, digits after decimal seems wrong: ', dig
      goto 99
   endif

   ! -- We have our SN exponent, compute values
   shifted_val = real_val * 10._wp**(-1*exp_both)
   shifted_err = real_err * 10._wp**(-1*exp_both)

   ! -- Write val and err to floats with the right number of digits
   ! -- 10 is because we could have many leading values if config
   write(fltfmt,'(a,i2.2,a,i2.2,a)') '(f', 10+dig, '.', dig, ')'

   write(str_val,fltfmt) shifted_val
   write(str_err_pm,fltfmt) shifted_err

   str_val = adjustl(str_val)
   str_err_pm = adjustl(str_err_pm)

   ! -- siunitx has weird behavior. If a number is given as 3. ,
   ! -- it is interpreted as having 2 sig figures, and printed as 3.0
   ! -- Also, there appears to be a bug when printing something as 3. \pm 3.
   ! -- Therefore, strip trailing decimal points from both the val and err
   ! -- tex SE is here: https://tex.stackexchange.com/questions/446074/wrong-value-with-decimals-and-uncertainty-in-siunitx
   l = len_trim(str_val)
   if (str_val(l:l)=='.') str_val(l:l) = ' '

   l = len_trim(str_err_pm)
   if (str_err_pm(l:l)=='.') str_err_pm(l:l) = ' '

   ! -- Also write number in SI exponent, and two forms for convenience
   if (exp_both/=0) then
      write(str_exp,'(i4)') exp_both
      str_exp = adjustl(str_exp)

      str_exp_e = ' e'//trim(str_exp)
      str_exp_times = ' \times 10^{'//trim(str_exp)//'}'
   else
      str_exp = ''
      str_exp_e = ''
      str_exp_times = ''
   endif

   ! -- At this point, we have the following (eg)
   !     ~ str_val         1.234
   !     ~ str_err_pm      0.012
   !     ~ str_err_paren   12
   !     ~ str_exp         -3

   ! -- Now we write depending on the format
   select case (cur_output_format)
   case (OUTPUT_FORMAT_SIUNITX_PLUSMINUS, OUTPUT_FORMAT_SCOLUMN_PLUSMINUS)
      ! -- \num{1234 \pm 0.012 e-3} or without \num{} wrapper
      ! -- Construct without wrapper first
      str_out = trim(str_val)//' \pm '//trim(str_err_pm)//trim(str_exp_e)

      ! -- There appears to be a bug in siunitx where `\pm 0` causes an error
      ! -- Therefore, for this case only, revert to parenthesis format
      ! -- Bug report: https://github.com/josephwright/siunitx/issues/349
      if (real_err==0._wp) then
         str_out = trim(str_val)//'('//trim(str_err_pm)//')'//trim(str_exp_e)
      endif

      ! -- If we are for SIUNITX not SCOLUMN add \num{} wrapper
      if (cur_output_format==OUTPUT_FORMAT_SIUNITX_PLUSMINUS) then
         str_out = '\num{'//trim(str_out)//'}'
      endif

   case (OUTPUT_FORMAT_SIUNITX_PARENTHESIS)
      ! -- \num{1234(12) e-3} or without \num{} wrapper
      ! -- Construct without wrapper first
      str_out = trim(str_val)//'('//trim(str_err_paren)//')'//trim(str_exp_e)

      ! -- If we are for SIUNITX not SCOLUMN add \num{} wrapper
      if (cur_output_format==OUTPUT_FORMAT_SIUNITX_PARENTHESIS) then
         str_out = '\num{'//trim(str_out)//'}'
      endif

   case (OUTPUT_FORMAT_LATEX_PLUSMINUS)
      ! -- $1234 \pm 0.012 \times 10^{-3}$
      str_out = '$'//trim(str_val)//' \pm '//trim(str_err_pm)//&
         trim(str_exp_times)//'$'

   case (OUTPUT_FORMAT_LATEX_PARENTHESIS)
      ! -- $1234(12) \times 10^{-3}$
      str_out = '$'//trim(str_val)//'('//trim(str_err_paren)//')'//&
         trim(str_exp_times)//'$'

   end select

   if (ld) write(olun,'(a,2(es15.8,2x),a)') ' == Generated str: ', &
      real_val, real_err, '  "'//trim(str_out)//'"'

   return
99 write(olun,*) '** Error in subroutine pvu_gen_str'
   ier = 1
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

   ! -- First, get exponent and normalize
   ! -- eg 1.23, 1,49, 1.79, 1.95, 4.67, 9.87, 9.98
   exp_tmp = FLOOR(log10(real_err))
   normalized = real_err * 10._wp**(-1*exp_tmp)

   ! -- Our first special case, if the number is 9.5 or more, it will round to 10
   ! -- when rounding to one digit, so divde out an extra 10
   ! -- If we ever want uncertainty like (99), this line will be wrong
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
      if (one_digit==1) then
         sf_err = 2
      else
         sf_err = 1
      endif
   case (MODE_UNCERT_SF_NINETEEN)
      ! -- If the first digit of two_digit is one, two digits
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

   if (ld) write(olun,'(a,2x,es15.8,2x,a5,2i4)') ' == Processed uncertainty: ',&
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
! -- For now, failed test is the same as error in the test
! ------------------------------------------------------------------------------
subroutine pvu_test_process_uncertainty( real_err, expected_str, expected_sf, &
      expected_lsd_exp, ier)
   real(wp), intent(IN) :: real_err
   character(*), intent(IN) :: expected_str     ! -- Wildcard length
   integer, intent(IN) :: expected_sf, expected_lsd_exp
   integer, intent(OUT) :: ier
   integer :: received_sf, received_lsd_exp
   character(strlen) :: received_str

   ier = 0

   call pvu_process_uncertainty( real_err, received_str, received_sf, &
      received_lsd_exp, ier )
   if (ier/=0) goto 99

   if ( (received_str/=expected_str) .OR. (received_sf/=expected_sf) .OR. &
         (received_lsd_exp/=expected_lsd_exp) ) then
      write(olun,*) '** Failed test for uncertainty: ', real_err
      write(olun,*) '** str:     "'//trim(expected_str)//&
         '" "'//trim(received_str)//'"'
      write(olun,*) '** sf:      ', expected_sf, received_sf
      write(olun,*) '** lsd_exp: ', expected_lsd_exp, received_lsd_exp
      goto 99
   endif

   return
99 write(olun,*) '** Error in subroutine pvu_test_process_uncertainty'
   ier = 1
end subroutine pvu_test_process_uncertainty

! ------------------------------------------------------------------------------
! -- Call the gen str routine and compare to expected string
! -- For now, failed test is the same as error in the test
! ------------------------------------------------------------------------------
subroutine pvu_test_gen_str( real_val, real_err, expected_str, ier)
   real(wp), intent(IN) :: real_val, real_err
   character(*), intent(IN) :: expected_str
   integer, intent(OUT) :: ier
   character(len=strlen) :: received_str

   ier = 0

   call pvu_gen_str( real_val, real_err, received_str, ier)
   if (ier/=0) goto 99

   if (received_str/=expected_str) then
      write(olun,*) '** Failed test for gen_str'
      write(olun,*) '** val, err: ', real_val, real_err
      write(olun,*) '** expected, received: "'//trim(expected_str)//'" "'// &
         trim(received_str)//'"'
      goto 99
   endif

   return
99 write(olun,*) '** Error in subroutine pvu_test_gen_str'
   ier = 1
end subroutine pvu_test_gen_str

! ------------------------------------------------------------------------------
! -- Write a value/uncertainty pair in two ways to a latex file
! -- Intended to be compiled and manually compared to each other
! -- pvu_test_tex_init must be called first
! ------------------------------------------------------------------------------
subroutine pvu_test_tex_write( real_val, real_err, ier)
   real(wp), intent(IN) :: real_val, real_err
   integer, intent(OUT) :: ier
   character(len=strlen) :: str1, str2

   ! -- Generate two ways
   call pvu_config(ier, output_format=OUTPUT_FORMAT_SIUNITX_PLUSMINUS)
   if (ier/=0) goto 99

   call pvu_gen_str( real_val, real_err, str1, ier)
   if (ier/=0) goto 99

   call pvu_config(ier, output_format=OUTPUT_FORMAT_LATEX_PARENTHESIS)
   if (ier/=0) goto 99

   call pvu_gen_str( real_val, real_err, str2, ier)
   if (ier/=0) goto 99

   call pvu_config(ier, output_format=OUTPUT_FORMAT_DEFAULT)
   if (ier/=0) goto 99

   write(fid_tt,'(3x,a,2x,"&",3x,a,2x,"\\")') str1, str2

   return
99 write(olun,*) '** Error in subroutine pvu_test_tex_write'
   ier = 1
end subroutine pvu_test_tex_write

! ------------------------------------------------------------------------------
! -- Initialize latex file to have values written to it
! ------------------------------------------------------------------------------
subroutine pvu_test_tex_init(ier)
   integer, intent(OUT) :: ier

   ier = 0

   open(fid_tt, file=trim(fname_tex_test), action='write', status='replace')

   write(fid_tt,'(a)') '\documentclass{article}'
   write(fid_tt,'(a)') '\usepackage{siunitx}'
   write(fid_tt,'(a)') '\usepackage[onehalfspacing]{setspace}'
   write(fid_tt,'(a)') '\begin{document}'
   write(fid_tt,'(a)') '\begin{tabular}{ l r }'
   write(fid_tt,'(a)') '   With siunitx & Vanilla \LaTeX \\'

end subroutine pvu_test_tex_init

! ------------------------------------------------------------------------------
! -- Finalize the latex file
! ------------------------------------------------------------------------------
subroutine pvu_test_tex_finalize(ier)
   integer, intent(OUT) :: ier

   ier = 0

   write(fid_tt,'(a)') '\end{tabular}'
   write(fid_tt,'(a)') '\end{document}'

   close(fid_tt)

   write(olun,*) '-- Wrote test latex file, build: "pdflatex '//&
      trim(fname_tex_test)//'"'

end subroutine pvu_test_tex_finalize

! ------------------------------------------------------------------------------
! -- Perform self test, primarily to aid development. Only works with defaults.
! -- On success, PASSED is printed with ier=0
! -- On failure, FAILED is printed with ier=1
! -- Should be run with the defaults
! ------------------------------------------------------------------------------
subroutine pvu_self_test(ier)
   integer, intent(OUT) :: ier
   integer :: jer

   ier = 0

   ! -- Test just uncertainty
   call pvu_test_process_uncertainty(0.0123_wp, '12', 2, -3, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0149_wp, '15', 2, -3, jer)
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

   ! -- These depend on mode, only a few modes were tested
   call pvu_test_process_uncertainty(0.0150_wp, '15', 2, -3, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0179_wp, '18', 2, -3, jer)
   if (jer/=0) ier = 2

   call pvu_config(jer, mode_uncert_sf=MODE_UNCERT_SF_FIFTEEN)
   if (jer/=0) goto 99

   call pvu_test_process_uncertainty(0.0150_wp, '2', 1, -2, jer)
   if (jer/=0) ier = 2
   call pvu_test_process_uncertainty(0.0179_wp, '2', 1, -2, jer)
   if (jer/=0) ier = 2

   call pvu_config(jer, mode_uncert_sf=MODE_UNCERT_SF_DEFAULT)
   if (jer/=0) goto 99

   ! -- Test full
   call pvu_test_gen_str(1.234_wp, 0.012_wp, '\num{1.234 \pm 0.012}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1.234_wp, 0.024_wp, '\num{1.23 \pm 0.02}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1.234_wp, 20.024_wp, '\num{0 \pm 2 e1}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(35.234_wp, 0.024_wp, '\num{35.23 \pm 0.02}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(8.123_wp, 12.024_wp, '\num{8 \pm 12}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(99.999_wp, 0.01_wp, '\num{99.999 \pm 0.010}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(99.9999_wp, 0.01_wp, '\num{100.000 \pm 0.010}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(99.99999_wp, 12._wp, '\num{100 \pm 12}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(99.99999_wp, 30._wp, '\num{1.0 \pm 0.3 e2}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(100._wp, 0._wp, '\num{100(0)}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(0._wp, 0._wp, '\num{0(0)}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1.234e-1_wp, 0.05e-1_wp, '\num{0.123 \pm 0.005}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1.234e-2_wp, 0.05e-2_wp, '\num{0.0123 \pm 0.0005}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1.234e-3_wp, 0.05e-3_wp, '\num{1.23 \pm 0.05 e-3}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1.234e-4_wp, 0.05e-4_wp, '\num{1.23 \pm 0.05 e-4}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1.234_wp, 0.05_wp, '\num{1.23 \pm 0.05}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(12.34_wp, 0.5_wp, '\num{12.3 \pm 0.5}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(123.4_wp, 5._wp, '\num{123 \pm 5}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(123.4_wp, 50._wp, '\num{1.2 \pm 0.5 e2}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1234._wp, 50._wp, '\num{1.23 \pm 0.05 e3}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(1234._wp, 5._wp, '\num{1.234 \pm 0.005 e3}', jer)
   if (jer/=0) ier = 2
   call pvu_test_gen_str(6.123e-5_wp, 13.123e-5_wp, '\num{6 \pm 13 e-5}', jer)
   if (jer/=0) ier = 2

   ! -- Test siunitx by writing to latex file in two ways
   call pvu_test_tex_init(jer)
   if (jer/=0) goto 99

   call pvu_test_tex_write( 3.0_wp, 3.0_wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 3.0_wp, 0.3_wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 1.234_wp, 0.012_wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 123.53_wp, 0.9_wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 2500.53_wp, 0.9_wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 2500.53_wp, 10.9_wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 1._wp, 0._wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 100._wp, 0._wp, jer)
   if (jer/=0) goto 99
   call pvu_test_tex_write( 0._wp, 0._wp, jer)
   if (jer/=0) goto 99

   call pvu_test_tex_finalize(jer)
   if (jer/=0) goto 99

   if (ier==0) then
      write(olun,'(a,20x,a)') ' -- Testing parse_value_uncertainty module', &
         '===== PASSED ====='
   else
      write(olun,'(a,20x,a)') ' -- Testing parse_value_uncertainty module', &
         '===== FAILED ====='
   endif

   return
99 write(olun,*) '** Error in subroutine pvu_self_test'
   write(olun,*) '** Testing parse_value_uncertainty module ===== FAILED ====='
   ier = 1
end subroutine pvu_self_test

end module parse_value_uncertainty
! ------------------------------------------------------------------------------
