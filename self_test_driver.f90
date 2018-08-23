! -- Main program to run pvu_self_test
program main
   use parse_value_uncertainty, only : pvu_self_test
   implicit none
   integer :: ier

   call pvu_self_test(ier)
   if (ier/=0) stop 1
end program main
