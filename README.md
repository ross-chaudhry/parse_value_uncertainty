# Parse Value Uncertainty

Constructs a string for a given value and uncertainty

Author: Ross S. Chaudhry, August 2018

Sample usage:

       use parse_value_uncertainty, only pvu_gen_str
       ...
       call pvu_gen_str( 1.23456, 0.087, str_out, ier)
       ! str_out now contains '\num{1.23 +- 0.09}', suitable for siunitx in latex

Designed for REAQCT (Univ. of Minnesota's Quasi-Classical Trajectory code),
this module was adapted for general use. 
Hosted on the author's personal github with MIT license:
https://github.com/ross-chaudhry

Output type, positive and negative threshold to use scientific notation,
and uncertainty significant figure behavior are configurable.

A good reference is Taylor, "An Introduction to Error Analysis", 1982.
Also Taylor and Kuyatt, "Guidelines for Evaluating and Expressing the
Uncertainty of NIST Measurement Results", 1994

### Public Subroutines
* pvu_config                     - Set configuration options
* pvu_gen_str                    - Generate string from value and uncertainty
* pvu_self_test                  - Check things are working

More complete documentation is in the main source file.
Relevant languages/tools: fortran, latex, siunitx.
