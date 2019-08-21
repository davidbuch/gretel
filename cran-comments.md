## Third Submission
"""
Thanks,

Please always and only write package names, software names and API names 
in single quotes in the title and the description field.
f.i.: --> 'gretel'  &  --> ... generalized path value function ...

Please fix and resubmit.

Best,
Martina Schmirl
"""

Comments were addressed on 8-21-2019
* Fixed usage of single quotes in description field of DESCRIPTION

## Second submission
Reviewer Commments:
"""
Thanks,

The Description field is intended to be a (one paragraph) description
of what the package does and why it may be useful.
Please elaborate.

If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: authors (year) <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for 
auto-linking.

Please always write TRUE and FALSE instead of T and F.

Best,
Martina Schmirl
""""

Comments were addressed on 08-20-2019
* Changed all 'T/F' logicals to 'TRUE/FALSE'
* Elaborated on description field  in DESCRIPTION
* Added references to description field in DESCRIPTION
* Re-ran R CMD check --as-cran with no new warnings, errors or notes

## Test environments
* local OS X install, R 3.6.1 and R-dev
* win-builder (devel and release)

## "R CMD check --as-cran" Results
There were no ERRORs or WARNINGs

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘David Buch <davidbuch42@gmail.com>’
New submission

