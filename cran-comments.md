## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Luise Quoss <luise.quoss@idiv.de>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-07-29 for multiple policy
    violations.

  On Internet access. Spamming in HTML the email address of a team
    member.
    
## Last CRAN feedback
- Internet check done wrong

## FIX
- checking the internet using curl::has_internet and additional check for the
  data portal website
