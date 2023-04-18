## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Last CRAN feedback
- Having package code which is run as part of the checks and attempts to write to the user library violates the CRAN Policy's

## FIX
- a 'read only' flag was missing - added it (in ebv_datacubepaths.R)
