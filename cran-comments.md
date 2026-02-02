## Resubmission
This is a resubmission. In this version I have:

* Changed title to reduce redundancy. Added citation to description field. 
  Updated ReadMe and other associated files.
* Improved ggstrat_bulk_save() to conform to CRAN policy so user must enter 
  desired save directory and example uses \donttest (due to run time), but also
  otherwise writes to tempdir().
* load_data functions now return message that can be suppressed instead of 
  automatically writing to the console.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
