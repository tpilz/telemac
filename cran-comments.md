## Resubmission
This is a resubmission. Added missing `\value` tags to .Rd files of exported methods.

3 further NOTEs remain, see below.

## Test environments
* local linux openSUSE LEAP 15.2, R 4.0.3
* win-builder, R 4.0.3, devel r79988
* Mac OS X 10.15.7 (github actions), R 4.0.3
* Microsoft Windows Server 2019 10.0.17763 (github actions), R 4.0.3
* Ubuntu 20.04.2 (github actions), R 4.0.3, devel r79883

## R CMD check results
0 errors, 0 warnings, 3 notes:

* Maintainer: 'Tobias Pilz <topilz@pik-potsdam.de>'

  New submission

* Possibly mis-spelled words in DESCRIPTION:
   TELEMAC (3:27, 4:36, 6:40)
   
   -> This was intentionally written in captial letters.

* installed size is  5.4Mb
    sub-directories of 1Mb or more:
      doc       3.7Mb
      telemac   1.0Mb
      
  -> According to CRAN Repository Policy "neither data nor documentation should exceed 5MB". As this is the case under all test environments I think this NOTE should not be an issue.
