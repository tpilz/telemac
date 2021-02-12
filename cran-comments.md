## Test environments
* local linux openSUSE LEAP 15.2, R 4.0.3
* win-builder, R 4.0.3, devel r79988
* Mac OS X 10.15.7 (github actions), R 4.0.3
* Microsoft Windows Server 2019 10.0.17763 (github actions), R 4.0.3
* Ubuntu 20.04.2 (github actions), R 4.0.3, devel r79883

## R CMD check results
0 errors, 0 warnings, 4 notes:

* Maintainer: 'Tobias Pilz <topilz@pik-potsdam.de>'

  New submission

* Possibly mis-spelled words in DESCRIPTION:
   TELEMAC (3:27, 4:36, 6:40)
   
   -> This was intentionally written in captial letters.

* Package has a FOSS license but eventually depends on the following package which restricts use: RTriangle

  -> License of RTriangle is CC BY-NC-SA 4.0 (which is, I think, no appropriate software license). Further License information at https://github.com/davidcsterratt/RTriangle/blob/master/pkg/LICENSE.note. I think it should not be a problem for me choosing GPL for my telemac package.

* installed size is  5.4Mb
    sub-directories of 1Mb or more:
      doc       3.7Mb
      telemac   1.0Mb
      
  -> According to CRAN Repository Policy "neither data nor documentation should exceed 5MB". As this is the case under all test environments I think this NOTE should not be an issue.
