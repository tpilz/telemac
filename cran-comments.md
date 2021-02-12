## Test environments
* local linux openSUSE LEAP 15.2, R 4.0.3
* win-builder (release, devel)

## R CMD check results


* Maintainer: 'Tobias Pilz <topilz@pik-potsdam.de>'

  New submission

* Possibly mis-spelled words in DESCRIPTION:
   TELEMAC (3:27, 4:36, 6:40)
   
   -> This was intentionally written in captial letters.

* Package has a FOSS license but eventually depends on the following package which restricts use: RTriangle

  -> License of RTriangle is CC BY-NC-SA 4.0 (which is, I think, no appropriate software license). Further License not at https://github.com/davidcsterratt/RTriangle/blob/master/pkg/LICENSE.note. I think is should not be a problem for me choosing GPL for my telemac package.
