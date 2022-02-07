# version 0.1.1, Feb 2022
## Enhancements
- added unit tests (so far only for `tin()`) and codecov integration
- added `read_msh()`
- `tin()` supports Gmsh mesh output (only format version 2) to infer TIN
- added package documentation `?telemac`
- improved performance
  - `tin()` (determination of ikle matrix)
- compliance with dplyr 1.0.8

## Fixes
- `tin()`
  - check of curve orientation of outer boundary
- `cas_lineadapt()`
  - break of line within keyword or filename caused TELEMAC error

# version 0.1.0, Feb 2021, first release 
