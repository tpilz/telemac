# Package development
- add more unit tests
- improve performance
    - use RcppArmadillo for matrix and vector operations in c++ code
    - read/writing of possibly large files (should only be effective for geo and results files)

# Functionalities
- get flow through defined section
    - replace control section output in TELEMAC-2D; should be part of post-processing  and independent of model setup (more flexible)
    - input: 2 points / coordinates or a line
    - define line based on two node points, i.e. equally spaced points with specific resolution that form the line
    - interpolate mesh point values of water depth and velocity (u and v) from results file to line vertices (use telemac::interpol())
    - get discharge in u and v direction (h*v and h*u)
    - compute flux though polyline (see also flux_2d() in scripts/python3/data_manip/computation/polyline_integrals.py)
        - get normalised coordinates for each vertex with first point being (0,0)
        - get segment length, i.e. length of each vertex to previous vertex
        - calculate fluxes as weighted sums over vertices (i):
                product_i = flux_x[i]*normals[i+1][0] + flux_y[i]*normals[i+1][1]
                product_i_plus_1 = flux_x[i+1]*normals[i+1][0] + flux_y[i+1]*normals[i+1][1]
                flux += (product_i + product_i_plus_1)*lengthes[i+1]/2.
- support holes (inner boundaries) in TIN generation (so far only outer boundary and breaklines)
- Gmsh support: add function to prepare Gmsh input (geometry file as in pputils gis2gmsh.py)
- line_spacing(): works for segments, should also work for closed boundaries (distance between first and last point should match)

# CRAN submission (general)
- In case of incompatible licence of a package in `Imports`: move package to `Suggests` (in case the package is not fundamentally required, in such a case an own implementation would be required)
- Add `@return` to *every* function (automated checks do not complain but cran reviewer did in my case)
