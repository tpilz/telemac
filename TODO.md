# Package development
- add unit tests
    - use `testthat`, see respective chapter in [R Packages](https://r-pkgs.org/tests.html)
    - register at [codecov](https://about.codecov.io)
    - see also package [covr](https://cran.r-project.org/package=covr) 

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
