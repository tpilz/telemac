#include <RcppArmadillo.h>
using namespace Rcpp;


// INTERNAL FUNCTIONS

// Euclidean distance between two points (x1,y1) and (x2,y2)
double dist(double x1, double y1, double x2, double y2) {
  return std::sqrt(std::pow(x1-x2, 2) + std::pow(y1-y2, 2));
}

// // get indices of ordered x
// IntegerVector order(NumericVector x) {
//   NumericVector sorted = clone(x).sort();
//   return match(sorted, x) - 1;
// }


// EXTERNAL FUNCTIONS (USED IN R CODE)

// T: two columns (x and y coordinates); each row is a point, 3 subsequent rows define a triangle
// P: two columns (x and y coordinates); unique points given in T
// output: Matrix; each row a triangle defined by 3 points (cols); no. of rows = nrow(T) / 3;
// values are row indices of P giving the x and y coordinates of a point; each triangle oriented in CCW direction
// [[Rcpp::export]]
IntegerMatrix find_ikle(NumericMatrix T, NumericMatrix P) {

  IntegerMatrix M( T.nrow()/3, 3);
  int h = 0;
  int k = 0;
  double x1, x2, x3, y1, y2, y3;
  int t0, t2;

  for (int i = 0; i < T.nrow(); ++i) {
    k = i / 3;

    // find current point in T
    for (int j = 0; j < P.nrow(); ++j) {
      if ( (T( i, 0) == P( j, 0)) && (T( i, 1) == P( j, 1)) ) {
        // point found, write index into M
        M( k, h) = j + 1;
        break;
      }
    }

    // check CCW direction of points and correct if necessary
    if (h == 2) {
      x1 = P( M(k, 0)-1, 0);
      x2 = P( M(k, 1)-1, 0);
      x3 = P( M(k, 2)-1, 0);
      y1 = P( M(k, 0)-1, 1);
      y2 = P( M(k, 1)-1, 1);
      y3 = P( M(k, 2)-1, 1);
      if ( (y3-y1) * (x2-x1) < (y2-y1) * (x3-x1) ) {
        t0 = M(k, 0);
        t2 = M(k, 2);
        M(k, 0) = t2;
        M(k, 2) = t0;
      }
    }

    h = (h+1) % 3;
  }

  return M;
}


// ikle: Mesh connectivity matrix, e.g. output of find_ikle()
// x: vector for X coordinates of mesh points (values in ikle point to positions in x and y)
// y: vector for Y coordinates of mesh points (values in ikle point to positions in x and y)
// output: vector of mesh boundaries needed by Telemac
// Function based on Fortran code of bnd_extr_stbtel.f90 in pputils toolbox which in turn is
// based on Telemac code; code translated into C++ and cleaned.
// [[Rcpp::export]]
IntegerVector find_ipobo(IntegerMatrix ikle, NumericVector x, NumericVector y) {

  // declare / assign helper / iteration variables
  int npoin = x.size();
  int nelem = ikle.nrow();
  int nface = 3;
  int ndp = 3;
  int idimat = ndp*2*nelem;
  IntegerVector mat1(idimat);
  IntegerVector mat2(idimat);
  IntegerVector mat3(idimat);
  IntegerVector nvois(npoin);
  IntegerVector iadr(npoin);
  IntegerMatrix ifabor(nelem,3);
  IntegerMatrix trav1(npoin,2);
  IntegerVector nbor(npoin);
  int g, h, i1, i2, m1, m2, ii, ji;
  int nptfr, noeud1, noeud2, nile;
  double som1, som2, y2;
  bool swap;

  // nvois: occurrences of each point in triangles (ikle)
  // sum of nvois gives idimat
  for (int i = 0; i < nface; ++i) {
    h = (i+1) % 3;
    for (int j = 0; j < nelem; ++j) {
      i1 = ikle(j,i) - 1;
      i2 = ikle(j,h) - 1;
      nvois[i1] = nvois[i1] + 1;
      nvois[i2] = nvois[i2] + 1;
    }
  }

  // addresses of each point
  iadr[0] = 1;
  for (int i = 1; i < npoin; ++i) {
    iadr[i] = iadr[i-1] + nvois[i-1];
  }

  // fill ifabor (if -1 it is a bounbdary point)
  for (int i = 0; i < nface; ++i) {
    h = (i+1) % 3;
    for (int j = 0; j < nelem; ++j) {
      ifabor(j,i) = -1;

      // global node numbers
      i1 = ikle(j,i) - 1;
      i2 = ikle(j,h) - 1;

      // ordered node numbers
      m1 = std::min(i1,i2);
      m2 = std::max(i1,i2);

      for (int k = 0; k < nvois[m1]; ++k) {
        g = iadr[m1]+k-1;
        if (mat1[g] == 0) {
          mat1[g] = m2;
          mat2[g] = j;
          mat3[g] = i;
          break;
        } else if (mat1[g] == m2) {
          ji = mat2[g];
          ii = mat3[g];
          ifabor(j,i) = ji;
          ifabor(ji,ii) = j;
          break;
        }
      }
    }
  }

  // fill matrix trav1 (external boundary points)
  // and assign nptfr (number of external boundary points)
  nptfr = 0;
  for (int j = 0; j < nelem; ++j) {
    for (int i = 0; i < nface; ++i) {
      h = (i+1) % 3;
      if (ifabor(j,i) < 0) {
        trav1(nptfr,0) = ikle(j,i) - 1;
        trav1(nptfr,1) = ikle(j,h) - 1;
        nptfr += 1;
      }
    }
  }

  //Rcout << "nptfr is " << nptfr << std::endl;

  // bring trav1 into correct form
  som2 = x[0] + y[0];
  y2 = y[0];
  g = nptfr - 1;
  for (int i = 0; i < nptfr; ++i) {
    h = trav1(i,0);
    som1 = x[h] + y[h];
    if (std::abs(som1-som2) < std::abs(1e-12*som1)) {
      if (y[h] < y2) {
        y2 = y[h];
        som2 = som1;
        g = i;
      }
    } else if (som1 < som2) {
      y2 = y[h];
      som2 = som1;
      g = i;
    }
  }

  //Rcout << "g is " << g << std::endl;
  noeud1 = trav1(g,0);
  noeud2 = trav1(g,1);
  trav1(g,0) = trav1(0,0);
  trav1(g,1) = trav1(0,1);
  trav1(0,0) = noeud1;
  trav1(0,1) = noeud2;

  nile = 0;
  for (int i = 1; i < nptfr; ++i) {
    swap = false;
    for (int j = i; j < nptfr; ++j) {
      if (trav1(j,0) == trav1(i-1,1)) {
        noeud1 = trav1(j,0);
        noeud2 = trav1(j,1);
        trav1(j,0) = trav1(i,0);
        trav1(j,1) = trav1(i,1);
        trav1(i,0) = noeud1;
        trav1(i,1) = noeud2;
        swap = true;
        break;
      }
    }
    if (swap) continue;
    if (trav1(nile,0) != trav1(i-1,1))
      stop("error in storing edge segments for node %i", i);
    nile = i;
  }

  // output nbor
  for (int i = 0; i < nptfr; ++i) {
    nbor[i] = trav1(i,0) + 1;
  }

  return nbor;
}


// ikle: Mesh connectivity matrix, e.g. output of find_ikle().
// Get triangle edges from ikle matrix; one edge per line, columns are xy coordinates.
// There will be duplicated lines that can subsequently be removed via unique(edgm)
// which is (slightly) faster than programming it in c++ (with the solutions I found).
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::Mat<int> get_edges(arma::Mat<int> ikle) {
  unsigned int n = ikle.n_rows, p = ikle.n_cols, np = n*p;
  // matrix pointing to ikle
  arma::Row<int> rowi(n);
  // output matrix
  arma::Mat<int> edgma(np, 2);
  arma::uvec edg(2);
  unsigned int k, h;

  // loop over triangles
  h = 0;
  for (unsigned int i = 0; i < n; ++i) {
    rowi = ikle.row(i);
    // p edges (3 per triangle); sorted
    for (unsigned int j = 0; j < p; ++j) {
      k = (j+1) % p;
      edg = {j, k};
      edgma.row(h) = sort(rowi.elem(edg).t());
      h += 1;
    }
  }

  return edgma;
}


// Based on code of package interp (C++ function interpDeltri)
// x: vector for X coordinates of mesh points (values in ikle point to positions in x and y)
// y: vector for Y coordinates of mesh points (values in ikle point to positions in x and y)
// z: vector of values for each mesh point that are used for interpolation
// ikle: Mesh connectivity matrix, e.g. output of find_ikle()
// xgrd: x values of target grid
// ygrd: y values of target grid
// grd: output grid to which mesh values z shall be linearly interpolated
// [[Rcpp::export]]
NumericMatrix interpol_grd(NumericVector x, NumericVector y, NumericVector z, IntegerMatrix ikle,
                           NumericVector xgrd, NumericVector ygrd, NumericMatrix grd) {

  // no of mesh elements
  int nelem = ikle.nrow();
  int ndp = ikle.ncol();
  // dimensions of target grid
  int nx = grd.nrow();
  int ny = grd.ncol();
  // coordinates and values of a mesh element
  NumericVector xt(ndp);
  NumericVector yt(ndp);
  NumericVector zt(ndp);
  // mesh bounding box
  double xsw, ysw, xne, yne;
  // mesh bounding box as grid indices
  int gxsw, gysw, gxne, gyne;
  // barycentric coordinates
  double a, b, c, denom;

  // determine bounding boxes (SW <-> NE) of all mesh elements
  for (int i=0; i<nelem; i++) {
    // get coordinates of mesh element's points
    xt = x[ikle(i,_)-1];
    yt = y[ikle(i,_)-1];
    zt = z[ikle(i,_)-1];

    // bounding box
    xsw = min(xt);
    ysw = min(yt);
    xne = max(xt);
    yne = max(yt);

    // element bounding box as grid indices
    gxsw = 0;
    gysw = 0;
    gxne = nx;
    gyne = ny;
    for (int j=0; j<nx; j++){
      if(xgrd[j] < xsw) gxsw = j;
      if(xgrd[nx-j-1] > xne) gxne = nx-j-1;
    }
    for (int k=0; k<ny; k++){
      if(ygrd[k] < ysw) gysw = k;
      if(ygrd[ny-k-1] > yne) gyne = ny-k-1;
    }

    // iterate over grid points inside mesh element bounding box
    for (int j = gxsw; j < gxne; j++) {
      for (int k = gysw; k < gyne; k++) {
        // barycentric coordinates
        denom = (yt[1] - yt[2]) * (xt[0] - xt[2]) + (xt[2] - xt[1]) * (yt[0] - yt[2]);
        a = ((yt[1] - yt[2])*(xgrd[j] - xt[2]) + (xt[2] - xt[1])*(ygrd[k] - yt[2])) / denom;
        b = ((yt[2] - yt[0]) * (xgrd[j] - xt[2]) + (xt[0] - xt[2]) * (ygrd[k] - yt[2])) / denom;
        c = 1. - a - b;

        // check if point is inside triangle
        if(0. <= a && a <= 1. && 0. <= b && b <= 1. && 0. <= c && c <= 1.) {
          // linear interpolation
          grd(j,k) = a * zt[0] + b * zt[1] + c * zt[2];
        }
      }
    }
  }

  return grd;
}


// Function analyses all segments of a line; inserts additional vertices if segment
// is longer than s by simple linear interpolation and ensuring new segments lengths
// are exactly s;
// If segment lengths are generally much smaller than s the function might return strange results.
// [[Rcpp::export]]
DataFrame line_seg_adapt(NumericVector x, NumericVector y, double s) {
  int n = x.size();
  int np;
  int h = 0;
  double len, dx, dy, xh, yh;
  double sh = s / 2.;
  NumericVector outx, outy;

  outx.push_back(x[0]);
  outy.push_back(y[0]);
  for (int i = 1; i < n; ++i) {
    // segment length
    len = dist(x[i], y[i], outx[h], outy[h]);

    // segment too long
    if (len > sh) {
      // insert extra points
      dx = (x[i] - outx[h]) / len; // change in x
      dy = (y[i] - outy[h]) / len; // change in y
      np = round(len / s);
      for (int j = 0; j < np; ++j) {
        xh = outx[h] + s * dx;
        outx.push_back(xh);
        yh = outy[h] + s * dy;
        outy.push_back(yh);
        h++;
      }
    }
    // segment too short: go to next point
  }

  return(DataFrame::create(_["x"] = outx, _["y"] = outy));
}


// note: works but is very slow, use gstat::idw() instead
// // Interpolation via inverse distance weighting using nearest neighbours
// // src: Matrix with 2 columns of x and y coordinates of source locations
// // z: vector of values at src location to be interpolated to trg locations
// // trg: Matrix with 2 columns of x and y coordinates of target locations
// // n: number of nearest neighbours to include
// // [[Rcpp::export]]
// NumericVector interpol_idw_nn(NumericMatrix src, NumericVector z, NumericMatrix trg, int n) {
//   int ns = src.nrow();
//   int nt = trg.nrow();
//   double wsum = 0.;
//   NumericVector dists(ns);
//   IntegerVector idsort(ns);
//   NumericVector wgt(n);
//   NumericVector out(nt);
//
//   // loop over target locations
//   for (int i = 0; i < nt; ++i) {
//     wsum = 0.;
//
//     // calculate distance to each source location
//     for (int j = 0; j < ns; ++j)
//       dists[j] = dist(src(j, 0), src(j, 1), trg(i, 0), trg(i, 1));
//
//     // get n nearest neighbours
//     idsort = order(dists);
//     dists.sort();
//
//     // calculate interpolation weights from inverse distances
//     for (int k = 0; k < n; ++k) {
//       wgt[k] = 1. / pow(dists[k], 2);
//       wsum += wgt[k];
//     }
//
//     // interpolate value at target location
//     for (int k = 0; k < n; ++k) {
//       out[i] += z[idsort[k]] * wgt[k] / wsum;
//     }
//   }
//
//   return out;
// }
