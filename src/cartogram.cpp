#include <iostream>
#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <math.h>

// //' @title Function dougenik_core_cpp
// //' 
// //' @description Computes point moves in Dougenik et al. algorithm
// //' 
// //' @param pts complex, the coordinates of the current points
// //' @param C complex vector, the centroid coordinates of all polygons
// //' @param radii double vector, the "radius" of each polygon sensu Dougenik
// //' @param masses double vector, the "mass" of each polygon sensu Dougenik
// //' @param forceReductionFactor double, the force Reduction Factor sensu Dougenik
// //'
// //' @return new coordinates of the points as a complex number.
// //'
// [[Rcpp::export]]
Rcpp::ComplexVector dougenik_core_cpp(Rcpp::ComplexVector pts, Rcpp::ComplexVector C, Rcpp::DoubleVector radii, Rcpp::DoubleVector masses, double forceReductionFactor) {
    int n = C.size();
    const std::complex<double> c01 (0.0, 1.0);
    int nP = pts.size();
    Rcpp::ComplexVector ptsNew(nP);
    for (int ic=0; ic<nP; ic++) {
        std::complex<double> vecSum (0.0, 0.0);
        Rcomplex c = pts[ic];
        std::complex<double> cc (c.r, c.i);
        for (int i=0; i<n; i++) {
            std::complex<double> Ci (C[i].r, C[i].i);
            std::complex<double> v = cc - Ci;
            double distance = std::abs(v);
            double radius = radii[i];
            double mass = masses[i];
            double Fij;
            if (distance > radius) {
                Fij = mass * radius / distance;
            } else {
                Fij = mass * pow(distance/radius,2) * (4 - (3 * distance / radius));
            }
            std::complex<double> vv = std::polar(Fij, std::arg(v));
            vecSum += vv;
        }
        std::complex<double> resu = cc + (forceReductionFactor * vecSum);
        Rcomplex Resu; Resu.r = std::real(resu); Resu.i = std::imag(resu);
        ptsNew[ic] = Resu;
    }
    return(ptsNew);
}
