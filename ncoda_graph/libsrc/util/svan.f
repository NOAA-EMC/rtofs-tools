      real function svan (s, t, p0, sigma)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  svan
c
c DESCRIPTION:  Computes specific volume anomaly (steric anomaly)
c               based on 1980 equation of state for seawater and
c               the 1978 practical salinity scale.
c
c REFERENCES:   Millero et al. (1980), Deep Sea Res., 27A, 255-264
c               Millero and Poisson (1981), Deep Sea Res., 28A, 625-629.
c
c UNITS:        Pressure         p0        Decibars
c               Temperature      t         Deg Celsius (IPTS-68)
c               Salinity         s         (pss-78)
c               Spec. Vol. Ano.  svan      1.0E-8 m**3/kg
c               Density Ano.     sigma     kg/m**3
c
c CHECK VALUES: svan=981.30210E-8 m**3/kg for
c               s = 40 (pss-78), t = 40 deg c, p0=10000 decibars
c               sigma = 59.82037 kg/m**3 for
c               s = 40 (pss-78), t = 40 deg c, p0=10000 decibars
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c      
c PARAMETERS:
c      Name        Type        Usage           Description
c   ----------   ---------    -------    -------------------------
c      p0          real        input     pressure in decibars
c       t          real        input     temperature (celsius)
c       s          real        input     salinity (pss-78)
c   sigma          real        output    density anomaly (kg/m**3)
c    svan          real        output    specific volume anomaly
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..define local parameters
c
      real       r3500
      parameter (r3500 = 1028.1063)
c
      real       r4
      parameter (r4 = 4.8314E-4)
c
      real       dr350
      parameter (dr350 = 28.106331)
c
      real      a, aw, a1
      real      b, bw, b1
      real      c
      real      d, dk, dr35p, dvan
      real      e
      real      gam
      real      kw, k0, k35
      real      p, pk, p0
      real      r1, r2, r3
      real      s, sig, sigma, sr, sva
      real      t
      real      v350p
c
      equivalence (e,d,b1), (bw,b,r3), (c,a1,r2)
      equivalence (aw,a,r1), (kw,k0)
c
c...............................executable..............................
c
c     ..r4 is referred to as c in Millero and Poisson 1981 
c       convert pressure to bars and take square root of salinity 
c 
      p = p0 / 10.  
      sr = sqrt (abs (s)) 
c 
c     ..pure water density at atmospheric pressure  
c       Bigg P.H., (1967) Br. J. Applied Physics, 8, 521-537  
c 
      r1 = ((((6.536332e-9 * t - 1.120083e-6) * t + 1.001685e-4) * t  
     *     -9.095290e-3) * t + 6.793952e-2) * t - 28.263737 
c 
c     ..sea water density at atm. pressure 
c       coefficient involving density 
c       r2 = a in notation of Millero and Poisson 1981 
c 
      r2 = (((5.3875e-9 * t-8.2467e-7) * t+7.6438e-5) * t-4.0899e-3) * t 
     *     + 8.24493e-1 
c 
c     ..r3 = b in notation of Millero and Poisson 1981 
c 
      r3 = (-1.6546e-6 * t + 1.0227e-4) * t - 5.72466e-3 
c 
c     ..international one-atmosphere equation of state of seawater 
c 
      sig = (r4 * s + r3 * sr + r2) * s + r1 
c 
c     ..specific volume at atmospheric pressure 
c 
      v350p = 1. / r3500 
      sva = -sig * v350p / (r3500 + sig) 
      sigma = sig + dr350 
c 
c     ..scale specific vol. anomaly to normally reported units 
c 
      svan = sva * 1.0e+8 
      if (p .eq. 0.) return 
c 
c     ..new high pressure equation of state for seawater 
c       Millero et al, 1980, DSR 27a, 255-264. 
c       constant notation follows article 
c 
c     ..compute compression terms 
c 
      e = (9.1697e-10 * t + 2.0816e-8) * t - 9.9348e-7 
      bw = (5.2787e-8 * t - 6.12293e-6) * t + 3.47718e-5 
      b = bw + e * s 
c 
      d = 1.91075e-4 
      c = (-1.6078e-6 * t - 1.0981e-5) * t + 2.2838e-3 
      aw = ((-5.77905e-7 * t + 1.16092e-4) * t + 1.43713e-3) * t 
     *     - 0.1194975 
      a = (d * sr + c) * s + aw 
c 
      b1 = (-5.3009e-4 * t + 1.6483e-2) * t + 7.944e-2 
      a1 = ((-6.1670e-5 * t + 1.09987e-2) * t-0.603459) * t + 54.6746 
      kw = (((-5.155288e-5 * t + 1.360477e-2) * t-2.327105) * t 
     *     + 148.4206) * t - 1930.06 
      k0 = (b1 * sr + a1) * s + kw 
c 
c     ..evaluate pressure polynomial 
c 
c     ..k equals the secant bulk modulus of seawater 
c       dk = k(s,t,p) - k(35,0,p) 
c       k35 = k(35,0,p) 
c 
      dk = (b * p + a) * p + k0 
      k35 = (5.03217e-5 * p + 3.359406) * p + 21582.27 
      gam = p / k35 
      pk = 1.0 - gam 
      sva = sva * pk + (v350p + sva) * p * dk / (k35 * (k35 + dk)) 
c 
c     ..scale specific volume anomaly to normally reported units 
c 
      svan = sva * 1.0e+8 
      v350p = v350p * pk 
c 
c     ..compute density anomaly with respect to 1000.0 kg/m**3 
c       dr350. density anomaly at 35 (pss-78), 0 deg. c and 0 decibars 
c       dr35p: dentity anomaly 35 (pss-78), 0 deg. c, pres. variation 
c       dvan : density anomaly variations involving specific vol. anom 
c 
c     ..check value: sigma = 59.82037 kg/m**3 for s = 40 (pss-78), 
c       t = 40 deg c, p0 = 10000 decibars. 
c 
      dr35p = gam / v350p 
      dvan = sva / (v350p * (v350p + sva)) 
      sigma = dr350 + dr35p - dvan 
c 
      return
      end
