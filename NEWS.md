# npfda 0.2-1 (2026-11-12)

* Added S3 generic `as.npf.data()`, with methods for `array` 
  and `fdata` classes.

* Added S3 generic `as.fdata()`, with a method for `npf.data` class.
  
  
# npfda 0.2-0 (2025-06-11) 

* Added `ozone2` data (updated raw data with missing values).

* Changes in `svar.bin()` S3-methods:

    - Changes in `svar.bin.npf.data()` to support missing data.

    - Changed the default value of the `maxlag` argument.

    - Changed the default value of the `nlags` argument when the `maxlag` 
      argument is specified.
      
* Added `standardized` argument to `residuals.npf.model()` method.


# npfda 0.1-4 (2023-05-15) 

* Added S3 generic `npf.fit()` (with methods for `npf.data` 
  and `npf.locpol` classes).
  
* Updated package vignette.  

* Added pkgdown function reference index.  


# npfda 0.1-3 (2023-05-12) 

* Added `h.cv()` S3 methods for `npf.bin` and `npf.bin.res2` classes.

* Added `npf.model()` (constructor of `npf.model`-class) 

* Added `residuals()` S3 methods for `npf.locpol` and `npf.model` classes.

* Added `plot()` S3 methods for `npf.locpol` and `npf.model` classes.
  

# npfda 0.1-2 (2023-05-10) 

* Added `svar.bin()` S3 methods for `npf.data` and 
  `npf.locpol` classes (results `data` components are not fully compatible 
  with the `npsp` package).

* Added `np.svar()` S3 method for `npf.data` class.


# npfda 0.1-1 (2023-05-07) 

* Added `npf.bin.res2()` and S3 generic `np.var()` (with methods for `npf.locpol` 
  and `npf.bin.res2` classes; results `data` components are not fully compatible 
  with the `npsp` package). 
  
* Added `predict()` S3 methods for `npf.locpol` and `npf.var` classes.

* Changes in the `data` component of the return value of `npf.binning()`
  (results are now not fully compatible with the `npsp` package;
  care should be taken when using this function together with `npsp::as.bin.data()`).

* Classes `npf.locpol.bin` and `npf.bin.data` renamed as `npf.locpol` and `npf.bin`, 
  respectively.

  
# npfda 0.1-0 (2023-05-01) 

* Initial version in package form.
  
