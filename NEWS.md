# npfda 0.1-3 (2023-05-12) 

* Added `h.cv()` S3 methods for `npf.bin` and `npf.bin.res2` classes.
  

# npfda 0.1-2 (2023-05-10) 

* Added `svar.bin()` S3 methods for `npf.data` and 
  `npf.locpol` classes (results `data` components are not fully compatible 
  with the `npsp` package).

* Added `np.svar()` S3 method for `npf.data` class.


# npfda 0.1-1 (2023-05-07) 

* Added `npf.bin.res2()` and S3 generic `np.var()` (with methods for classes 
  `npf.locpol` and `npf.bin.res2`; results `data` components are not fully compatible 
  with the `npsp` package). 
  
* Added `predict()` S3 methods for classes `npf.locpol` and `npf.var`.

* Changes in the `data` component of the return value of `npf.binning()`
  (results are now not fully compatible with the `npsp` package;
  care should be taken when using this function together with `npsp::as.bin.data()`).

* Classes `npf.locpol.bin` and `npf.bin.data` renamed as `npf.locpol` and `npf.bin`, 
  respectively.

  
# npfda 0.1-0 (2023-05-01) 

* Initial version in package form.
  
