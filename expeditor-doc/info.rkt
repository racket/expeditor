#lang info

(define collection "expeditor")

(define scribblings '(("expeditor.scrbl" (multi-page) (tool-library))))

(define build-deps '("scribble-lib"
                     "expeditor-lib"
                     "racket-doc"))
(define deps '("base"))
(define update-implies '("expeditor-lib"))

(define pkg-desc "documentation part of \"expeditor\"")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
