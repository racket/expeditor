#lang info

(define collection "expeditor")

(define build-deps '("gui-lib"
                     "syntax-color-lib"
                     "expeditor-lib"))
(define deps '("base"))
(define update-implies '("expeditor-lib"))

(define pkg-desc "test part of \"expeditor\"")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
