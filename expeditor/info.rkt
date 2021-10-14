#lang info

(define collection 'multi)

(define deps '("expeditor-lib"
               "expeditor-doc"))
(define implies '("expeditor-lib"
                  "expeditor-doc"))

(define pkg-desc "Terminal expression editor for Racket")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
