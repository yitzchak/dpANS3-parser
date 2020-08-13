(defpackage #:dpANS3-parser
  (:use :cl)
  (:export
    #:define-tex-macro
    #:interpreter
    #:freeze-tex-macro
    #:thaw-tex-macro))


(defpackage #:dpANS3-parser/core
  (:use :cl)
  (:export
    #:|def|))
