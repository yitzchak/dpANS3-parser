(ql:quickload "dpans3-parser")

(defparameter z (make-instance 'dpans3-parser:interpreter))

(dpans3-parser:tex-input z "dpANS3/chap-5.tex")

(with-open-file (s "test.sexpr" :direction :output :if-exists :supersede)
  (pprint (dpans3-parser:evaluate z) s))
