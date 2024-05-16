(asdf:defsystem "dpans3-parser"
  :description "A parser for dpANS3."
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ()
  :components ((:module code
                :serial t
                :components ((:file "package")
                             (:file "tokenizer")
                             (:file "interpreter")
                             (:file "core")))))
