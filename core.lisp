(in-package :dpANS3-parser/core)


(defun |def| (interpreter))


(defun |beginchapter| (interpreter)
  (write-token interpreter
               (list :type :chapter
                     :number (car (push-and-evaluate interpreter))
                     :label (car (push-and-evaluate interpreter))
                     :tag-1 (car (push-and-evaluate interpreter))
                     :tag-2 (car (push-and-evaluate interpreter)))))


(defun |endchapter| (interpreter)
  (collect-text interpreter :chapter))


(defun |beginSection| (interpreter)
  (write-token interpreter
               (list :type :section
                     :label (car (push-and-evaluate interpreter)))))


(defun |endSection| (interpreter)
  (collect-text interpreter :section))



