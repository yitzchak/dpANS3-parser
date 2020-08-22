(in-package :dpANS3-parser/core)


(defun user-apply (args body)
  (if (listp body)
    (mapcar (lambda (x)
              (user-apply args x)) body)
    body))


(defun user-macro (interpreter args body)
  (push-token interpreter (user-apply nil body)))


(defun |def| (interpreter)
  (prog ((name (pop-token interpreter)) token args)
   repeat
    (setf token (pop-token interpreter))
    (when (listp token)
      (define-tex-macro interpreter name
                        (lambda (interpreter)
                          (funcall #'user-macro interpreter args token)))
      (return))
    (push token args)
    (go repeat)))


(defun |beginchapter| (interpreter)
  (write-token interpreter
               (list :type :chapter
                     :number (car (getf (car (push-and-evaluate interpreter)) :children))
                     :label (car (getf (car (push-and-evaluate interpreter)) :children))
                     :tags (append (getf (car (push-and-evaluate interpreter)) :children)
                                   (getf (car (push-and-evaluate interpreter)) :children)))))


(defun |endchapter| (interpreter)
  (collect-text interpreter :chapter))


(defun |beginSection| (interpreter)
  (write-token interpreter
               (list :type :section
                     :label (car (getf (car (push-and-evaluate interpreter)) :children))
                     :tags nil)))


(defun |DefineSection| (interpreter)
  (let ((tags (push-and-evaluate interpreter))
        (section (find-previous-by-type interpreter :section :subsection :subsubsection :subsubsubsection)))
    (when section
      (setf (getf section :tags) (nconc (getf section :tags) (getf (car tags) :children))))))


(defun |endSection| (interpreter)
  (collect-text interpreter :section))


(defun |beginsubSection| (interpreter)
  (write-token interpreter
               (list :type :subsection
                     :label (car (getf (car (push-and-evaluate interpreter)) :children))
                     :tags nil)))


(defun |endsubSection| (interpreter)
  (collect-text interpreter :subsection))


(defun |beginsubsubsection| (interpreter)
  (write-token interpreter
               (list :type :subsubsection
                     :label (car (getf (car (push-and-evaluate interpreter)) :children))
                     :tags nil)))


(defun |endsubsubsubsection| (interpreter)
  (collect-text interpreter :subsubsubsection))


(defun |beginsubsubsubsection| (interpreter)
  (write-token interpreter
               (list :type :subsubsubsection
                     :label (car (getf (car (push-and-evaluate interpreter)) :children))
                     :tags nil)))


(defun |endsubsubsection| (interpreter)
  (collect-text interpreter :subsubsection))


(defun |input| (interpreter)
  (let ((name (string-trim '(#\Space #\Tab #\Newline) (car (getf (car (push-and-evaluate interpreter :line-or-group)) :children)))))
    (unless (member name '("setup") :test #'string=)
      (tex-input interpreter
                 (make-pathname :directory '(:relative "dpANS3")
                                :name name
                                :type "tex")))))


(defun |code| (interpreter)
  (write-token interpreter
               (list :type :code))
  (begin-group interpreter
               #\space   +other-category+
               #\_       +other-category+
               #\{       +other-category+
               #\}       +other-category+
               #\soh     +other-category+
               #\newline +other-category+
               #\tab     +other-category+
               #\vt      +other-category+
               #\&       +other-category+
               #\#       +other-category+
               #\%       +other-category+
               #\^       +other-category+
               #\~       +other-category+
               #\$       +other-category+))


(defun |endcode| (interpreter)
  (end-group interpreter)
  (collect-text interpreter :code))


(defun |~| (interpreter)
  (write-token interpreter (code-char #xa0)))


(defun |b| (interpreter)
  (write-token interpreter
               (list :type :bold
                     :children (getf (car (push-and-evaluate interpreter)) :children))))


(defun |i| (interpreter)
  (write-token interpreter
               (list :type :italic
                     :children (getf (car (push-and-evaluate interpreter)) :children))))

(defun |j| (interpreter)
  (write-token interpreter
               (list :type :italic
                     :children (getf (car (push-and-evaluate interpreter)) :children))))


(defun |f| (interpreter)
  (write-token interpreter
               (list :type :fixed
                     :children (getf (car (push-and-evaluate interpreter)) :children))))
