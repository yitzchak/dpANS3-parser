(in-package :dpANS3-parser)


(defun block-token-p (token)
  (and (listp token)
       (member (getf token :type) '(:chapter :code :paragraph :section :subsection) :test #'eql)))


(defstruct tex-macro
  frozen
  definitions)


(defstruct styled-char
  style
  char)


(defun do-define-tex-macro (interpreter macro function &optional global)
  (unless (tex-macro-frozen macro)
    (let* ((level (if global 0 (interpreter-level interpreter)))
           (pair (assoc level (tex-macro-definitions macro))))
      (if pair
        (setf (cdr pair) function)
        (setf (tex-macro-definitions macro)
              (if global
                (append (tex-macro-definitions macro) (list (cons 0 function)))
                (acons level function (tex-macro-definitions macro))))))))


(defclass state ()
  ((style
     :accessor state-style
     :initform nil
     :initarg :style)))


(defclass frame ()
  ((token-stack
     :accessor frame-token-stack
     :initform nil)
   (output-sequence
     :accessor frame-output-sequence
     :initform (make-array 64 :adjustable t :fill-pointer 0))))


(defclass interpreter ()
  ((level
     :accessor interpreter-level
     :initform 1)
   (state-stack
     :accessor interpreter-state-stack
     :initform (list (make-instance 'state)))
   (tokenizer
     :reader interpreter-tokenizer
     :initform (make-instance 'tokenizer))
   (macros
     :reader interpreter-macros
     :initform (make-hash-table :test #'equal))
   (frame-stack
     :accessor interpreter-frame-stack
     :initform (list (make-instance 'frame)))
   (stream-stack
     :accessor interpreter-stream-stack
     :initform nil)))


(defgeneric sanitize (form)
  (:method (form)
    form))


(defmethod sanitize ((form string))
  (replace-all (string (code-char #x201c)) "``"
               (replace-all (string (code-char #x201d)) "''"
                            (replace-all (string (code-char #x2013)) "--"
                                         (replace-all (string (code-char #x2014)) "---"
                                                      form)))))


(defmethod sanitize ((form list))
  (do (result
       (tail form (cddr tail)))
      ((null tail) (nreverse result))
    (let ((val (if (listp (cadr tail))
                 (mapcar #'sanitize (cadr tail))
                 (sanitize (cadr tail)))))
      (when val
        (push (car tail) result)
        (push val result)))))


(defun macro-name (tag)
  (cond
    ((symbolp tag)
      (symbol-name tag))
    ((typep tag 'control-sequence)
      (control-sequence-value tag))
    (t
      tag)))


(defun define-tex-macro (interpreter tag function &optional global)
  (let ((name (macro-name tag)))
    (do-define-tex-macro
      interpreter
      (or (gethash name (interpreter-macros interpreter))
          (setf (gethash name (interpreter-macros interpreter))
                (make-tex-macro)))
      function global)))


(defun freeze-tex-macro (interpreter tag)
  (let* ((name (macro-name tag))
         (macro (gethash name (interpreter-macros interpreter))))
    (if macro
      (setf (tex-macro-frozen macro) t)
      (setf (gethash name (interpreter-macros interpreter))
            (make-tex-macro :frozen t)))))


(defun thaw-tex-macro (interpreter tag)
  (let* ((name (macro-name tag))
         (macro (gethash control-sequence (interpreter-macros interpreter))))
    (if macro
      (setf (tex-macro-frozen macro) nil)
      (setf (gethash name (interpreter-macros interpreter))
            (make-tex-macro)))))


(defun fill-token-sequence (interpreter)
  (with-slots (tokenizer frame-stack stream-stack)
              interpreter
    (with-slots (token-stack)
                (car frame-stack)
      (prog (token)
       repeat
        (unless (or token-stack
                    (null stream-stack))
          (setq token (read-token tokenizer (car stream-stack)))
          (when token
            (push token token-stack)
            (return))
          (close (pop stream-stack))
          (go repeat))))))


(defun write-token (interpreter token)
  (vector-push-extend token (frame-output-sequence (car (interpreter-frame-stack interpreter)))))


(defun peek-token (interpreter)
  (unless (cdr (interpreter-frame-stack interpreter))
    (fill-token-sequence interpreter))
  (car (frame-token-stack (car (interpreter-frame-stack interpreter)))))


(defun pop-token (interpreter)
  (unless (cdr (interpreter-frame-stack interpreter))
    (fill-token-sequence interpreter))
  (pop (frame-token-stack (car (interpreter-frame-stack interpreter)))))


(defun push-token (interpreter token)
  (with-slots (token-stack)
              (car (interpreter-frame-stack interpreter))
    (if (listp token)
      (setf token-stack (nconc (list (make-control-sequence :value "begingroup"))
                               token
                               (list (make-control-sequence :value "endgroup"))
                               token-stack))
      (push token token-stack))))


(defun push-frame-stack (interpreter &optional (parameter-type :single))
  (let ((frame (make-instance 'frame)))
    (case parameter-type
      (:single
        (push (pop-token interpreter) (frame-token-stack frame)))
      (:line-or-group
        (do ((token (peek-token interpreter) (peek-token interpreter)))
            ((or (null token)
                 (typep token 'comment)))
          (cond
            ((eql :eol token)
              (pop-token interpreter)
              (return))
            (t
              (push token (frame-token-stack frame))
              (pop-token interpreter))))
        (setf (frame-token-stack frame) (nreverse (frame-token-stack frame)))))
    (push frame (interpreter-frame-stack interpreter))))


(defun pop-frame-stack (interpreter)
  (pop (interpreter-frame-stack interpreter)))


(defun evaluate-control-sequence (interpreter token)
  (let ((macro (gethash (control-sequence-value token) (interpreter-macros interpreter))))
    (when macro
      (let ((fun (cdar (tex-macro-definitions macro))))
        (when fun
          (funcall fun interpreter))))))


(defun replace-all (new-seq old-seq seq)
  (prog ((start 0) end result)
   repeat
    (setf end (search old-seq seq :start2 start))
    (when end
      (setf result (concatenate 'string result (subseq seq start end) new-seq))
      (setf start (+ end (length old-seq)))
      (go repeat))
    (return (concatenate 'string result (subseq seq start)))))


(defun assemble-text (text-sequence)
  (let (result paragraph text tk eol style)
    (dotimes (position (length text-sequence) (nreverse result))
      (setf tk (elt text-sequence position))
      (cond
        ((eql :italic-correction tk))
        ((block-token-p tk)
          (setf paragraph nil)
          (setf text nil)
          (setf style nil)
          (setf eol nil)
          (push tk result))
        ((and eol
              (eql :space tk)))
        ((and (typep tk 'styled-char)
              (equalp style (styled-char-style tk))
              text)
          (when eol
            (vector-push-extend #\space text)
            (setf eol nil))
          (vector-push-extend (styled-char-char tk) text))
        ((typep tk 'styled-char)
          (when eol
            (unless text
              (setf text (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character))
              (unless paragraph
                (setf paragraph (list :type :paragraph :children nil))
                (push paragraph result))
              (setf (getf paragraph :children)
                    (nconc (getf paragraph :children) (list text))))
            (vector-push-extend #\space text)
            (setf eol nil))
          (setf text (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character))
          (unless paragraph
            (setf paragraph (list :type :paragraph :children nil))
            (push paragraph result))
          (setf (getf paragraph :children)
                (nconc (getf paragraph :children)
                       (list (list :type :span :style (styled-char-style tk) :children (list text)))))
          (setf style (styled-char-style tk))
          (vector-push-extend (styled-char-char tk) text))
        ((or (characterp tk)
             (eql :space tk))
          (unless (and text (not style))
            (setf text (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character))
            (setf style nil)
            (unless paragraph
              (setf paragraph (list :type :paragraph :children nil))
              (push paragraph result))
            (setf (getf paragraph :children)
                  (nconc (getf paragraph :children) (list text))))
          (when eol
            (unless (eql :space tk)
              (vector-push-extend #\space text))
            (setf eol nil))
          (vector-push-extend (if (eql :space tk) #\space tk) text))
        ((and eol
              (eql :eol tk))
          (setf eol nil)
          (setf text nil)
          (setf style nil)
          (setf paragraph nil))
        ((and text
              (eql :eol tk))
          (setf eol t))
        ((eql :eol tk))
        (t
          (setf text nil)
          (setf style nil)
          (unless paragraph
            (setf paragraph (list :type :paragraph :children nil))
            (push paragraph result))
          (setf (getf paragraph :children)
                (nconc (getf paragraph :children) (list tk))))))))


(defun find-previous-by-type (interpreter &rest types)
  (with-slots (output-sequence)
              (car (interpreter-frame-stack interpreter))
    (let ((pos (position-if (lambda (token)
                              (and (listp token)
                                   (member (getf token :type) types :test #'eql)))
                            output-sequence :from-end t)))
      (values (when pos (elt output-sequence pos)) pos))))


(defun collect-text (interpreter &rest types)
  (with-slots (output-sequence)
              (car (interpreter-frame-stack interpreter))
    (let ((pos (position-if (lambda (token)
                              (and (listp token)
                                   (member (getf token :type) types :test #'eql)))
                            output-sequence :from-end t)))
      (when pos
        (setf (cdr (last (elt output-sequence pos)))
              (list :children
                    (assemble-text (subseq output-sequence (1+ pos)))))
        (setf (fill-pointer output-sequence) (1+ pos))))))


(defun evaluate-token (interpreter)
  (with-slots (token-stack output-sequence)
              (car (interpreter-frame-stack interpreter))
    (do ((token (pop-token interpreter) (pop-token interpreter)))
        ((null token))
      (cond
        ((typep token 'control-sequence)
          (evaluate-control-sequence interpreter token))
        ((listp token)
          (setf token-stack (nconc token token-stack)))
        ((typep token 'comment))
        ((and (interpreter-style interpreter)
              (characterp token))
          (vector-push-extend (make-styled-char :char token :style (interpreter-style interpreter)) output-sequence))
        (t
          (vector-push-extend token output-sequence)
          (return))))))


(defun evaluate (interpreter)
  (with-slots (frame-stack)
              interpreter
    (with-slots (token-stack output-sequence)
                (car frame-stack)
      (do ((token (peek-token interpreter) (peek-token interpreter)))
          ((null token))
        (evaluate-token interpreter))
      (mapcar #'sanitize (assemble-text (frame-output-sequence (pop frame-stack)))))))


(defun push-and-evaluate (interpreter &optional (parameter-type :single))
  (push-frame-stack interpreter parameter-type)
  (evaluate interpreter))

(defun tex-input (interpreter path)
  (push (open path) (interpreter-stream-stack interpreter)))


(defmethod initialize-instance :after ((instance interpreter) &rest initargs &key &allow-other-keys)
  (do-external-symbols (sym 'dpANS3-parser/core)
    (when (fboundp sym)
      (define-tex-macro instance sym (symbol-function sym) t)
      (freeze-tex-macro instance sym))))


(defun interpreter-style (interpreter)
  (state-style (car (interpreter-state-stack interpreter))))


(defun (setf interpreter-style) (new-value interpreter)
  (setf (state-style (car (interpreter-state-stack interpreter)))
        new-value))


(defun begin-group (interpreter &rest catcodes)
  (with-slots (tokenizer state-stack)
              interpreter
    (push (make-instance 'state :style (state-style (car state-stack)))
          state-stack)
    (tokenizer-begin tokenizer)
    (do ((head catcodes (cddr head)))
        ((null head))
      (set-catcode tokenizer (car head) (cadr head)))))


(defun end-group (interpreter)
  (tokenizer-end (interpreter-tokenizer interpreter))
  (pop (interpreter-state-stack interpreter)))
