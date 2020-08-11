(in-package :dpANS3-parser)


(defstruct tex-macro
  frozen
  definitions)


(defun do-define-tex-macro (macro function &optional global)
  (unless (tex-macro-frozen macro)
    (let ((level (if global 0 (interpreter-level interpreter)))
           (pair (assoc level (tex-macro-definitions macro))))
      (if pair
        (setf (cdr pair) function)
        (setf (tex-macro-definitions macro)
              (if global
                (append (tex-macro-definitions macro) (list (cons 0 function)))
                (acons level function (tex-macro-definitions macro))))))))


(defclass interpreter ()
  ((level
     :accessor interpreter-level
     :initform 1)
   (tokenizer
     :reader interpreter-tokenizer
     :initform (make-instance 'tokenizer))
   (macros
     :reader interpreter-macros
     :initform (make-hash-table :test #'equal))
   (token-sequence
     :accessor interpreter-token-sequence
     :initform (make-array 32 :fill-pointer 0 :adjustable t))
   (token-position
     :accessor interpreter-token-position
     :initform 0)
   (stream-stack
     :accessor interpreter-stream-stack
     :initform nil)))


(defun define-tex-macro (interpreter control-sequence function &optional global)
  (do-define-tex-macro
    (or (gethash control-sequence (interpreter-macros interpreter))
      (setf (gethash control-sequence (interpreter-macros interpreter))
            (make-tex-macro)))
          function global))


(defun freeze-tex-macro (interpreter control-sequence)
  (let ((macro (gethash control-sequence (interpreter-macros interpreter))))
    (if macro
      (setf (tex-macro-frozen macro) t)
      (setf (gethash control-sequence (interpreter-macros interpreter))
            (make-tex-macro :frozen t)))))


(defun thaw-tex-macro (interpreter control-sequence)
  (let ((macro (gethash control-sequence (interpreter-macros interpreter))))
    (if macro
      (setf (tex-macro-frozen macro) nil)
      (setf (gethash control-sequence (interpreter-macros interpreter))
            (make-tex-macro)))))


(defun evaluate-token (interpreter)
  (incf (interpreter-token-position interpreter)))


(defun evaluate (interpreter)
  (with-slots (tokenizer token-sequence token-position stream-stack)
              interpreter
    (tagbody
     repeat
      (cond
        ((< token-position (length token-sequence))
          (evaluate-token interpreter)
          (go repeat))
        (stream-stack
          (let ((token (read-token tokenizer (car stream-stack))))
            (if token
              (vector-push-extend token token-sequence)
              (pop stream-stack)))
          (go repeat))))
    token-sequence))


(defun tex-load (interpreter stream))
