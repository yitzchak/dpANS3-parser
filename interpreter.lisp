(in-package :dpANS3-parser)


(defstruct tex-macro
  frozen
  definitions)


(defun do-define-tex-macro (macro function &optional global)
  (unless (tex-macro-frozen macro)
    (let* ((level (if global 0 (interpreter-level interpreter)))
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
   (tokens
     :accessor interpreter-tokens
     :initform nil)
   (token-head
     :accessor interpreter-token-head
     :initform nil)
   (token-tail
     :accessor interpreter-token-tail
     :initform nil)
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


(defun fill-token-sequence (interpreter)
  (with-slots (tokenizer token-head token-tail stream-stack)
              interpreter
    (prog (token)
     repeat
      (unless (or token-head
                  (null stream-stack))
        (setq token (read-token tokenizer (car stream-stack)))
        (when token
          (setf token-head (cons token nil))
          (when token-tail
            (setf (cdr token-tail) token-head))
          (return))
        (close (pop stream-stack))
        (go repeat)))))


(defun peek-token (interpreter)
  (fill-token-sequence interpreter)
  (car (interpreter-token-head interpreter)))


(defun pop-token (interpreter)
  (fill-token-sequence interpreter)
  (with-slots (token-head token-tail tokens)
              interpreter
    (prog1
      (car token-head)
      (setf token-head (cdr token-head))
      (unless tokens
        (setf tokens token-head))
      (when token-tail
        (setf (cdr token-tail) token-head)))))


(defun evaluate (interpreter)
  (with-slots (token-head token-tail tokens)
              interpreter
    (do ((token (peek-token interpreter) (peek-token interpreter)))
        ((null token) tokens)
      (typecase token
        (list
          (setf token-head (nconc token token-head))
          (unless tokens
            (setf tokens token-head))
          (when token-tail
            (setf (cdr token-tail) token-head)))
        (otherwise
          (setf token-tail token-head)
          (setf token-head (cdr token-head))
          (unless tokens
            (setf tokens token-head)))))))


(defun tex-input (interpreter path)
  (push (open path) (interpreter-stream-stack interpreter)))


(defmethod initialize-instance :after ((instance interpreter) &rest initargs &key &allow-other-keys)
  (do-external-symbols (sym 'dpANS3-parser/core)
    (when (fboundp sym)
      (define-tex-macro instance (make-control-sequence :value (symbol-name sym)) (symbol-function sym) t))))
