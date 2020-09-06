(in-package :dpANS3-parser)


(defconstant +escape-category+ 0)
(defconstant +begin-category+ 1)
(defconstant +end-category+ 2)
(defconstant +math-shift-category+ 3)
(defconstant +alignment-tab-category+ 4)
(defconstant +eol-category+ 5)
(defconstant +parameter-category+ 6)
(defconstant +superscript-category+ 7)
(defconstant +subscript-category+ 8)
(defconstant +ignored-category+ 9)
(defconstant +space-category+ 10)
(defconstant +letter-category+ 11)
(defconstant +other-category+ 12)
(defconstant +active-char-category+ 13)
(defconstant +comment-category+ 14)
(defconstant +invalid-category+ 15)


(defstruct control-sequence
  value)


(defstruct comment
  value)


(defstruct parameter
  level
  index)


(defclass tokenizer ()
  ((level
     :accessor tokenizer-level
     :initform 0)
   (catcode-table
     :reader tokenizer-catcode-table
     :initform (make-hash-table))))


(defun catcode (tokenizer char)
  (when char
    (or (cdar (gethash char (tokenizer-catcode-table tokenizer)))
        +other-category+)))


(defun set-catcode (tokenizer char value)
  (with-slots (level catcode-table)
              tokenizer
    (let* ((assignments (gethash char catcode-table))
           (pair (assoc level assignments)))
      (if pair
        (setf (cdr pair) value)
        (setf (gethash char catcode-table) (acons level value assignments))))))


(defun tokenizer-begin (tokenizer)
  (incf (tokenizer-level tokenizer)))


(defun tokenizer-end (tokenizer)
  (with-slots (level catcode-table)
              tokenizer
    (maphash (lambda (char assignments)
               (when (= level (caar assignments))
                 (setf (gethash char catcode-table) (cdr assignments))))
             catcode-table)
    (decf level)))


(defmethod (setf catcode) (new-value tokenizer char &optional end-char)
  (if end-char
    (dotimes (offset (1+ (- (char-code end-char) (char-code char))))
      (set-catcode tokenizer (code-char (+ offset (char-code char))) new-value))
    (set-catcode tokenizer char new-value)))


(defmethod initialize-instance :after ((tokenizer tokenizer) &rest initargs)
  (declare (ignore initargs))
  (setf (catcode tokenizer #\space)   +space-category+
        (catcode tokenizer #\_)       +subscript-category+
        (catcode tokenizer #\{)       +begin-category+
        (catcode tokenizer #\})       +end-category+
        (catcode tokenizer #\\)       +escape-category+
        (catcode tokenizer #\null)    +ignored-category+
        (catcode tokenizer #\soh)     +subscript-category+
        (catcode tokenizer #\newline) +eol-category+
        (catcode tokenizer #\tab)     +space-category+
        (catcode tokenizer #\vt)      +superscript-category+
        (catcode tokenizer #\rubout)  +invalid-category+
        (catcode tokenizer #\&)       +alignment-tab-category+
        (catcode tokenizer #\#)       +parameter-category+
        (catcode tokenizer #\%)       +comment-category+
        (catcode tokenizer #\^)       +superscript-category+
        (catcode tokenizer #\~)       +active-char-category+
        (catcode tokenizer #\$)       +math-shift-category+
        (catcode tokenizer #\a #\z)   +letter-category+
        (catcode tokenizer #\A #\Z)   +letter-category+)
  (tokenizer-begin tokenizer))


(defun read-char-and-catcode (tokenizer stream)
  (do* ((char (read-char stream nil) (read-char stream nil))
        (catcode (catcode tokenizer char) (catcode tokenizer char)))
      ((null char) (values nil nil))
    (when (equal +invalid-category+ catcode)
      (error "Invalid character ~S" char))
    (unless (equal +ignored-category+ catcode)
      (return (values char catcode)))))


(defun peek-char-and-catcode (tokenizer stream)
  (do* ((char (peek-char nil stream nil) (peek-char nil stream nil))
        (catcode (catcode tokenizer char) (catcode tokenizer char)))
      ((null char) (values nil nil))
    (when (equal +invalid-category+ catcode)
      (error "Invalid character ~S" char))
    (unless (equal +ignored-category+ catcode)
      (return (values char catcode)))
    (read-char stream)))


(defun eat-space (tokenizer stream)
  (prog ()
   repeat
    (multiple-value-bind (char catcode)
                         (peek-char-and-catcode tokenizer stream)
      (declare (ignore char))
      (unless (equal +space-category+ catcode)
        (return))
      (read-char stream))
    (go repeat)))


(defun read-escape-sequence (tokenizer stream)
  (multiple-value-bind (char catcode)
                       (read-char-and-catcode tokenizer stream)
    (cond
      ((equal +letter-category+ catcode)
        (prog ((result (make-array 32 :initial-element char :fill-pointer 1 :adjustable t :element-type 'character)))
         repeat
          (multiple-value-bind (char catcode)
                               (peek-char-and-catcode tokenizer stream)
            (unless (equal +letter-category+ catcode)
              (eat-space tokenizer stream)
              (return (make-control-sequence :value result)))
            (read-char stream)
            (vector-push-extend char result)
            (go repeat))))
      ((char= #\/ char)
        :italic-correction)
      (t
        char))))


(defun read-comment (tokenizer stream)
  (prog ((result (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character)))
   repeat
    (multiple-value-bind (char catcode)
                         (read-char-and-catcode tokenizer stream)
      (when (or (not char)
                (equal +eol-category+ catcode))
        (return (make-comment :value result)))
      (vector-push-extend char result)
      (go repeat))))


(defun read-parameter (tokenizer stream)
  (prog ((count 1))
   repeat
    (multiple-value-bind (char catcode)
                         (peek-char-and-catcode tokenizer stream)
      (when (equal +parameter-category+ catcode)
        (incf count)
        (read-char stream)
        (go repeat))
      (let ((weight (digit-char-p char)))
        (when weight
          (read-char stream)
          (return (make-parameter :level count :index weight)))
        (return #\#)))))


(defun read-group (tokenizer stream)
  (do (result tail
       (token (read-token tokenizer stream t) (read-token tokenizer stream t)))
      ((eql :end token) result)
    (unless token
      (error "Group ended prematurely"))
    (cond
      (tail
        (setf (cdr tail) (cons token nil))
        (setq tail (cdr tail)))
      (t
        (setq tail (cons token nil))
        (setq result tail)))))


(defun read-token (tokenizer stream &optional end-allowed)
  (multiple-value-bind (char catcode)
                       (read-char-and-catcode tokenizer stream)
    (cond
      ((equal +escape-category+ catcode)
        (read-escape-sequence tokenizer stream))
      ((equal +begin-category+ catcode)
        (read-group tokenizer stream))
      ((equal +end-category+ catcode)
        (unless end-allowed
          (error "Misplaced end token"))
        :end)
      ((equal +math-shift-category+ catcode)
        :math-shift)
      ((equal +alignment-tab-category+ catcode)
        :alignment-tab)
      ((equal +eol-category+ catcode)
        :eol)
      ((equal +parameter-category+ catcode)
        (read-parameter tokenizer stream))
      ((equal +superscript-category+ catcode)
        :superscript)
      ((equal +subscript-category+ catcode)
        :subscript)
      ((equal +active-char-category+ catcode)
        (make-control-sequence :value (string char)))
      ((equal +comment-category+ catcode)
        (read-comment tokenizer stream))
      ((equal +space-category+ catcode)
        (eat-space tokenizer stream)
        :space)
      (t
        char))))
