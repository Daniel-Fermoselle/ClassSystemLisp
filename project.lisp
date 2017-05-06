(defparameter class-system (make-hash-table))

(defun create-constructor (slots constructor classes-names)
  `(defun ,constructor (&key ,@slots)
     (let ((instance (make-hash-table)))
       (progn
         (setf (gethash 'slots instance) (vector ,@slots))
         (setf (gethash 'instance-of instance) (car ',classes-names))
         (return-from ,constructor instance)))))

(defun index-list (list length)
  (let ((indexes (loop for x from 0 to length collect (car (list x)))))
    (mapcar #'cons list indexes)))

(defun create-getter (getter-index)
  `(defun ,(car getter-index) (instance)
     (aref (gethash 'slots instance) ,(cdr getter-index))))

(defmacro def-class (classes-names &rest slots)
  (let*
      ((this (if (listp classes-names)
                 (car classes-names)
                 (car (list classes-names))))
       (constructor
         (intern (concatenate 'STRING
                              "MAKE" "-" (symbol-name (car classes-names)))))
       (getters
         (index-list
          (mapcar #'(lambda (x)
                      (intern (concatenate 'STRING (symbol-name (car classes-names)) "-" (symbol-name x))))
                  slots)
          (- (length slots) 1)))
       (recognizer
         (intern (concatenate 'STRING
                              (symbol-name (car classes-names)) "?"))))
    `(progn
       ,(create-constructor slots constructor classes-names)
       ,@(mapcar 'create-getter getters))))
