(defparameter class-system (make-hash-table))


(defun create-hashtable (alist)
  (let ((ht (make-hash-table)))
    (loop for (key . value) in alist
          do (setf (gethash key ht) (car value)))
    ht))


(defun add-to-class-system (class-name slots superclasses)
  `(let ((class (make-hash-table)))
     (progn
       (setf (gethash 'is-a class) ',superclasses)
       (setf (gethash 'slot-names class) ',slots)
       (setf (gethash ',class-name class-system) class))))


(defun create-constructor (class-name slots)
  (let ((constructor
          (intern (concatenate 'STRING  "MAKE" "-" (symbol-name class-name)))))
    `(defun ,constructor (&key ,@slots)
       (let* ((instance (make-hash-table))
              (slot-values (list ,@slots))
              (init-list  (mapcar #'(lambda (x y) `(,x ,y)) ',slots slot-values))
              (slots-hash (create-hashtable init-list)))
         (progn
           (setf (gethash 'slots instance) slots-hash)
           (setf (gethash 'instance-of instance) (gethash ',class-name class-system))
           (return-from ,constructor instance))))))

;Not used
(defun index-list (list length)
  (let ((indexes (loop for x from 0 to length collect (car (list x)))))
    (mapcar #'cons list indexes)))

;Not used
(defun create-getter (getter-index)
  `(defun ,(car getter-index) (instance)
     (aref (gethash 'slots instance) ,(cdr getter-index))))

(defmacro def-class (classes-names &rest slots)
  (let*
      ((subclass? (listp classes-names))
       (class-name
         (if subclass?
             (car classes-names)
             classes-names))
       (superclasses
         (if subclass?
             classes-names
             class-name)))
    `(progn
                                        ;       ,@(mapcar 'create-getter getters)
       ,(add-to-class-system class-name slots superclasses)
       ,(format t "This class slots        -> ~a~%" slots)
       ,(format t "Is subclass?            -> ~a~%" subclass?)
       ,(format t "This class name         -> ~a~%" class-name)
       ,(format t "This class superclasses -> ~a~%" superclasses)
       ,(create-constructor class-name slots)
       )
    ))


