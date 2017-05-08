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


(defun create-getter (class-name slot)
  (let ((getter
          (intern (concatenate 'STRING  (symbol-name class-name) "-" (symbol-name slot)))))
    `(defun ,getter (instance)
       (multiple-value-bind (ret exist?) (gethash ',slot (gethash 'slots instance))
         (if exist?
             (return-from ,getter ret)
             nil)))))


(defun create-recognizer (class-name)
  (let ((recognizer
          (intern (concatenate 'STRING (symbol-name class-name) "?"))))
    `(defun ,recognizer (instance)
       (if
        (eql (find ',class-name (gethash 'is-a (gethash 'instance-of instance))) nil)
        nil
        t))))


(defun get-superclasses-slots (classes slots)
  (let* ((split-slots
           (mapcar #'(lambda (x) (gethash 'slot-names (gethash x class-system))) classes))
         (appended-slots (apply #'append split-slots)))
    (remove-duplicates (append slots appended-slots) :from-end t )))

(defun get-superclasses-names (classes)
  (let* ((split-names
           (mapcar #'(lambda (x) (gethash 'is-a (gethash x class-system))) (rest classes)))
         (appended-names (apply #'append split-names)))
    (remove-duplicates (append (list (car classes)) appended-names) :from-end t )))


(defmacro def-class (classes-names &rest slots)
  (let*
      ((subclass? (listp classes-names))
       (class-name
         (if subclass?
             (car classes-names)
             classes-names))
       (superclasses
         (if subclass?
             (get-superclasses-names classes-names)
             (list class-name)))
       (superclasses-slots
         (if subclass?
             (get-superclasses-slots (rest classes-names) slots)
             slots)))
    `(progn
       ,(add-to-class-system class-name superclasses-slots superclasses)
       ,(format t "This class slots        -> ~a~%" slots)
       ,(format t "This superclass slots   -> ~a~%" superclasses-slots)
       ,(format t "Is subclass?            -> ~a~%" subclass?)
       ,(format t "This class name         -> ~a~%" class-name)
       ,(format t "This class superclasses -> ~a~%" superclasses)
       ,(create-constructor class-name superclasses-slots)
       ,@(mapcar 'create-getter
                 (make-list (length superclasses-slots) :initial-element class-name)
                 superclasses-slots)
       ,(create-recognizer class-name))))
