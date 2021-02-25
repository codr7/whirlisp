(defpackage whirlisp
  (:use cl)
  (:export tests))

(in-package whirlisp)

(defclass table ()
  ((name :initarg :name
         :reader name)
   (primary-key :initarg :primary-key
		:reader primary-key)	 
   (columns :initarg :columns
            :reader columns)
   (file :initarg :file
         :reader file)
   (records :initform (make-hash-table :test 'equal)
            :reader records)))

(defun new-table (nam &rest cols)
  "Returns new table with NAM and COLS"
  (make-instance 'table
                 :name nam
		 :primary-key (remove-if-not (lambda (c)
		                               (primary-key? c))
					     cols)
                 :columns cols))

(defun read-records (tbl fil)
  "Reads records from FIL into TBL"
  (with-slots (records) tbl
    (let ((key (read fil nil)))
      (when key
        (let ((rec (read fil nil)))
          (unless rec
  	    (error "Missing record for key ~a" key))
          (setf (gethash key records) rec)
          (read-records tbl fil))))))

(defun open-table (tbl)
  "Opens and assigns TBL's file"
  (let ((fil (open (format nil "~a.tbl" (string-downcase (symbol-name (name tbl))))
                   :direction :io
		   :if-exists :overwrite
		   :if-does-not-exist :create)))
    (setf (slot-value tbl 'file) fil)
    (read-records tbl fil)))

(defun close-table (tbl)
  "Closes and unbinds TBL's file"
  (close (file tbl))
  (slot-makunbound tbl 'file))

(defun column-count (tbl)
  "Returns number of columns in TBL"
  (length (columns tbl)))

(defun record-count (tbl)
  "Returns number of records in TBL"
  (hash-table-count (records tbl)))

(defclass column ()
  ((name :initarg :name
         :reader name)
   (primary-key? :initarg :primary-key?
		 :initform nil
                 :reader primary-key?)))

(defun new-column (nam &rest opts)
  "Returns new columns for NAM and OPTS"
  (apply #'make-instance
         'column
         :name nam
	 opts))

(defun column-value (rec col)
  "Returns value for COL in REC"
  (rest (assoc col rec)))

(defun set-column-values (rec &rest flds)
  "Returns REC with updated FLDS"
  (labels ((acc (in out)
	     (if in
		 (acc (rest (rest in)) (cons (cons (first in) (second in)) out))
		 out)))
    (acc flds rec)))

(defun new-record (&rest flds)
  "Returns new record with FLDS"
  (apply #'set-column-values nil flds))

(defun test-setup ()
  (when (probe-file "users.tbl")
    (assert (delete-file "users.tbl"))))

(defun test-1a ()
  (test-setup)

  (let ((users (new-table 'users
                          (new-column 'username :primary-key? t)
			  (new-column 'password))))
    (assert (string= (name users) 'users))
    (assert (= (column-count users) 2))
    (assert (eq (name (first (primary-key users))) 'username))
    (open-table users)
    (unwind-protect
	 (assert (= (record-count users) 0))
      (close-table users))))

(defmacro with-open-table ((tbl) &body body)
  (let (($tbl (gensym)))
    `(let ((,$tbl ,tbl))
       (open-table ,$tbl)
       (unwind-protect
	    (progn ,@body)
         (close-table ,$tbl)))))

(defun test-1b ()
  (test-setup)
  
  (let ((users (new-table 'users
                          (new-column 'username :primary-key? t)
			  (new-column 'password))))
    (assert (string= (name users) 'users))
    (assert (= (column-count users) 2))
    (assert (eq (name (first (primary-key users))) 'username))
    (with-open-table (users)
      (assert (= (record-count users) 0)))))

(defmacro let-table ((name &rest cols) &body body)
  `(let ((,name (new-table ',name
			   ,@(mapcar (lambda (c)
                                       (if (listp c) `(new-column ',(first c) ,@(rest c)) `(new-column ',c)))
				     cols))))
     ,@body))

(defun test-1c ()
  (test-setup)
  
  (let-table (users (username :primary-key? t) password)
    (assert (string= (name users) 'users))
    (assert (= (column-count users) 2))
    (assert (eq (name (first (primary-key users))) 'username))
    (with-open-table (users)
      (assert (= (record-count users) 0)))))

(defmethod upsert (tbl rec)
  "Inserts or updates REC in TBL"
  (with-slots (file) tbl
    (let ((key (mapcar (lambda (c)
                         (rest (assoc (name c) rec)))
                       (primary-key tbl))))
      (write key :stream file)
      (write rec :stream file)
      (setf (gethash key (records tbl)) (copy-alist rec)))))

(defmethod find-key (tbl &rest key)
  "Returns record for KEY in TBL if found, otherwise NIL"
  (gethash key (records tbl)))

(defun test-2 ()
  (test-setup)
  
  (let-table (users (username :primary-key? t) password)
    (with-open-table (users)
      (let ((rec (new-record 'username "ben_dover"
			     'password "badumdish")))
        (upsert users rec)
        (assert (string= (column-value (find-key users "ben_dover") 'password)
                         "badumdish"))
	
        (let ((rec (set-column-values rec 'password "leaving!")))
          (upsert users rec)
          (assert (string= (column-value (find-key users "ben_dover") 'password)
                           "leaving!")))))))

(defun tests ()
  (test-1a)
  (test-1b)
  (test-1c)
  (test-2))