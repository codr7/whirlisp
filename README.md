# ![Lisp Mascot](lisp.png?raw=true) a Whirlwind Lisp Adventure


### Introduction

We will implement a log-based, relational database in Common Lisp; and take it just far enough to be practically useful. To get a real worldish perspective of how macros may be used to shrink wrap the language around the solution, which allows compressing code **and** increasing clarity.

Macro programming, or meta programming, means writing code that runs in a separate dimension; one step above regular code, with its own additional set of capabilities and difficulties; code that writes code. As should be expected, writing macros takes a tiny bit more discipline and effort than regular code; but the reward is more or less unlimited power within the limits of the language implementation. Mostly anything that may be accomplished by someone implementing Common Lisp; is available to regular users of the language, from within the language, in the form of macros.

Besides the [code](https://github.com/codr7/whirlisp/blob/main/whir.lisp), you'll need a Common Lisp implementation; the most popular open source alternative being [SBCL](http://sbcl.org/). I personally prefer [Emacs](https://www.gnu.org/software/emacs/) with [SLIME](https://common-lisp.net/project/slime/) backed by SBCL as my Lisp IDE; an aquired taste, or complete lack of depending on who you ask. [The Common Lisp Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm) contains everything you could possibly want to know and the kitchen sink regarding specific features.

### Implementation

Lets start by creating a class to represent tables with names, columns and records.

```lisp
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

(defun new-table (name &rest cols)
  "Returns new table with NAME and COLS"
  (make-instance 'table
                 :name name
		 :primary-key (remove-if-not #'primary-key? cols)
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
```

Next up is deciding on a representation for columns, we will go with a class here as well.

```lisp
(defclass column ()
  ((name :initarg :name
         :reader name)
   (primary-key? :initarg :primary-key?
		 :initform nil
                 :reader primary-key?)))

(defun new-column (name &rest opts)
  "Returns new columns for NAME and OPTS"
  (apply #'make-instance
         'column
         :name name
	 opts))
```

We will use immutable lists of pairs, aka. association lists or alists to represent records.

```lisp
(defun column-value (rec col)
  "Returns value for COL in REC"
  (rest (assoc col rec)))

(defun set-column-values (rec &rest flds)
  "Returns REC with updated FLDS"
  (labels ((acc (in out)
	     (if in
		 (acc (rest (rest in))
		      (cons (cons (first in) (second in))
		      	    out))
		 out)))
    (acc flds rec)))

(defun new-record (&rest flds)
  "Returns new record with FLDS"
  (apply #'set-column-values nil flds))

(defun record-key (rec tbl)
  "Returns key for REC in TBL"
  (mapcar (lambda (c)
            (column-value rec (name c)))
          (primary-key tbl)))
```

Lets try it out!

```lisp
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
```

### Macros

In most commonly used languages, this is about as far as it would be possible to take the API. There are various tricks depending on language, method chaining being one of the more common approaches; but it more often than not ends up feeling like chasing your tail rather than actually improving the situation. Ruby's implicit block arguments, Python's with-statement and Java's try-with-resources are examples of facilities provided to solve specific classes of macro problems.

I can spot two relatively trivial possibilities to improve the API using macros:

First uo is adding a `with-open-tables`-macro to get rid of the need to manually close the file, as well as `unwind-protect`. We're generating a unique symbol, stored in `$tbl`, to avoid shadowing existing identifiers on expansion.

```lisp
(defmacro with-open-tables ((&rest tbls) &body body)
  (let (($tbl (gensym)))
    `(progn
       (dolist ((,$tbl ,@tbls))
	 (open-table ,$tbl))
       (unwind-protect
	    (progn ,@body)
	 (dolist ((,$tbl ,@tbls))
           (close-table ,$tbl))))))
```

Rewriting the tests results in the following code:

```lisp
(defun test-1b ()
  (test-setup)
  
  (let ((users (new-table 'users
                          (new-column 'username :primary-key? t)
			  (new-column 'password))))
    (assert (string= (name users) 'users))
    (assert (= (column-count users) 2))
    (assert (eq (name (first (primary-key users))) 'username))
    (with-open-tables (users)
      (assert (= (record-count users) 0)))))
```

Next up, I would like to do something about the `new-table`-mess:

```lisp
(defmacro let-tables ((&rest tables) &body body)
  (labels ((bind (name &rest cols)
	     `(,name (new-table ',name
				,@(mapcar (lambda (c)
					    (if (listp c)
						`(new-column ',(first c) ,@(rest c))
						`(new-column ',c)))
					  cols)))))
    `(let (,@(mapcar (lambda (x) (apply #'bind x)) tables)) 
       ,@body)))
```

Which results in a final rewrite of the tests as follows:

```lisp
(defun test-1c ()
  (test-setup)
  
  (let-tables ((users (username :primary-key? t) password))
    (assert (string= (name users) 'users))
    (assert (= (column-count users) 2))
    (assert (eq (name (first (primary-key users))) 'username))
    (with-open-tables (users)
      (assert (= (record-count users) 0)))))
```

Mentally expanding macros gets old fast, `macroexpand` does the job for you:

```
> (macroexpand `((let-tables ((users (username :primary-key? t) password)))))
(LET ((USERS
       (NEW-TABLE 'USERS (NEW-COLUMN 'USERNAME :PRIMARY-KEY? T)
                  (NEW-COLUMN 'PASSWORD)))))
```

### Missing pieces

With convenience macros in place, it's time to add the final missing features: storing and finding records:

```lisp
(defun store-record (tbl rec)
  "Stores REC in TBL"
  (with-slots (file records) tbl
    (let ((key (record-key rec tbl))
	  (rec (remove-duplicates rec :key #'first :from-end t)))
      (write key :stream file)
      (write rec :stream file)
      (terpri file)
      (setf (gethash key records) rec))))

(defun find-record (tbl &rest key)
  "Returns record for KEY in TBL if found, otherwise NIL"
  (gethash key (records tbl)))

(defun test-2 ()
  (test-setup)
  
  (let-tables ((users (username :primary-key? t) password))
    (with-open-tables (users)
      (let ((rec (new-record 'username "Boris Kogen"
			     'password "badum")))
        (store-record users rec)
        (assert (string= (column-value (find-record users "Boris Kogen") 'password)
                         "badum"))
	
        (let ((rec (set-column-values rec 'password "dish")))
          (store-record users rec)
          (assert (string= (column-value (find-record users "Boris Kogen") 'password)
                           "dish")))))))

(defun tests ()
  (test-1a)
  (test-1b)
  (test-1c)
  (test-2))
```

All that remains is loading loading `whir.lisp` and evaluating `(whirlisp:tests)` to run all tests.

### File format
This is what the file `users.tbl` contains after running `test-2`, the first list contains the key and the second the complete record:

```
("Boris Kogen")((PASSWORD . "badum") (USERNAME . "Boris Kogen"))
("Boris Kogen")((PASSWORD . "dish") (USERNAME . "Boris Kogen"))
```

### Current Limitations

Before we part, I feel obliged to mention a few limitations of the current implementation, lest someone gets any crazy ideas:

* One thread at a time, people
* All records are stored in RAM
* NoACID, no transactions are provided and no attempts made to deal gracefully with write failures
* Modify primary keys at your own peril
* At some point it will probably make sense to prune the log
* Record deletion is left as an exercise
