# ![Lisp Mascot](lisp.png?raw=true) A whirlwind Lisp adventure


### Introduction

We will implement a log-based, relational database in Common Lisp; and take it just far enough to be practically useful. To get a real worldish perspective of how macros may be used to mould the language around the solution, which allows compressing code **and** increasing clarity.

Besides the [code](https://github.com/codr7/whirlisp/blob/main/whir.lisp), you'll need a Common Lisp implementation; the most popular open source alternative being [SBCL](http://sbcl.org/).

I personally prefer [Emacs](https://www.gnu.org/software/emacs/) with [SLIME](https://common-lisp.net/project/slime/) backed by SBCL as my Lisp IDE, with syntax coloring turned off; it's an aquired taste, or complete lack of depending on who you ask.

It's worth noting that Common Lisp's OOP facilities started out as a bunch of macros on top of a third party implementation; and even though modern compilers usually have built-in support, there was no need for major changes during standardization since Lisp macros already operate on the language implementation level.

Which brings us to the point where we can start to get an appreciation for what macro programming, or meta programming, means. It means writing code that runs in a separate dimension, one step above regular code, with its own additional set of capabilities and restrictions; or simply code that writes code.

As should be expected, writing macros takes a tiny bit more discipline and effort than regular code; but the reward is more or less unlimited power within the limits of the language implementation. Mostly anything that may be accomplished by someone implementing Common Lisp; is available for regular users of the language, from within the language.

[The Common Lisp Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm) contains everything you could possibly want to know and the kitchen sink regarding specific Lisp features.

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

(defun new-table (nam &rest cols)
  "Returns new table with NAM and COLS"
  (make-instance 'table
                 :name nam
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

(defun new-column (nam &rest opts)
  "Returns new columns for NAM and OPTS"
  (apply #'make-instance
         'column
         :name nam
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
		 (acc (rest (rest in)) (cons (cons (first in) (second in)) out))
		 out)))
    (acc flds rec)))

(defun new-record (&rest flds)
  "Returns new record with FLDS"
  (apply #'set-column-values nil flds))
```

And adding tests to make sure we're on the right track.

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

In most commonly used languages, this is about as far as it would be possible to take the API. There are various tricks circulating depending on language, method chaining being one of the more common approaches; but it more often than not ends up feeling more like chasing your own tail than actually improving the situation. Ruby's implicit block arguments, Python's with-statement and Java's try-with-resources are examples of facilities provided to solve specific classes of macro problems.

I can spot two obvious possibilities to improve the API using macros:

The first would be to add a `with-open-table`-macro to get rid of the need to manually close the file, as well as `unwind-protect`.

```lisp
(defmacro with-open-table ((tbl) &body body)
  (let (($tbl (gensym)))
    `(let ((,$tbl ,tbl))
       (open-table ,$tbl)
       (unwind-protect
	    (progn ,@body)
         (close-table ,$tbl)))))
```

Rewriting the tests results in the following code.

```lisp
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
```

Followed by a `let-table`-macro to improve the `new-table`-mess.

```lisp
(defmacro let-table ((name &rest cols) &body body)
  `(let ((,name (new-table ',name
			   ,@(mapcar (lambda (c)
                                       (if (listp c) `(new-column ',(first c) ,@(rest c)) `(new-column ',c)))
				     cols))))
     ,@body))
```

Which results in a final rewrite of the tests as follows.

```lisp
(defun test-1c ()
  (test-setup)
  
  (let-table (users (username :primary-key? t) password)
    (assert (string= (name users) 'users))
    (assert (= (column-count users) 2))
    (assert (eq (name (first (primary-key users))) 'username))
    (with-open-table (users)
      (assert (= (record-count users) 0)))))
```

One indispensable tool for debugging macros is `macroexpand`.

```
> (macroexpand `(let-table (users (username :primary-key? t) password)))
(LET ((USERS
       (NEW-TABLE 'USERS (NEW-COLUMN 'USERNAME :PRIMARY-KEY? T)
                  (NEW-COLUMN 'PASSWORD)))))
T
```

### Missing pieces

We will use generic methods, a core feature of Common Lisp's OOP facilities; to add the final missing pieces of functionality: inserting and finding records. This allows external code to extend or override the implementation without modifying the library.

```lisp
(defmethod upsert (tbl rec)
  "Inserts or updates REC in TBL"
  (with-slots (file) tbl
    (let ((key (mapcar (lambda (c)
                         (rest (assoc (name c) rec)))
                       (primary-key tbl)))
	  (rec (remove-duplicates rec :key #'first :from-end t)))
      (write key :stream file)
      (write rec :stream file)
      (terpri file)
      (setf (gethash key (records tbl)) rec))))

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
```

### Limitations

All that remains is executing `(whirlisp:tests)` after loading `whir.lisp` to run all tests.

Before we part, I feel obliged to mention a few limitations, lest someone gets any crazy ideas:

* One thread at a time, people; multi-threading requires safe system-wide serialization of file accesses.
* All records are stored in RAM; while not impossible to get around, doing so would add significant complexity.
* NoACID, as in no transactions and no attempt to deal with hardware failures; both possible but complicated.
* Column values have to support being written and read back again, fortunately that includes most values you will encounter in Lisp.

I have intentionally left deletion as an exercise; the most obvious solution I can think of is writing a sentinel value, `:deleted` for example, in place of the record and adding the required logic to `read-records` using `(remhash key tbl)`.

Thanks a bunch for listening, no point in talking otherwise :)

/codr7