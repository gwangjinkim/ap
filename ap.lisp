;;;; ap.lisp
;;;; Quick REPL "apropos++" with docs, kind filters, multi-package selection,
;;;; and optional thematic ordering (no LLMs).
;;;;
;;;; Conventions:
;;;;   pkg selector:
;;;;     "."   => current package
;;;;     NIL   => all packages
;;;;     ""    => all packages
;;;;     "CL" / :cl / #<PACKAGE ...> => that package
;;;;     '("CL" "SB-EXT") => multiple packages
;;;;   query:
;;;;     NIL or "" => no filtering
;;;;     "=FOO"    => exact match (name/doc depending on :tgt)
;;;;     otherwise => regex (cl-ppcre), compiled once
;;;;   thematic:
;;;;     pass :min 0.08 (or any number) to enable thematic ordering
;;;;   kinds:
;;;;     default functions+macros+variables
;;;;     pass :k '(function macro) or :k '(:function :macro)

(in-package #:ap)

;;; cl-ppcre is a system dependency; no lazy loading needed.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CL-PPCRE")
    (error "System dependency CL-PPCRE is not available. Ensure ASDF loads ap with cl-ppcre.")))

;;;; ----------------------------
;;;; Defaults (short globals)
;;;; ----------------------------

(defparameter *kinds* '(:function :macro :variable))
(defparameter *exp* t)       ; exported-only by default
(defparameter *case* t)      ; case-insensitive by default
(defparameter *lim* nil)     ; no limit by default

;;;; ----------------------------
;;;; Helpers
;;;; ----------------------------

(defun %pkg (x)
  "Resolve X to a package object.

Accepted:
  - package object => itself
  - string         => case-insensitive package name
  - symbol/keyword => case-insensitive package name via SYMBOL-NAME
Signals an error if not found."
  (cond
    ((packagep x) x)
    ((stringp x)
     (or (find-package (string-upcase x))
         (error "No such package: ~S" x)))
    ((symbolp x)  ;; includes keywords
     (or (find-package (string-upcase (symbol-name x)))
         (error "No such package: ~S" x)))
    (t
     (error "Invalid package designator: ~S" x))))

(defun %pkgs (sel)
  (cond
    ((or (null sel) (and (stringp sel) (string= sel "")))
     (list-all-packages))
    ((and (stringp sel) (string= sel "."))
     (list *package*))
    ((or (packagep sel) (stringp sel) (symbolp sel))
     (list (%pkg sel)))
    ((listp sel)
     (mapcar #'%pkg sel))
    (t (error "Unsupported package selector: ~S" sel))))

(defun %line (s &key (max 140))
  (let ((s (and s (string-trim '(#\Space #\Tab #\Newline #\Return) s))))
    (when (and s (plusp (length s)))
      (let* ((pos (position #\Newline s))
             (line (if pos (subseq s 0 pos) s)))
        (if (<= (length line) max)
            line
            (concatenate 'string (subseq line 0 (max 0 (- max 3))) "..."))))))

(defun %doc (sym)
  (or (%line (documentation sym 'function))
      (%line (documentation sym 'variable))
      (%line (documentation sym 'type))
      nil))

(defun %kset (sym)
  (let (k)
    (when (macro-function sym) (push :macro k))
    (when (and (fboundp sym) (not (macro-function sym))) (push :function k))
    (when (special-operator-p sym) (push :special-operator k))
    (when (find-class sym nil) (push :class k))
    (when (boundp sym)
      (if (constantp sym) (push :constant k) (push :variable k)))
    (nreverse k)))

(defun %klab (kinds)
  (if kinds
      (format nil "~{~A~^,~}"
              (mapcar (lambda (k) (string-downcase (symbol-name k))) kinds))
      "symbol"))

(defun %norm-kinds (k)
  "Accept (:function ...) or (function ...) etc. Returns list of keywords."
  (cond
    ((or (null k) (eq k :all)) :all)
    ((and (listp k) (null (cdr k)) (eq (car k) :all)) :all)
    ((listp k)
     (mapcar (lambda (x)
               (cond
                 ((keywordp x) x)
                 ((symbolp x) (intern (string-upcase (symbol-name x)) :keyword))
                 (t (error "Invalid kind: ~S" x))))
             k))
    (t (error "Kinds must be a list or :all, got: ~S" k))))

(defun %want-kind-p (item-kinds desired)
  (cond
    ((eq desired :all) t)
    (t (some (lambda (k) (member k item-kinds)) desired))))

(defun %syms (pkg &key (exp t))
  (let ((out '()))
    (do-external-symbols (s pkg) (push s out))
    (unless exp
      (do-symbols (s pkg)
        (multiple-value-bind (_ status) (find-symbol (symbol-name s) pkg)
          (declare (ignore _))
          (when (eq status :internal) (push s out)))))
    (remove-duplicates out :test #'eq)))

;;;; ----------------------------
;;;; Query inference (regex always, exact via "=...")
;;;; ----------------------------

(defun %qmode (q)
  "Returns (values normalized-q mode) where mode ∈ (:none :exact :regex)."
  (cond
    ((or (null q) (and (stringp q) (string= q "")))
     (values nil :none))
    ((and (stringp q) (plusp (length q)) (char= (char q 0) #\=))
     (values (subseq q 1) :exact))
    (t
     (values q :regex))))

(defun %mk-matcher (query &key (case t))
  "Returns a predicate (lambda (string) boolean)."
  (multiple-value-bind (q mode) (%qmode query)
    (ecase mode
      (:none
       (lambda (s) (declare (ignore s)) t))
      (:exact
       (let ((qq (if case (string-downcase q) q)))
         (lambda (s)
           (let ((ss (if case (string-downcase s) s)))
             (string= ss qq)))))
      (:regex
       (let* ((pat (if case (concatenate 'string "(?i)" q) q))
              (scanner (cl-ppcre:create-scanner pat)))
         (lambda (s)
           (not (null (cl-ppcre:scan scanner s)))))))))

;;;; ----------------------------
;;;; Thematic ordering (no LLMs)
;;;; ----------------------------

(defparameter *stop*
  '("a" "an" "and" "or" "the" "to" "of" "in" "on" "for" "with" "from" "by"
    "is" "are" "be" "this" "that" "it" "as" "at" "into" "nil" "t"))

(defun %tok (s)
  (let* ((s (string-downcase s))
         (len (length s))
         (ts '())
         (start nil))
    (labels ((emit (i)
               (when start
                 (let ((tkn (subseq s start i)))
                   (when (and (>= (length tkn) 2)
                              (not (member tkn *stop* :test #'string=)))
                     (push tkn ts)))
                 (setf start nil))))
      (dotimes (i len)
        (let ((ch (char s i)))
          (if (alphanumericp ch)
              (unless start (setf start i))
              (emit i))))
      (emit len))
    (remove-duplicates (nreverse ts) :test #'string=)))

(defun %feat (name doc)
  (%tok (if doc (format nil "~A ~A" name doc) name)))

(defun %jac (a b)
  (let* ((a (remove-duplicates a :test #'string=))
         (b (remove-duplicates b :test #'string=))
         (inter 0))
    (dolist (x a) (when (member x b :test #'string=) (incf inter)))
    (let ((uni (- (+ (length a) (length b)) inter)))
      (if (zerop uni) 0.0 (/ inter uni)))))

(defun %theme (items min)
  "Greedy nearest-neighbor ordering by token Jaccard similarity."
  (when items
    (let* ((rem (copy-list items))
           (start (car (sort rem #'> :key (lambda (it) (length (getf it :f)))))))
      (setf rem (remove start rem :test #'eq))
      (let ((out (list start))
            (cur start))
        (loop while rem do
          (let ((best nil) (best-s -1.0))
            (dolist (cand rem)
              (let ((s (%jac (getf cur :f) (getf cand :f))))
                (when (> s best-s) (setf best cand best-s s))))
            (when (or (null best) (< best-s min))
              (setf best (car (sort rem #'> :key (lambda (it) (length (getf it :f)))))))
            (push best out)
            (setf rem (remove best rem :test #'eq))
            (setf cur best)))
        (nreverse out)))))

;;;; ----------------------------
;;;; Main command: ap
;;;; ----------------------------

(defun %ap (&key
              (pkg ".")
              (q nil)
              (k *kinds*)
              (exp *exp*)
              (case *case*)
              (lim *lim*)
              (min nil)
              (tgt :both)
              (u nil)
              (s *standard-output*))
  "Worker for AP (rich &KEY interface, good for DESCRIBE).

This is the \"real\" implementation. The public function `ap` is a REPL-friendly
wrapper that accepts positional arguments as a convenience, then forwards into
%AP with the same keyword interface.

Keys:

  :pkg  Package selector.

        Accepts:
          - \".\"              => current *package*
          - NIL or \"\"        => all packages
          - \"CL\" or :cl      => that package
          - a package object   => that package
          - (\"CL\" \"SB-EXT\") => multiple packages

  :q    Query string.

        By default this is treated as a CL-PPCRE regex (fast; compiled once).
        For exact match, prefix with \"=\", e.g. \"=WITH-TIMEOUT\".

        If :q is NIL or \"\", no filtering is done (useful to list an API).

  :k    Kinds filter: list or :all.
        Examples:
          '(function) '(function macro) '(class) :all

  :exp  Exported-only-p. Default T.
        When NIL, include internal symbols too.

  :case Case-insensitive-p. Default T.

  :lim  Limit number of printed rows. Default *LIM* (often NIL).

  :min  Thematic ordering threshold.
        If a number, enables thematic ordering (local token similarity; no LLMs).
        If NIL, results are sorted description-first, then name.

  :tgt  Match target: :name, :doc, or :both (default :both).

  :u    Include undocumented symbols. Default NIL.
        When NIL, symbols with no docstring are filtered out.

  :s    Output stream. Default *STANDARD-OUTPUT*.

Returns:
  (values count items)
where ITEMS is the internal item list used for printing."

  (let* ((pkgs (%pkgs pkg))
         (kinds (%norm-kinds k))
         (m (%mk-matcher q :case case))
         (items '()))
    (dolist (p pkgs)
      (dolist (sym (%syms p :exp exp))
        (let* ((name (symbol-name sym))
               (doc (%doc sym))
               (ks (%kset sym)))
          (when (and (%want-kind-p ks kinds)
                     (or u doc)
                     (ecase tgt
                       (:name (funcall m name))
                       (:doc  (funcall m (or doc "")))
                       (:both (or (funcall m name)
                                  (funcall m (or doc ""))))))
            (push (list :p p :sym sym :n name :d doc :k ks :f (%feat name doc))
                  items)))))

    ;; de-dup by (package, name)
    (setf items
          (remove-duplicates items
                             :test (lambda (a b)
                                     (and (eq (getf a :p) (getf b :p))
                                          (string= (getf a :n) (getf b :n))))))

    ;; order: thematic if :min is a number, else description-first
    (setf items
          (if (numberp min)
              (%theme items min)
              (sort items
                    (lambda (a b)
                      (let ((da (or (getf a :d) ""))
                            (db (or (getf b :d) ""))
                            (na (getf a :n))
                            (nb (getf b :n)))
                        (or (string< da db)
                            (and (string= da db) (string< na nb))))))))

    ;; print
    (format s "~&~A~%" (make-string 96 :initial-element #\=))
    (format s "~&ap  pkg: ~A  exp:~A  k(kinds):~A  tgt:~A  min:~A  q:~S~%"
            (cond
              ((or (null pkg) (and (stringp pkg) (string= pkg ""))) "ALL")
              ((and (stringp pkg) (string= pkg ".")) (package-name *package*))
              ((listp pkg) (format nil "~{~A~^, ~}" (mapcar #'package-name pkgs)))
              (t (package-name (car pkgs))))
            exp (if (eq kinds :all) "ALL" kinds) tgt min q)
    (format s "~A~%" (make-string 96 :initial-element #\-))
    (format s "~&~68A  ~18A  ~14A  ~A~%" "Description" "Name" "Kind" "Package")
    (format s "~A~%" (make-string 96 :initial-element #\-))

    (let ((n 0))
      (dolist (it items)
        (incf n)
        (format s "~&~68A  ~18A  ~14A  ~A~%"
                (or (getf it :d) "")
                (getf it :n)
                (%klab (getf it :k))
                (package-name (getf it :p)))
        (when (and lim (>= n lim)) (return)))
      (format s "~&~A~%Printed ~D item~:P.~%"
              (make-string 96 :initial-element #\=) n)
      (values n items))))

(defun %pkg-name->package (x)
  "Return a package object if X names a package, otherwise NIL.
Case-insensitive for strings/symbols/keywords."
  (cond
    ((packagep x) x)
    ((stringp x) (find-package (string-upcase x)))
    ((symbolp x) (find-package (string-upcase (symbol-name x))))
    (t nil)))

(defun %package-designator-p (x)
  "True if X looks like a *single* package selector.
Includes NIL (meaning all packages) and special strings \".\"/\"\"."
  (cond
    ((null x) t)
    ((packagep x) t)
    ((and (stringp x) (or (string= x ".") (string= x ""))) t)
    ((or (stringp x) (symbolp x)) (not (null (%pkg-name->package x))))
    (t nil)))

(defun %list-of-packages-p (x)
  "True if X is a non-empty list of package designators."
  (and (listp x)
       (not (null x))
       (every #'%package-designator-p x)))

(defun %ap-option-keyword-p (x)
  (and (keywordp x)
       (member x '(:pkg :q :k :exp :case :lim :min :tgt :u :s) :test #'eq)))

(defun %fix-implicit-q (plist)
  "Accept (:pkg X \"q\" ...) by inserting :q if :q is absent and plist is odd."
  (cond
    ((null plist) plist)
    ((member :q plist :test #'eq) plist)
    ((and (member :pkg plist :test #'eq)
          (oddp (length plist))
          (not (keywordp (car (last plist)))))
     (append (butlast plist 1) (list :q (car (last plist)))))
    (t plist)))

(defun ap (&rest args)
  "Public REPL-friendly AP with DWIM positional parsing.

Rules:
  - If first arg is a package designator (package/symbol/keyword/string naming a package,
    NIL, \".\", \"\"), it is PKG.
  - If first arg is a non-empty list, it is a list of packages if every element is a
    package designator.
  - Otherwise, first arg is Q (query).
  - Q may be a string/NIL, or an alist (or any object).
  - Remaining args are a keyword plist (supports (:pkg X \"q\") implicit :q).

Examples:
  (ap :cl-excel \"sheet\")
  (ap \"cl-excel\" \"sheet\")
  (ap \"sheet\")
  (ap :pkg :cl-excel \"sheet\")            ; implicit :q
  (ap '(:cl :sb-ext) \"hash\" :k '(macro))"
  ;; Keyword-first mode only if first keyword is an actual AP option key.
  ;; Otherwise keywords like :cl-excel are treated as positional PKG designators.
  (when (and args (%ap-option-keyword-p (first args)))
    (return-from ap (apply #'%ap (%fix-implicit-q args))))

  (let* ((pkg ".")
         (q nil)
         (rest args))

    ;; consume PKG if first arg looks like pkg or list-of-pkgs
    (when (and rest
               (or (%package-designator-p (first rest))
                   (%list-of-packages-p (first rest))))
      (setf pkg (pop rest)))

    ;; consume Q if next arg exists and is not the start of a keyword plist
    (when (and rest (not (%ap-option-keyword-p (first rest))))
      (setf q (pop rest)))

    (let ((plist (%fix-implicit-q rest)))
      (apply #'%ap
             :pkg (if (member :pkg plist :test #'eq) (getf plist :pkg) pkg)
             :q   (if (member :q   plist :test #'eq) (getf plist :q)   q)
             plist))))
