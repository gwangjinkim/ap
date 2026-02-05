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

(defun %q->string (x)
  "Normalize a single query atom to a string (or NIL)."
  (cond
    ((null x) nil)
    ((stringp x) x)
    ((symbolp x) (symbol-name x)) ; includes keywords
    (t (princ-to-string x))))

(defun %alistp (x)
  "Heuristic: an alist is a proper list of conses."
  (and (listp x)
       (every (lambda (e) (and (consp e))) x)))

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

(defun %pkg-maybe (x)
  "Like %PKG but returns NIL instead of error if not found."
  (handler-case (%pkg x)
    (error () nil)))

(defun %pkgs (pkg)
  "Return a list of package objects from PKG selector."
  (cond
    ((or (null pkg) (and (stringp pkg) (string= pkg "")))
     (list-all-packages))
    ((and (stringp pkg) (string= pkg "."))
     (list *package*))
    ((listp pkg)
     (mapcar #'%pkg pkg))
    (t
     (list (%pkg pkg)))))

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

;;; ---- parsing helpers ----

(defun %ap-option-keyword-p (x)
  "Keywords understood by AP's public interface."
  (and (keywordp x)
       (member x '(:pkg :q :k :exp :case :lim :min :tgt :u :s)
               :test #'eq)))

(defun %package-designator-p (x)
  "True if X can be interpreted as a *single* package selector."
  (cond
    ((null x) t)                           ; NIL => all packages
    ((packagep x) t)
    ((and (stringp x) (or (string= x ".") (string= x ""))) t)
    ;; string/symbol/keyword counts as pkg designator only if package exists
    ((or (stringp x) (symbolp x)) (not (null (%pkg-maybe x))))
    (t nil)))

(defun %list-of-packages-p (x)
  "True if X is a non-empty list of package designators."
  (and (listp x)
       (not (null x))
       (every #'%package-designator-p x)))

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

;;;; ----------------------------
;;;; Query inference (regex always, exact via "=...")
;;;; ----------------------------

(defun %qexpr-p (x)
  "True if X looks like (and ...) or (or ...) query expression."
  (and (consp x) (symbolp (car x)) (member (car x) '(and or) :test #'eq)))

(defun %compile-q (q &key (case t))
  "Compile Q into a predicate (lambda (name doc tgt) ...) -> boolean.

Q forms:

Atoms:
  - string / symbol / keyword / number ... => treated as regex (or exact with \"=...\")
  - alist: '((:name . \"...\") (:doc . \"...\") (:both . \"...\"))  ; may contain symbols as values too

Combinators:
  (or q1 q2 ...)
  (and q1 q2 ...)
Nested combinations are allowed.

Semantics:
  - Atoms respect TGT passed at runtime (:name/:doc/:both),
    except alist atoms, which can specify :name/:doc/:both internally."
  (labels ((norm-s (x) (%q->string x))
           (exactp (s) (and (stringp s) (> (length s) 0) (char= (char s 0) #\=)))
           (drop= (s) (subseq s 1))
           (mk-scanner (pat)
             (cl-ppcre:create-scanner pat :case-insensitive-mode case))
           (field-match (scanner field)
             (and field (cl-ppcre:scan scanner field)))
           (field-exact (needle field)
             (and field (if case (string-equal needle field) (string= needle field))))
           (atom-matcher (atom)
             ;; returns (lambda (name doc tgt) ...)
             (cond
               ;; nil / "" => match-all atom
               ((or (null atom) (and (stringp atom) (string= atom "")))
                (lambda (name doc tgt) (declare (ignore name doc tgt)) t))

               ;; alist atom
               ((%alistp atom)
                (let ((m (%mk-matcher atom :case case)))
                  ;; our earlier %mk-matcher(alist) already returns (name doc tgt)->bool
                  m))

               ;; any other atom => string/symbol/etc
               (t
                (let* ((s (norm-s atom)))
                  (if (exactp s)
                      (let ((needle (drop= s)))
                        (lambda (name doc tgt)
                          (ecase tgt
                            (:name (field-exact needle name))
                            (:doc  (field-exact needle doc))
                            (:both (or (field-exact needle name)
                                       (field-exact needle doc))))))
                      (let ((scanner (mk-scanner s)))
                        (lambda (name doc tgt)
                          (ecase tgt
                            (:name (field-match scanner name))
                            (:doc  (field-match scanner doc))
                            (:both (or (field-match scanner name)
                                       (field-match scanner doc)))))))))))

           (%%compile (x)
             (cond
               ((%qexpr-p x)
                (let* ((op (car x))
                       (subs (mapcar #'%%compile (cdr x))))
                  (ecase op
                    (or  (lambda (name doc tgt)
                           (some (lambda (f) (funcall f name doc tgt)) subs)))
                    (and (lambda (name doc tgt)
                           (every (lambda (f) (funcall f name doc tgt)) subs))))))
               ;; A plain list that is NOT (and/or ...) should default to (or ...)
               ;; This makes (sheet workbook) mean OR by default (your example).
               ((consp x)
                (let ((subs (mapcar #'%%compile x)))
                  (lambda (name doc tgt)
                    (some (lambda (f) (funcall f name doc tgt)) subs))))
               (t
                (atom-matcher x)))))
    (%%compile q)))


(defun %qmode (q)
  "Returns (values normalized-q mode) where mode ∈ (:none :exact :regex)."
  (cond
    ((or (null q) (and (stringp q) (string= q "")))
     (values nil :none))
    ((and (stringp q) (plusp (length q)) (char= (char q 0) #\=))
     (values (subseq q 1) :exact))
    (t
     (values q :regex))))

(defun %mk-matcher (q &key (case t))
  "Build a matcher closure.

Returns a function (lambda (name doc tgt) ...) => boolean

Q can be:
  - NIL / \"\"                 => match everything
  - string                     => regex by default, exact match if starts with \"=\"
  - symbol/keyword             => treated like its SYMBOL-NAME string
  - alist                      => e.g. '((:name . \"sheet\") (:doc . \"iterator\"))
                                 Keys: :name :doc :both. Values can be strings or symbols.
  - list of atoms              => defaults to OR  (sheet workbook) == (or sheet workbook)
  - boolean DSL                => (or q1 q2 ...), (and q1 q2 ...), nestable"
  (labels
      ((norm-s (x) (%q->string x))
       (mk-scanner (pat)
         (cl-ppcre:create-scanner pat :case-insensitive-mode case))
       (exactp (s)
         (and (stringp s) (> (length s) 0) (char= (char s 0) #\=)))
       (drop= (s) (subseq s 1))
       (field-match (scanner field)
         (and field (cl-ppcre:scan scanner field)))
       (field-exact (needle field)
         (and field
              (if case (string-equal needle field) (string= needle field))))
       (%qexpr-p (x)
         (and (consp x) (symbolp (car x)) (member (car x) '(and or) :test #'eq)))
       (%atom-matcher (atom)
         (cond
           ;; nil / "" => match all
           ((or (null atom) (and (stringp atom) (string= atom "")))
            (lambda (name doc tgt) (declare (ignore name doc tgt)) t))

           ;; alist atom: targets override tgt
           ((%alistp atom)
            (let ((pairs atom)
                  (name-tests '())
                  (doc-tests '())
                  (both-tests '()))
              (dolist (kv pairs)
                (destructuring-bind (k . v) kv
                  (let ((s (norm-s v)))
                    (when s
                      (labels ((push-test (where s)
                                 (push (if (exactp s)
                                           (list :exact (drop= s))
                                           (list :re (mk-scanner s)))
                                       where)))
                        (cond
                          ((eq k :name) (push-test name-tests s))
                          ((eq k :doc)  (push-test doc-tests s))
                          ((or (eq k :both) (eq k t)) (push-test both-tests s))
                          (t (push-test both-tests s))))))))
              (lambda (name doc tgt)
                (declare (ignore tgt))
                (flet ((run1 (test field)
                         (ecase (first test)
                           (:re    (field-match (second test) field))
                           (:exact (field-exact (second test) field)))))
                  (or (some (lambda (tst) (run1 tst name)) name-tests)
                      (some (lambda (tst) (run1 tst doc))  doc-tests)
                      (some (lambda (tst) (or (run1 tst name) (run1 tst doc)))
                            both-tests))))))

           ;; regular atom: string/symbol/keyword/other -> string
           (t
            (let* ((s (norm-s atom)))
              (if (exactp s)
                  (let ((needle (drop= s)))
                    (lambda (name doc tgt)
                      (ecase tgt
                        (:name (field-exact needle name))
                        (:doc  (field-exact needle doc))
                        (:both (or (field-exact needle name)
                                   (field-exact needle doc))))))
                  (let ((scanner (mk-scanner s)))
                    (lambda (name doc tgt)
                      (ecase tgt
                        (:name (field-match scanner name))
                        (:doc  (field-match scanner doc))
                        (:both (or (field-match scanner name)
                                   (field-match scanner doc)))))))))))
       (%cmp (x)
         (cond
           ;; boolean DSL
           ((%qexpr-p x)
            (let* ((op (car x))
                   (subs (mapcar #'%cmp (cdr x))))
              (ecase op
                (or  (lambda (name doc tgt)
                       (some (lambda (f) (funcall f name doc tgt)) subs)))
                (and (lambda (name doc tgt)
                       (every (lambda (f) (funcall f name doc tgt)) subs))))))

           ;; plain list (not and/or): default OR across elements
           ((consp x)
            (let ((subs (mapcar #'%cmp x)))
              (lambda (name doc tgt)
                (some (lambda (f) (funcall f name doc tgt)) subs))))

           ;; atom
           (t
            (%atom-matcher x)))))
    (%cmp q)))


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
                     (funcall m name (or doc "") tgt))
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

;;; ---- final public wrapper ----

(defun ap (&rest args)
  "Public REPL-friendly AP with DWIM positional parsing.

Parsing rules:

1) If first arg is an AP option keyword (:pkg :q :k :exp :case :lim :min :tgt :u :s),
   we treat the call as keyword-style and forward into %AP.

2) Otherwise, if the first arg is a package selector, consume it as PKG.
   - package objects, NIL, \".\"/\"\" strings are package selectors
   - strings/symbols/keywords are package selectors only if FIND-PACKAGE succeeds
   - lists are package selectors only if every element is a package selector

3) After PKG is decided, the next positional argument (if present) is Q unless it is an AP option keyword.
   Q may be a string, symbol, NIL, an alist, or any object.

4) Remaining args must be a keyword plist (supports (:pkg X \"q\") as implicit :q).

Examples:
  (ap \"sheet\")                       ; query in current package
  (ap 'sheet)                          ; query symbol in current package
  (ap :cl-excel \"sheet\")              ; package if CL-EXCEL package exists
  (ap \"cl-excel\" 'sheet :tgt :name)   ; string pkg (case-insensitive), query symbol
  (ap '(:cl :sb-ext) \"hash\" :k '(macro))
  (ap :pkg :cl-excel \"sheet\")         ; implicit :q
  (ap :pkg :cl-excel :q 'sheet)         ; explicit

Introspection:
  (describe 'ap::%ap) shows the full keyword interface and defaults."
  ;; keyword-first mode
  (when (and args (%ap-option-keyword-p (first args)))
    (return-from ap (apply #'%ap (%fix-implicit-q args))))

  (let* ((pkg ".")
         (q nil)
         (rest args))

    ;; PKG positional?
    (when (and rest
               (or (%package-designator-p (first rest))
                   (%list-of-packages-p (first rest))))
      (setf pkg (pop rest)))

    ;; Q positional? (may be symbol, alist, etc.)
    (when (and rest (not (%ap-option-keyword-p (first rest))))
      (setf q (pop rest)))

    (let ((plist (%fix-implicit-q rest)))
      (apply #'%ap
             :pkg (if (member :pkg plist :test #'eq) (getf plist :pkg) pkg)
             :q   (if (member :q   plist :test #'eq) (getf plist :q)   q)
             plist))))
