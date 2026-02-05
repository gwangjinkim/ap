(defpackage #:ap
  (:use #:cl)
  (:nicknames #:apro)
  (:export
   ;; main command
   #:ap

   ;; optional helpers / knobs (keep them short)
   #:*kinds*          ; default kinds
   #:*exp*            ; exported-only default
   #:*case*           ; case-insensitive default
   #:*lim*            ; default limit (nil => no limit)
   ))
