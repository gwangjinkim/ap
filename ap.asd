(asdf:defsystem "ap"
  :description "Quick REPL apropos++: description-first symbol browser with kind filters and optional thematic ordering."
  :author "Gwang-Jin Kim <gwang.jin.kim.phd@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on ("cl-ppcre")
  :components
  ((:file "package")
   (:file "ap")))
