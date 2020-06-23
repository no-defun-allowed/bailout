(asdf:defsystem :bailout
  :depends-on (:alexandria :bordeaux-threads :safe-queue)
  :serial t
  :components ((:file "package")
               (:file "job")
               (:file "supervisor")
               (:file "incident")
               (:file "problem")
               (:file "recoveries")
               (:file "option")))
