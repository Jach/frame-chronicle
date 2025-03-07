(asdf:defsystem "com.thejach.frame-chronicle"
  :description "A simple thread-safe way to collect game state on specific frames"
  :version "0.1.0"
  :author "Jach <github@thejach.com>"
  :license "Unlicense / Public Domain"
  :depends-on ("bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "main"))
  :in-order-to ((asdf:test-op (asdf:test-op "com.thejach.frame-chronicle/test"))))

(asdf:defsystem "com.thejach.frame-chronicle/test"
  :depends-on ("com.thejach.frame-chronicle"
               "fiveam")
  :serial t
  :pathname "test"
  :components ((:file "basic-tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call ':5am '#:run-all-tests ':summary ':suite)))
