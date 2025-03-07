(defpackage #:com.thejach.frame-chronicle/test
  (:use #:cl #:fiveam)
  (:local-nicknames (#:fc #:com.thejach.frame-chronicle)))
(in-package #:com.thejach.frame-chronicle/test)

;(setf *run-test-when-defined* 'T)
;(asdf:test-system "com.thejach.frame-chronicle")

(defun plist-equal (list1 list2)
  (equalp (alexandria:plist-hash-table list1)
          (alexandria:plist-hash-table list2)))


(def-suite query-record-format-tests
  :description "Various tests to verify that querying gives back expected formatted data
                for various possibilities of recorded state.")
(in-suite query-record-format-tests)

(def-test all-records ()
  (fc:make-store :sample-on-frames '(0 1))
  (fc:frame-tick)
  (fc:record :height 17)
  (fc:record :size 2)
  (fc:record :size 4)
  (fc:frame-tick)
  (fc:record :power 100)

  (let* ((expected '(0 (:height (17) :size (2 4))
                     1 (:power (100))))
         (actual1 (fc:query nil))
         (actual2 (fc:query nil :frames '(0 1))))
    (is (equal expected actual1))
    (is (equal expected actual2))))

(def-test records-by-id ()
  (fc:make-store :sample-on-frames '(0 1))
  (fc:frame-tick)
  (fc:record :height 17)
  (fc:record :size 2)
  (fc:record :size 4)
  (fc:frame-tick)
  (fc:record :power 100)
  (fc:record :size 18)

  (is (equal '(0 (2 4)
               1 (18))
             (fc:query :size)))
  (is (equal '(0 (17))
             (fc:query :height))))

(def-test single-record ()
  (fc:make-store :sample-on-frames '(0 1))
  (fc:frame-tick)
  (fc:record :height 17)

  (is (equal '(17)
             (fc:query :height :frames 0))))

(def-test all-ids-one-frame ()
  (fc:make-store :sample-on-frames '(0 1))
  (fc:frame-tick)
  (fc:record :height 17)
  (fc:record :weight 1)
  (fc:frame-tick)
  (fc:record :height 18)

  (is (equal '(:height (17) :weight (1))
             (fc:query nil :frames 0))))


(def-suite storage-tests
  :description "Various tests to verify that expected data is stored and cleared as specified.")
(in-suite storage-tests)

(def-test empty-store ()
  (fc:make-store :sample-on-frames '(0))
  (fc:frame-tick)

  (is (equal nil (fc:query nil)))
  (is (equal nil (fc:query :foo)))
  (is (equal nil (fc:query nil :frames 0)))
  (is (equal nil (fc:query nil :frames '(0 1))))
  (is (equal nil (fc:query :bar :frames 0)))
  (is (equal nil (fc:query :baz :frames '(0 1))))
  )

(def-test store-and-clear ()
  (fc:make-store :sample-on-frames '(0))
  (fc:frame-tick)
  (fc:record :thing "val")

  (is (equal '("val")
             (fc:query :thing :frames 0)))

  (fc:clear)

  (is (equal nil
             (fc:query :thing :frames 0))))

(def-test sample-always ()
  (fc:make-store :sample-always t)
  (dotimes (i 10)
    (fc:frame-tick)
    (fc:record :foo :bar))

  (is (equal (* 2 10) (length (fc:query nil))))
  (is (equal '(0 (:BAR) 1 (:BAR) 2 (:BAR) 3 (:BAR) 4 (:BAR) 5 (:BAR) 6 (:BAR) 7 (:BAR) 8 (:BAR) 9 (:BAR))
             (fc:query :foo))))

(def-test sample-on-frames ()
  (fc:make-store :sample-on-frames '(0 2 3))
  (fc:frame-tick) ; 0
  (fc:record :state :idle)
  (fc:frame-tick) ; 1
  (fc:record :state :attack)
  (fc:frame-tick) ; 2
  (fc:record :state :dying)
  (fc:frame-tick) ; 3
  (fc:record :state :dead)
  (fc:frame-tick) ; 4
  (fc:record :state :reviving)

  (is (equal '(0 (:idle) 2 (:dying) 3 (:dead))
             (fc:query :state))))

(def-test sample-every-nth-frame ()
  (fc:make-store :sample-every-nth-frame 10)
  (dotimes (i 51)
    (fc:frame-tick)
    (fc:record :debug (format nil "frame~a" i)))


  (is (equal '(0 ("frame0") 10 ("frame10") 20 ("frame20") 30 ("frame30") 40 ("frame40") 50 ("frame50"))
             (fc:query :debug)))
  (is (equal '("frame30")
             (fc:query :debug :frames 30)))
  (is (equal '(20 ("frame20") 50 ("frame50"))
             (fc:query :debug :frames '(20 50)))))

(def-test sample-on-function ()
  (fc:make-store :sample-on-function #'oddp)
  (dotimes (i 10)
    (fc:frame-tick)
    (fc:record :a (format nil "frame~a" i)))
  (is (equal '(1 (:a ("frame1"))
               3 (:a ("frame3"))
               5 (:a ("frame5"))
               7 (:a ("frame7"))
               9 (:a ("frame9")))
             (fc:query nil))))

(def-test paused-recording ()
  (unwind-protect
    (progn
      (fc:make-store :sample-always t)
      (fc:frame-tick)
      (fc:record :thing :one-thing)
      (setf fc:*disabled?* t)
      (dotimes (i 10)
        (fc:frame-tick)
        (fc:record :thing :another-thing?))

      (is (equal '(0 (:one-thing))
                 (fc:query :thing)))
      (setf fc:*disabled?* nil)
      (fc:record :thing :and-another-thing)

      (is (equal '(0 (:one-thing)
                   10 (:and-another-thing))
                 (fc:query :thing))))

    (setf fc:*disabled?* nil)))
