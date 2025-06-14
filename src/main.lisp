(defpackage #:com.thejach.frame-chronicle
  (:use #:cl #:bordeaux-threads)
  (:export #:make-store
           #:frame-tick
           #:record
           #:query
           #:clear
           #:*disabled?*
           ))
(in-package #:com.thejach.frame-chronicle)

(defvar *disabled?* nil
  "Flag to disable all recording.")

(defvar *store* nil
  "The 'store object to be used in context of exported functions. Will be set by #'make-store.")

(defgeneric %record (store id value)
  (:documentation "Internal method to handle actually storing the recorded value associated with frame number and ID.
                   Instead of recording every frame, sampling options can be given to #'make-store.
                   Recording is thus skippable via :around methods in different kinds of sampling subclasses."))

(defclass store ()
  ((current-frame :accessor current-frame :initform -1 :documentation "The frame number that calls to #'record will be associated with.")
   (data :accessor data :initform (make-hash-table :test #'equal) :documentation "Internal storage for all of this store's data.")
   (mutex :accessor mutex :initform (bt:make-lock) :documentation "Per-instance lock to support concurrent read/writes."))
  (:documentation "Simple base store class, will keep all data recorded."))


(defmethod %record ((store store) id value)
  "Stores ID and value associated with the current frame number.
   Each ID is associated with a list of entries.
   Each entry is a property list. Currently the only property is the :value itself, but in the future we may add timestamping or something else."
  (let* ((frame-table (or (gethash (current-frame store) (data store))
                          (setf (gethash (current-frame store) (data store)) (make-hash-table :test #'equal))))
         (entry (list :value value)))
    (push entry (gethash id frame-table))))


(defclass sampled-on-frames-store (store)
  ((frames :accessor sampled-on-frames :initarg :frames :initform '() :documentation "A list of frame numbers"))
  (:documentation "Store subclass wehre if the 'current-frame matches a frame in the 'frames list,
                   then calls to #'record will actually store the data, otherwise no data will be stored."))

(defmethod %record :around ((store sampled-on-frames-store) id value)
  (when (member (current-frame store) (sampled-on-frames store))
    (call-next-method)))

(defclass sampled-every-nth-frame-store (store)
  ((nth-frame :accessor sampled-nth-frame :initarg :nth-frame :documentation "The nth frame number to sample on."))
  (:documentation "Store subclass where storage happens only if the 'current-frame % 'nth-frame == 0."))

(defmethod %record :around ((store sampled-every-nth-frame-store) id value)
  (when (zerop (mod (current-frame store) (sampled-nth-frame store)))
    (call-next-method)))

(defclass sampled-on-function-store (store)
  ((sampled-function :accessor sampled-function :initarg :function :documentation "The custom function that takes the 'current-frame as input and returns true or false."))
  (:documentation "Store subclass where storage happens only if the provided function returns a true value."))

(defmethod %record :around ((store sampled-on-function-store) id value)
  (when (funcall (sampled-function store) (current-frame store))
    (call-next-method)))


(defun make-store (&key (sample-always nil)
                        (sample-on-frames '() sample-on-frames-p)
                        (sample-every-nth-frame most-positive-fixnum sample-every-nth-frame-p)
                        (sample-on-function (lambda (frame)
                                              (declare (ignorable frame))
                                              "A function that takes the current frame as input and returns true
                                               if #'record should actually store."
                                              nil)
                                            sample-on-function-p))
  "Creates a frame chronicle data store that filters what is recorded according to
   the passed keyword arguments.
   This store is used as the default for other exported functions in this package.
   The store itself is returned, so you can if you desire make a copy of it and bind
   fc::*store* to make use of it instead of the last-set default."
  (let ((store (cond
                 (sample-always (make-instance 'store))
                 (sample-on-frames-p (make-instance 'sampled-on-frames-store :frames sample-on-frames))
                 (sample-every-nth-frame-p (make-instance 'sampled-every-nth-frame-store :nth-frame sample-every-nth-frame))
                 (sample-on-function-p (make-instance 'sampled-on-function-store :function sample-on-function)))))
    (setf *store* store)
    store))

(defun frame-tick ()
  "Should be called at the beginning of a new frame.
   The first frame is numbered as 0, then incremented from there."
  (bt:with-lock-held ((mutex *store*))
    (incf (current-frame *store*))))

(defun record (id value)
  "If storage conditions on the current frame are met, stores a value associated with the given ID.
   Values associated with the same ID are all collected as a group, one does not overwrite another."
  (unless *disabled?*
    (bt:with-lock-held ((mutex *store*))
      (%record *store* id value))))

(defun frame-table-plist (frame-table)
  "Given the hash table frame-table of id -> values-list,
   give it as a plist, with the values reversed so that the first one
   is the first stored."
  (nreverse (mapcar (lambda (val-cell) (getf val-cell :value))
                    frame-table)))

(defun query (id &key frames)
  "Return stored entries optionally filtered by 'ID and 'frames numbers.
   'ID can be NIL to get all IDs.
   'frames can either be a single frame or a list of frames.

   If a nil 'ID and no frames are given, all stored data is returned as a property list in the form:
   (frame-number1 (:some-id (val1) :some-other-id (val1 val2))
    frame-number2 (:some-other-id (val1)))

   Note each frame -> frame-value is itself a property list of id -> values-list.
   This form is the same if nil 'ID is given but the two frame numbers are specified by 'frames.

   If just one frame is given in 'frames, we just return the value part of the property list. e.g. for only
   frame-number1:
   (:some-id (val1) :some-other-id (val1 val2))

   If just an 'ID is given, but no specific 'frames, then the format is for example using :some-other-id:
   (frame-number1 (val1 val2)
    frame-number2 (val1))

   Note each frame -> frame-value now just has the frame-value equal to the values-list of the given 'ID.
   Again this form is the same if given an 'ID and those two specific frames.

   Lastly, if an 'ID and a single 'frames number are given, the form is
   (val1)

   That is, just the values-list for that id and that frame.

   If no values exist for a frame, that frame will not be included in the results even if specified by frames."
  (bt:with-lock-held ((mutex *store*))
    (loop for frame-num being the hash-keys in (data *store*)
          using (hash-value frame-table)
          for id-values = (loop for stored-id being the hash-keys in frame-table
                                nconc (cond
                                        ((and id (equal id stored-id))
                                         (frame-table-plist (gethash stored-id frame-table)))
                                        ((null id)
                                         (list stored-id
                                               (frame-table-plist (gethash stored-id frame-table))))))
          if id-values
          nconc (cond
                  ((and frames (equal frame-num frames))
                   id-values)
                  ((or (and frames (listp frames) (member frame-num frames))
                       (null frames))
                   (list frame-num id-values))))))

(defun clear ()
  "Clears all stored data and resets the frame counter to -1 so that after the next #'frame-tick recording will be back on starting frame 0."
  (bt:with-lock-held ((mutex *store*))
    (setf (data *store*) (make-hash-table :test #'equal)
          (current-frame *store*) -1)))
