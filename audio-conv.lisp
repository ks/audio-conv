(in-package #:audio-conv)

;; call it from main thread before anything else
(defun init ()
  (mixalot:main-thread-init))


(defun make-streamer (file)
  (if-let (probe-file file)
    (let ((audio-type (pathname-type file)))
      (cond ((string-equal audio-type "mp3")
             (values (mixalot-mp3:make-mp3-streamer file)
                     #'mixalot-mp3:mp3-streamer-release-resources))
            ((string-equal audio-type "ogg")
             (values (mixalot-vorbis:make-vorbis-streamer file)
                     #'mixalot-vorbis:vorbis-streamer-release-resources))
            ((string-equal audio-type "flac")
             (values (mixalot-flac:make-flac-streamer file)
                     nil))
            (t
             (error "file ~A is of unknown audio type ~A" file audio-type))))
    (error "file ~A not found" file)))


(defclass stream-monitor ()
  ((eof-flag :accessor eof-flag :initform nil))
  (:documentation "Used to detect end of stream. Ugly hack."))


(defmethod mixalot:mixer-remove-streamer ((mixer stream-monitor) stream)
  (setf (eof-flag mixer) t))


(defun drain-streamer (streamer callback &key (buffer-size 16384))
  (let ((monitor (make-instance 'stream-monitor))
        (buffer (make-array buffer-size :element-type 'mixalot:stereo-sample)))
    (flet ((process-buffer (time)
             (fill buffer 0)
             (mixalot:streamer-write-into streamer monitor buffer 0 buffer-size time)
             (funcall callback buffer time (mixalot:streamer-position streamer monitor))))
      (loop
         :for time :upfrom 0 :by buffer-size
         :do (process-buffer time)
         :until (eof-flag monitor)
         :finally (mixalot:streamer-cleanup streamer monitor)))))


(defun raw-vector (file &key verbose)
  (multiple-value-bind (streamer cleanup-fun)
      (make-streamer file)
    (let*((streamer-length (mixalot:streamer-length streamer nil))
          (raw-vector (make-array streamer-length :element-type 'mixalot:stereo-sample))
          (last-end nil))
      (when verbose
        (format *trace-output* "file ~A approx length: ~A samples~%" file streamer-length))
      (flet ((callback (buffer start end)
               (when verbose
                 (format *trace-output* "copyping ~A -> ~A~%" start end))
               (replace raw-vector buffer
                        :start1 start :end1 end
                        :start2 0 :end2 (- end start))
               (setf last-end end)))
        (drain-streamer streamer #'callback)
        (when cleanup-fun (funcall cleanup-fun streamer))
        (subseq raw-vector 0 last-end)))))
    

;; result of this can be imported to Audacity for example (File -> Import -> Raw Data)
(defun convert-to-raw (infile outfile &key verbose)
  (let* ((raw-vector (raw-vector infile :verbose verbose))
         (elt-type (array-element-type raw-vector)))
    (with-output-to-file (out outfile :element-type elt-type :if-exists :supersede)
      (let ((written (sb-vector-io:write-vector-data raw-vector out)))
        (assert (= (length raw-vector) written))
        written))))
        
