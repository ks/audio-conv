(in-package #:audio-conv)


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


(defun raw-vector (file &key (buffer-size 16384) verbose)
  (multiple-value-bind (streamer cleanup-fun)
      (make-streamer file)
    (let* ((streamer-length (mixalot:streamer-length streamer nil))
           (raw-vector (make-array streamer-length :element-type 'mixalot:stereo-sample))
           (countdown (ceiling streamer-length buffer-size))
           (last-end nil))
      (when verbose
        (format *trace-output* "file ~A approx length: ~A samples~%" file streamer-length))
      (flet ((callback (buffer start end)
               (when verbose
                 (princ countdown *trace-output*)
                 (princ #\space *trace-output*)
                 (decf countdown))
               (replace raw-vector buffer
                        :start1 start :end1 end
                        :start2 0 :end2 (- end start))
               (setf last-end end)))
        (when verbose
          (format *trace-output* "extracting raw vector: "))
        (drain-streamer streamer #'callback :buffer-size buffer-size)
        (when verbose (terpri *trace-output*))
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


;;;;;;;;;;

(defparameter *lame-binary* "/usr/bin/lame")
(defparameter *mandatory-lame-options* '("--silent"))
(defparameter *lame-options* '("--preset" "cbr" "192" "-q" "2"))
(defparameter *temp-dir* "/tmp/")
(defparameter *temp-infile* "tmp-audio-conv-input")
(defparameter *temp-outfile* "tmp-audio-conv-output")

(define-condition lame-error (error)
  ((exit-code :initarg :exit-code :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "LAME encoding failed: exit code ~A" (exit-code condition)))))

(defun convert-to-mp3 (infile outfile &key (lame-options *lame-options*) (temp-dir *temp-dir*) verbose)
  (let ((raw-filename (concatenate 'string temp-dir (pathname-name infile) ".raw")))
    (unwind-protect
         (progn
           (convert-to-raw infile raw-filename :verbose verbose)
           (let ((args (append *mandatory-lame-options* lame-options (list raw-filename outfile))))
             (when verbose
               (format *trace-output* "running ~A with args: ~A~%" *lame-binary* args))
             (let* ((lame-process (sb-ext:run-program *lame-binary* args))
                    (exit-code (sb-ext:process-exit-code lame-process)))
               (unless (zerop exit-code)
                 (error 'lame-error :exit-code exit-code))
               outfile)))
      (when (probe-file raw-filename)
        (delete-file raw-filename)))))
  

(defun directory-convert-to-mp3 (relative-directory-mask out-absolute-directory
                                 &key
                                 (default-pathname-defaults *default-pathname-defaults*)
                                 (lame-options *lame-options*)
                                 (temp-dir *temp-dir*)
                                 (verbose t))
  (ensure-directories-exist temp-dir :verbose verbose)
  (unless (eq (car (pathname-directory relative-directory-mask)) :relative)
    (error "RELATIVE-DIRECTORY-MASK ~A must not start with '/' use DEFAULT-PATHNAME-DEFAULTS to set the root of the path"
           relative-directory-mask))
  (unless (eq (car (pathname-directory out-absolute-directory)) :absolute)
    (error "OUT-ABSOLUTE-DIRECTORY ~A must start with '/'" out-absolute-directory))
  (unless (char= (last-elt out-absolute-directory) #\/)
    (error "OUT-ABSOLUTE-DIRECTORY ~A must end with '/'" out-absolute-directory))
  (unless (probe-file out-absolute-directory)
    (unless (nth-value 1 (ensure-directories-exist out-absolute-directory :verbose verbose))
      (error "can't create OUT-ABSOLUTE-DIRECTORY ~A" out-absolute-directory)))
  (let* ((absolute-directory-mask (merge-pathnames relative-directory-mask default-pathname-defaults))
         (absolute-directory-prefix-length (length (namestring default-pathname-defaults)))
         (to-wildcard (concatenate 'string out-absolute-directory "**/*.mp3"))
         (infiles (mapcar #'namestring (directory absolute-directory-mask)))
         (infiles-length (length infiles)))
    (loop
       :for infile :in infiles
       :for idx :from 1
       :do (with-simple-restart (skip-file "skip converting ~A" infile)
             (let* ((relative-infile (subseq (namestring infile) absolute-directory-prefix-length))
                    (outfile (namestring (translate-pathname relative-infile "**/*.*" to-wildcard))))
               (when verbose
                 (format *trace-output* "[~A/~A] infile: ~A --> outfile: ~A~%" idx infiles-length infile outfile))
               (ensure-directories-exist outfile :verbose verbose)
               (let ((tmp-infile (namestring
                                  (merge-pathnames (make-pathname :name *temp-infile*
                                                                  :type (pathname-type infile))
                                                   temp-dir)))
                     (tmp-outfile (namestring (merge-pathnames *temp-outfile* temp-dir))))
                 (unwind-protect
                      (progn
                        (copy-file infile tmp-infile)
                        (convert-to-mp3 tmp-infile tmp-outfile
                                        :lame-options lame-options :temp-dir temp-dir :verbose verbose))
                   (when (probe-file tmp-infile)
                     (delete-file tmp-infile))
                   (when (probe-file tmp-outfile)
                     (copy-file tmp-outfile outfile)
                     (delete-file tmp-outfile)
                     (when verbose
                       (format *trace-output* "... converted OK~%"))))))))))
