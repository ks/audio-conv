;;;; audio-conv.asd

(asdf:defsystem #:audio-conv
  :serial t
  :description "Converts mp3, ogg and flac audio into constant rate mp3."
  :author "Karol Skocik"
  :license "MIT"
  :depends-on (#:alexandria
               #:sb-vector-io    ;; SBCL only!!
               #:mixalot         ;; requires latest Mixalot from https://github.com/ahefner/mixalot (>= 1.9.2012)
               #:mixalot-mp3
               #:mixalot-vorbis
               #:mixalot-flac)
  :components ((:file "package")
               (:file "audio-conv")))

