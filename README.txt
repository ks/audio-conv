SBCL (Linux) only (is there a portable sb-vector-io like library?)

* What it does currently:
  - can take mp3, ogg or flac file and save the raw data of the audio into another file - (function audio-conv::convert-to-raw does that)
    (This file can then be imported to Audacity for example)

  - can convert one file using LAME respecting the settings - (function audio-conv::onvert-to-mp3)

  - can convert whole subdirectories according to the directory mask - (function audio-conv::directory-convert-to-mp3)

    Parameters:
    * relative-directory-mask - (ex.: music/chillout/**/*.flac - takes all flac files (even nested in subdirs) under music/chillout directory)
    * out-absolute-directory - (ex.: /tmp/converted/ - result will be stored as /tmp/converted/music/chillout/..... )
    * default-pathname-defaults - where is the relative-directory-mask searched for (it's a prefix path for relative-directory-mask)
    * lame-options - default are: "--preset cbr 192 -q 2" - constant bitrate 192 kbs, quality 2
    * temp-dir - where audio-conv puts it's temporary files 
    * verbose - should the function report the progress (default t)

