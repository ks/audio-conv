Not yet finished, and SBCL (Linux) only (is there a portable sb-vector-io like library?)

* What it does currently:
  - can take mp3, ogg or flac file and save the raw data of the audio into another file - function audio-conv::convert-to-raw does that
    (This file can then be imported to Audacity for example)


* TODO:
  - walking directories and converting audio files to mp3 with specified settings using LAME encoder
