(in-package #:org.shirakumo.fraf.trial.harmony)

(defmethod trial:report-on-error ((error mixed:mixed-error))
  (case (mixed:error-code error)
    ((:out-of-memory 1)
     (trial:emessage "Failed to allocate memory. Are you sure you have enough RAM free to run this game?"))
    (T
     (call-next-method))))

#+linux
(trial::dont-deploy
 org.shirakumo.fraf.mixed.pulse.cffi::libpulse-simple
 org.shirakumo.fraf.mixed.pulse.cffi::libpulse
 org.shirakumo.fraf.mixed.alsa.cffi::libasound
 org.shirakumo.fraf.mixed.pipewire.cffi::libpipewire)
#+darwin
(trial::dont-deploy
 org.shirakumo.fraf.mixed.coreaudio.cffi::audio-toolbox
 org.shirakumo.fraf.mixed.coreaudio.cffi::audio-unit)
#+windows
(trial::dont-deploy
 org.shirakumo.fraf.mixed.winmm.cffi::winmm
 org.shirakumo.fraf.mixed.wasapi.cffi::avrt)
