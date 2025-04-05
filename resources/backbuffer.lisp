(in-package #:org.shirakumo.fraf.trial)

(defclass backbuffer (framebuffer)
  ((context :initarg :context)
   (data-pointer :initform 0)))

(defmethod gl-name ((framebuffer backbuffer))
  0)

(defmethod allocated-p ((framebuffer backbuffer))
  T)

(defmethod width ((framebuffer backbuffer))
  (width (context framebuffer)))

(defmethod height ((framebuffer backbuffer))
  (height (context framebuffer)))

(defmethod resize ((framebuffer backbuffer) width height)
  (resize (context framebuffer) width height))

(defmethod attachments ((framebuffer backbuffer))
  ;; TODO: provide virtual textures to glReadPixels with.
  ())

(defmethod allocate :around ((framebuffer backbuffer))
  framebuffer)

(defmethod deallocate :around ((framebuffer backbuffer))
  framebuffer)

(defmethod dependencies ((framebuffer backbuffer)))
