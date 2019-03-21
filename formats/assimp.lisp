#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod classimp->mesh (source)
  (let* ((mesh (make-instance 'vertex-mesh :vertex-type 'basic+-vertex
                                           :face-length (classimp:primitive-types source)))
         (verts (vertices mesh))
         (faces (faces mesh)))
    (loop for location across (classimp:vertices source)
          for normal across (classimp:normals source)
          for uv across (aref (classimp:texture-coords source) 0)
          for tangent across (classimp:tangents source)
          for vertex = (make-instance 'basic+-vertex
                                      :location (vec-from-vector location)
                                      :normal (vec-from-vector normal)
                                      :uv (vec2 (aref uv 0) (aref uv 1))
                                      :tangent (vec-from-vector tangent))
          do (vector-push-extend vertex verts))
    (loop for face across (classimp:faces source)
          do (loop for index across face
                   do (vector-push-extend index faces)))
    mesh))

(defmethod read-geometry (file format &key &allow-other-keys)
  (let* ((geom (make-instance 'geometry))
         (scene (classimp:import-into-lisp file :processing-flags '(:ai-process-triangulate
                                                                    :ai-process-flip-u-vs
                                                                    :ai-process-fix-infacing-normals
                                                                    :ai-process-calc-tangent-space
                                                                    ;; FIXME: should scene-graph sometime
                                                                    :ai-process-pre-transform-vertices)))
         (meshes (map 'vector #'classimp->mesh (classimp:meshes scene))))
    (labels ((traverse (node)
               (map NIL #'traverse (classimp:children node))
               (case (length (classimp:meshes node))
                 (0)
                 (1 (setf (gethash (classimp:name node) (meshes geom))
                          (aref meshes (aref (classimp:meshes node) 0))))
                 (T (loop for i from 0 below (length (classimp:meshes node))
                          for name = (format NIL "~a-~d" (classimp:name node) i)
                          do (setf (gethash name (meshes geom))
                                   (aref meshes (aref (classimp:meshes node) i))))))))
      (traverse (classimp:root-node scene))
      geom)))

