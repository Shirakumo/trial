(in-package #:org.shirakumo.fraf.trial)

(docs:define-docs
  (variable +map-key-events+
   "Controls whether key events are mapped.

This is useful to set to NIL when within a text input field, to avoid
key bindings interfering with text input."))
