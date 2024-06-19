(in-package #:org.shirakumo.fraf.trial)

(docs:define-docs
  (variable +map-key-events+
   "Controls whether key events are mapped.

This is useful to set to NIL when within a text input field, to avoid
key bindings interfering with text input.")

  (asset (trial fullscreen-square)
    "A simple mesh representing a zero-centred 2x2 square.

It is primarily useful for rendering a full-screen square in the
absence of a perspective transform.")

  (asset (trial empty-vertex-array)
    "A vertex array with no vertices.

This is useful if the vertices are fully computed in the vertex shader
instead of being driven by static data. You must however make sure to
pass the correct number of vertices when initiating the draw.")

  (pool trial
    "Asset pool for base engine things.

These assets can be loaded without any format extensions being
necessary, and are used by the core engine modules. You can also make
use of them yourself.")

  (asset (trial ascii)
    "A texture atlas for ASCII characters.

Each glyph is 9 pixels wide and 17 pixels tall.
The first set of ASCII characters are skipped, as thei are all
non-graphical. The first represented character is Space (#32).")

  (asset (trial brdf-lut)
    "The BRDF LUT texture for use during PBR IBL rendering.")

  (asset (trial cat)
    "The infamous Trial cat texture.")

  (asset (trial missing)
    "A pink-black checkerboard texture useful as a placeholder.")

  (asset (trial neutral-mro)
    "A one-pixel texture of a neutral PBR MRO map.")

  (asset (trial neutral-normal)
    "A one-pixel texture of a netural normal map.")

  (asset (trial random)
    "A white-noise texture.")

  (asset (trial black)
    "A one-pixel texture of pure black.")

  (asset (trial white)
    "A one-pixel texture of pure white.")

  (asset (trial unit-cube)
    "A UV-mapped zero-centred 1x1 cube.")

  (asset (trial unit-sphere)
    "A UV-mapped zero-centred sphere with radius 1.")

  (asset (trial unit-square)
    "A UV-mapped zero-centred 1x1 square in the XY plane.")

  (asset (trial unit-disc)
    "A disc in the XY plane with radius 1.")

  (asset (trial unit-cylinder)
    "A cylinder standing on the XZ plane with height and radius 1.")

  (asset (trial unit-cone)
    "A cone standing on the XZ plane with height and radius 1.")

  (asset (trial unit-tube)
    "A tube standing on the XZ plane with height and radius 1 and thickness 0.5.")

  (asset (trial unit-point)
    "A triangle with zero area.")

  (asset (trial grid)
    "A uniform line grid 100x100 in size, with 10 cells in each direction.")

  (asset (trial axes)
    "A line in each axis direction, each 10 in length.")

  (asset (trial 2d-axes)
    "A line in each axis direction, each infinitely long from the origin."))
