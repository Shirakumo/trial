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

(docs:define-docs
  (function location
    "Accesses the local relative location of the entity.

Note that the global location may be impacted by inherited transforms.

See GLOBAL-LOCATION")

  (function orientation
    "Accesses the local relative orientation of the entity.

Note that the global orientation may be impacted by inherited
transforms.

See GLOBAL-ORIENTATION")

  (function bsize
    "Accesses the local bounding box size of the entity.

Each dimension is axis-aligned and designates the distance from the
center along that dimension to the face of the box.

Note that the global bounding box may be impacted by inherited
transforms.

See GLOBAL-BSIZE")

  (function bradius
    "Accesses the local bounding sphere radius of the entity.

Note that the global radius may be impacted by inherited transforms.

See GLOBAL-BRADIUS")

  (function compute-bounding-box
    "Compute a bounding box for the entity relative to its location as a center,bsize tuple.

Falls back on (vec3 0),BSIZE

See LOCATION
See BSIZE")

  (function compute-bounding-sphere
    "Compute a bounding sphere for the entity relative to its location as a center,radius tuple.

Falls back on (vec3 0),BRADIUS

See LOCATION
See BRADIUS")

  (function global-transform-matrix
    "Return the global transform matrix of the entity.

Stores the result in the optional argument and returns it.

Defaults to using APPLY-TRANSFORMS on the entity to compute the
matrix.

See APPLY-TRANSFORMS")

  (function global-location
    "Return the global location of the entity.

Stores the result in the optional argument and returns it.

Defaults to using the GLOBAL-TRANSFORM-MATRIX.

See LOCATION
See GLOBAL-TRANSFORM-MATRIX")

  (function global-orientation
    "Return the global orientation of the entity.

Stores the result in the optional argument and returns it.

Defaults to using the GLOBAL-TRANSFORM-MATRIX.

See ORIENTATION
See GLOBAL-TRANSFORM-MATRIX")
  
  (function global-bsize
    "ReturnCompute the global bounding box size of the entity relative to its global location.

Stores the result in the optional argument and returns it.

Defaults to using the local BSIZE and transforming it by the
GLOBAL-TRANSFORM-MATRIX.

See BSIZE
See GLOBAL-LOCATION
See GLOBAL-TRANSFORM-MATRIX
See GLOBAL-BOUNDING-BOX")
  
  (function global-bradius
    "Return the global bounding sphere radius of the entity relative to its global location.

Stores the result in the optional argument and returns it.

Defaults to using the local BRADIUS and transforming it by the
GLOBAL-TRANSFORM-MATRIX.

See BRADIUS
See GLOBAL-LOCATION
See GLOBAL-TRANSFORM-MATRIX
See GLOBAL-BOUNDING-SPHERE")
  
  (function global-bounding-box
    "Return the global bounding box of the entity.

This may give a tighter bound than using GLOBAL-LOCATION and
GLOBAL-BSIZE, as the center of the bounding box may be offset
from the entity's location.

Stores the result in the optional arguments and returns them.

Defaults to using GLOBAL-LOCATION and GLOBAL-BSIZE.
Also works for sequences, using each element's global bounding box.

See GLOBAL-LOCATION
See GLOBAL-BSIZE")
  
  (function global-bounding-sphere
    "Return the global bounding sphere of the entity.

This may give a tighter bound than using GLOBAL-LOCATION and
GLOBAL-RADIUS, as the center of the bounding sphere may be offset
from the entity's location.

Stores the location in the optional argument and returns it and the
radius.

Defaults to using GLOBAL-LOCATION and GLOBAL-BRADIUS.
Also works for sequences, using each element's global bounding sphere.

See GLOBAL-LOCATION
See GLOBAL-BRADIUS"))
