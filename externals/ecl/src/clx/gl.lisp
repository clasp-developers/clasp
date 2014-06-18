(defpackage :gl
  (:use :common-lisp :xlib)
  (:import-from :glx
                "*CURRENT-CONTEXT*"
                "CONTEXT"
                "CONTEXT-P"
                "CONTEXT-DISPLAY"
                "CONTEXT-TAG"
                "CONTEXT-RBUF"
                "CONTEXT-INDEX"
                )
  (:import-from :xlib
                "DATA"
                "WITH-BUFFER-REQUEST"
                "WITH-BUFFER-REQUEST-AND-REPLY"
                "CARD32-GET"
                "SEQUENCE-GET"

                "WITH-DISPLAY"
                "DISPLAY-FORCE-OUTPUT"

                "INT8" "INT16" "INT32" "INTEGER"
                "CARD8" "CARD16" "CARD32"

                "ASET-CARD8"
                "ASET-CARD16"
                "ASET-CARD32"
                "ASET-INT8"
                "ASET-INT16"
                "ASET-INT32"

                "DECLARE-BUFFUN"

                ;; Types
                "ARRAY-INDEX"
                "BUFFER-BYTES"
                )

  (:export "GET-STRING"

           ;; Rendering commands (alphabetical order)

           "ACCUM"
           "ACTIVE-TEXTURE-ARB"
           "ALPHA-FUNC"
           "BEGIN"
           "BIND-TEXTURE"
           "BLEND-COLOR"
           "BLEND-EQUOTION"
           "BLEND-FUNC"
           "CALL-LIST"
           "CLEAR"
           "CLEAR-ACCUM"
           "CLEAR-COLOR"
           "CLEAR-DEPTH"
           "CLEAR-INDEX"
           "CLEAR-STENCIL"
           "CLIP-PLANE"
           "COLOR-3B"
           "COLOR-3D"
           "COLOR-3F"
           "COLOR-3I"
           "COLOR-3S"
           "COLOR-3UB"
           "COLOR-3UI"
           "COLOR-3US"
           "COLOR-4B"
           "COLOR-4D"
           "COLOR-4F"
           "COLOR-4I"
           "COLOR-4S"
           "COLOR-4UB"
           "COLOR-4UI"
           "COLOR-4US"
           "COLOR-MASK"
           "COLOR-MATERIAL"
           "CONVOLUTION-PARAMETER-F"
           "CONVOLUTION-PARAMETER-I"
           "COPY-COLOR-SUB-TABLE"
           "COPY-COLOR-TABLE"
           "COPY-CONVOLUTION-FILTER-ID"
           "COPY-CONVOLUTION-FILTER-2D"
           "COPY-PIXELS"
           "COPY-TEX-IMAGE-1D"
           "COPY-TEX-IMAGE-2D"
           "COPY-TEX-SUB-IMAGE-1D"
           "COPY-TEX-SUB-IMAGE-2D"
           "COPY-TEX-SUB-IMAGE-3D"
           "CULL-FACE"
           "DEPTH-FUNC"
           "DEPTH-MASK"
           "DEPTH-RANGE"
           "DRAW-BUFFER"
           "EDGE-FLAG-V"
           "END"
           "EVAL-COORD-1D"
           "EVAL-COORD-1F"
           "EVAL-COORD-2D"
           "EVAL-COORD-2F"
           "EVAL-MESH-1"
           "EVAL-MESH-2"
           "EVAL-POINT-1"
           "EVAL-POINT-2"
           "FOG-F"
           "FOG-I"
           "FRONT-FACE"
           "FRUSTUM"
           "HINT"
           "HISTOGRAM"
           "INDEX-MASK"
           "INDEX-D"
           "INDEX-F"
           "INDEX-I"
           "INDEX-S"
           "INDEX-UB"
           "INIT-NAMES"
           "LIGHT-MODEL-F"
           "LIGHT-MODEL-I"
           "LIGHT-F"
           "LIGHT-FV"
           "LIGHT-I"
           "LIGHT-IV"
           "LINE-STIPPLE"
           "LINE-WIDTH"
           "LIST-BASE"
           "LOAD-IDENTITY"
           "LOAD-NAME"
           "LOGIC-OP"
           "MAP-GRID-1D"
           "MAP-GRID-1F"
           "MAP-GRID-2D"
           "MAP-GRID-2F"
           "MATERIAL-F"
           "MATERIAL-FV"
           "MATERIAL-I"
           "MATERIAL-IV"
           "MATRIX-MODE"
           "MINMAX"
           "MULTI-TEX-COORD-1D-ARB"
           "MULTI-TEX-COORD-1F-ARB"
           "MULTI-TEX-COORD-1I-ARB"
           "MULTI-TEX-COORD-1S-ARB"
           "MULTI-TEX-COORD-2D-ARB"
           "MULTI-TEX-COORD-2F-ARB"
           "MULTI-TEX-COORD-2I-ARB"
           "MULTI-TEX-COORD-2S-ARB"
           "MULTI-TEX-COORD-3D-ARB"
           "MULTI-TEX-COORD-3F-ARB"
           "MULTI-TEX-COORD-3I-ARB"
           "MULTI-TEX-COORD-3S-ARB"
           "MULTI-TEX-COORD-4D-ARB"
           "MULTI-TEX-COORD-4F-ARB"
           "MULTI-TEX-COORD-4I-ARB"
           "MULTI-TEX-COORD-4S-ARB"
           "NORMAL-3B"
           "NORMAL-3D"
           "NORMAL-3F"
           "NORMAL-3I"
           "NORMAL-3S"
           "ORTHO"
           "PASS-THROUGH"
           "PIXEL-TRANSFER-F"
           "PIXEL-TRANSFER-I"
           "PIXEL-ZOOM"
           "POINT-SIZE"
           "POLYGON-MODE"
           "POLYGON-OFFSET"
           "POP-ATTRIB"
           "POP-MATRIX"
           "POP-NAME"
           "PUSH-ATTRIB"
           "PUSH-MATRIX"
           "PUSH-NAME"
           "RASTER-POS-2D"
           "RASTER-POS-2F"
           "RASTER-POS-2I"
           "RASTER-POS-2S"
           "RASTER-POS-3D"
           "RASTER-POS-3F"
           "RASTER-POS-3I"
           "RASTER-POS-3S"
           "RASTER-POS-4D"
           "RASTER-POS-4F"
           "RASTER-POS-4I"
           "RASTER-POS-4S"
           "READ-BUFFER"
           "RECT-D"
           "RECT-F"
           "RECT-I"
           "RECT-S"
           "RESET-HISTOGRAM"
           "RESET-MINMAX"
           "ROTATE-D"
           "ROTATE-F"
           "SCALE-D"
           "SCALE-F"
           "SCISSOR"
           "SHADE-MODEL"
           "STENCIL-FUNC"
           "STENCIL-MASK"
           "STENCIL-OP"
           "TEX-ENV-F"
           "TEX-ENV-I"
           "TEX-GEN-D"
           "TEX-GEN-F"
           "TEX-GEN-I"
           "TEX-PARAMETER-F"
           "TEX-PARAMETER-I"
           "TRANSLATE-D"
           "TRANSLATE-F"
           "VERTEX-2D"
           "VERTEX-2F"
           "VERTEX-2I"
           "VERTEX-2S"
           "VERTEX-3D"
           "VERTEX-3F"
           "VERTEX-3I"
           "VERTEX-3S"
           "VERTEX-4D"
           "VERTEX-4F"
           "VERTEX-4I"
           "VERTEX-4S"
           "VIEWPORT"

           ;; * Where did this come from?
           ;;"NO-FLOATS"

           ;; Non-rendering commands
           "NEW-LIST"
           "END-LIST"
           "GEN-LISTS"
           "ENABLE"
           "DISABLE"
           "FLUSH"
           "FINISH"

           ;; Constants

           ;; Boolean

           "+FALSE+"
           "+TRUE+"

           ;; Types

           "+BYTE+"
           "+UNSIGNED-BYTE+"
           "+SHORT+"
           "+UNSIGNED-SHORT+"
           "+INT+"
           "+UNSIGNED-INT+"
           "+FLOAT+"
           "+DOUBLE+"
           "+2-BYTES+"
           "+3-BYTES+"
           "+4-BYTES+"

           ;; Primitives

           "+POINTS+"
           "+LINES+"
           "+LINE-LOOP+"
           "+LINE-STRIP+"
           "+TRIANGLES+"
           "+TRIANGLE-STRIP+"
           "+triangle-fan+"
           "+QUADS+"
           "+QUAD-STRIP+"
           "+POLYGON+"

           ;; Arrays

           "+VERTEX-ARRAY+"
           "+NORMAL-ARRAY+"
           "+COLOR-ARRAY+"
           "+INDEX-ARRAY+"
           "+TEXTURE-COORD-ARRAY+"
           "+EDGE-FLAG-ARRAY+"
           "+VERTEX-ARRAY-SIZE+"
           "+VERTEX-ARRAY-TYPE+"
           "+VERTEX-ARRAY-STRIDE+"
           "+NORMAL-ARRAY-TYPE+"
           "+NORMAL-ARRAY-STRIDE+"
           "+COLOR-ARRAY-SIZE+"
           "+COLOR-ARRAY-TYPE+"
           "+COLOR-ARRAY-STRIDE+"
           "+INDEX-ARRAY-TYPE+"
           "+INDEX-ARRAY-STRIDE+"
           "+TEXTURE-COORD-ARRAY-SIZE+"
           "+TEXTURE-COORD-ARRAY-TYPE+"
           "+TEXTURE-COORD-ARRAY-STRIDE+"
           "+EDGE-FLAG-ARRAY-STRIDE+"
           "+VERTEX-ARRAY-POINTER+"
           "+NORMAL-ARRAY-POINTER+"
           "+COLOR-ARRAY-POINTER+"
           "+INDEX-ARRAY-POINTER+"
           "+TEXTURE-COORD-ARRAY-POINTER+"
           "+EDGE-FLAG-ARRAY-POINTER+"

           ;; Array formats

           "+V2F+"
           "+V3F+"
           "+C4UB-V2F+"
           "+C4UB-V3F+"
           "+C3F-V3F+"
           "+N3F-V3F+"
           "+C4F-N3F-V3F+"
           "+T2F-V3F+"
           "+T4F-V4F+"
           "+T2F-C4UB-V3F+"
           "+T2F-C3F-V3F+"
           "+T2F-N3F-V3F+"
           "+T2F-C4F-N3F-V3F+"
           "+T4F-C4F-N3F-V4F+"

           ;; Matrices

           "+MATRIX-MODE+"
           "+MODELVIEW+"
           "+PROJECTION+"
           "+TEXTURE+"

           ;; Points

           "+POINT-SMOOTH+"
           "+POINT-SIZE+"
           "+POINT-SIZE-GRANULARITY+"
           "+POINT-SIZE-RANGE+"

           ;; Lines

           "+LINE-SMOOTH+"
           "+LINE-STIPPLE+"
           "+LINE-STIPPLE-PATTERN+"
           "+LINE-STIPPLE-REPEAT+"
           "+LINE-WIDTH+"
           "+LINE-WIDTH-GRANULARITY+"
           "+LINE-WIDTH-RANGE+"

           ;; Polygons

           "+POINT+"
           "+LINE+"
           "+FILL+"
           "+CW+"
           "+CCW+"
           "+FRONT+"
           "+BACK+"
           "+POLYGON-MODE+"
           "+POLYGON-SMOOTH+"
           "+POLYGON-STIPPLE+"
           "+EDGE-FLAG+"
           "+CULL-FACE+"
           "+CULL-FACE-MODE+"
           "+FRONT-FACE+"
           "+POLYGON-OFFSET-FACTOR+"
           "+POLYGON-OFFSET-UNITS+"
           "+POLYGON-OFFSET-POINT+"
           "+POLYGON-OFFSET-LINE+"
           "+POLYGON-OFFSET-FILL+"

           ;; Display Lists

           "+COMPILE+"
           "+COMPILE-AND-EXECUTE+"
           "+LIST-BASE+"
           "+LIST-INDEX+"
           "+LIST-MODE+"

           ;; Depth Buffer

           "+NEVER+"
           "+LESS+"
           "+EQUAL+"
           "+LEQUAL+"
           "+GREATER+"
           "+NOTEQUAL+"
           "+GEQUAL+"
           "+ALWAYS+"
           "+DEPTH-TEST+"
           "+DEPTH-BITS+"
           "+DEPTH-CLEAR-VALUE+"
           "+DEPTH-FUNC+"
           "+DEPTH-RANGE+"
           "+DEPTH-WRITEMASK+"
           "+DEPTH-COMPONENT+"

           ;; Lighting

           "+LIGHTING+"
           "+LIGHT0+"
           "+LIGHT1+"
           "+LIGHT2+"
           "+LIGHT3+"
           "+LIGHT4+"
           "+LIGHT5+"
           "+LIGHT6+"
           "+LIGHT7+"
           "+SPOT-EXPONENT+"
           "+SPOT-CUTOFF+"
           "+CONSTANT-ATTENUATION+"
           "+LINEAR-ATTENUATION+"
           "+QUADRATIC-ATTENUATION+"
           "+AMBIENT+"
           "+DIFFUSE+"
           "+SPECULAR+"
           "+SHININESS+"
           "+EMISSION+"
           "+POSITION+"
           "+SPOT-DIRECTION+"
           "+AMBIENT-AND-DIFFUSE+"
           "+COLOR-INDEXES+"
           "+LIGHT-MODEL-TWO-SIDE+"
           "+LIGHT-MODEL-LOCAL-VIEWER+"
           "+LIGHT-MODEL-AMBIENT+"
           "+FRONT-AND-BACK+"
           "+SHADE-MODEL+"
           "+FLAT+"
           "+SMOOTH+"
           "+COLOR-MATERIAL+"
           "+COLOR-MATERIAL-FACE+"
           "+COLOR-MATERIAL-PARAMETER+"
           "+NORMALIZE+"

           ;; Clipping planes

           "+CLIP-PLANE0+"
           "+CLIP-PLANE1+"
           "+CLIP-PLANE2+"
           "+CLIP-PLANE3+"
           "+CLIP-PLANE4+"
           "+CLIP-PLANE5+"

           ;; Accumulation buffer

           "+ACCUM-RED-BITS+"
           "+ACCUM-GREEN-BITS+"
           "+ACCUM-BLUE-BITS+"
           "+ACCUM-ALPHA-BITS+"
           "+ACCUM-CLEAR-VALUE+"
           "+ACCUM+"
           "+ADD+"
           "+LOAD+"
           "+MULT+"
           "+RETURN+"

           ;; Alpha Testing

           "+ALPHA-TEST+"
           "+ALPHA-TEST-REF+"
           "+ALPHA-TEST-FUNC+"

           ;; Blending

           "+BLEND+"
           "+BLEND-SRC+"
           "+BLEND-DST+"
           "+ZERO+"
           "+ONE+"
           "+SRC-COLOR+"
           "+ONE-MINUS-SRC-COLOR+"
           "+DST-COLOR+"
           "+ONE-MINUS-DST-COLOR+"
           "+SRC-ALPHA+"
           "+ONE-MINUS-SRC-ALPHA+"
           "+DST-ALPHA+"
           "+ONE-MINUS-DST-ALPHA+"
           "+SRC-ALPHA-SATURATE+"
           "+CONSTANT-COLOR+"
           "+ONE-MINUS-CONSTANT-COLOR+"
           "+CONSTANT-ALPHA+"
           "+ONE-MINUS-CONSTANT-ALPHA+"

           ;; Render mode

           "+FEEDBACK+"
           "+RENDER+"
           "+SELECT+"

           ;; Feedback

           "+2D+"
           "+3D+"
           "+3D-COLOR+"
           "+3D-COLOR-TEXTURE+"
           "+4D-COLOR-TEXTURE+"
           "+POINT-TOKEN+"
           "+LINE-TOKEN+"
           "+LINE-RESET-TOKEN+"
           "+POLYGON-TOKEN+"
           "+BITMAP-TOKEN+"
           "+DRAW-PIXEL-TOKEN+"
           "+COPY-PIXEL-TOKEN+"
           "+PASS-THROUGH-TOKEN+"
           "+FEEDBACK-BUFFER-POINTER+"
           "+FEEDBACK-BUFFER-SIZE+"
           "+FEEDBACK-BUFFER-TYPE+"

           ;; Selection

           "+SELECTION-BUFFER-POINTER+"
           "+SELECTION-BUFFER-SIZE+"

           ;; Fog

           "+FOG+"
           "+FOG-MODE+"
           "+FOG-DENSITY+"
           "+FOG-COLOR+"
           "+FOG-INDEX+"
           "+FOG-START+"
           "+FOG-END+"
           "+LINEAR+"
           "+EXP+"
           "+EXP2+"

           ;; Logic operations

           "+LOGIC-OP+"
           "+INDEX-LOGIC-OP+"
           "+COLOR-LOGIC-OP+"
           "+LOGIC-OP-MODE+"
           "+CLEAR+"
           "+SET+"
           "+COPY+"
           "+COPY-INVERTED+"
           "+NOOP+"
           "+INVERT+"
           "+AND+"
           "+NAND+"
           "+OR+"
           "+NOR+"
           "+XOR+"
           "+EQUIV+"
           "+AND-REVERSE+"
           "+AND-INVERTED+"
           "+OR-REVERSE+"
           "+OR-INVERTED+"

           ;; Stencil

           "+STENCIL-TEST+"
           "+STENCIL-WRITEMASK+"
           "+STENCIL-BITS+"
           "+STENCIL-FUNC+"
           "+STENCIL-VALUE-MASK+"
           "+STENCIL-REF+"
           "+STENCIL-FAIL+"
           "+STENCIL-PASS-DEPTH-PASS+"
           "+STENCIL-PASS-DEPTH-FAIL+"
           "+STENCIL-CLEAR-VALUE+"
           "+STENCIL-INDEX+"
           "+KEEP+"
           "+REPLACE+"
           "+INCR+"
           "+DECR+"

           ;; Buffers, Pixel Drawing/Reading

           "+NONE+"
           "+LEFT+"
           "+RIGHT+"
           "+FRONT-LEFT+"
           "+FRONT-RIGHT+"
           "+BACK-LEFT+"
           "+BACK-RIGHT+"
           "+AUX0+"
           "+AUX1+"
           "+AUX2+"
           "+AUX3+"
           "+COLOR-INDEX+"
           "+RED+"
           "+GREEN+"
           "+BLUE+"
           "+ALPHA+"
           "+LUMINANCE+"
           "+LUMINANCE-ALPHA+"
           "+ALPHA-BITS+"
           "+RED-BITS+"
           "+GREEN-BITS+"
           "+BLUE-BITS+"
           "+INDEX-BITS+"
           "+SUBPIXEL-BITS+"
           "+AUX-BUFFERS+"
           "+READ-BUFFER+"
           "+DRAW-BUFFER+"
           "+DOUBLEBUFFER+"
           "+STEREO+"
           "+BITMAP+"
           "+COLOR+"
           "+DEPTH+"
           "+STENCIL+"
           "+DITHER+"
           "+RGB+"
           "+RGBA+"

           ;; Implementation Limits

           "+MAX-LIST-NESTING+"
           "+MAX-ATTRIB-STACK-DEPTH+"
           "+MAX-MODELVIEW-STACK-DEPTH+"
           "+MAX-NAME-STACK-DEPTH+"
           "+MAX-PROJECTION-STACK-DEPTH+"
           "+MAX-TEXTURE-STACK-DEPTH+"
           "+MAX-EVAL-ORDER+"
           "+MAX-LIGHTS+"
           "+MAX-CLIP-PLANES+"
           "+MAX-TEXTURE-SIZE+"
           "+MAX-PIXEL-MAP-TABLE+"
           "+MAX-VIEWPORT-DIMS+"
           "+MAX-CLIENT-ATTRIB-STACK-DEPTH+"

           ;; Gets

           "+ATTRIB-STACK-DEPTH+"
           "+CLIENT-ATTRIB-STACK-DEPTH+"
           "+COLOR-CLEAR-VALUE+"
           "+COLOR-WRITEMASK+"
           "+CURRENT-INDEX+"
           "+CURRENT-COLOR+"
           "+CURRENT-NORMAL+"
           "+CURRENT-RASTER-COLOR+"
           "+CURRENT-RASTER-DISTANCE+"
           "+current-raster-index+"
           "+CURRENT-RASTER-POSITION+"
           "+CURRENT-RASTER-TEXTURE-COORDS+"
           "+CURRENT-RASTER-POSITION-VALID+"
           "+CURRENT-TEXTURE-COORDS+"
           "+INDEX-CLEAR-VALUE+"
           "+INDEX-MODE+"
           "+INDEX-WRITEMASK+"
           "+MODELVIEW-MATRIX+"
           "+MODELVIEW-STACK-DEPTH+"
           "+NAME-STACK-DEPTH+"
           "+PROJECTION-MATRIX+"
           "+PROJECTION-STACK-DEPTH+"
           "+RENDER-MODE+"
           "+RGBA-MODE+"
           "+TEXTURE-MATRIX+"
           "+TEXTURE-STACK-DEPTH+"
           "+VIEWPORT+"

           ;; GL Evaluators

           "+AUTO-NORMAL+"
           "+MAP1-COLOR-4+"
           "+MAP1-GRID-DOMAIN+"
           "+MAP1-GRID-SEGMENTS+"
           "+MAP1-INDEX+"
           "+MAP1-NORMAL+"
           "+MAP1-TEXTURE-COORD-1+"
           "+MAP1-TEXTURE-COORD-2+"
           "+MAP1-TEXTURE-COORD-3+"
           "+MAP1-TEXTURE-COORD-4+"
           "+MAP1-VERTEX-3+"
           "+MAP1-VERTEX-4+"
           "+MAP2-COLOR-4+"
           "+MAP2-GRID-DOMAIN+"
           "+MAP2-GRID-SEGMENTS+"
           "+MAP2-INDEX+"
           "+MAP2-NORMAL+"
           "+MAP2-TEXTURE-COORD-1+"
           "+MAP2-TEXTURE-COORD-2+"
           "+MAP2-TEXTURE-COORD-3+"
           "+MAP2-TEXTURE-COORD-4+"
           "+MAP2-VERTEX-3+"
           "+MAP2-VERTEX-4+"
           "+COEFF+"
           "+DOMAIN+"
           "+ORDER+"

           ;; Hints

           "+FOG-HINT+"
           "+LINE-SMOOTH-HINT+"
           "+PERSPECTIVE-CORRECTION-HINT+"
           "+POINT-SMOOTH-HINT+"
           "+POLYGON-SMOOTH-HINT+"
           "+DONT-CARE+"
           "+FASTEST+"
           "+NICEST+"

           ;; Scissor box

           "+SCISSOR-TEST+"
           "+SCISSOR-BOX+"

           ;; Pixel Mode / Transfer

           "+MAP-COLOR+"
           "+MAP-STENCIL+"
           "+INDEX-SHIFT+"
           "+INDEX-OFFSET+"
           "+RED-SCALE+"
           "+RED-BIAS+"
           "+GREEN-SCALE+"
           "+GREEN-BIAS+"
           "+BLUE-SCALE+"
           "+BLUE-BIAS+"
           "+ALPHA-SCALE+"
           "+ALPHA-BIAS+"
           "+DEPTH-SCALE+"
           "+DEPTH-BIAS+"
           "+PIXEL-MAP-S-TO-S-SIZE+"
           "+PIXEL-MAP-I-TO-I-SIZE+"
           "+PIXEL-MAP-I-TO-R-SIZE+"
           "+PIXEL-MAP-I-TO-G-SIZE+"
           "+PIXEL-MAP-I-TO-B-SIZE+"
           "+PIXEL-MAP-I-TO-A-SIZE+"
           "+PIXEL-MAP-R-TO-R-SIZE+"
           "+PIXEL-MAP-G-TO-G-SIZE+"
           "+PIXEL-MAP-B-TO-B-SIZE+"
           "+PIXEL-MAP-A-TO-A-SIZE+"
           "+PIXEL-MAP-S-TO-S+"
           "+PIXEL-MAP-I-TO-I+"
           "+PIXEL-MAP-I-TO-R+"
           "+PIXEL-MAP-I-TO-G+"
           "+PIXEL-MAP-I-TO-B+"
           "+PIXEL-MAP-I-TO-A+"
           "+PIXEL-MAP-R-TO-R+"
           "+PIXEL-MAP-G-TO-G+"
           "+PIXEL-MAP-B-TO-B+"
           "+PIXEL-MAP-A-TO-A+"
           "+PACK-ALIGNMENT+"
           "+PACK-LSB-FIRST+"
           "+PACK-ROW-LENGTH+"
           "+PACK-SKIP-PIXELS+"
           "+PACK-SKIP-ROWS+"
           "+PACK-SWAP-BYTES+"
           "+UNPACK-ALIGNMENT+"
           "+UNPACK-LSB-FIRST+"
           "+UNPACK-ROW-LENGTH+"
           "+UNPACK-SKIP-PIXELS+"
           "+UNPACK-SKIP-ROWS+"
           "+UNPACK-SWAP-BYTES+"
           "+ZOOM-X+"
           "+ZOOM-Y+"

           ;; Texture Mapping

           "+TEXTURE-ENV+"
           "+TEXTURE-ENV-MODE+"
           "+TEXTURE-1D+"
           "+TEXTURE-2D+"
           "+TEXTURE-WRAP-S+"
           "+TEXTURE-WRAP-T+"
           "+TEXTURE-MAG-FILTER+"
           "+TEXTURE-MIN-FILTER+"
           "+TEXTURE-ENV-COLOR+"
           "+TEXTURE-GEN-S+"
           "+TEXTURE-GEN-T+"
           "+TEXTURE-GEN-MODE+"
           "+TEXTURE-BORDER-COLOR+"
           "+TEXTURE-WIDTH+"
           "+TEXTURE-HEIGHT+"
           "+TEXTURE-BORDER+"
           "+TEXTURE-COMPONENTS+"
           "+TEXTURE-RED-SIZE+"
           "+TEXTURE-GREEN-SIZE+"
           "+TEXTURE-BLUE-SIZE+"
           "+TEXTURE-ALPHA-SIZE+"
           "+TEXTURE-LUMINANCE-SIZE+"
           "+TEXTURE-INTENSITY-SIZE+"
           "+NEAREST-MIPMAP-NEAREST+"
           "+NEAREST-MIPMAP-LINEAR+"
           "+LINEAR-MIPMAP-NEAREST+"
           "+LINEAR-MIPMAP-LINEAR+"
           "+OBJECT-LINEAR+"
           "+OBJECT-PLANE+"
           "+EYE-LINEAR+"
           "+EYE-PLANE+"
           "+SPHERE-MAP+"
           "+DECAL+"
           "+MODULATE+"
           "+NEAREST+"
           "+REPEAT+"
           "+CLAMP+"
           "+S+"
           "+T+"
           "+R+"
           "+Q+"
           "+TEXTURE-GEN-R+"
           "+TEXTURE-GEN-Q+"

           ;; GL 1.1 Texturing

           "+PROXY-TEXTURE-1D+"
           "+PROXY-TEXTURE-2D+"
           "+TEXTURE-PRIORITY+"
           "+TEXTURE-RESIDENT+"
           "+TEXTURE-BINDING-1D+"
           "+TEXTURE-BINDING-2D+"
           "+TEXTURE-INTERNAL-FORMAT+"
           "+PACK-SKIP-IMAGES+"
           "+PACK-IMAGE-HEIGHT+"
           "+UNPACK-SKIP-IMAGES+"
           "+UNPACK-IMAGE-HEIGHT+"
           "+TEXTURE-3D+"
           "+PROXY-TEXTURE-3D+"
           "+TEXTURE-DEPTH+"
           "+TEXTURE-WRAP-R+"
           "+MAX-3D-TEXTURE-SIZE+"
           "+TEXTURE-BINDING-3D+"

           ;; Internal texture formats (GL 1.1)
           "+ALPHA4+"
           "+ALPHA8+"
           "+ALPHA12+"
           "+ALPHA16+"
           "+LUMINANCE4+"
           "+LUMINANCE8+"
           "+LUMINANCE12+"
           "+LUMINANCE16+"
           "+LUMINANCE4-ALPHA4+"
           "+LUMINANCE6-ALPHA2+"
           "+LUMINANCE8-ALPHA8+"
           "+LUMINANCE12-ALPHA4+"
           "+LUMINANCE12-ALPHA12+"
           "+LUMINANCE16-ALPHA16+"
           "+INTENSITY+"
           "+INTENSITY4+"
           "+INTENSITY8+"
           "+INTENSITY12+"
           "+INTENSITY16+"
           "+R3-G3-B2+"
           "+RGB4+"
           "+RGB5+"
           "+RGB8+"
           "+RGB10+"
           "+RGB12+"
           "+RGB16+"
           "+RGBA2+"
           "+RGBA4+"
           "+RGB5-A1+"
           "+RGBA8+"
           "+rgb10-a2+"
           "+RGBA12+"
           "+RGBA16+"

           ;; Utility

           "+VENDOR+"
           "+RENDERER+"
           "+VERSION+"
           "+EXTENSIONS+"

           ;; Errors

           "+NO-ERROR+"
           "+INVALID-VALUE+"
           "+INVALID-ENUM+"
           "+INVALID-OPERATION+"
           "+STACK-OVERFLOW+"
           "+STACK-UNDERFLOW+"
           "+OUT-OF-MEMORY+"

           ;; OpenGL 1.2

           "+RESCALE-NORMAL+"
           "+CLAMP-TO-EDGE+"
           "+MAX-ELEMENTS-VERTICES+"
           "+MAX-ELEMENTS-INDICES+"
           "+BGR+"
           "+BGRA+"
           "+UNSIGNED-BYTE-3-3-2+"
           "+UNSIGNED-BYTE-2-3-3-REV+"
           "+UNSIGNED-SHORT-5-6-5+"
           "+UNSIGNED-SHORT-5-6-5-REV+"
           "+UNSIGNED-SHORT-4-4-4-4+"
           "+UNSIGNED-SHORT-4-4-4-4-REV+"
           "+UNSIGNED-SHORT-5-5-5-1+"
           "+UNSIGNED-SHORT-1-5-5-5-REV+"
           "+UNSIGNED-INT-8-8-8-8+"
           "+UNSIGNED-INT-8-8-8-8-REV+"
           "+UNSIGNED-INT-10-10-10-2+"
           "+UNSIGNED-INT-2-10-10-10-REV+"
           "+LIGHT-MODEL-COLOR-CONTROL+"
           "+SINGLE-COLOR+"
           "+SEPARATE-SPECULAR-COLOR+"
           "+TEXTURE-MIN-LOD+"
           "+TEXTURE-MAX-LOD+"
           "+TEXTURE-BASE-LEVEL+"
           "+TEXTURE-MAX-LEVEL+"
           "+SMOOTH-POINT-SIZE-RANGE+"
           "+SMOOTH-POINT-SIZE-GRANULARITY+"
           "+SMOOTH-LINE-WIDTH-RANGE+"
           "+SMOOTH-LINE-WIDTH-GRANULARITY+"
           "+ALIASED-POINT-SIZE-RANGE+"
           "+ALIASED-LINE-WIDTH-RANGE+"

           ;; OpenGL 1.2 Imaging subset
           ;; GL_EXT_color_table
           "+COLOR-TABLE+"
           "+POST-CONVOLUTION-COLOR-TABLE+"
           "+POST-COLOR-MATRIX-COLOR-TABLE+"
           "+PROXY-COLOR-TABLE+"
           "+PROXY-POST-CONVOLUTION-COLOR-TABLE+"
           "+PROXY-POST-COLOR-MATRIX-COLOR-TABLE+"
           "+COLOR-TABLE-SCALE+"
           "+COLOR-TABLE-BIAS+"
           "+COLOR-TABLE-FORMAT+"
           "+COLOR-TABLE-WIDTH+"
           "+COLOR-TABLE-RED-SIZE+"
           "+COLOR-TABLE-GREEN-SIZE+"
           "+COLOR-TABLE-BLUE-SIZE+"
           "+COLOR-TABLE-ALPHA-SIZE+"
           "+COLOR-TABLE-LUMINANCE-SIZE+"
           "+COLOR-TABLE-INTENSITY-SIZE+"
           ;; GL_EXT_convolution and GL_HP_convolution
           "+CONVOLUTION-1D+"
           "+CONVOLUTION-2D+"
           "+SEPARABLE-2D+"
           "+CONVOLUTION-BORDER-MODE+"
           "+CONVOLUTION-FILTER-SCALE+"
           "+CONVOLUTION-FILTER-BIAS+"
           "+REDUCE+"
           "+CONVOLUTION-FORMAT+"
           "+CONVOLUTION-WIDTH+"
           "+CONVOLUTION-HEIGHT+"
           "+MAX-CONVOLUTION-WIDTH+"
           "+MAX-CONVOLUTION-HEIGHT+"
           "+POST-CONVOLUTION-RED-SCALE+"
           "+POST-CONVOLUTION-GREEN-SCALE+"
           "+POST-CONVOLUTION-BLUE-SCALE+"
           "+POST-CONVOLUTION-ALPHA-SCALE+"
           "+POST-CONVOLUTION-RED-BIAS+"
           "+POST-CONVOLUTION-GREEN-BIAS+"
           "+POST-CONVOLUTION-BLUE-BIAS+"
           "+POST-CONVOLUTION-ALPHA-BIAS+"
           "+CONSTANT-BORDER+"
           "+REPLICATE-BORDER+"
           "+CONVOLUTION-BORDER-COLOR+"
           ;; GL_SGI_color_matrix
           "+COLOR-MATRIX+"
           "+COLOR-MATRIX-STACK-DEPTH+"
           "+MAX-COLOR-MATRIX-STACK-DEPTH+"
           "+POST-COLOR-MATRIX-RED-SCALE+"
           "+POST-COLOR-MATRIX-GREEN-SCALE+"
           "+POST-COLOR-MATRIX-BLUE-SCALE+"
           "+POST-COLOR-MATRIX-ALPHA-SCALE+"
           "+POST-COLOR-MATRIX-RED-BIAS+"
           "+POST-COLOR-MATRIX-GREEN-BIAS+"
           "+POST-COLOR-MATRIX-BLUE-BIAS+"
           "+POST-COLOR-MATRIX-ALPHA-BIAS+"
           ;; GL_EXT_histogram
           "+HISTOGRAM+"
           "+PROXY-HISTOGRAM+"
           "+HISTOGRAM-WIDTH+"
           "+HISTOGRAM-FORMAT+"
           "+HISTOGRAM-RED-SIZE+"
           "+HISTOGRAM-GREEN-SIZE+"
           "+HISTOGRAM-BLUE-SIZE+"
           "+HISTOGRAM-ALPHA-SIZE+"
           "+HISTOGRAM-LUMINANCE-SIZE+"
           "+HISTOGRAM-SINK+"
           "+MINMAX+"
           "+MINMAX-FORMAT+"
           "+MINMAX-SINK+"
           "+TABLE-TOO-LARGE+"
           ;; GL_EXT_blend_color, GL_EXT_blend_minmax
           "+BLEND-EQUATION+"
           "+MIN+"
           "+MAX+"
           "+FUNC-ADD+"
           "+FUNC-SUBTRACT+"
           "+FUNC-REVERSE-SUBTRACT+"

           ;; glPush/PopAttrib bits

           "+CURRENT-BIT+"
           "+POINT-BIT+"
           "+LINE-BIT+"
           "+POLYGON-BIT+"
           "+POLYGON-STIPPLE-BIT+"
           "+PIXEL-MODE-BIT+"
           "+LIGHTING-BIT+"
           "+FOG-BIT+"
           "+DEPTH-BUFFER-BIT+"
           "+ACCUM-BUFFER-BIT+"
           "+STENCIL-BUFFER-BIT+"
           "+VIEWPORT-BIT+"
           "+TRANSFORM-BIT+"
           "+ENABLE-BIT+"
           "+COLOR-BUFFER-BIT+"
           "+HINT-BIT+"
           "+EVAL-BIT+"
           "+LIST-BIT+"
           "+TEXTURE-BIT+"
           "+SCISSOR-BIT+"
           "+ALL-ATTRIB-BITS+"
           "+CLIENT-PIXEL-STORE-BIT+"
           "+CLIENT-VERTEX-ARRAY-BIT+"
           "+CLIENT-ALL-ATTRIB-BITS+"

           ;; ARB Multitexturing extension

           "+ARB-MULTITEXTURE+"
           "+TEXTURE0-ARB+"
           "+TEXTURE1-ARB+"
           "+TEXTURE2-ARB+"
           "+TEXTURE3-ARB+"
           "+TEXTURE4-ARB+"
           "+TEXTURE5-ARB+"
           "+TEXTURE6-ARB+"
           "+TEXTURE7-ARB+"
           "+TEXTURE8-ARB+"
           "+TEXTURE9-ARB+"
           "+TEXTURE10-ARB+"
           "+TEXTURE11-ARB+"
           "+TEXTURE12-ARB+"
           "+TEXTURE13-ARB+"
           "+TEXTURE14-ARB+"
           "+TEXTURE15-ARB+"
           "+TEXTURE16-ARB+"
           "+TEXTURE17-ARB+"
           "+TEXTURE18-ARB+"
           "+TEXTURE19-ARB+"
           "+TEXTURE20-ARB+"
           "+TEXTURE21-ARB+"
           "+TEXTURE22-ARB+"
           "+TEXTURE23-ARB+"
           "+TEXTURE24-ARB+"
           "+TEXTURE25-ARB+"
           "+TEXTURE26-ARB+"
           "+TEXTURE27-ARB+"
           "+TEXTURE28-ARB+"
           "+TEXTURE29-ARB+"
           "+TEXTURE30-ARB+"
           "+TEXTURE31-ARB+"
           "+ACTIVE-TEXTURE-ARB+"
           "+CLIENT-ACTIVE-TEXTURE-ARB+"
           "+MAX-TEXTURE-UNITS-ARB+"

;;; Misc extensions

           "+EXT-ABGR+"
           "+ABGR-EXT+"
           "+EXT-BLEND-COLOR+"
           "+CONSTANT-COLOR-EXT+"
           "+ONE-MINUS-CONSTANT-COLOR-EXT+"
           "+CONSTANT-ALPHA-EXT+"
           "+ONE-MINUS-CONSTANT-ALPHA-EXT+"
           "+blend-color-ext+"
           "+EXT-POLYGON-OFFSET+"
           "+POLYGON-OFFSET-EXT+"
           "+POLYGON-OFFSET-FACTOR-EXT+"
           "+POLYGON-OFFSET-BIAS-EXT+"
           "+EXT-TEXTURE3D+"
           "+PACK-SKIP-IMAGES-EXT+"
           "+PACK-IMAGE-HEIGHT-EXT+"
           "+UNPACK-SKIP-IMAGES-EXT+"
           "+UNPACK-IMAGE-HEIGHT-EXT+"
           "+TEXTURE-3D-EXT+"
           "+PROXY-TEXTURE-3D-EXT+"
           "+TEXTURE-DEPTH-EXT+"
           "+TEXTURE-WRAP-R-EXT+"
           "+MAX-3D-TEXTURE-SIZE-EXT+"
           "+TEXTURE-3D-BINDING-EXT+"
           "+EXT-TEXTURE-OBJECT+"
           "+TEXTURE-PRIORITY-EXT+"
           "+TEXTURE-RESIDENT-EXT+"
           "+TEXTURE-1D-BINDING-EXT+"
           "+TEXTURE-2D-BINDING-EXT+"
           "+EXT-RESCALE-NORMAL+"
           "+RESCALE-NORMAL-EXT+"
           "+EXT-VERTEX-ARRAY+"
           "+VERTEX-ARRAY-EXT+"
           "+NORMAL-ARRAY-EXT+"
           "+COLOR-ARRAY-EXT+"
           "+INDEX-ARRAY-EXT+"
           "+TEXTURE-COORD-ARRAY-EXT+"
           "+EDGE-FLAG-ARRAY-EXT+"
           "+VERTEX-ARRAY-SIZE-EXT+"
           "+VERTEX-ARRAY-TYPE-EXT+"
           "+VERTEX-ARRAY-STRIDE-EXT+"
           "+VERTEX-ARRAY-COUNT-EXT+"
           "+NORMAL-ARRAY-TYPE-EXT+"
           "+NORMAL-ARRAY-STRIDE-EXT+"
           "+NORMAL-ARRAY-COUNT-EXT+"
           "+COLOR-ARRAY-SIZE-EXT+"
           "+COLOR-ARRAY-TYPE-EXT+"
           "+COLOR-ARRAY-STRIDE-EXT+"
           "+COLOR-ARRAY-COUNT-EXT+"
           "+INDEX-ARRAY-TYPE-EXT+"
           "+INDEX-ARRAY-STRIDE-EXT+"
           "+INDEX-ARRAY-COUNT-EXT+"
           "+TEXTURE-COORD-ARRAY-SIZE-EXT+"
           "+TEXTURE-COORD-ARRAY-TYPE-EXT+"
           "+TEXTURE-COORD-ARRAY-STRIDE-EXT+"
           "+TEXTURE-COORD-ARRAY-COUNT-EXT+"
           "+EDGE-FLAG-ARRAY-STRIDE-EXT+"
           "+EDGE-FLAG-ARRAY-COUNT-EXT+"
           "+VERTEX-ARRAY-POINTER-EXT+"
           "+NORMAL-ARRAY-POINTER-EXT+"
           "+COLOR-ARRAY-POINTER-EXT+"
           "+INDEX-ARRAY-POINTER-EXT+"
           "+TEXTURE-COORD-ARRAY-POINTER-EXT+"
           "+EDGE-FLAG-ARRAY-POINTER-EXT+"
           "+SGIS-TEXTURE-EDGE-CLAMP+"
           "+CLAMP-TO-EDGE-SGIS+"
           "+EXT-BLEND-MINMAX+"
           "+FUNC-ADD-EXT+"
           "+MIN-EXT+"
           "+MAX-EXT+"
           "+BLEND-EQUATION-EXT+"
           "+EXT-BLEND-SUBTRACT+"
           "+FUNC-SUBTRACT-EXT+"
           "+FUNC-REVERSE-SUBTRACT-EXT+"
           "+EXT-BLEND-LOGIC-OP+"
           "+EXT-POINT-PARAMETERS+"
           "+POINT-SIZE-MIN-EXT+"
           "+POINT-SIZE-MAX-EXT+"
           "+POINT-FADE-THRESHOLD-SIZE-EXT+"
           "+DISTANCE-ATTENUATION-EXT+"
           "+EXT-PALETTED-TEXTURE+"
           "+TABLE-TOO-LARGE-EXT+"
           "+COLOR-TABLE-FORMAT-EXT+"
           "+COLOR-TABLE-WIDTH-EXT+"
           "+COLOR-TABLE-RED-SIZE-EXT+"
           "+COLOR-TABLE-GREEN-SIZE-EXT+"
           "+COLOR-TABLE-BLUE-SIZE-EXT+"
           "+COLOR-TABLE-ALPHA-SIZE-EXT+"
           "+COLOR-TABLE-LUMINANCE-SIZE-EXT+"
           "+COLOR-TABLE-INTENSITY-SIZE-EXT+"
           "+TEXTURE-INDEX-SIZE-EXT+"
           "+COLOR-INDEX1-EXT+"
           "+COLOR-INDEX2-EXT+"
           "+COLOR-INDEX4-EXT+"
           "+COLOR-INDEX8-EXT+"
           "+COLOR-INDEX12-EXT+"
           "+COLOR-INDEX16-EXT+"
           "+EXT-CLIP-VOLUME-HINT+"
           "+CLIP-VOLUME-CLIPPING-HINT-EXT+"
           "+EXT-COMPILED-VERTEX-ARRAY+"
           "+ARRAY-ELEMENT-LOCK-FIRST-EXT+"
           "+ARRAY-ELEMENT-LOCK-COUNT-EXT+"
           "+HP-OCCLUSION-TEST+"
           "+OCCLUSION-TEST-HP+"
           "+OCCLUSION-TEST-RESULT-HP+"
           "+EXT-SHARED-TEXTURE-PALETTE+"
           "+SHARED-TEXTURE-PALETTE-EXT+"
           "+EXT-STENCIL-WRAP+"
           "+INCR-WRAP-EXT+"
           "+DECR-WRAP-EXT+"
           "+NV-TEXGEN-REFLECTION+"
           "+NORMAL-MAP-NV+"
           "+REFLECTION-MAP-NV+"
           "+EXT-TEXTURE-ENV-ADD+"
           "+MESA-WINDOW-POS+"
           "+MESA-RESIZE-BUFFERS+"
           
           ))


(in-package :gl)



;;; Opcodes.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +get-string+       129)
(defconstant +new-list+         101)
(defconstant +end-list+         102)
(defconstant +gen-lists+        104)
(defconstant +finish+           108)
(defconstant +disable+          138)
(defconstant +enable+           139)
(defconstant +flush+            142)



;;; Constants.
;;; Shamelessly taken from CL-SDL.

;; Boolean

(defconstant +false+                            #x0) 
(defconstant +true+                             #x1) 

;; Types

(defconstant +byte+                          #x1400) 
(defconstant +unsigned-byte+                 #x1401) 
(defconstant +short+                         #x1402) 
(defconstant +unsigned-short+                #x1403) 
(defconstant +int+                           #x1404) 
(defconstant +unsigned-int+                  #x1405) 
(defconstant +float+                         #x1406) 
(defconstant +double+                        #x140a) 
(defconstant +2-bytes+                       #x1407) 
(defconstant +3-bytes+                       #x1408) 
(defconstant +4-bytes+                       #x1409) 

;; Primitives

(defconstant +points+                        #x0000) 
(defconstant +lines+                         #x0001) 
(defconstant +line-loop+                     #x0002) 
(defconstant +line-strip+                    #x0003) 
(defconstant +triangles+                     #x0004) 
(defconstant +triangle-strip+                #x0005) 
(defconstant +triangle-fan+                  #x0006) 
(defconstant +quads+                         #x0007) 
(defconstant +quad-strip+                    #x0008) 
(defconstant +polygon+                       #x0009) 

;; Arrays

(defconstant +vertex-array+                  #x8074) 
(defconstant +normal-array+                  #x8075) 
(defconstant +color-array+                   #x8076) 
(defconstant +index-array+                   #x8077) 
(defconstant +texture-coord-array+           #x8078) 
(defconstant +edge-flag-array+               #x8079) 
(defconstant +vertex-array-size+             #x807a) 
(defconstant +vertex-array-type+             #x807b) 
(defconstant +vertex-array-stride+           #x807c) 
(defconstant +normal-array-type+             #x807e) 
(defconstant +normal-array-stride+           #x807f) 
(defconstant +color-array-size+              #x8081) 
(defconstant +color-array-type+              #x8082) 
(defconstant +color-array-stride+            #x8083) 
(defconstant +index-array-type+              #x8085) 
(defconstant +index-array-stride+            #x8086) 
(defconstant +texture-coord-array-size+      #x8088) 
(defconstant +texture-coord-array-type+      #x8089) 
(defconstant +texture-coord-array-stride+    #x808a) 
(defconstant +edge-flag-array-stride+        #x808c) 
(defconstant +vertex-array-pointer+          #x808e) 
(defconstant +normal-array-pointer+          #x808f) 
(defconstant +color-array-pointer+           #x8090) 
(defconstant +index-array-pointer+           #x8091) 
(defconstant +texture-coord-array-pointer+   #x8092) 
(defconstant +edge-flag-array-pointer+       #x8093) 

;; Array formats

(defconstant +v2f+                           #x2a20) 
(defconstant +v3f+                           #x2a21) 
(defconstant +c4ub-v2f+                      #x2a22) 
(defconstant +c4ub-v3f+                      #x2a23) 
(defconstant +c3f-v3f+                       #x2a24) 
(defconstant +n3f-v3f+                       #x2a25) 
(defconstant +c4f-n3f-v3f+                   #x2a26) 
(defconstant +t2f-v3f+                       #x2a27) 
(defconstant +t4f-v4f+                       #x2a28) 
(defconstant +t2f-c4ub-v3f+                  #x2a29) 
(defconstant +t2f-c3f-v3f+                   #x2a2a) 
(defconstant +t2f-n3f-v3f+                   #x2a2b) 
(defconstant +t2f-c4f-n3f-v3f+               #x2a2c) 
(defconstant +t4f-c4f-n3f-v4f+               #x2a2d) 

;; Matrices

(defconstant +matrix-mode+                   #x0ba0) 
(defconstant +modelview+                     #x1700) 
(defconstant +projection+                    #x1701) 
(defconstant +texture+                       #x1702) 

;; Points

(defconstant +point-smooth+                  #x0b10) 
(defconstant +point-size+                    #x0b11) 
(defconstant +point-size-granularity+        #x0b13) 
(defconstant +point-size-range+              #x0b12) 

;; Lines

(defconstant +line-smooth+                   #x0b20) 
(defconstant +line-stipple+                  #x0b24) 
(defconstant +line-stipple-pattern+          #x0b25) 
(defconstant +line-stipple-repeat+           #x0b26) 
(defconstant +line-width+                    #x0b21) 
(defconstant +line-width-granularity+        #x0b23) 
(defconstant +line-width-range+              #x0b22) 

;; Polygons

(defconstant +point+                         #x1b00) 
(defconstant +line+                          #x1b01) 
(defconstant +fill+                          #x1b02) 
(defconstant +cw+                            #x0900) 
(defconstant +ccw+                           #x0901) 
(defconstant +front+                         #x0404) 
(defconstant +back+                          #x0405) 
(defconstant +polygon-mode+                  #x0b40) 
(defconstant +polygon-smooth+                #x0b41) 
(defconstant +polygon-stipple+               #x0b42) 
(defconstant +edge-flag+                     #x0b43) 
(defconstant +cull-face+                     #x0b44) 
(defconstant +cull-face-mode+                #x0b45) 
(defconstant +front-face+                    #x0b46) 
(defconstant +polygon-offset-factor+         #x8038) 
(defconstant +polygon-offset-units+          #x2a00) 
(defconstant +polygon-offset-point+          #x2a01) 
(defconstant +polygon-offset-line+           #x2a02) 
(defconstant +polygon-offset-fill+           #x8037) 

;; Display Lists

(defconstant +compile+                       #x1300) 
(defconstant +compile-and-execute+           #x1301) 
(defconstant +list-base+                     #x0b32) 
(defconstant +list-index+                    #x0b33) 
(defconstant +list-mode+                     #x0b30) 

;; Depth Buffer

(defconstant +never+                         #x0200) 
(defconstant +less+                          #x0201) 
(defconstant +equal+                         #x0202) 
(defconstant +lequal+                        #x0203) 
(defconstant +greater+                       #x0204) 
(defconstant +notequal+                      #x0205) 
(defconstant +gequal+                        #x0206) 
(defconstant +always+                        #x0207) 
(defconstant +depth-test+                    #x0b71) 
(defconstant +depth-bits+                    #x0d56) 
(defconstant +depth-clear-value+             #x0b73) 
(defconstant +depth-func+                    #x0b74) 
(defconstant +depth-range+                   #x0b70) 
(defconstant +depth-writemask+               #x0b72) 
(defconstant +depth-component+               #x1902) 

;; Lighting

(defconstant +lighting+                      #x0b50) 
(defconstant +light0+                        #x4000) 
(defconstant +light1+                        #x4001) 
(defconstant +light2+                        #x4002) 
(defconstant +light3+                        #x4003) 
(defconstant +light4+                        #x4004) 
(defconstant +light5+                        #x4005) 
(defconstant +light6+                        #x4006) 
(defconstant +light7+                        #x4007) 
(defconstant +spot-exponent+                 #x1205) 
(defconstant +spot-cutoff+                   #x1206) 
(defconstant +constant-attenuation+          #x1207) 
(defconstant +linear-attenuation+            #x1208) 
(defconstant +quadratic-attenuation+         #x1209) 
(defconstant +ambient+                       #x1200) 
(defconstant +diffuse+                       #x1201) 
(defconstant +specular+                      #x1202) 
(defconstant +shininess+                     #x1601) 
(defconstant +emission+                      #x1600) 
(defconstant +position+                      #x1203) 
(defconstant +spot-direction+                #x1204) 
(defconstant +ambient-and-diffuse+           #x1602) 
(defconstant +color-indexes+                 #x1603) 
(defconstant +light-model-two-side+          #x0b52) 
(defconstant +light-model-local-viewer+      #x0b51) 
(defconstant +light-model-ambient+           #x0b53) 
(defconstant +front-and-back+                #x0408) 
(defconstant +shade-model+                   #x0b54) 
(defconstant +flat+                          #x1d00) 
(defconstant +smooth+                        #x1d01) 
(defconstant +color-material+                #x0b57) 
(defconstant +color-material-face+           #x0b55) 
(defconstant +color-material-parameter+      #x0b56) 
(defconstant +normalize+                     #x0ba1) 

;; Clipping planes

(defconstant +clip-plane0+                   #x3000) 
(defconstant +clip-plane1+                   #x3001) 
(defconstant +clip-plane2+                   #x3002) 
(defconstant +clip-plane3+                   #x3003) 
(defconstant +clip-plane4+                   #x3004) 
(defconstant +clip-plane5+                   #x3005) 

;; Accumulation buffer

(defconstant +accum-red-bits+                #x0d58) 
(defconstant +accum-green-bits+              #x0d59) 
(defconstant +accum-blue-bits+               #x0d5a) 
(defconstant +accum-alpha-bits+              #x0d5b) 
(defconstant +accum-clear-value+             #x0b80) 
(defconstant +accum+                         #x0100) 
(defconstant +add+                           #x0104) 
(defconstant +load+                          #x0101) 
(defconstant +mult+                          #x0103) 
(defconstant +return+                        #x0102) 

;; Alpha Testing

(defconstant +alpha-test+                    #x0bc0) 
(defconstant +alpha-test-ref+                #x0bc2) 
(defconstant +alpha-test-func+               #x0bc1) 

;; Blending

(defconstant +blend+                         #x0be2) 
(defconstant +blend-src+                     #x0be1) 
(defconstant +blend-dst+                     #x0be0) 
(defconstant +zero+                             #x0) 
(defconstant +one+                              #x1) 
(defconstant +src-color+                     #x0300) 
(defconstant +one-minus-src-color+           #x0301) 
(defconstant +dst-color+                     #x0306) 
(defconstant +one-minus-dst-color+           #x0307) 
(defconstant +src-alpha+                     #x0302) 
(defconstant +one-minus-src-alpha+           #x0303) 
(defconstant +dst-alpha+                     #x0304) 
(defconstant +one-minus-dst-alpha+           #x0305) 
(defconstant +src-alpha-saturate+            #x0308) 
(defconstant +constant-color+                #x8001) 
(defconstant +one-minus-constant-color+      #x8002) 
(defconstant +constant-alpha+                #x8003) 
(defconstant +one-minus-constant-alpha+      #x8004) 

;; Render mode

(defconstant +feedback+                      #x1c01) 
(defconstant +render+                        #x1c00) 
(defconstant +select+                        #x1c02) 

;; Feedback

(defconstant +2d+                            #x0600) 
(defconstant +3d+                            #x0601) 
(defconstant +3d-color+                      #x0602) 
(defconstant +3d-color-texture+              #x0603) 
(defconstant +4d-color-texture+              #x0604) 
(defconstant +point-token+                   #x0701) 
(defconstant +line-token+                    #x0702) 
(defconstant +line-reset-token+              #x0707) 
(defconstant +polygon-token+                 #x0703) 
(defconstant +bitmap-token+                  #x0704) 
(defconstant +draw-pixel-token+              #x0705) 
(defconstant +copy-pixel-token+              #x0706) 
(defconstant +pass-through-token+            #x0700) 
(defconstant +feedback-buffer-pointer+       #x0df0) 
(defconstant +feedback-buffer-size+          #x0df1) 
(defconstant +feedback-buffer-type+          #x0df2) 

;; Selection

(defconstant +selection-buffer-pointer+      #x0df3) 
(defconstant +selection-buffer-size+         #x0df4) 

;; Fog

(defconstant +fog+                           #x0b60) 
(defconstant +fog-mode+                      #x0b65) 
(defconstant +fog-density+                   #x0b62) 
(defconstant +fog-color+                     #x0b66) 
(defconstant +fog-index+                     #x0b61) 
(defconstant +fog-start+                     #x0b63) 
(defconstant +fog-end+                       #x0b64) 
(defconstant +linear+                        #x2601) 
(defconstant +exp+                           #x0800) 
(defconstant +exp2+                          #x0801) 

;; Logic operations

(defconstant +logic-op+                      #x0bf1) 
(defconstant +index-logic-op+                #x0bf1) 
(defconstant +color-logic-op+                #x0bf2) 
(defconstant +logic-op-mode+                 #x0bf0) 
(defconstant +clear+                         #x1500) 
(defconstant +set+                           #x150f) 
(defconstant +copy+                          #x1503) 
(defconstant +copy-inverted+                 #x150c) 
(defconstant +noop+                          #x1505) 
(defconstant +invert+                        #x150a) 
(defconstant +and+                           #x1501) 
(defconstant +nand+                          #x150e) 
(defconstant +or+                            #x1507) 
(defconstant +nor+                           #x1508) 
(defconstant +xor+                           #x1506) 
(defconstant +equiv+                         #x1509) 
(defconstant +and-reverse+                   #x1502) 
(defconstant +and-inverted+                  #x1504) 
(defconstant +or-reverse+                    #x150b) 
(defconstant +or-inverted+                   #x150d) 

;; Stencil

(defconstant +stencil-test+                  #x0b90) 
(defconstant +stencil-writemask+             #x0b98) 
(defconstant +stencil-bits+                  #x0d57) 
(defconstant +stencil-func+                  #x0b92) 
(defconstant +stencil-value-mask+            #x0b93) 
(defconstant +stencil-ref+                   #x0b97) 
(defconstant +stencil-fail+                  #x0b94) 
(defconstant +stencil-pass-depth-pass+       #x0b96) 
(defconstant +stencil-pass-depth-fail+       #x0b95) 
(defconstant +stencil-clear-value+           #x0b91) 
(defconstant +stencil-index+                 #x1901) 
(defconstant +keep+                          #x1e00) 
(defconstant +replace+                       #x1e01) 
(defconstant +incr+                          #x1e02) 
(defconstant +decr+                          #x1e03) 

;; Buffers, Pixel Drawing/Reading

(defconstant +none+                             #x0) 
(defconstant +left+                          #x0406) 
(defconstant +right+                         #x0407) 
(defconstant +front-left+                    #x0400) 
(defconstant +front-right+                   #x0401) 
(defconstant +back-left+                     #x0402) 
(defconstant +back-right+                    #x0403) 
(defconstant +aux0+                          #x0409) 
(defconstant +aux1+                          #x040a) 
(defconstant +aux2+                          #x040b) 
(defconstant +aux3+                          #x040c) 
(defconstant +color-index+                   #x1900) 
(defconstant +red+                           #x1903) 
(defconstant +green+                         #x1904) 
(defconstant +blue+                          #x1905) 
(defconstant +alpha+                         #x1906) 
(defconstant +luminance+                     #x1909) 
(defconstant +luminance-alpha+               #x190a) 
(defconstant +alpha-bits+                    #x0d55) 
(defconstant +red-bits+                      #x0d52) 
(defconstant +green-bits+                    #x0d53) 
(defconstant +blue-bits+                     #x0d54) 
(defconstant +index-bits+                    #x0d51) 
(defconstant +subpixel-bits+                 #x0d50) 
(defconstant +aux-buffers+                   #x0c00) 
(defconstant +read-buffer+                   #x0c02) 
(defconstant +draw-buffer+                   #x0c01) 
(defconstant +doublebuffer+                  #x0c32) 
(defconstant +stereo+                        #x0c33) 
(defconstant +bitmap+                        #x1a00) 
(defconstant +color+                         #x1800) 
(defconstant +depth+                         #x1801) 
(defconstant +stencil+                       #x1802) 
(defconstant +dither+                        #x0bd0) 
(defconstant +rgb+                           #x1907) 
(defconstant +rgba+                          #x1908) 

;; Implementation Limits

(defconstant +max-list-nesting+              #x0b31) 
(defconstant +max-attrib-stack-depth+        #x0d35) 
(defconstant +max-modelview-stack-depth+     #x0d36) 
(defconstant +max-name-stack-depth+          #x0d37) 
(defconstant +max-projection-stack-depth+    #x0d38) 
(defconstant +max-texture-stack-depth+       #x0d39) 
(defconstant +max-eval-order+                #x0d30) 
(defconstant +max-lights+                    #x0d31) 
(defconstant +max-clip-planes+               #x0d32) 
(defconstant +max-texture-size+              #x0d33) 
(defconstant +max-pixel-map-table+           #x0d34) 
(defconstant +max-viewport-dims+             #x0d3a) 
(defconstant +max-client-attrib-stack-depth+ #x0d3b) 

;; Gets

(defconstant +attrib-stack-depth+            #x0bb0) 
(defconstant +client-attrib-stack-depth+     #x0bb1) 
(defconstant +color-clear-value+             #x0c22) 
(defconstant +color-writemask+               #x0c23) 
(defconstant +current-index+                 #x0b01) 
(defconstant +current-color+                 #x0b00) 
(defconstant +current-normal+                #x0b02) 
(defconstant +current-raster-color+          #x0b04) 
(defconstant +current-raster-distance+       #x0b09) 
(defconstant +current-raster-index+          #x0b05) 
(defconstant +current-raster-position+       #x0b07) 
(defconstant +current-raster-texture-coords+ #x0b06) 
(defconstant +current-raster-position-valid+ #x0b08) 
(defconstant +current-texture-coords+        #x0b03) 
(defconstant +index-clear-value+             #x0c20) 
(defconstant +index-mode+                    #x0c30) 
(defconstant +index-writemask+               #x0c21) 
(defconstant +modelview-matrix+              #x0ba6) 
(defconstant +modelview-stack-depth+         #x0ba3) 
(defconstant +name-stack-depth+              #x0d70) 
(defconstant +projection-matrix+             #x0ba7) 
(defconstant +projection-stack-depth+        #x0ba4) 
(defconstant +render-mode+                   #x0c40) 
(defconstant +rgba-mode+                     #x0c31) 
(defconstant +texture-matrix+                #x0ba8) 
(defconstant +texture-stack-depth+           #x0ba5) 
(defconstant +viewport+                      #x0ba2) 

;; GL Evaluators

(defconstant +auto-normal+                   #x0d80) 
(defconstant +map1-color-4+                  #x0d90) 
(defconstant +map1-grid-domain+              #x0dd0) 
(defconstant +map1-grid-segments+            #x0dd1) 
(defconstant +map1-index+                    #x0d91) 
(defconstant +map1-normal+                   #x0d92) 
(defconstant +map1-texture-coord-1+          #x0d93) 
(defconstant +map1-texture-coord-2+          #x0d94) 
(defconstant +map1-texture-coord-3+          #x0d95) 
(defconstant +map1-texture-coord-4+          #x0d96) 
(defconstant +map1-vertex-3+                 #x0d97) 
(defconstant +map1-vertex-4+                 #x0d98) 
(defconstant +map2-color-4+                  #x0db0) 
(defconstant +map2-grid-domain+              #x0dd2) 
(defconstant +map2-grid-segments+            #x0dd3) 
(defconstant +map2-index+                    #x0db1) 
(defconstant +map2-normal+                   #x0db2) 
(defconstant +map2-texture-coord-1+          #x0db3) 
(defconstant +map2-texture-coord-2+          #x0db4) 
(defconstant +map2-texture-coord-3+          #x0db5) 
(defconstant +map2-texture-coord-4+          #x0db6) 
(defconstant +map2-vertex-3+                 #x0db7) 
(defconstant +map2-vertex-4+                 #x0db8) 
(defconstant +coeff+                         #x0a00) 
(defconstant +domain+                        #x0a02) 
(defconstant +order+                         #x0a01) 

;; Hints

(defconstant +fog-hint+                      #x0c54) 
(defconstant +line-smooth-hint+              #x0c52) 
(defconstant +perspective-correction-hint+   #x0c50) 
(defconstant +point-smooth-hint+             #x0c51) 
(defconstant +polygon-smooth-hint+           #x0c53) 
(defconstant +dont-care+                     #x1100) 
(defconstant +fastest+                       #x1101) 
(defconstant +nicest+                        #x1102) 

;; Scissor box

(defconstant +scissor-test+                  #x0c11) 
(defconstant +scissor-box+                   #x0c10) 

;; Pixel Mode / Transfer

(defconstant +map-color+                     #x0d10) 
(defconstant +map-stencil+                   #x0d11) 
(defconstant +index-shift+                   #x0d12) 
(defconstant +index-offset+                  #x0d13) 
(defconstant +red-scale+                     #x0d14) 
(defconstant +red-bias+                      #x0d15) 
(defconstant +green-scale+                   #x0d18) 
(defconstant +green-bias+                    #x0d19) 
(defconstant +blue-scale+                    #x0d1a) 
(defconstant +blue-bias+                     #x0d1b) 
(defconstant +alpha-scale+                   #x0d1c) 
(defconstant +alpha-bias+                    #x0d1d) 
(defconstant +depth-scale+                   #x0d1e) 
(defconstant +depth-bias+                    #x0d1f) 
(defconstant +pixel-map-s-to-s-size+         #x0cb1) 
(defconstant +pixel-map-i-to-i-size+         #x0cb0) 
(defconstant +pixel-map-i-to-r-size+         #x0cb2) 
(defconstant +pixel-map-i-to-g-size+         #x0cb3) 
(defconstant +pixel-map-i-to-b-size+         #x0cb4) 
(defconstant +pixel-map-i-to-a-size+         #x0cb5) 
(defconstant +pixel-map-r-to-r-size+         #x0cb6) 
(defconstant +pixel-map-g-to-g-size+         #x0cb7) 
(defconstant +pixel-map-b-to-b-size+         #x0cb8) 
(defconstant +pixel-map-a-to-a-size+         #x0cb9) 
(defconstant +pixel-map-s-to-s+              #x0c71) 
(defconstant +pixel-map-i-to-i+              #x0c70) 
(defconstant +pixel-map-i-to-r+              #x0c72) 
(defconstant +pixel-map-i-to-g+              #x0c73) 
(defconstant +pixel-map-i-to-b+              #x0c74) 
(defconstant +pixel-map-i-to-a+              #x0c75) 
(defconstant +pixel-map-r-to-r+              #x0c76) 
(defconstant +pixel-map-g-to-g+              #x0c77) 
(defconstant +pixel-map-b-to-b+              #x0c78) 
(defconstant +pixel-map-a-to-a+              #x0c79) 
(defconstant +pack-alignment+                #x0d05) 
(defconstant +pack-lsb-first+                #x0d01) 
(defconstant +pack-row-length+               #x0d02) 
(defconstant +pack-skip-pixels+              #x0d04) 
(defconstant +pack-skip-rows+                #x0d03) 
(defconstant +pack-swap-bytes+               #x0d00) 
(defconstant +unpack-alignment+              #x0cf5) 
(defconstant +unpack-lsb-first+              #x0cf1) 
(defconstant +unpack-row-length+             #x0cf2) 
(defconstant +unpack-skip-pixels+            #x0cf4) 
(defconstant +unpack-skip-rows+              #x0cf3) 
(defconstant +unpack-swap-bytes+             #x0cf0) 
(defconstant +zoom-x+                        #x0d16) 
(defconstant +zoom-y+                        #x0d17) 

;; Texture Mapping

(defconstant +texture-env+                   #x2300) 
(defconstant +texture-env-mode+              #x2200) 
(defconstant +texture-1d+                    #x0de0) 
(defconstant +texture-2d+                    #x0de1) 
(defconstant +texture-wrap-s+                #x2802) 
(defconstant +texture-wrap-t+                #x2803) 
(defconstant +texture-mag-filter+            #x2800) 
(defconstant +texture-min-filter+            #x2801) 
(defconstant +texture-env-color+             #x2201) 
(defconstant +texture-gen-s+                 #x0c60) 
(defconstant +texture-gen-t+                 #x0c61) 
(defconstant +texture-gen-mode+              #x2500) 
(defconstant +texture-border-color+          #x1004) 
(defconstant +texture-width+                 #x1000) 
(defconstant +texture-height+                #x1001) 
(defconstant +texture-border+                #x1005) 
(defconstant +texture-components+            #x1003) 
(defconstant +texture-red-size+              #x805c) 
(defconstant +texture-green-size+            #x805d) 
(defconstant +texture-blue-size+             #x805e) 
(defconstant +texture-alpha-size+            #x805f) 
(defconstant +texture-luminance-size+        #x8060) 
(defconstant +texture-intensity-size+        #x8061) 
(defconstant +nearest-mipmap-nearest+        #x2700) 
(defconstant +nearest-mipmap-linear+         #x2702) 
(defconstant +linear-mipmap-nearest+         #x2701) 
(defconstant +linear-mipmap-linear+          #x2703) 
(defconstant +object-linear+                 #x2401) 
(defconstant +object-plane+                  #x2501) 
(defconstant +eye-linear+                    #x2400) 
(defconstant +eye-plane+                     #x2502) 
(defconstant +sphere-map+                    #x2402) 
(defconstant +decal+                         #x2101) 
(defconstant +modulate+                      #x2100) 
(defconstant +nearest+                       #x2600) 
(defconstant +repeat+                        #x2901) 
(defconstant +clamp+                         #x2900) 
(defconstant +s+                             #x2000) 
(defconstant +t+                             #x2001) 
(defconstant +r+                             #x2002) 
(defconstant +q+                             #x2003) 
(defconstant +texture-gen-r+                 #x0c62) 
(defconstant +texture-gen-q+                 #x0c63) 

;; GL 1.1 Texturing

(defconstant +proxy-texture-1d+              #x8063) 
(defconstant +proxy-texture-2d+              #x8064) 
(defconstant +texture-priority+              #x8066) 
(defconstant +texture-resident+              #x8067) 
(defconstant +texture-binding-1d+            #x8068) 
(defconstant +texture-binding-2d+            #x8069) 
(defconstant +texture-internal-format+       #x1003) 
(defconstant +pack-skip-images+              #x806b) 
(defconstant +pack-image-height+             #x806c) 
(defconstant +unpack-skip-images+            #x806d) 
(defconstant +unpack-image-height+           #x806e) 
(defconstant +texture-3d+                    #x806f) 
(defconstant +proxy-texture-3d+              #x8070) 
(defconstant +texture-depth+                 #x8071) 
(defconstant +texture-wrap-r+                #x8072) 
(defconstant +max-3d-texture-size+           #x8073) 
(defconstant +texture-binding-3d+            #x806a) 

;; Internal texture formats (GL 1.1)
(defconstant +alpha4+                        #x803b) 
(defconstant +alpha8+                        #x803c) 
(defconstant +alpha12+                       #x803d) 
(defconstant +alpha16+                       #x803e) 
(defconstant +luminance4+                    #x803f) 
(defconstant +luminance8+                    #x8040) 
(defconstant +luminance12+                   #x8041) 
(defconstant +luminance16+                   #x8042) 
(defconstant +luminance4-alpha4+             #x8043) 
(defconstant +luminance6-alpha2+             #x8044) 
(defconstant +luminance8-alpha8+             #x8045) 
(defconstant +luminance12-alpha4+            #x8046) 
(defconstant +luminance12-alpha12+           #x8047) 
(defconstant +luminance16-alpha16+           #x8048) 
(defconstant +intensity+                     #x8049) 
(defconstant +intensity4+                    #x804a) 
(defconstant +intensity8+                    #x804b) 
(defconstant +intensity12+                   #x804c) 
(defconstant +intensity16+                   #x804d) 
(defconstant +r3-g3-b2+                      #x2a10) 
(defconstant +rgb4+                          #x804f) 
(defconstant +rgb5+                          #x8050) 
(defconstant +rgb8+                          #x8051) 
(defconstant +rgb10+                         #x8052) 
(defconstant +rgb12+                         #x8053) 
(defconstant +rgb16+                         #x8054) 
(defconstant +rgba2+                         #x8055) 
(defconstant +rgba4+                         #x8056) 
(defconstant +rgb5-a1+                       #x8057) 
(defconstant +rgba8+                         #x8058) 
(defconstant +rgb10-a2+                      #x8059) 
(defconstant +rgba12+                        #x805a) 
(defconstant +rgba16+                        #x805b) 

;; Utility

(defconstant +vendor+                        #x1f00) 
(defconstant +renderer+                      #x1f01) 
(defconstant +version+                       #x1f02) 
(defconstant +extensions+                    #x1f03) 

;; Errors

(defconstant +no-error+                         #x0) 
(defconstant +invalid-value+                 #x0501) 
(defconstant +invalid-enum+                  #x0500) 
(defconstant +invalid-operation+             #x0502) 
(defconstant +stack-overflow+                #x0503) 
(defconstant +stack-underflow+               #x0504) 
(defconstant +out-of-memory+                 #x0505) 

;; OpenGL 1.2

(defconstant +rescale-normal+                #x803a) 
(defconstant +clamp-to-edge+                 #x812f) 
(defconstant +max-elements-vertices+         #x80e8) 
(defconstant +max-elements-indices+          #x80e9) 
(defconstant +bgr+                           #x80e0) 
(defconstant +bgra+                          #x80e1) 
(defconstant +unsigned-byte-3-3-2+           #x8032) 
(defconstant +unsigned-byte-2-3-3-rev+       #x8362) 
(defconstant +unsigned-short-5-6-5+          #x8363) 
(defconstant +unsigned-short-5-6-5-rev+      #x8364) 
(defconstant +unsigned-short-4-4-4-4+        #x8033) 
(defconstant +unsigned-short-4-4-4-4-rev+    #x8365) 
(defconstant +unsigned-short-5-5-5-1+        #x8034) 
(defconstant +unsigned-short-1-5-5-5-rev+    #x8366) 
(defconstant +unsigned-int-8-8-8-8+          #x8035) 
(defconstant +unsigned-int-8-8-8-8-rev+      #x8367) 
(defconstant +unsigned-int-10-10-10-2+       #x8036) 
(defconstant +unsigned-int-2-10-10-10-rev+   #x8368) 
(defconstant +light-model-color-control+     #x81f8) 
(defconstant +single-color+                  #x81f9) 
(defconstant +separate-specular-color+       #x81fa) 
(defconstant +texture-min-lod+               #x813a) 
(defconstant +texture-max-lod+               #x813b) 
(defconstant +texture-base-level+            #x813c) 
(defconstant +texture-max-level+             #x813d) 
(defconstant +smooth-point-size-range+       #x0b12) 
(defconstant +smooth-point-size-granularity+ #x0b13) 
(defconstant +smooth-line-width-range+       #x0b22) 
(defconstant +smooth-line-width-granularity+ #x0b23) 
(defconstant +aliased-point-size-range+      #x846d) 
(defconstant +aliased-line-width-range+      #x846e) 

;; OpenGL 1.2 Imaging subset
;; GL_EXT_color_table
(defconstant +color-table+                   #x80d0) 
(defconstant +post-convolution-color-table+  #x80d1) 
(defconstant +post-color-matrix-color-table+ #x80d2) 
(defconstant +proxy-color-table+             #x80d3) 
(defconstant +proxy-post-convolution-color-table+  #x80d4) 
(defconstant +proxy-post-color-matrix-color-table+ #x80d5) 
(defconstant +color-table-scale+             #x80d6) 
(defconstant +color-table-bias+              #x80d7) 
(defconstant +color-table-format+            #x80d8) 
(defconstant +color-table-width+             #x80d9) 
(defconstant +color-table-red-size+          #x80da) 
(defconstant +color-table-green-size+        #x80db) 
(defconstant +color-table-blue-size+         #x80dc) 
(defconstant +color-table-alpha-size+        #x80dd) 
(defconstant +color-table-luminance-size+    #x80de) 
(defconstant +color-table-intensity-size+    #x80df) 
;; GL_EXT_convolution and GL_HP_convolution
(defconstant +convolution-1d+                #x8010) 
(defconstant +convolution-2d+                #x8011) 
(defconstant +separable-2d+                  #x8012) 
(defconstant +convolution-border-mode+       #x8013) 
(defconstant +convolution-filter-scale+      #x8014) 
(defconstant +convolution-filter-bias+       #x8015) 
(defconstant +reduce+                        #x8016) 
(defconstant +convolution-format+            #x8017) 
(defconstant +convolution-width+             #x8018) 
(defconstant +convolution-height+            #x8019) 
(defconstant +max-convolution-width+         #x801a) 
(defconstant +max-convolution-height+        #x801b) 
(defconstant +post-convolution-red-scale+    #x801c) 
(defconstant +post-convolution-green-scale+  #x801d) 
(defconstant +post-convolution-blue-scale+   #x801e) 
(defconstant +post-convolution-alpha-scale+  #x801f) 
(defconstant +post-convolution-red-bias+     #x8020) 
(defconstant +post-convolution-green-bias+   #x8021) 
(defconstant +post-convolution-blue-bias+    #x8022) 
(defconstant +post-convolution-alpha-bias+   #x8023) 
(defconstant +constant-border+               #x8151) 
(defconstant +replicate-border+              #x8153) 
(defconstant +convolution-border-color+      #x8154) 
;; GL_SGI_color_matrix
(defconstant +color-matrix+                  #x80b1) 
(defconstant +color-matrix-stack-depth+      #x80b2) 
(defconstant +max-color-matrix-stack-depth+  #x80b3) 
(defconstant +post-color-matrix-red-scale+   #x80b4) 
(defconstant +post-color-matrix-green-scale+ #x80b5) 
(defconstant +post-color-matrix-blue-scale+  #x80b6) 
(defconstant +post-color-matrix-alpha-scale+ #x80b7) 
(defconstant +post-color-matrix-red-bias+    #x80b8) 
(defconstant +post-color-matrix-green-bias+  #x80b9) 
(defconstant +post-color-matrix-blue-bias+   #x80ba) 
(defconstant +post-color-matrix-alpha-bias+  #x80bb) 
;; GL_EXT_histogram
(defconstant +histogram+                     #x8024) 
(defconstant +proxy-histogram+               #x8025) 
(defconstant +histogram-width+               #x8026) 
(defconstant +histogram-format+              #x8027) 
(defconstant +histogram-red-size+            #x8028) 
(defconstant +histogram-green-size+          #x8029) 
(defconstant +histogram-blue-size+           #x802a) 
(defconstant +histogram-alpha-size+          #x802b) 
(defconstant +histogram-luminance-size+      #x802c) 
(defconstant +histogram-sink+                #x802d) 
(defconstant +minmax+                        #x802e) 
(defconstant +minmax-format+                 #x802f) 
(defconstant +minmax-sink+                   #x8030) 
(defconstant +table-too-large+               #x8031) 
;; GL_EXT_blend_color, GL_EXT_blend_minmax
(defconstant +blend-equation+                #x8009) 
(defconstant +min+                           #x8007) 
(defconstant +max+                           #x8008) 
(defconstant +func-add+                      #x8006) 
(defconstant +func-subtract+                 #x800a) 
(defconstant +func-reverse-subtract+         #x800b) 

;; glPush/PopAttrib bits

(defconstant +current-bit+               #x00000001) 
(defconstant +point-bit+                 #x00000002) 
(defconstant +line-bit+                  #x00000004) 
(defconstant +polygon-bit+               #x00000008) 
(defconstant +polygon-stipple-bit+       #x00000010) 
(defconstant +pixel-mode-bit+            #x00000020) 
(defconstant +lighting-bit+              #x00000040) 
(defconstant +fog-bit+                   #x00000080) 
(defconstant +depth-buffer-bit+          #x00000100) 
(defconstant +accum-buffer-bit+          #x00000200) 
(defconstant +stencil-buffer-bit+        #x00000400) 
(defconstant +viewport-bit+              #x00000800) 
(defconstant +transform-bit+             #x00001000) 
(defconstant +enable-bit+                #x00002000) 
(defconstant +color-buffer-bit+          #x00004000) 
(defconstant +hint-bit+                  #x00008000) 
(defconstant +eval-bit+                  #x00010000) 
(defconstant +list-bit+                  #x00020000) 
(defconstant +texture-bit+               #x00040000) 
(defconstant +scissor-bit+               #x00080000) 
(defconstant +all-attrib-bits+           #x000fffff) 
(defconstant +client-pixel-store-bit+    #x00000001) 
(defconstant +client-vertex-array-bit+   #x00000002) 
(defconstant +client-all-attrib-bits+    #xffffffff) 

;; ARB Multitexturing extension

(defconstant +arb-multitexture+                   1) 
(defconstant +texture0-arb+                  #x84c0) 
(defconstant +texture1-arb+                  #x84c1) 
(defconstant +texture2-arb+                  #x84c2) 
(defconstant +texture3-arb+                  #x84c3) 
(defconstant +texture4-arb+                  #x84c4) 
(defconstant +texture5-arb+                  #x84c5) 
(defconstant +texture6-arb+                  #x84c6) 
(defconstant +texture7-arb+                  #x84c7) 
(defconstant +texture8-arb+                  #x84c8) 
(defconstant +texture9-arb+                  #x84c9) 
(defconstant +texture10-arb+                 #x84ca) 
(defconstant +texture11-arb+                 #x84cb) 
(defconstant +texture12-arb+                 #x84cc) 
(defconstant +texture13-arb+                 #x84cd) 
(defconstant +texture14-arb+                 #x84ce) 
(defconstant +texture15-arb+                 #x84cf) 
(defconstant +texture16-arb+                 #x84d0) 
(defconstant +texture17-arb+                 #x84d1) 
(defconstant +texture18-arb+                 #x84d2) 
(defconstant +texture19-arb+                 #x84d3) 
(defconstant +texture20-arb+                 #x84d4) 
(defconstant +texture21-arb+                 #x84d5) 
(defconstant +texture22-arb+                 #x84d6) 
(defconstant +texture23-arb+                 #x84d7) 
(defconstant +texture24-arb+                 #x84d8) 
(defconstant +texture25-arb+                 #x84d9) 
(defconstant +texture26-arb+                 #x84da) 
(defconstant +texture27-arb+                 #x84db) 
(defconstant +texture28-arb+                 #x84dc) 
(defconstant +texture29-arb+                 #x84dd) 
(defconstant +texture30-arb+                 #x84de) 
(defconstant +texture31-arb+                 #x84df) 
(defconstant +active-texture-arb+            #x84e0) 
(defconstant +client-active-texture-arb+     #x84e1) 
(defconstant +max-texture-units-arb+         #x84e2) 

;;; Misc extensions

(defconstant +ext-abgr+                           1) 
(defconstant +abgr-ext+                      #x8000) 
(defconstant +ext-blend-color+                    1) 
(defconstant +constant-color-ext+            #x8001) 
(defconstant +one-minus-constant-color-ext+  #x8002) 
(defconstant +constant-alpha-ext+            #x8003) 
(defconstant +one-minus-constant-alpha-ext+  #x8004) 
(defconstant +blend-color-ext+               #x8005) 
(defconstant +ext-polygon-offset+                 1) 
(defconstant +polygon-offset-ext+            #x8037) 
(defconstant +polygon-offset-factor-ext+     #x8038) 
(defconstant +polygon-offset-bias-ext+       #x8039) 
(defconstant +ext-texture3d+                      1) 
(defconstant +pack-skip-images-ext+          #x806b) 
(defconstant +pack-image-height-ext+         #x806c) 
(defconstant +unpack-skip-images-ext+        #x806d) 
(defconstant +unpack-image-height-ext+       #x806e) 
(defconstant +texture-3d-ext+                #x806f) 
(defconstant +proxy-texture-3d-ext+          #x8070) 
(defconstant +texture-depth-ext+             #x8071) 
(defconstant +texture-wrap-r-ext+            #x8072) 
(defconstant +max-3d-texture-size-ext+       #x8073) 
(defconstant +texture-3d-binding-ext+        #x806a) 
(defconstant +ext-texture-object+                 1) 
(defconstant +texture-priority-ext+          #x8066) 
(defconstant +texture-resident-ext+          #x8067) 
(defconstant +texture-1d-binding-ext+        #x8068) 
(defconstant +texture-2d-binding-ext+        #x8069) 
(defconstant +ext-rescale-normal+                 1) 
(defconstant +rescale-normal-ext+            #x803a) 
(defconstant +ext-vertex-array+                   1) 
(defconstant +vertex-array-ext+              #x8074) 
(defconstant +normal-array-ext+              #x8075) 
(defconstant +color-array-ext+               #x8076) 
(defconstant +index-array-ext+               #x8077) 
(defconstant +texture-coord-array-ext+       #x8078) 
(defconstant +edge-flag-array-ext+           #x8079) 
(defconstant +vertex-array-size-ext+         #x807a) 
(defconstant +vertex-array-type-ext+         #x807b) 
(defconstant +vertex-array-stride-ext+       #x807c) 
(defconstant +vertex-array-count-ext+        #x807d) 
(defconstant +normal-array-type-ext+         #x807e) 
(defconstant +normal-array-stride-ext+       #x807f) 
(defconstant +normal-array-count-ext+        #x8080) 
(defconstant +color-array-size-ext+          #x8081) 
(defconstant +color-array-type-ext+          #x8082) 
(defconstant +color-array-stride-ext+        #x8083) 
(defconstant +color-array-count-ext+         #x8084) 
(defconstant +index-array-type-ext+          #x8085) 
(defconstant +index-array-stride-ext+        #x8086) 
(defconstant +index-array-count-ext+         #x8087) 
(defconstant +texture-coord-array-size-ext+  #x8088) 
(defconstant +texture-coord-array-type-ext+  #x8089) 
(defconstant +texture-coord-array-stride-ext+ #x808a) 
(defconstant +texture-coord-array-count-ext+ #x808b) 
(defconstant +edge-flag-array-stride-ext+    #x808c) 
(defconstant +edge-flag-array-count-ext+     #x808d) 
(defconstant +vertex-array-pointer-ext+      #x808e) 
(defconstant +normal-array-pointer-ext+      #x808f) 
(defconstant +color-array-pointer-ext+       #x8090) 
(defconstant +index-array-pointer-ext+       #x8091) 
(defconstant +texture-coord-array-pointer-ext+ #x8092) 
(defconstant +edge-flag-array-pointer-ext+   #x8093) 
(defconstant +sgis-texture-edge-clamp+            1) 
(defconstant +clamp-to-edge-sgis+            #x812f) 
(defconstant +ext-blend-minmax+                   1) 
(defconstant +func-add-ext+                  #x8006) 
(defconstant +min-ext+                       #x8007) 
(defconstant +max-ext+                       #x8008) 
(defconstant +blend-equation-ext+            #x8009) 
(defconstant +ext-blend-subtract+                 1) 
(defconstant +func-subtract-ext+             #x800a) 
(defconstant +func-reverse-subtract-ext+     #x800b) 
(defconstant +ext-blend-logic-op+                 1) 
(defconstant +ext-point-parameters+               1) 
(defconstant +point-size-min-ext+            #x8126) 
(defconstant +point-size-max-ext+            #x8127) 
(defconstant +point-fade-threshold-size-ext+ #x8128) 
(defconstant +distance-attenuation-ext+      #x8129) 
(defconstant +ext-paletted-texture+               1) 
(defconstant +table-too-large-ext+           #x8031) 
(defconstant +color-table-format-ext+        #x80d8) 
(defconstant +color-table-width-ext+         #x80d9) 
(defconstant +color-table-red-size-ext+      #x80da) 
(defconstant +color-table-green-size-ext+    #x80db) 
(defconstant +color-table-blue-size-ext+     #x80dc) 
(defconstant +color-table-alpha-size-ext+    #x80dd) 
(defconstant +color-table-luminance-size-ext+ #x80de) 
(defconstant +color-table-intensity-size-ext+ #x80df) 
(defconstant +texture-index-size-ext+        #x80ed) 
(defconstant +color-index1-ext+              #x80e2) 
(defconstant +color-index2-ext+              #x80e3) 
(defconstant +color-index4-ext+              #x80e4) 
(defconstant +color-index8-ext+              #x80e5) 
(defconstant +color-index12-ext+             #x80e6) 
(defconstant +color-index16-ext+             #x80e7) 
(defconstant +ext-clip-volume-hint+               1) 
(defconstant +clip-volume-clipping-hint-ext+ #x80f0) 
(defconstant +ext-compiled-vertex-array+          1) 
(defconstant +array-element-lock-first-ext+  #x81a8) 
(defconstant +array-element-lock-count-ext+  #x81a9) 
(defconstant +hp-occlusion-test+                  1) 
(defconstant +occlusion-test-hp+             #x8165) 
(defconstant +occlusion-test-result-hp+      #x8166) 
(defconstant +ext-shared-texture-palette+         1) 
(defconstant +shared-texture-palette-ext+    #x81fb) 
(defconstant +ext-stencil-wrap+                   1) 
(defconstant +incr-wrap-ext+                 #x8507) 
(defconstant +decr-wrap-ext+                 #x8508) 
(defconstant +nv-texgen-reflection+               1) 
(defconstant +normal-map-nv+                 #x8511) 
(defconstant +reflection-map-nv+             #x8512) 
(defconstant +ext-texture-env-add+                1) 
(defconstant +mesa-window-pos+                    1) 
(defconstant +mesa-resize-buffers+                1)
)



;;; Utility stuff

(deftype bool () 'card8)
(deftype float32 () 'single-float)
(deftype float64 () 'double-float)

(declaim (inline aset-float32 aset-float64))

#+sbcl
(defun aset-float32 (value array index)
  (declare (type single-float value)
           (type buffer-bytes array)
           (type array-index index))
  #.(declare-buffun)
  (let ((bits (sb-kernel:single-float-bits value)))
    (declare (type (unsigned-byte 32) bits))
    (aset-card32 bits array index))
  value)


#+cmu
(defun aset-float32 (value array index)
  (declare (type single-float value)
           (type buffer-bytes array)
           (type array-index index))
  #.(declare-buffun)
  (let ((bits (kernel:single-float-bits  value)))
    (declare (type (unsigned-byte 32) bits))
    (aset-card32 bits array index))
  value)


#+openmcl
(defun aset-float32 (value array index)
  (declare (type single-float value)
           (type buffer-bytes array)
           (type array-index index))
  #.(declare-buffun)
  (let ((bits (ccl::single-float-bits value)))
    (declare (type (unsigned-byte 32) bits))
    (aset-card32 bits array index))
  value)


#+sbcl
(defun aset-float64 (value array index)
  (declare (type double-float value)
           (type buffer-bytes array)
           (type array-index index))
  #.(declare-buffun)
  (let ((low (sb-kernel:double-float-low-bits value))
        (high (sb-kernel:double-float-high-bits value)))
    (declare (type (unsigned-byte 32) low high))
    (aset-card32 low array index)
    (aset-card32 high array (the array-index (+ index 4))))
  value)


#+cmu
(defun aset-float64 (value array index)
  (declare (type double-float value)
           (type buffer-bytes array)
           (type array-index index))
  #.(declare-buffun)
  (let ((low (kernel:double-float-low-bits value))
        (high (kernel:double-float-high-bits value)))
    (declare (type (unsigned-byte 32) low high))
    (aset-card32 low array index)
    (aset-card32 high array (+ index 4)))
  value)


#+openmcl
(defun aset-float64 (value array index)
  (declare (type double-float value)
           (type buffer-bytes array)
           (type array-index index))
  #.(declare-buffun)
  (multiple-value-bind (low high)
      (ccl::double-float-bits value)
    (declare (type (unsigned-byte 32) low high))
    (aset-card32 low array index)
    (aset-card32 high array (the array-index (+ index 4))))
  value)


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun byte-width (type)
  (ecase type
    ((int8 card8 bool)          1)
    ((int16 card16)             2)
    ((int32 card32 float32)     4)
    ((float64)                  8)))


(defun setter (type)
  (ecase type
    (int8       'aset-int8)
    (int16      'aset-int16)
    (int32      'aset-int32)
    (bool       'aset-card8)
    (card8      'aset-card8)
    (card16     'aset-card16)
    (card32     'aset-card32)
    (float32    'aset-float32)
    (float64    'aset-float64)))


(defun sequence-setter (type)
  (ecase type
    (int8       'sset-int8)
    (int16      'sset-int16)
    (int32      'sset-int32)
    (bool       'sset-card8)
    (card8      'sset-card8)
    (card16     'sset-card16)
    (card32     'sset-card32)
    (float32    'sset-float32)
    (float64    'sset-float64)))


(defmacro define-sequence-setter (type)
  `(defun ,(intern (format nil "~A-~A" 'sset type)) (seq buffer start length)
     (declare (type sequence seq)
              (type buffer-bytes buffer)
              (type array-index start)
              (type fixnum length))
     #.(declare-buffun)
     (assert (= length (length seq))
             (length seq)
             "SEQUENCE length should be ~D, not ~D." length (length seq))
     (typecase seq
       (list
        (let ((offset 0))
          (declare (type fixnum offset))
          (dolist (n seq)
            (declare (type ,type n))
            (,(setter type) n buffer (the array-index (+ start offset)))
            (incf offset ,(byte-width type)))))
       ((simple-array ,type)
        (dotimes (i ,(byte-width type))
          (,(setter type)
            (aref seq i)
            buffer
            (the array-index (+ start (* i ,(byte-width type)))))))
       (vector
        (dotimes (i ,(byte-width type))
          (,(setter type)
            (svref seq i)
            buffer
            (the array-index (+ start (* i ,(byte-width type))))))))))


(define-sequence-setter int8)
(define-sequence-setter int16)
(define-sequence-setter int32)
(define-sequence-setter bool)
(define-sequence-setter card8)
(define-sequence-setter card16)
(define-sequence-setter card32)
(define-sequence-setter float32)
(define-sequence-setter float64)



(defun make-argspecs (list)
  (destructuring-bind (name type)
      list
    (etypecase type
      (symbol `(,name ,type 1 nil))
      (list
       `(,name
         ,(second type)
         ,(third type)
         ,(if (consp (third type))
              (make-symbol (format nil "~A-~A" name 'length))
              nil))))))


(defun byte-width-calculation (argspecs)
  (let ((constant 0)
        (calculated ()))
    (loop
       for (name type length length-var) in argspecs
       do (let ((byte-width (byte-width type)))
            (typecase length
              (number (incf constant (* byte-width length)))
              (symbol (push `(* ,byte-width ,length) calculated))
              (cons  (push `(* ,byte-width ,length-var) calculated)))))
    (if (null calculated)
        constant
        (list* '+ constant calculated))))


(defun composite-args (argspecs)
  (loop
     for (name type length length-var) in argspecs
     when (consp length)
     collect (list length-var length)))


(defun make-setter-forms (argspecs)
  (loop
     for (name type length length-var) in argspecs
     collecting `(progn
                   ,(if (and (numberp length)
                             (= 1 length))
                        `(,(setter type) ,name .rbuf. .index.)
                        `(,(sequence-setter type) ,name .rbuf. .index.
                           ,(if length-var length-var length)))
                   (setf .index. (the array-index
                                   (+ .index.
                                      (the fixnum (* ,(byte-width type)
                                                     ,(if length-var length-var length)))))))))


(defmacro define-rendering-command (name opcode &rest args)
  ;; FIXME: Must heavily type-annotate.
  (labels ((expand-args (list)
             (loop
                for (arg type) in list
                if (consp arg)
                append (loop
                          for name in arg
                          collecting (list name type))
                else
                collect (list arg type))))

    (let* ((args (expand-args args))
           (argspecs (mapcar 'make-argspecs args))
           (total-byte-width (byte-width-calculation argspecs))
           (composite-args (composite-args argspecs)))
      
      `(defun ,name ,(mapcar #'first argspecs)
         (declare ,@(mapcar #'(lambda (list)
                                (if (symbolp (second list))
                                    (list* 'type (reverse list))
                                    `(type sequence ,(first list))))
                            args))
         #.(declare-buffun)
         (assert (context-p *current-context*)
                 (*current-context*)
                 "*CURRENT-CONTEXT* is not set (~S)." *current-context*)
         (let* ((.ctx. *current-context*)
                (.index0. (context-index .ctx.))
                (.index. (+ .index0. 4))
                (.rbuf. (context-rbuf .ctx.))
                ,@composite-args
                (.length. (+ 4 (* 4 (ceiling ,total-byte-width 4)))))

           (declare (type context .ctx.)
                    (type array-index .index. .index0.)
                    (type buffer-bytes .rbuf.)
                    ,@(mapcar #'(lambda (list)
                                  `(type fixnum ,(first list)))
                              composite-args)
                    (type fixnum .length.))

           (when (< (- (length .rbuf.) 8)
                    (+ .index. .length.))
             (error "Rendering command sequence too long.  Implement automatic buffer flushing."))

           (aset-card16 .length. .rbuf. (the array-index .index0.))
           (aset-card16 ,opcode .rbuf. (the array-index (+ .index0. 2)))
           ,@(make-setter-forms argspecs)
           (setf (context-index .ctx.) (the array-index (+ .index0. .length.))))))))

) ;; eval-when


;;; Command implementation.


(defun get-string (name)
  (assert (context-p *current-context*)
          (*current-context*)
          "*CURRENT-CONTEXT* is not set (~S)." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +get-string+)
         ;; *** This is CONTEXT-TAG
         (card32 (context-tag ctx))
         ;; *** This is ENUM.
         (card32 name))
      (let* ((length (card32-get 12))
             (bytes (sequence-get :format card8
                                  :result-type '(simple-array card8 (*))
                                  :index 32
                                  :length length)))
        (declare (type (simple-array card8 (*)) bytes)
                 (type fixnum length))
        ;; FIXME: How does this interact with unicode?
        (map-into (make-string (1- length)) #'code-char bytes)))))




;;; Rendering commands (in alphabetical order).


(define-rendering-command accum 137
  ;; *** ENUM
  (op           card32)
  (value        float32))


(define-rendering-command active-texture-arb 197
  ;; *** ENUM
  (texture      card32))


(define-rendering-command alpha-func 159
  ;; *** ENUM
  (func         card32)
  (ref          float32))


(define-rendering-command begin 4
  ;; *** ENUM
  (mode         card32))


(define-rendering-command bind-texture 4117
  ;; *** ENUM
  (target       card32)
  (texture      card32))


(define-rendering-command blend-color 4096
  (red          float32)
  (green        float32)
  (blue         float32)
  (alpha        float32))


(define-rendering-command blend-equotion 4097
  ;; *** ENUM
  (mode         card32))


(define-rendering-command blend-func 160
  ;; *** ENUM
  (sfactor      card32)
  ;; *** ENUM
  (dfactor      card32))


(define-rendering-command call-list 1
  (list         card32))


(define-rendering-command clear 127
  ;; *** BITFIELD
  (mask         card32))


(define-rendering-command clear-accum 128
  (red          float32)
  (green        float32)
  (blue         float32)
  (alpha        float32))


(define-rendering-command clear-color 130 
  (red          float32)
  (green        float32)
  (blue         float32)
  (alpha        float32))


(define-rendering-command clear-depth 132
  (depth        float64))


(define-rendering-command clear-index 129
  (c            float32))


(define-rendering-command clear-stencil 131
  (s            int32))


(define-rendering-command clip-plane 77
  (equotion-0   float64)
  (equotion-1   float64)
  (equotion-2   float64)
  (equotion-3   float64)
  ;; *** ENUM
  (plane        card32))


(define-rendering-command color-3b 6
  ((r g b)      int8))

(define-rendering-command color-3d 7
  ((r g b)      float64))

(define-rendering-command color-3f 8
  ((r g b)      float32))

(define-rendering-command color-3i 9
  ((r g b)      int32))

(define-rendering-command color-3s 10
  ((r g b)      int16))

(define-rendering-command color-3ub 11
  ((r g b)      card8))

(define-rendering-command color-3ui 12
  ((r g b)      card32))

(define-rendering-command color-3us 13
  ((r g b)      card16))


(define-rendering-command color-4b 14
  ((r g b a)    int8))

(define-rendering-command color-4d 15
  ((r g b a)    float64))

(define-rendering-command color-4f 16
  ((r g b a)    float32))

(define-rendering-command color-4i 17
  ((r g b a)    int32))

(define-rendering-command color-4s 18
  ((r g b a)    int16))

(define-rendering-command color-4ub 19
  ((r g b a)    card8))

(define-rendering-command color-4ui 20
  ((r g b a)    card32))

(define-rendering-command color-4us 21
  ((r g b a)    card16))


(define-rendering-command color-mask 134
  (red          bool)
  (green        bool)
  (blue         bool)
  (alpha        bool))


(define-rendering-command color-material 78
  ;; *** ENUM
  (face         card32)
  ;; *** ENUM
  (mode         card32))


(define-rendering-command color-table-parameter-fv 2054
  ;; *** ENUM
  (target       card32)
  ;; TODO:
  ;; +GL-COLOR-TABLE-SCALE+ (#x80D6) => (length params) = 4
  ;; +GL-COLOR-TABLE-BIAS+ (#x80d7) => (length params) = 4
  ;; else (length params) = 0 (command is erronous)
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 4)))


(define-rendering-command color-table-parameter-iv 2055
  ;; *** ENUM
  (target       card32)
  ;; TODO:
  ;; +GL-COLOR-TABLE-SCALE+ (#x80D6) => (length params) = 4
  ;; +GL-COLOR-TABLE-BIAS+ (#x80d7) => (length params) = 4
  ;; else (length params) = 0 (command is erronous)
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 4)))


(define-rendering-command convolution-parameter-f 4103
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (params       float32))


(define-rendering-command convolution-parameter-fv 4104
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 (ecase pname
                                ((#.+convolution-border-mode+
                                  #.+convolution-format+
                                  #.+convolution-width+
                                  #.+convolution-height+
                                  #.+max-convolution-width+
                                  #.+max-convolution-width+)
                                 1)
                                ((#.+convolution-filter-scale+
                                  #.+convolution-filter-bias+)
                                 4)))))


(define-rendering-command convolution-parameter-i 4105
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (params       int32))


(define-rendering-command convolution-parameter-iv 4106
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 (ecase pname
                              ((#.+convolution-border-mode+
                                #.+convolution-format+
                                #.+convolution-width+
                                #.+convolution-height+
                                #.+max-convolution-width+
                                #.+max-convolution-width+)
                               1)
                              ((#.+convolution-filter-scale+
                                #.+convolution-filter-bias+)
                               4)))))


(define-rendering-command copy-color-sub-table 196
  ;; *** ENUM
  (target       card32)
  (start        int32)
  (x            int32)
  (y            int32)
  (width        int32))


(define-rendering-command copy-color-table 2056
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (internalformat card32)
  (x            int32)
  (y            int32)
  (width        int32))


(define-rendering-command copy-convolution-filter-id 4107
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (internalformat card32)
  (x            int32)
  (y            int32)
  (width        int32))


(define-rendering-command copy-convolution-filter-2d 4108
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (internalformat card32)
  (x            int32)
  (y            int32)
  (width        int32)
  (height       int32))


(define-rendering-command copy-pixels 172
  (x            int32)
  (y            int32)
  (width        int32)
  (height       int32)
  ;; *** ENUM
  (type         card32))


(define-rendering-command copy-tex-image-1d 4119
  ;; *** ENUM
  (target       card32)
  (level        int32)
  ;; *** ENUM
  (internalformat card32)
  (x            int32)
  (y            int32)
  (width        int32)
  (border       int32))


(define-rendering-command copy-tex-image-2d 4120
  ;; *** ENUM
  (target       card32)
  (level        int32)
  ;; *** ENUM
  (internalformat card32)
  (x            int32)
  (y            int32)
  (width        int32)
  (height       int32)
  (border       int32))


(define-rendering-command copy-tex-sub-image-1d 4121
  ;; *** ENUM
  (target       card32)
  (level        int32)
  (xoffset      int32)
  (x            int32)
  (y            int32)
  (width        int32))


(define-rendering-command copy-tex-sub-image-2d 4122
  ;; *** ENUM
  (target       card32)
  (level        int32)
  (xoffset      int32)
  (yoffset      int32)
  (x            int32)
  (y            int32)
  (width        int32)
  (height       int32))


(define-rendering-command copy-tex-sub-image-3d 4123
  ;; *** ENUM
  (target       card32)
  (level        int32)
  (xoffset      int32)
  (yoffset      int32)
  (zoffset      int32)
  (x            int32)
  (y            int32)
  (width        int32)
  (height       int32))


(define-rendering-command cull-face 79
  ;; *** ENUM
  (mode         card32))


(define-rendering-command depth-func 164
  ;; *** ENUM
  (func         card32))


(define-rendering-command depth-mask 135
  (mask         bool))


(define-rendering-command depth-range 174
  (z-near       float64)
  (z-far        float64))


(define-rendering-command draw-buffer 126
  ;; *** ENUM
  (mode         card32))


(define-rendering-command edge-flag-v 22
  (flag-0       bool))


(define-rendering-command end 23)


(define-rendering-command eval-coord-1d 151
  (u-0          float64))

(define-rendering-command eval-coord-1f 152
  (u-0          float32))


(define-rendering-command eval-coord-2d 153
  ((u-0 u-1)    float64))

(define-rendering-command eval-coord-2f 154
  ((u-0 u-1)    float32))


(define-rendering-command eval-mesh-1 155
  ;; *** ENUM
  (mode         card32)
  ((i1 i2)      int32))


(define-rendering-command eval-mesh-2 157
  ;; *** ENUM
  (mode         card32)
  ((i1 i2 j1 j2) int32))


(define-rendering-command eval-point-1 156
  (i            int32))


(define-rendering-command eval-point-2 158
  (i            int32)
  (j            int32))


(define-rendering-command fog-f 80
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command fog-fv 81
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 (ecase pname
                                ((#.+fog-index+
                                  #.+fog-density+
                                  #.+fog-start+
                                  #.+fog-end+
                                  #.+fog-mode+)
                                 1)
                                ((#.+fog-color+)
                                 4)))))



(define-rendering-command fog-i 82
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command fog-iv 83
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 (ecase pname
                              ((#.+fog-index+
                                #.+fog-density+
                                #.+fog-start+
                                #.+fog-end+
                                #.+fog-mode+)
                               1)
                              ((#.+fog-color+)
                               4)))))


(define-rendering-command front-face 84
  ;; *** ENUM
  (mode         card32))


(define-rendering-command frustum 175
  (left         float64)
  (right        float64)
  (bottom       float64)
  (top          float64)
  (z-near       float64)
  (z-far        float64))


(define-rendering-command hint 85
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (mode         card32))


(define-rendering-command histogram 4110
  ;; *** ENUM
  (target       card32)
  (width        int32)
  ;; *** ENUM
  (internalformat card32)
  (sink         bool))


(define-rendering-command index-mask 136
  (mask         card32))


(define-rendering-command index-d 24
  (c-0          float64))

(define-rendering-command index-f 25
  (c-0          float32))

(define-rendering-command index-i 26
  (c-0          int32))

(define-rendering-command index-s 27
  (c-0          int16))

(define-rendering-command index-ub 194
  (c-0          card8))


(define-rendering-command init-names 121)


(define-rendering-command light-model-f 90
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command light-model-fv 91
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 (ecase pname
                                ((#.+light-model-color-control+
                                  #.+light-model-local-viewer+
                                  #.+light-model-two-side+)
                                 1)
                                ((#.+light-model-ambient+)
                                 4)))))

(define-rendering-command light-model-i 92
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command light-model-iv 93
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 (ecase pname
                              ((#.+light-model-color-control+
                                #.+light-model-local-viewer+
                                #.+light-model-two-side+)
                               1)
                              ((#.+light-model-ambient+)
                               4)))))


(define-rendering-command light-f 86
  ;; *** ENUM
  (light        card32)
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command light-fv 87
  ;; *** ENUM
  (light        card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 (ecase pname
                                ((#.+ambient+
                                  #.+diffuse+
                                  #.+specular+
                                  #.+position+)
                                 4)
                                ((#.+spot-direction+)
                                 3)
                                ((#.+spot-exponent+
                                  #.+spot-cutoff+
                                  #.+constant-attenuation+
                                  #.+linear-attenuation+
                                  #.+quadratic-attenuation+)
                                 1)))))


(define-rendering-command light-i 88
  ;; *** ENUM
  (light        card32)
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command light-iv 89
  ;; *** ENUM
  (light        card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 (ecase pname
                              ((#.+ambient+
                                #.+diffuse+
                                #.+specular+
                                #.+position+)
                               4)
                              ((#.+spot-direction+)
                               3)
                              ((#.+spot-exponent+
                                #.+spot-cutoff+
                                #.+constant-attenuation+
                                #.+linear-attenuation+
                                #.+quadratic-attenuation+)
                               1)))))


(define-rendering-command line-stipple 94
  (factor       int32)
  (pattern      card16))


(define-rendering-command line-width 95
  (width        float32))


(define-rendering-command list-base 3
  (base         card32))


(define-rendering-command load-identity 176)


(define-rendering-command load-matrix-d 178
  (m            (list float64 16)))


(define-rendering-command load-matrix-f 177
  (m            (list float32 16)))


(define-rendering-command load-name 122
  (name         card32))


(define-rendering-command logic-op 161
  ;; *** ENUM
  (name         card32))


(define-rendering-command map-grid-1d 147
  (u1           float64)
  (u2           float64)
  (un           int32))

(define-rendering-command map-grid-1f 148
  (un           int32)
  (u1           float32)
  (u2           float32))


(define-rendering-command map-grid-2d 149
  (u1           float64)
  (u2           float64)
  (v1           float64)
  (v2           float64)
  (un           int32)
  (vn           int32))


(define-rendering-command map-grid-2f 150
  (un           int32)
  (u1           float32)
  (u2           float32)
  (vn           int32)
  (v1           float32)
  (v2           float32))


(define-rendering-command material-f 96
  ;; *** ENUM
  (face         card32)
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command material-fv 97
  ;; *** ENUM
  (face         card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 (ecase pname
                                ((#.+ambient+
                                  #.+diffuse+
                                  #.+specular+
                                  #.+emission+
                                  #.+ambient-and-diffuse+)
                                 4)
                                ((#.+shininess+)
                                 1)
                                ((#.+color-index+)
                                 3)))))


(define-rendering-command material-i 98
  ;; *** ENUM
  (face         card32)
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command material-iv 99
  ;; *** ENUM
  (face         card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 (ecase pname
                              ((#.+ambient+
                                #.+diffuse+
                                #.+specular+
                                #.+emission+
                                #.+ambient-and-diffuse+)
                               4)
                              ((#.+shininess+)
                               1)
                              ((#.+color-index+)
                               3)))))


(define-rendering-command matrix-mode 179
  ;; *** ENUM
  (mode         card32))


(define-rendering-command minmax 4111
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (internalformat card32)
  (sink         bool))


(define-rendering-command mult-matrix-d 181
  (m            (list float64 16)))


(define-rendering-command mult-matrix-f 180
  (m            (list float32 16)))


;;; *** Note that TARGET is placed last for FLOAT64 versions.
(define-rendering-command multi-tex-coord-1d-arb 198
  (v-0          float64)
  ;; *** ENUM
  (target       card32))

(define-rendering-command multi-tex-coord-1f-arb 199
  ;; *** ENUM
  (target       card32)
  (v-0          float32))

(define-rendering-command multi-tex-coord-1i-arb 200
  ;; *** ENUM
  (target       card32)
  (v-0          int32))

(define-rendering-command multi-tex-coord-1s-arb 201
  ;; *** ENUM
  (target       card32)
  (v-0          int16))


(define-rendering-command multi-tex-coord-2d-arb 202
  ((v-0 v-1)    float64)
  ;; *** ENUM
  (target       card32))

(define-rendering-command multi-tex-coord-2f-arb 203
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1)    float32))

(define-rendering-command multi-tex-coord-2i-arb 204
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1)    int32))

(define-rendering-command multi-tex-coord-2s-arb 205
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1)    int16))


(define-rendering-command multi-tex-coord-3d-arb 206
  ((v-0 v-1 v-2) float64)
  ;; *** ENUM
  (target       card32))

(define-rendering-command multi-tex-coord-3f-arb 207
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1 v-2) float32))

(define-rendering-command multi-tex-coord-3i-arb 208
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1 v-2) int32))

(define-rendering-command multi-tex-coord-3s-arb 209
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1 v-2) int16))


(define-rendering-command multi-tex-coord-4d-arb 210
  ((v-0 v-1 v-2 v-3) float64)
  ;; *** ENUM
  (target       card32))

(define-rendering-command multi-tex-coord-4f-arb 211
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1 v-2 v-3) float32))

(define-rendering-command multi-tex-coord-4i-arb 212
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1 v-2 v-3) int32))

(define-rendering-command multi-tex-coord-4s-arb 213
  ;; *** ENUM
  (target       card32)
  ((v-0 v-1 v-2 v-3) int16))


(define-rendering-command normal-3b 28
  ((v-0 v-1 v-2) int8))

(define-rendering-command normal-3d 29
  ((v-0 v-1 v-2) float64))

(define-rendering-command normal-3f 30
  ((v-0 v-1 v-2) float32))

(define-rendering-command normal-3i 31
  ((v-0 v-1 v-2) int32))

(define-rendering-command normal-3s 32
  ((v-0 v-1 v-2) int16))


(define-rendering-command ortho 182
  (left         float64)
  (right        float64)
  (bottom       float64)
  (top          float64)
  (z-near       float64)
  (z-far        float64))


(define-rendering-command pass-through 123
  (token        float32))


(define-rendering-command pixel-transfer-f 166
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command pixel-transfer-i 167
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command pixel-zoom 165
  (xfactor      float32)
  (yfactor      float32))


(define-rendering-command point-size 100
  (size         float32))


(define-rendering-command polygon-mode 101
  ;; *** ENUM
  (face         card32)
  ;; *** ENUM
  (mode         card32))


(define-rendering-command polygon-offset 192
  (factor       float32)
  (units        float32))


(define-rendering-command pop-attrib 141)


(define-rendering-command pop-matrix 183)


(define-rendering-command pop-name 124)


(define-rendering-command prioritize-textures 4118
  (n            int32)
  (textures     (list card32 n))
  (priorities   (list float32 n)))


(define-rendering-command push-attrib 142
  ;; *** BITFIELD
  (mask         card32))


(define-rendering-command push-matrix 184)


(define-rendering-command push-name 125
  (name         card32))


(define-rendering-command raster-pos-2d 33
  ((v-0 v-1)    float64))

(define-rendering-command raster-pos-2f 34
  ((v-0 v-1)    float32))

(define-rendering-command raster-pos-2i 35
  ((v-0 v-1)    int32))

(define-rendering-command raster-pos-2s 36
  ((v-0 v-1)    int16))


(define-rendering-command raster-pos-3d 37
  ((v-0 v-1 v-2) float64))

(define-rendering-command raster-pos-3f 38
  ((v-0 v-1 v-2) float32))

(define-rendering-command raster-pos-3i 39
  ((v-0 v-1 v-2) int32))

(define-rendering-command raster-pos-3s 40
  ((v-0 v-1 v-2) int16))


(define-rendering-command raster-pos-4d 41
  ((v-0 v-1 v-2 v-3) float64))

(define-rendering-command raster-pos-4f 42
  ((v-0 v-1 v-2 v-3) float32))

(define-rendering-command raster-pos-4i 43
  ((v-0 v-1 v-2 v-3) int32))

(define-rendering-command raster-pos-4s 44
  ((v-0 v-1 v-2 v-3) int16))


(define-rendering-command read-buffer 171
  ;; *** ENUM
  (mode         card32))


(define-rendering-command rect-d 45
  ((v1-0 v1-1 v2-0 v2-1) float64))

(define-rendering-command rect-f 46
  ((v1-0 v1-1 v2-0 v2-1) float32))

(define-rendering-command rect-i 47
  ((v1-0 v1-1 v2-0 v2-1) int32))

(define-rendering-command rect-s 48
  ((v1-0 v1-1 v2-0 v2-1) int16))


(define-rendering-command reset-histogram 4112
  ;; *** ENUM
  (target       card32))


(define-rendering-command reset-minmax 4113
  ;; *** ENUM
  (target       card32))


(define-rendering-command rotate-d 185
  ((angle x y z) float64))


(define-rendering-command rotate-f 186
  ((angle x y z) float32))


(define-rendering-command scale-d 187
  ((x y z)      float64))


(define-rendering-command scale-f 188
  ((x y z)      float32))


(define-rendering-command scissor 103
  ((x y width height) int32))


(define-rendering-command shade-model 104
  ;; *** ENUM
  (mode         card32))


(define-rendering-command stencil-func 162
  ;; *** ENUM
  (func         card32)
  (ref          int32)
  (mask         card32))


(define-rendering-command stencil-mask 133
  (mask         card32))


(define-rendering-command stencil-op 163
  ;; *** ENUM
  (fail         card32)
  ;; *** ENUM
  (zfail        card32)
  ;; *** ENUM
  (zpass        card32))


(define-rendering-command tex-env-f 111
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command tex-env-fv 112
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (param        (list float32 (ecase pname
                                (#.+texture-env-mode+ 1)
                                (#.+texture-env-color+ 4)))))


(define-rendering-command tex-env-i 113
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command tex-env-iv 114
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (param        (list int32 (ecase pname
                              (#.+texture-env-mode+ 1)
                              (#.+texture-env-color+ 4)))))


;;; *** 
;;; last there.
(define-rendering-command tex-gen-d 115
  (param        float64)
  ;; *** ENUM
  (coord        card32)
  ;; *** ENUM
  (pname        card32))


(define-rendering-command tex-gen-dv 116
  ;; *** ENUM
  (coord        card32)
  ;; *** ENUM
  (pname        card32)
  ;; +texture-gen-mode+ n=1
  ;; +object-plane+     n=4
  ;; +eye-plane+        n=1
  (params       (list float64 (ecase pname
                                ((#.+texture-gen-mode+ #.+eye-plane+) 1)
                                (#.+object-plane+ 4)))))


(define-rendering-command tex-gen-f 117
  ;; *** ENUM
  (coord        card32)
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command tex-gen-fv 118
  ;; *** ENUM
  (coord        card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 (ecase pname
                                ((#.+texture-gen-mode+ #.+eye-plane+) 1)
                                (#.+object-plane+ 4)))))


(define-rendering-command tex-gen-i 119
  ;; *** ENUM
  (coord        card32)
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command tex-gen-iv 120
  ;; *** ENUM
  (coord        card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 (ecase pname
                              ((#.+texture-gen-mode+ #.+eye-plane+) 1)
                              (#.+object-plane+ 4)))))


(define-rendering-command tex-parameter-f 105
  ;; *** ENUM
  (target        card32)
  ;; *** ENUM
  (pname        card32)
  (param        float32))


(define-rendering-command tex-parameter-fv 106
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list float32 (ecase pname
                                ((#.+texture-border-color+)
                                 4)
                                ((#.+texture-mag-filter+
                                  #.+texture-min-filter+
                                  #.+texture-wrap-s+
                                  #.+texture-wrap-t+)
                                 1)))))


(define-rendering-command tex-parameter-i 107
  ;; *** ENUM
  (target        card32)
  ;; *** ENUM
  (pname        card32)
  (param        int32))


(define-rendering-command tex-parameter-iv 108
  ;; *** ENUM
  (target       card32)
  ;; *** ENUM
  (pname        card32)
  (params       (list int32 (ecase pname
                              ((#.+texture-border-color+)
                               4)
                              ((#.+texture-mag-filter+
                                #.+texture-min-filter+
                                #.+texture-wrap-s+
                                #.+texture-wrap-t+)
                               1)))))


(define-rendering-command translate-d 189
  ((x y z)      float64))

(define-rendering-command translate-f 190
  ((x y z)      float32))


(define-rendering-command vertex-2d 65
  ((x y)        float64))

(define-rendering-command vertex-2f 66
  ((x y)        float32))

(define-rendering-command vertex-2i 67
  ((x y)        int32))

(define-rendering-command vertex-2s 68
  ((x y)        int16))


(define-rendering-command vertex-3d 69
  ((x y z)      float64))

(define-rendering-command vertex-3f 70
  ((x y z)      float32))

(define-rendering-command vertex-3i 71
  ((x y z)      int32))

(define-rendering-command vertex-3s 72
  ((x y z)      int16))


(define-rendering-command vertex-4d 73
  ((x y z w)    float64))

(define-rendering-command vertex-4f 74
  ((x y z w)    float32))

(define-rendering-command vertex-4i 75
  ((x y z w)    int32))

(define-rendering-command vertex-4s 76
  ((x y z w)    int16))


(define-rendering-command viewport 191
  ((x y width height) int32))


;;; Potentially lerge rendering commands.


#-(and)
(define-large-rendering-command call-lists 2
  (n            int32)
  ;; *** ENUM
  (type         card32)
  (lists        (list type n)))



;;; Requests for GL non-rendering commands.

(defun new-list (list mode)
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +new-list+)
      ;; *** GLX_CONTEXT_TAG
      (card32 (context-tag ctx))
      (card32 list)
      ;; *** ENUM
      (card32 mode))))


(defun gen-lists (range)
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +gen-lists+)
         ;; *** GLX_CONTEXT_TAG
         (card32 (context-tag ctx))
         (integer range))
      (card32-get 8))))


(defun end-list ()
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +end-list+)
      ;; *** GLX_CONTEXT_TAG
      (card32 (context-tag ctx)))))


(defun enable (cap)
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +enable+)
         ;; *** GLX_CONTEXT_TAG
         (card32 (context-tag ctx))
         ;; *** ENUM?
         (card32 cap)))))


;;; FIXME: FLUSH and FINISH should send *all* buffered data, including
;;; buffered rendering commands.
(defun flush ()
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +flush+)
      ;; *** GLX_CONTEXT_TAG
      (card32 (context-tag ctx)))))


(defun finish ()
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +finish+)
         ;; *** GLX_CONTEXT_TAG
         (card32 (context-tag ctx))))))
