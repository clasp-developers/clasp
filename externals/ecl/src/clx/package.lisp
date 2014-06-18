;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*-

;;; Copyright 1990 Massachusetts Institute of Technology, Cambridge,
;;; Massachusetts.  All Rights Reserved.
;;; 
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted, provided
;;; that the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation, and that the name MIT not be used in advertising or
;;; publicity pertaining to distribution of the software without specific,
;;; written prior permission.

;;; The CLtL way

#-clx-ansi-common-lisp 
(lisp:in-package :xlib :use '(:lisp))

#+(and (or kcl ibcl) (not clx-ansi-common-lisp))
(shadow 
  '(
    rational
    ))

#+(and CMU (not clx-ansi-common-lisp))
(shadow '(define-condition))

#+(and lispm (not clx-ansi-common-lisp))
(import
  '(
    sys:arglist
    sys:with-stack-list
    sys:with-stack-list*
    ))

#+(and Genera (not clx-ansi-common-lisp))
(import
  '(
    future-common-lisp:print-unreadable-object
    future-common-lisp:with-standard-io-syntax
    zwei:indentation
    ))

#+(and lcl3.0 (not clx-ansi-common-lisp))
(import
  '(
    lcl:arglist
    lcl:dynamic-extent
    lcl:type-error
    lucid::type-error-datum
    lucid::type-error-expected-type
    ))

#+(and excl (not clx-ansi-common-lisp)) 
(import
  '(
    excl::arglist
    excl::dynamic-extent
    excl::type-error
    excl::type-error-datum
    excl::type-error-expected-type
    ))

#+(and allegro (not clx-ansi-common-lisp))
(import
  '(
    excl::without-interrupts
    ))

#-clx-ansi-common-lisp
(export
  '(
    *version* access-control access-error access-hosts
    activate-screen-saver add-access-host add-resource add-to-save-set
    alist alloc-color alloc-color-cells alloc-color-planes alloc-error
    allow-events angle arc-seq array-index atom-error atom-name
    bell bit-gravity bitmap bitmap-format bitmap-format-lsb-first-p
    bitmap-format-p bitmap-format-pad bitmap-format-unit bitmap-image
    boole-constant boolean card16 card29 card32 card8
    card8->char change-active-pointer-grab change-keyboard-control
    change-keyboard-mapping change-pointer-control change-property
    char->card8 char-ascent char-attributes char-descent
    char-left-bearing char-right-bearing char-width character->keysyms
    character-in-map-p circulate-window-down circulate-window-up clear-area
    close-display close-down-mode close-font closed-display color
    color-blue color-green color-p color-red color-rgb colormap
    colormap-display colormap-equal colormap-error colormap-id colormap-p
    colormap-plist colormap-visual-info connection-failure convert-selection
    copy-area copy-colormap-and-free copy-gcontext copy-gcontext-components
    copy-image copy-plane create-colormap create-cursor
    create-gcontext create-glyph-cursor create-image create-pixmap
    create-window cursor cursor-display cursor-equal cursor-error
    cursor-id cursor-p cursor-plist cut-buffer declare-event decode-core-error
    default-error-handler default-keysym-index default-keysym-translate
    define-error define-extension define-gcontext-accessor
    define-keysym define-keysym-set delete-property delete-resource
    destroy-subwindows destroy-window device-busy device-event-mask
    device-event-mask-class discard-current-event discard-font-info display
    display-after-function display-authorization-data display-authorization-name
    display-bitmap-format display-byte-order display-default-screen
    display-display display-error-handler 
    display-extended-max-request-length display-finish-output
    display-force-output display-host display-image-lsb-first-p
    display-invoke-after-function display-keycode-range display-max-keycode
    display-max-request-length display-min-keycode display-motion-buffer-size
    display-nscreens display-p display-pixmap-formats display-plist
    display-protocol-major-version display-protocol-minor-version
    display-protocol-version display-release-number
    display-report-asynchronous-errors display-resource-id-base
    display-resource-id-mask display-roots display-vendor
    display-vendor-name display-xdefaults display-xid draw-arc
    draw-arcs draw-direction draw-glyph draw-glyphs draw-image-glyph
    draw-image-glyphs draw-line draw-lines draw-point draw-points
    draw-rectangle draw-rectangles draw-segments drawable
    drawable-border-width drawable-depth drawable-display drawable-equal
    drawable-error drawable-height drawable-id drawable-p
    drawable-plist drawable-root drawable-width drawable-x drawable-y
    error-key event-case event-cond event-handler event-key
    event-listen event-mask event-mask-class extension-opcode
    find-atom font font-all-chars-exist-p font-ascent
    font-default-char font-descent font-direction font-display
    font-equal font-error font-id font-max-byte1 font-max-byte2
    font-max-char font-min-byte1 font-min-byte2 font-min-char
    font-name font-p font-path font-plist font-properties
    font-property fontable force-gcontext-changes free-colormap
    free-colors free-cursor free-gcontext free-pixmap gcontext
    gcontext-arc-mode gcontext-background 
    gcontext-cache-p gcontext-cap-style
    gcontext-clip-mask gcontext-clip-ordering gcontext-clip-x
    gcontext-clip-y gcontext-dash-offset gcontext-dashes gcontext-display
    gcontext-equal gcontext-error gcontext-exposures gcontext-fill-rule
    gcontext-fill-style gcontext-font gcontext-foreground gcontext-function
    gcontext-id gcontext-join-style gcontext-key gcontext-line-style
    gcontext-line-width gcontext-p gcontext-plane-mask gcontext-plist
    gcontext-stipple gcontext-subwindow-mode gcontext-tile gcontext-ts-x
    gcontext-ts-y generalized-boolean get-external-event-code get-image get-property
    get-raw-image get-resource get-search-resource get-search-table
    get-standard-colormap get-wm-class global-pointer-position grab-button
    grab-key grab-keyboard grab-pointer grab-server grab-status
    icon-sizes iconify-window id-choice-error illegal-request-error
    image image-blue-mask image-depth image-green-mask image-height
    image-name image-pixmap image-plist image-red-mask image-width
    image-x image-x-hot image-x-p image-xy image-xy-bitmap-list
    image-xy-p image-y-hot image-z image-z-bits-per-pixel image-z-p
    image-z-pixarray implementation-error input-focus install-colormap
    installed-colormaps int16 int32 int8 intern-atom invalid-font
    keyboard-control keyboard-mapping keycode->character keycode->keysym
    keysym keysym->character keysym->keycodes keysym-in-map-p
    keysym-set kill-client kill-temporary-clients length-error
    list-extensions list-font-names list-fonts list-properties
    lookup-color lookup-error make-color make-event-handlers
    make-event-keys make-event-mask make-resource-database make-state-keys
    make-state-mask make-wm-hints make-wm-size-hints map-resource
    map-subwindows map-window mapping-notify mask16 mask32
    match-error max-char-ascent max-char-attributes max-char-descent
    max-char-left-bearing max-char-right-bearing max-char-width
    merge-resources min-char-ascent min-char-attributes min-char-descent
    min-char-left-bearing min-char-right-bearing min-char-width
    missing-parameter modifier-key modifier-mapping modifier-mask
    motion-events name-error no-operation open-display open-font
    pixarray pixel pixmap pixmap-display pixmap-equal
    pixmap-error pixmap-format pixmap-format-bits-per-pixel
    pixmap-format-depth pixmap-format-p pixmap-format-scanline-pad
    pixmap-id pixmap-p pixmap-plist point-seq pointer-control
    pointer-event-mask pointer-event-mask-class pointer-mapping
    pointer-position process-event put-image put-raw-image
    query-best-cursor query-best-stipple query-best-tile query-colors
    query-extension query-keymap query-pointer query-tree queue-event
    read-bitmap-file read-resources recolor-cursor rect-seq
    remove-access-host remove-from-save-set reparent-window repeat-seq
    reply-length-error reply-timeout request-error reset-screen-saver
    resource-database resource-database-timestamp resource-error
    resource-id resource-key rgb-colormaps rgb-val root-resources
    rotate-cut-buffers rotate-properties screen screen-backing-stores
    screen-black-pixel screen-default-colormap screen-depths
    screen-event-mask-at-open screen-height screen-height-in-millimeters
    screen-max-installed-maps screen-min-installed-maps screen-p
    screen-plist screen-root screen-root-depth screen-root-visual
    screen-root-visual-info screen-save-unders-p screen-saver
    screen-white-pixel screen-width screen-width-in-millimeters seg-seq
    selection-owner send-event sequence-error set-access-control
    set-close-down-mode set-input-focus set-modifier-mapping
    set-pointer-mapping set-screen-saver set-selection-owner
    set-standard-colormap set-standard-properties set-wm-class
    set-wm-properties set-wm-resources state-keysym-p state-mask-key
    store-color store-colors stringable text-extents text-width
    timestamp transient-for translate-coordinates translate-default
    translation-function type-error undefine-keysym unexpected-reply
    ungrab-button ungrab-key ungrab-keyboard ungrab-pointer
    ungrab-server uninstall-colormap unknown-error unmap-subwindows
    unmap-window value-error visual-info visual-info-bits-per-rgb
    visual-info-blue-mask visual-info-class visual-info-colormap-entries
    visual-info-display visual-info-green-mask visual-info-id visual-info-p
    visual-info-plist visual-info-red-mask warp-pointer
    warp-pointer-if-inside warp-pointer-relative warp-pointer-relative-if-inside
    win-gravity window window-all-event-masks window-background
    window-backing-pixel window-backing-planes window-backing-store
    window-bit-gravity window-border window-class window-colormap
    window-colormap-installed-p window-cursor window-display
    window-do-not-propagate-mask window-equal window-error
    window-event-mask window-gravity window-id window-map-state
    window-override-redirect window-p window-plist window-priority
    window-save-under window-visual window-visual-info with-display
    with-event-queue with-gcontext with-server-grabbed with-state
    withdraw-window wm-client-machine wm-colormap-windows wm-command
    wm-hints wm-hints-flags wm-hints-icon-mask wm-hints-icon-pixmap
    wm-hints-icon-window wm-hints-icon-x wm-hints-icon-y
    wm-hints-initial-state wm-hints-input wm-hints-p wm-hints-window-group
    wm-icon-name wm-name wm-normal-hints wm-protocols wm-resources
    wm-size-hints wm-size-hints-base-height wm-size-hints-base-width
    wm-size-hints-height wm-size-hints-height-inc wm-size-hints-max-aspect
    wm-size-hints-max-height wm-size-hints-max-width wm-size-hints-min-aspect
    wm-size-hints-min-height wm-size-hints-min-width wm-size-hints-p
    wm-size-hints-user-specified-position-p wm-size-hints-user-specified-size-p
    wm-size-hints-width wm-size-hints-width-inc wm-size-hints-win-gravity
    wm-size-hints-x wm-size-hints-y wm-zoom-hints write-bitmap-file
    write-resources xatom
    ))


;;; The ANSI Common Lisp way

#+(and Genera clx-ansi-common-lisp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* si:*ansi-common-lisp-readtable*))

#+clx-ansi-common-lisp
(common-lisp:in-package :common-lisp-user)

#+ecl
(eval-when (#-stage1 :compile-toplevel :load-toplevel #-stage1 :execute)
  (require 'sockets))


#+clx-ansi-common-lisp
(defpackage xlib
  (:use common-lisp)
  (:size 3000)
  #+(or kcl ibcl) (:shadow rational)
  #+allegro (:use cltl1)
  #+allegro (:import-from excl without-interrupts)
  #+excl (:import-from excl arglist)
  #+Genera (:import-from zwei indentation)
  #+lcl3.0 (:import-from lcl arglist)
  #+lispm (:import-from lisp char-bit)
  #+lispm (:import-from sys arglist with-stack-list with-stack-list*)
  #+(or sbcl ecl) (:use sb-bsd-sockets)
  (:export
    *version* access-control access-error access-hosts
    activate-screen-saver add-access-host add-resource add-to-save-set
    alist alloc-color alloc-color-cells alloc-color-planes alloc-error
    allow-events angle arc-seq array-index atom-error atom-name
    bell bit-gravity bitmap bitmap-format bitmap-format-lsb-first-p
    bitmap-format-p bitmap-format-pad bitmap-format-unit bitmap-image
    boole-constant boolean card16 card29 card32 card8
    card8->char change-active-pointer-grab change-keyboard-control
    change-keyboard-mapping change-pointer-control change-property
    char->card8 char-ascent char-attributes char-descent
    char-left-bearing char-right-bearing char-width character->keysyms
    character-in-map-p circulate-window-down circulate-window-up clear-area
    close-display close-down-mode close-font closed-display color
    color-blue color-green color-p color-red color-rgb colormap
    colormap-display colormap-equal colormap-error colormap-id colormap-p
    colormap-plist colormap-visual-info connection-failure convert-selection
    copy-area copy-colormap-and-free copy-gcontext copy-gcontext-components
    copy-image copy-plane create-colormap create-cursor
    create-gcontext create-glyph-cursor create-image create-pixmap
    create-window cursor cursor-display cursor-equal cursor-error
    cursor-id cursor-p cursor-plist cut-buffer declare-event decode-core-error
    default-error-handler default-keysym-index default-keysym-translate
    define-error define-extension define-gcontext-accessor
    define-keysym define-keysym-set delete-property delete-resource
    destroy-subwindows destroy-window device-busy device-event-mask
    device-event-mask-class discard-current-event discard-font-info display
    display-after-function display-authorization-data display-authorization-name
    display-bitmap-format display-byte-order display-default-screen
    display-display display-error-handler 
    display-extended-max-request-length display-finish-output
    display-force-output display-host display-image-lsb-first-p
    display-invoke-after-function display-keycode-range display-max-keycode
    display-max-request-length display-min-keycode display-motion-buffer-size
    display-nscreens display-p display-pixmap-formats display-plist
    display-protocol-major-version display-protocol-minor-version
    display-protocol-version display-release-number
    display-report-asynchronous-errors display-resource-id-base
    display-resource-id-mask display-roots display-vendor
    display-vendor-name display-xdefaults display-xid draw-arc
    draw-arcs draw-direction draw-glyph draw-glyphs draw-image-glyph
    draw-image-glyphs draw-line draw-lines draw-point draw-points
    draw-rectangle draw-rectangles draw-segments drawable
    drawable-border-width drawable-depth drawable-display drawable-equal
    drawable-error drawable-height drawable-id drawable-p
    drawable-plist drawable-root drawable-width drawable-x drawable-y
    error-key event-case event-cond event-handler event-key
    event-listen event-mask event-mask-class extension-opcode
    find-atom font font-all-chars-exist-p font-ascent
    font-default-char font-descent font-direction font-display
    font-equal font-error font-id font-max-byte1 font-max-byte2
    font-max-char font-min-byte1 font-min-byte2 font-min-char
    font-name font-p font-path font-plist font-properties
    font-property fontable force-gcontext-changes free-colormap
    free-colors free-cursor free-gcontext free-pixmap gcontext
    gcontext-arc-mode gcontext-background
    gcontext-cache-p gcontext-cap-style
    gcontext-clip-mask gcontext-clip-ordering gcontext-clip-x
    gcontext-clip-y gcontext-dash-offset gcontext-dashes gcontext-display
    gcontext-equal gcontext-error gcontext-exposures gcontext-fill-rule
    gcontext-fill-style gcontext-font gcontext-foreground gcontext-function
    gcontext-id gcontext-join-style gcontext-key gcontext-line-style
    gcontext-line-width gcontext-p gcontext-plane-mask gcontext-plist
    gcontext-stipple gcontext-subwindow-mode gcontext-tile gcontext-ts-x
    gcontext-ts-y generalized-boolean get-external-event-code get-image get-property
    get-raw-image get-resource get-search-resource get-search-table
    get-standard-colormap get-wm-class global-pointer-position grab-button
    grab-key grab-keyboard grab-pointer grab-server grab-status
    icon-sizes iconify-window id-choice-error illegal-request-error
    image image-blue-mask image-depth image-green-mask image-height
    image-name image-pixmap image-plist image-red-mask image-width
    image-x image-x-hot image-x-p image-xy image-xy-bitmap-list
    image-xy-p image-y-hot image-z image-z-bits-per-pixel image-z-p
    image-z-pixarray implementation-error input-focus install-colormap
    installed-colormaps int16 int32 int8 intern-atom invalid-font
    keyboard-control keyboard-mapping keycode->character keycode->keysym
    keysym keysym->character keysym->keycodes keysym-in-map-p
    keysym-set kill-client kill-temporary-clients length-error
    list-extensions list-font-names list-fonts list-properties
    lookup-color lookup-error make-color make-event-handlers
    make-event-keys make-event-mask make-resource-database make-state-keys
    make-state-mask make-wm-hints make-wm-size-hints map-resource
    map-subwindows map-window mapping-notify mask16 mask32
    match-error max-char-ascent max-char-attributes max-char-descent
    max-char-left-bearing max-char-right-bearing max-char-width
    merge-resources min-char-ascent min-char-attributes min-char-descent
    min-char-left-bearing min-char-right-bearing min-char-width
    missing-parameter modifier-key modifier-mapping modifier-mask
    motion-events name-error no-operation 
    open-default-display open-display open-font
    pixarray pixel pixmap pixmap-display pixmap-equal
    pixmap-error pixmap-format pixmap-format-bits-per-pixel
    pixmap-format-depth pixmap-format-p pixmap-format-scanline-pad
    pixmap-id pixmap-p pixmap-plist point-seq pointer-control
    pointer-event-mask pointer-event-mask-class pointer-mapping
    pointer-position process-event put-image put-raw-image
    query-best-cursor query-best-stipple query-best-tile query-colors
    query-extension query-keymap query-pointer query-tree queue-event
    read-bitmap-file read-resources recolor-cursor rect-seq
    remove-access-host remove-from-save-set reparent-window repeat-seq
    reply-length-error reply-timeout request-error reset-screen-saver
    resource-database resource-database-timestamp resource-error
    resource-id resource-key rgb-colormaps rgb-val root-resources
    rotate-cut-buffers rotate-properties screen screen-backing-stores
    screen-black-pixel screen-default-colormap screen-depths
    screen-event-mask-at-open screen-height screen-height-in-millimeters
    screen-max-installed-maps screen-min-installed-maps screen-p
    screen-plist screen-root screen-root-depth screen-root-visual
    screen-root-visual-info screen-save-unders-p screen-saver
    screen-white-pixel screen-width screen-width-in-millimeters seg-seq
    selection-owner send-event sequence-error set-access-control
    set-close-down-mode set-input-focus set-modifier-mapping
    set-pointer-mapping set-screen-saver set-selection-owner
    set-standard-colormap set-standard-properties set-wm-class
    set-wm-properties set-wm-resources state-keysym-p state-mask-key
    store-color store-colors stringable text-extents text-width
    timestamp transient-for translate-coordinates translate-default
    translation-function undefine-keysym unexpected-reply
    ungrab-button ungrab-key ungrab-keyboard ungrab-pointer
    ungrab-server uninstall-colormap unknown-error unmap-subwindows
    unmap-window value-error visual-info visual-info-bits-per-rgb
    visual-info-blue-mask visual-info-class visual-info-colormap-entries
    visual-info-display visual-info-green-mask visual-info-id visual-info-p
    visual-info-plist visual-info-red-mask warp-pointer
    warp-pointer-if-inside warp-pointer-relative warp-pointer-relative-if-inside
    win-gravity window window-all-event-masks window-background
    window-backing-pixel window-backing-planes window-backing-store
    window-bit-gravity window-border window-class window-colormap
    window-colormap-installed-p window-cursor window-display
    window-do-not-propagate-mask window-equal window-error
    window-event-mask window-gravity window-id window-map-state
    window-override-redirect window-p window-plist window-priority
    window-save-under window-visual window-visual-info with-display
    with-event-queue with-gcontext with-server-grabbed with-state
    withdraw-window wm-client-machine wm-colormap-windows wm-command
    wm-hints wm-hints-flags wm-hints-icon-mask wm-hints-icon-pixmap
    wm-hints-icon-window wm-hints-icon-x wm-hints-icon-y
    wm-hints-initial-state wm-hints-input wm-hints-p wm-hints-window-group
    wm-icon-name wm-name wm-normal-hints wm-protocols wm-resources
    wm-size-hints wm-size-hints-base-height wm-size-hints-base-width
    wm-size-hints-height wm-size-hints-height-inc wm-size-hints-max-aspect
    wm-size-hints-max-height wm-size-hints-max-width wm-size-hints-min-aspect
    wm-size-hints-min-height wm-size-hints-min-width wm-size-hints-p
    wm-size-hints-user-specified-position-p wm-size-hints-user-specified-size-p
    wm-size-hints-width wm-size-hints-width-inc wm-size-hints-win-gravity
    wm-size-hints-x wm-size-hints-y wm-zoom-hints write-bitmap-file
    write-resources xatom))



