;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file contains definitions for vectorized access
;;; to specialized lisp arrays.
;;;

(in-package #:SSE)

;;; Prefetch: AREF-PREFETCH-*, ROW-MAJOR-AREF-PREFETCH-*

(def-aref-intrinsic #:PREFETCH-T0 nil cpu-prefetch-t0 nil :ref-size 0)
(def-aref-intrinsic #:PREFETCH-T1 nil cpu-prefetch-t1 nil :ref-size 0)
(def-aref-intrinsic #:PREFETCH-T2 nil cpu-prefetch-t2 nil :ref-size 0)
(def-aref-intrinsic #:PREFETCH-NTA nil cpu-prefetch-nta nil :ref-size 0)

(def-aref-intrinsic #:CLFLUSH nil cpu-clflush nil :ref-size 1)

;;; Single-float

;; AREF-SS, ROW-MAJOR-AREF-SS

(def-aref-intrinsic #:SS float-sse-pack mem-ref-ss mem-set-ss :ref-size 4)

;; AREF-PS, ROW-MAJOR-AREF-PS

(def-aref-intrinsic #:PS float-sse-pack mem-ref-ps mem-set-ps)

;; AREF-APS, ROW-MAJOR-AREF-APS (requires alignment)

(def-aref-intrinsic #:APS float-sse-pack mem-ref-aps mem-set-aps)

;; AREF-SPS, ROW-MAJOR-AREF-SPS (requires alignment; no write cache)

(def-aref-intrinsic #:SPS float-sse-pack mem-ref-aps stream-ps)

;;; Double-float

;; AREF-SD, ROW-MAJOR-AREF-SD

(def-aref-intrinsic #:SD double-sse-pack mem-ref-sd mem-set-sd :ref-size 8)

;; AREF-PD, ROW-MAJOR-AREF-PD

(def-aref-intrinsic #:PD double-sse-pack mem-ref-pd mem-set-pd)

;; AREF-APD, ROW-MAJOR-AREF-APD (requires alignment)

(def-aref-intrinsic #:APD double-sse-pack mem-ref-apd mem-set-apd)

;; AREF-SPD, ROW-MAJOR-AREF-SPD (requires alignment; no write cache)

(def-aref-intrinsic #:SPD double-sse-pack mem-ref-apd stream-pd)

;;; Integer

;; AREF-SI64, ROW-MAJOR-AREF-SI64

(def-aref-intrinsic #:SI64 int-sse-pack mem-ref-si64 mem-set-si64 :ref-size 8)

;; AREF-PI, ROW-MAJOR-AREF-PI

(def-aref-intrinsic #:PI int-sse-pack mem-ref-pi mem-set-pi)

;; AREF-API, ROW-MAJOR-AREF-API (requires alignment)

(def-aref-intrinsic #:API int-sse-pack mem-ref-api mem-set-api)

;; AREF-SPI, ROW-MAJOR-AREF-SPI (requires alignment; no write cache)

(def-aref-intrinsic #:SPI int-sse-pack mem-ref-api stream-pi)

