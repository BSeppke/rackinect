#lang racket

;; Module constants
(define rackinect-path (collection-path "rackinect"))
(define rackinect-version "1.0.0")

;; Load dll under windows, dylib under MacOS
(define rackinect-dylib-file
    (cond ((equal? (system-type 'os) 'windows) "kinect-grab_c.dll")
          ((equal? (system-type 'os) 'macosx)  "libkinect-grab_c.dylib")
          ((equal? (system-type 'os) 'unix)    "libkinect-grab_c.so")
          (else (error "Only macosx, windows and unix are supported"))))
(define rackinect-dylib-path (build-path rackinect-path rackinect-dylib-file))

 
;; For Windows: Add the dll directory to the systems path:
(when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string rackinect-path) ";" (getenv "PATH"))))

(provide rackinect-path
         rackinect-version
         rackinect-dylib-path)

