#lang racket

(require ffi/unsafe)

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

;; The compilation routine (at least for macosx and unix)
(define (rebuild-rackinect)
  (define old_path (current-directory))
  (define rackinect-grab_c-path (build-path rackinect-path "kinect-grab_c"))
  (define racket-bits (* 8 (ctype-sizeof _pointer)))
  (begin
    (if (or (equal? (system-type 'os) 'macosx)
            (equal? (system-type 'os) 'unix))
        (begin 
          (display "-------------- BUILDING LIBFREENECT-C-WRAPPER FOR KINECT GRABBING --------------")
          (newline)
          (current-directory rackinect-grab_c-path)
          (if (system* (format "build-~a.sh" racket-bits))
              (begin
                (copy-file (build-path (current-directory) "bin"  rackinect-dylib-file) rackinect-dylib-path #t)
                #t)
              (error "making the rackinect-grab_c lib failed, although vigra seems to be installed"))
          (current-directory old_path))
        ;;For windows: Just copy the correct binaries (no system called needed herein
        (let ((bindir     (build-path rackinect-grab_c-path "bin" (string-append "win"(number->string racket-bits)))))
          (if (equal? (system-type 'os) 'windows)
              (let* ((binaries   (find-files (compose (curry equal?  #".dll") path-get-extension)  bindir))
                     (result     (map (lambda (f) (copy-file f (path->string (build-path rackinect-path (file-name-from-path f))) #t)) binaries)))
                (if (foldl (lambda (x y) (and x y)) #t result)
                    #t
                    (error (string-append "Copying of rackinect-grab_c's binaries from " (path->string bindir) " to "  (path->string rackinect-path) " failed !"))))
              (error "Only windows, Mac OS X and Unix are supported!"))))
    ;;If it still fails loading -> give up and inform the user
    (void (ffi-lib rackinect-dylib-path #:fail (lambda() (error "The rackinect-grab_c (still) cannot be loaded after one rebuild phase"))))))


;; For Windows: Add the dll directory to the systems path:
(void (when (equal? (system-type 'os) 'windows)
        (putenv "PATH" (string-append (path->string rackinect-path) ";" (getenv "PATH")))))

;; For Mac OS X: Add the MacPorts directory to the systems path
(void (when (equal? (system-type 'os) 'macosx)
        (putenv "PATH" (string-append "/opt/local/bin" ":" (getenv "PATH")))))

;;Try to load the dylib: If this fails - try to rebuild it!
(void (ffi-lib rackinect-dylib-path #:fail rebuild-rackinect))


(provide rackinect-path
         rackinect-version
         rackinect-dylib-path)

