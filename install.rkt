#lang racket

(define local-collects-path
  (first (current-library-collection-paths)))

(define local-install-path  
  (build-path local-collects-path "rackinect"))

;; 1. Create local collects directory if not already existing
(when (not (directory-exists? local-collects-path))
  (make-directory* local-collects-path))

;; 2. If rackinect-collects dir extist, delete it.
(when (directory-exists? local-install-path)
  (delete-directory/files local-install-path))

;; 3. copy the installation contents to the local/collects/rackinect directory
(copy-directory/files (current-directory) local-install-path)

;; 4. Load dll under windows, dylib under MacOS
(define dylib-file
    (cond ((equal? (system-type 'os) 'windows) "kinect-grab_c.dll")
          ((equal? (system-type 'os) 'macosx)  "libkinect-grab_c.dylib")
          ((equal? (system-type 'os) 'unix)    "libkinect-grab_c.so")
          (else (error "Only macosx, windows and unix are supported"))))
(define dylib-path (build-path local-install-path dylib-file))

;; 5. For Windows: Add the dll directory to the systems path:
(when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string local-install-path) ";" (getenv "PATH"))))

;; 6. Stuff needed to compile the c-bindings if necessary...
(define base_login_script "~/.profile")
(define kinect-grab_c-path (build-path local-install-path "kinect-grab_c"))

(define login_script (if (file-exists? base_login_script)
                         base_login_script
                         (path->string (build-path kinect-grab_c-path "fallback.profile"))))

(define login_cmd (string-append ". " login_script))
(define (system-env arg) (system (string-append login_cmd " && " arg)))

;; 7. For windows, we need to find out, which architecture DrRacket is built
(require (only-in ffi/unsafe ctype-sizeof _pointer))
(define racket-bits (* 8 (ctype-sizeof _pointer)))
(define cmake_flags (if (= racket-bits 32)
                        "-DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS=-m32 -DCMAKE_C_FLAGS=-m32"
                        "-DCMAKE_BUILD_TYPE=Release"))

; 8. The compilation routine (at least for macosx and unix)
 (if (or (equal? (system-type 'os) 'macosx)
          (equal? (system-type 'os) 'unix))
      (begin 
        (display "-------------- BUILDING LIBFREENECT-C-WRAPPER FOR KINECT GRABBING --------------")
        (newline)
        (current-directory kinect-grab_c-path)
        (if (system-env (string-append "mkdir build && cd build && cmake " cmake_flags " .. && make && cd .."))
            (begin
              (copy-file (build-path (current-directory) "bin" dylib-file) dylib-path #t)
              #t)
            (error "Making the kinect-grab_c lib failed")))
      ;;For windows
      (if (equal? (system-type 'os) 'windows)
          (let ((bindir     (build-path kinect-grab_c-path "bin" (string-append "win"(number->string racket-bits)))))
             (begin
               (system (string-append "copy " (path->string bindir) "\\*.dll " (path->string local-install-path)))
               #t))
          (error "Only Mac OS X, Unix are supported for auto build of kinect_grab_c!")))
