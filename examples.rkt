#lang racket

(require vigracket)
(require rackinect)

(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width plt-image-width)
                    (image-height plt-image-height)))
(require 2htdp/universe)

(stopgrabbing)
(sleep 1)

(define pic_img   (grabvideo))
(define pic   (image->racket-image pic_img))

(define depth_img (grabdepth))
(define depth (image->racket-image depth_img))

(define depth+pic_img (grabdepth+video))

(define (status t update_each)
  (above (text "Number of Ticks:"  12 "black")
                           (text (number->string t) 16 "black")
                           (text "Refreshes:"  12 "black")
                           (text (number->string (quotient t update_each)) 16 "black")))

(define (live-view t [update_each 4])
  (when (= (modulo t update_each) 0)
      (begin
        (grabvideo/unsafe pic_img) 
        (image->racket-image pic_img pic)
        (grabdepth/unsafe depth_img)
        (image->racket-image depth_img depth)))
  (beside pic (status t update_each) depth))

;Example usage:
;(animate live-view)

(define (live-view-combined t [update_each 4])
  (when (= (modulo t update_each) 0)
      (begin
        (grabdepth+video/unsafe depth+pic_img)
        (image->racket-image (list (car depth+pic_img)) depth)
        (image->racket-image (cdr depth+pic_img) pic)))
  (beside pic (status t update_each)  depth))

;Example usage:
;(animate live-view-combined)

;; Test the speed of image acquisitions
(define (test-grabbing-speed [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (begin (grabvideo)
                     (grabdepth)))
        (test-grabbing-speed (- n 1)))))

;; Test the speed of unsafe/image acquisitions
(define (test-grabbing-speed/unsafe [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (begin (grabvideo/unsafe pic_img)
                     (grabdepth/unsafe depth_img)))
        (test-grabbing-speed/unsafe (- n 1)))))

;;Test the speed of grabbing and tranforming to racket images
(define (test-grabbing-speed-bitmaps [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (begin (image->racket-image (grabvideo))
                     (image->racket-image (grabdepth))))
        (test-grabbing-speed-bitmaps (- n 1)))))

;;Test the speed of grabbing and tranforming to racket images (unsafe mode)
(define (test-grabbing-speed-bitmaps/unsafe [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (begin (grabvideo/unsafe pic_img)
                     (image->racket-image pic_img pic)
                     (grabdepth/unsafe depth_img)
                     (image->racket-image depth_img)))
        (test-grabbing-speed-bitmaps/unsafe (- n 1)))))


;; Test the speed of combined image acquisitions
(define (test-combined-grabbing-speed [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (grabdepth+video))
        (test-combined-grabbing-speed (- n 1)))))

;; Test the speed of combined unsafe/image acquisitions
(define (test-combined-grabbing-speed/unsafe [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (grabdepth+video/unsafe depth+pic_img))
        (test-combined-grabbing-speed/unsafe (- n 1)))))

;;Test the speed of combined grabbing and tranforming to racket images
(define (test-combined-grabbing-speed-bitmaps [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (let ((drgb  (grabdepth+video)))
                (begin (image->racket-image (list (car drgb)))
                       (image->racket-image (cdr drgb)))))
        (test-grabbing-speed-bitmaps (- n 1)))))

;;Test the speed of combined grabbing and tranforming to racket images (unsafe mode)
(define (test-combined-grabbing-speed-bitmaps/unsafe [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (begin (grabdepth+video/unsafe depth+pic_img)
                     (image->racket-image (list (car depth+pic_img)) pic)
                     (image->racket-image (cdr depth+pic_img) depth)))
        (test-combined-grabbing-speed-bitmaps/unsafe (- n 1)))))




;; Round and make exact number for any other numeric type
(define roundex (compose inexact->exact round))

;; Threshold Kinect-depth images with valid distances smaller than x (mm)
(define (smaller img x)
  (image-map (lambda (val) (if (< 0 val x) 255.0 0.0)) img))

;; Combines the above functions to extract a (mouse) pointer position in Kinect
;; coordinates: 0 <= x < 640, 0 <= y < 480. Uses scaling to speed up process
(define (img->pointer img [closerThan 800.0] [opening 0] [scale 2.0])
  (let* ((scaled_img  (if (> scale 1.0)
                          (resizeimage img (roundex (/ (image-width img) scale)) (roundex (/ (image-height img) scale)) 0)
                          img))
         (thresh_img   (smaller scaled_img closerThan) )
         (work_img    (if (> opening 0)
                          (openingimage thresh_img opening)
                          thresh_img))
         (topLeft (band-while (lambda (x y v) (= v 0.0)) (car work_img))))
    (cons (* scale (car topLeft)) (* scale (cdr topLeft)))))

;; The pointer position
(define pointer_pos '(0 . 0))

;; The radius of the pointer
(define radius 10)

;; Shows the image with an overlaid pointer circle of a given radius
(define (live-pointer t)
  (when (= (modulo t 2) 0)
      (begin
        (grabvideo/unsafe pic_img) 
        (grabdepth/unsafe depth_img) 
        (set! pic (image->racket-image pic_img))
        (set! pointer_pos (grabtopleft))));(img->pointer depth_img))))
  (underlay/xy pic (- (car pointer_pos) (/ radius 2)) (- (cdr pointer_pos) (/ radius 2)) (circle radius "solid" "yellow")))
