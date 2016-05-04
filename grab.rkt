#lang racket

(require vigracket)

(require rackinect/config)

(require scheme/foreign)
(unsafe!)

  
;#################Get the captures from libfreenect ###########
(define kinect_stop_c
  (get-ffi-obj 'kinect_stop_c rackinect-dylib-path
               (_fun -> (res :  _int))))

(define kinect_grab_get_video_c
  (get-ffi-obj 'kinect_grab_get_video_c rackinect-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b width height device_id) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     [device_id : _int]
                     -> (res :  _int))))

(define kinect_grab_get_depth_c
  (get-ffi-obj 'kinect_grab_get_depth_c rackinect-dylib-path
               (_fun (img_vector_d width height device_id) :: [img_vector_d : _cvector]
                     [width : _int]
                     [height : _int]
                     [device_id : _int]
                     -> (res :  _int))))

(define kinect_grab_get_depth_and_video_c
  (get-ffi-obj 'kinect_grab_get_depth_and_video_c rackinect-dylib-path
               (_fun (img_vector_d img_vector_r img_vector_g img_vector_b width height device_id) :: [img_vector_d : _cvector]
                     [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     [device_id : _int]
                     -> (res :  _int))))


(define kinect_grab_get_video_for_depth_interval_c
  (get-ffi-obj 'kinect_grab_get_video_for_depth_interval_c rackinect-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b width height front back device_id) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     [front : _int]
                     [back : _int]
                     [device_id : _int]
                     -> (res :  _int))))

(define kinect_grab_get_topleft_c
  (get-ffi-obj 'kinect_grab_get_topleft_c rackinect-dylib-path
               (_fun (front back device_id) :: [pos_x : (_ptr o _int)] [pos_y : (_ptr o _int)]
                     [front : _int]
                     [back : _int]
                     [device_id : _int]
                     -> (res :  _int)
                     -> (cons res (cons pos_x pos_y))
                     )))

(define (stopgrabbing)
  (if (= 0 (kinect_stop_c))
      #t
      (error "Error in rackinect.grab.stopgrabbing: Stopping not possible!")))

(define (grabvideo [device_id 0])
  (let* ((width 640)
         (height 480)
         (img (make-image width height 3 0.0 0.0 0.0)))
    (grabvideo/unsafe img device_id)))

(define (grabdepth [device_id 0])
  (let* ((width 640)
         (height 480)
         (img (make-image width height 1 0.0)))
    (grabdepth/unsafe img device_id)))

(define (grabdepth+video [device_id 0])
  (let* ((width 640)
         (height 480)
         (img (make-image width height 4 0.0 0.0 0.0 0.0)))
    (grabdepth+video/unsafe img device_id)))

(define (grabvideo-depthinterval [front 1] [back 1000] [device_id 0])
  (let* ((width 640)
         (height 480)
         (img (make-image width height 3 0.0 0.0 0.0)))
    (grabvideo-depthinterval/unsafe img device_id)))

(define (grabtopleft [front 1] [back 800] [device_id 0])
  (let* ((res (kinect_grab_get_topleft_c front back device_id)))
    (case (car res)
      ((0) (cdr res))
      ((1)  (error "Error in rackinect.grab.grabtopleft: Capture not possible!")))))


(define (grabvideo/unsafe img [device_id 0])
  (case (kinect_grab_get_video_c (image-data img 0)  (image-data img 1)  (image-data img 2) (image-width img) (image-height img) device_id)
    ((0) img)
    ((1) (error "Error in rackinect.grab.grabvideo/unsafe: Sizes do not match!"))
    ((2) (error "Error in rackinect.grab.grabvideo/unsafe: Capture not possible!"))))

(define (grabdepth/unsafe img [device_id 0])
  (case (kinect_grab_get_depth_c (image-data img 0) (image-width img) (image-height img) device_id)
    ((0) img)
    ((1) (error "Error in rackinect.grab.grabdepth/unsafe: Sizes do not match!"))
    ((2) (error "Error in rackinect.grab.grabdepth/unsafe: Capture not possible!"))))
  
  
(define (grabdepth+video/unsafe img [device_id 0])
  (case (kinect_grab_get_depth_and_video_c (image-data img 0)  (image-data img 1)  (image-data img 2)  (image-data img 3) (image-width img) (image-height img) device_id)
    ((0) img)
    ((1) (error "Error in rackinect.grab.grabdepth+video/unsafe: Sizes do not match!"))
    ((2) (error "Error in rackinect.grab.grabdepth+video/unsafe: Capture not possible!"))))
  
  
(define (grabvideo-depthinterval/unsafe img [front 1] [back 1000] [device_id 0])
  (case (kinect_grab_get_video_for_depth_interval_c (image-data img 0)  (image-data img 1)  (image-data img 2) (image-width img) (image-height img) front back device_id)
    ((0) img)
    ((1) (error "Error in rackinect.grab.grabvideo/unsafe: Sizes do not match!"))
    ((2) (error "Error in rackinect.grab.grabvideo/unsafe: Capture not possible!"))))

(provide   stopgrabbing
           grabvideo
           grabdepth
           grabdepth+video
           grabvideo-depthinterval
           grabtopleft
           grabvideo/unsafe
           grabdepth/unsafe
           grabdepth+video/unsafe
           grabvideo-depthinterval/unsafe)