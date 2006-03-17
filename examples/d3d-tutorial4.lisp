;;;;
;;;; This example is a translation of the Direct3D Tutorial 4 which is
;;;; included in Microsoft's DirectX SDK documentation. It is
;;;; completely self-contained, except that you need to load CL-CLR
;;;; before this file.
;;;;
;;;; The translation stays very close to the original C# code, except
;;;; for using Lisp-like naming conventions. A few things could be
;;;; simplified a bit, but weren't, so that comparing the code to the
;;;; original tutorials would be easy.
;;;;
(defpackage :d3d-tutorials (:use :common-lisp :cl-clr))
(in-package :d3d-tutorials)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (load "assemblies.lisp"))

(use-namespaces "System"
                "System.Drawing"
                "System.Windows.Forms"
                "Microsoft.DirectX"
                "Microsoft.DirectX.Direct3D")

(defclass lights ()
  ((window :initform nil)
   (device :initform nil)
   (vertex-buffer :initform nil)
   (present-params :initform (new '?PresentParameters))
   (pause :initform nil)))

(defmethod initialize-instance :after ((this lights) &key &allow-other-keys)
  (with-slots (window) this
    (setf window (new '?Form))
    (setf (?.ClientSize window) (new '?Size 400 300))
    (setf (?.Text window) "Direct3D Tutorial 4 - Lights")

    ;; Since we're not really derived from a window, we have to wire
    ;; ourselves up to handle some events, instead of overriding
    ;; virtual functions. Luckily, the .NET classes make this easy.
    (?.add_Paint window
                 (new '?PaintEventHandler
                      #'(lambda (sender e)
                          (declare (ignore sender))
                          (on-paint this e))))
    (?.add_KeyPress window
                    (new '?KeyPressEventHandler
                         #'(lambda (sender e)
                             (declare (ignore sender))
                             (on-keypress this e))))
    (?.add_Resize window
                  (new '?EventHandler
                       #'(lambda (sender e)
                           (declare (ignore sender))
                           (on-resize this e))))))

(defmethod initialize-graphics ((this lights))
  (handler-case
   (with-slots (window device pause present-params) this
     (setf (?.Windowed present-params) t
           (?.SwapEffect present-params) (?.Discard '?SwapEffect)
           (?.EnableAutoDepthStencil present-params) t
           (?.AutoDepthStencilFormat present-params) (?.D16 '?DepthFormat))
     (setf device (new '?Device
                       0
                       (?.Hardware '?DeviceType)
                       window
                       (?.SoftwareVertexProcessing '?CreateFlags)
                       present-params))
     (?.add_DeviceReset device
                        (new '?EventHandler
                             #'(lambda (sender e)
                                 (on-reset-device this sender e))))
     (on-create-device this device nil)
     (on-reset-device this device nil)
     (setf pause nil)
     t)
    (rdnzl-error (condition)
      (print (?.ToString (rdnzl-error-exception condition)))
      nil)))

(defmethod on-create-device ((this lights) sender e)
  (with-slots (device vertex-buffer) this
    (setf vertex-buffer
          (new '?VertexBuffer
               (get-type-object '?CustomVertex+PositionNormal)
               100
               device
               (?.WriteOnly '?Usage)
               (?.Format '?CustomVertex+PositionNormal)
               (?.Default '?Pool)))
    (?.add_Created vertex-buffer
                   (new '?EventHandler
                        #'(lambda (sender event-args)
                            (on-create-vertex-buffer this sender event-args))))
    (on-create-vertex-buffer this vertex-buffer nil)))
  
(defmethod on-reset-device ((this lights) sender e)
  (with-slots (device) this
    (setf (?.CullMode      (?.RenderState device)) (?.None '?Cull)
          (?.ZBufferEnable (?.RenderState device)) t
          (?.Lighting      (?.RenderState device)) t)))
                               
(defmethod on-create-vertex-buffer ((this lights) vertex-buffer event-args)
  (declare (ignore sender event-args))
  (let ((verts (?.Lock vertex-buffer 0 (?.None '?LockFlags))))
    (unwind-protect
         (loop
            for i from 0 below 50
            for theta = (/ (* 2 pi i) 49.0f0)
            do
              (setf (aref* verts (* 2 i))
                    (new '?CustomVertex+PositionNormal
                         (new '?Vector3 (sin theta) -1 (cos theta))
                         (new '?Vector3 (sin theta)  0 (cos theta))))
              (setf (aref* verts (1+ (* 2 i)))
                    (new '?CustomVertex+PositionNormal
                         (new '?Vector3 (sin theta)  1 (cos theta))
                         (new '?Vector3 (sin theta)  0 (cos theta)))))
      (?.Unlock vertex-buffer))))

(defmethod setup-matrices ((this lights))
  (with-slots (device) this

    (let ((ticks (?.TickCount '?Environment)))
      (setf (?.World (?.Transform device))
            (?.RotationAxis
             '?Matrix
             (new '?Vector3 (cos (/ ticks 250.0f0)) 1 (sin (/ ticks 250.0f0)))
             (/ ticks 3000.0f0))))
    
    (setf (?.View (?.Transform device))
          (?.LookAtLH '?Matrix
                      (new '?Vector3 0.0f0 3.0f0 -5.0f0)
                      (new '?Vector3 0.0f0 0.0f0  0.0f0)
                      (new '?Vector3 0.0f0 1.0f0  0.0f0)))
    
    (setf (?.Projection (?.Transform device))
          (?.PerspectiveFovLH '?Matrix (/ pi 4.0f0) 1.0f0 1.0f0 100.0f0))))

(defmethod setup-lights ((this lights))
  (with-slots (device) this
    (let ((col (?.White '?Color))
          (mtrl (new '?Material)))
      (setf (?.Diffuse mtrl) col
            (?.Ambient mtrl) col)
      (setf (?.Material device) mtrl))

    (let ((light (?.Item (?.Lights device) 0))
          (ticks (?.TickCount '?Environment)))
      (setf (?.Type      light) (?.Directional '?LightType)
            (?.Diffuse   light) (?.DarkTurquoise '?Color)
            (?.Direction light) (new '?Vector3
                                    (cos (/ ticks 250.0f0))
                                    1.0f0
                                    (sin (/ ticks 250.0f0)))
            (?.Enabled   light) t))
    (setf (?.Ambient (?.RenderState device)) (?.FromArgb '?Color #X202020))))
    
(defmethod render ((this lights))
  (with-slots (device vertex-buffer pause) this
    (unless device
      (return-from render))
    (when pause
      (return-from render))
    (?.Clear device
             (or-enums (?.Target '?ClearFlags)
                       (?.ZBuffer '?ClearFlags))
             (?.Blue '?Color) 1.0f0 0)
    (?.BeginScene device)
    (setup-lights this)
    (setup-matrices this)
    
    (?.SetStreamSource device 0 vertex-buffer 0)
    (setf (?.VertexFormat device)
          (?.Format '?CustomVertex+PositionNormal))
    (?.DrawPrimitives device (?.TriangleStrip '?PrimitiveType) 0 (- (* 4 25) 2))
    (?.EndScene device)
    (?.Present device)))

(defmethod on-paint ((this lights) e)
  (declare (ignore e))
  (render this))

(defmethod on-keypress ((this lights) e)
  (when (eql (char-code (?.KeyChar e)) (enum-to-integer (?.Escape '?Keys)))
    (with-slots (window) this
      (?.Close window))))

(defmethod on-resize ((this lights) e)
  (declare (ignore e))
  (with-slots (window pause) this
    (setf pause (or (?.Equals (?.WindowState window)
                              (?.Minimized '?FormWindowState))
                    (not (?.Visible window))))))

;; Again, since we're not actually derived from Form, let's provide a
;; couple of methods that clients would use on us if we were.
(defmethod show ((this lights))
  (?.Show (slot-value this 'window)))

(defmethod created-p ((this lights))
  (?.Created (slot-value this 'window)))

(defmethod release-resources ((this lights))
  (with-slots (window device) this
    (when window (?.Dispose window))
    (when device (?.Dispose device))))

(defun tutorial4 ()
  ;; The *coerce-double-floats* flag works around a problem calling
  ;; .NET methods with floating point arguments. LispWorks for Windows
  ;; has only double floats, so it's impossible to call methods that
  ;; want single float arguments.
  (let* (#+(and :lispworks :win32) (*coerce-double-floats-to-single* t)
         (frm (make-instance 'lights)))
    (unwind-protect  ; Makes sure we kill the window!
         (progn
           (unless (initialize-graphics frm)
             (?.Show '?MessageBox
                     "Could not initialize Direct3D. This tutorial will exit.")
             (return-from tutorial4))
           (show frm)
           (loop
              while (created-p frm)
              do
                (render frm)
                (?.DoEvents '?Application)))
      (release-resources frm))))

(bind-clr-symbols)