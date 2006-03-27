;;;;
;;;; This example is a translation of the Direct3D Tutorial 3 which is
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

;; Be very specific about the assemblies that we load.
(load-assembly "System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(load-assembly "System.Drawing, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
(load-assembly "Microsoft.DirectX, Version=1.0.2902.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35")
(load-assembly "Microsoft.DirectX.Direct3D, Version=1.0.2902.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35")
(load-assembly "Microsoft.DirectX.Direct3DX, Version=1.0.2910.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35")

(use-namespaces "System"
                "System.Drawing"
                "System.Windows.Forms"
                "Microsoft.DirectX"
                "Microsoft.DirectX.Direct3D")

(defclass matrices ()
  ((window :initform nil)
   (device :initform nil)
   (vertex-buffer :initform nil)
   (present-params :initform (new '?PresentParameters))
   (pause :initform nil)))

(defmethod initialize-instance :after ((this matrices) &key &allow-other-keys)
  (with-slots (window) this
    (setf window (new '?Form))
    (setf (?.ClientSize window) (new '?Size 400 300))
    (setf (?.Text window) "Direct3D Tutorial 3 - Matrices")

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

(defmethod initialize-graphics ((this matrices))
  (handler-case
   (with-slots (window device present-params pause) this
     (setf (?.Windowed presentParams) t
           (?.SwapEffect presentParams) (?.Discard '?SwapEffect))
     (setf device (new '?Device
                       0
                       (?.Hardware '?DeviceType)
                       window
                       (?.SoftwareVertexProcessing '?CreateFlags)
                       presentParams))
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

(defmethod on-create-device ((this matrices) sender e)
  (with-slots (device vertex-buffer) this
    (setf vertex-buffer
          (new '?VertexBuffer
               (get-type-object '?CustomVertex+PositionColored)
               3
               device
               (?.None '?Usage)
               (?.Format '?CustomVertex+PositionColored)
               (?.Default '?Pool)))
    (?.add_Created vertex-buffer
                   (new '?EventHandler
                        #'(lambda (sender event-args)
                            (on-create-vertex-buffer this sender event-args))))
    (on-create-vertex-buffer this vertex-buffer nil)))
  
(defmethod on-reset-device ((this matrices) sender e)
  (with-slots (device) this
    (setf (?.CullMode (?.RenderState device)) (?.None '?Cull)
          (?.Lighting (?.RenderState device)) nil)))
                               
(defmethod on-create-vertex-buffer ((this matrices) sender event-args)
  (declare (ignore sender event-args))
  (with-slots (vertex-buffer) this
    (let ((verts (?.Lock vertex-buffer 0 (?.None '?LockFlags))))
      (unwind-protect
           (setf (aref* verts 0) (new '?CustomVertex+PositionColored
                                      -1.0f0
                                      -1.0f0
                                      0.0f0
                                      (?.ToArgb (?.DarkGoldenrod '?Color)))
                 (aref* verts 1) (new '?CustomVertex+PositionColored
                                      1.0f0
                                      -1.0f0
                                      0.0f0
                                      (?.ToArgb (?.MediumOrchid '?Color)))
                 (aref* verts 2) (new '?CustomVertex+PositionColored
                                      0.0f0
                                      1.0f0
                                      0.0f0
                                      (?.ToArgb (?.Cornsilk '?Color))))
        (?.Unlock vertex-buffer)))))

(defmethod render ((this matrices))
  (with-slots (device vertex-buffer pause) this
    (unless device
      (return-from render))
    (when pause
      (return-from render))
    (?.Clear device (?.Target '?ClearFlags) (?.Blue '?Color) 1.0f0 0)
    (?.BeginScene device)
    (setup-matrices this)
    
    (?.SetStreamSource device 0 vertex-buffer 0)
    (setf (?.VertexFormat device)
          (?.Format '?CustomVertex+PositionColored))
    (?.DrawPrimitives device (?.TriangleList '?PrimitiveType) 0 1)
    (?.EndScene device)
    (?.Present device)))

(defmethod setup-matrices ((this matrices))
  (with-slots (device) this
    (let* ((itime (rem (?.TickCount '?Environment) 1000))
           (fangle (/ (* itime 2.0f0 pi) 1000.0f0)))
      (setf (?.World (?.Transform device))
              (?.RotationY '?Matrix fangle)
            (?.View (?.Transform device))
              (?.LookAtLH '?Matrix
                          (new '?Vector3 0.0f0 3.0f0 -5.0f0)
                          (new '?Vector3 0.0f0 0.0f0  0.0f0)
                          (new '?Vector3 0.0f0 1.0f0  0.0f0))
            (?.Projection (?.Transform device))
              (?.PerspectiveFovLH '?Matrix (/ pi 4) 1.0f0 1.0f0 100.0f0)))))

(defmethod on-paint ((this matrices) e)
  (declare (ignore e))
  (render this))

(defmethod on-keypress ((this matrices) e)
  (when (eql (char-code (?.KeyChar e)) (enum-to-integer (?.Escape '?Keys)))
    (with-slots (window) this
      (?.Close window))))

(defmethod on-resize ((this matrices) e)
  (declare (ignore e))
  (with-slots (window pause) this
    (setf pause (or (?.Equals (?.WindowState window)
                              (?.Minimized '?FormWindowState))
                    (not (?.Visible window))))))

;; Again, since we're not actually derived from Form, let's provide a
;; couple of methods that clients would use on us if we were.
(defmethod show ((this matrices))
  (?.Show (slot-value this 'window)))

(defmethod created-p ((this matrices))
  (?.Created (slot-value this 'window)))

(defmethod release-resources ((this matrices))
  (with-slots (window device) this
    (when window (?.Dispose window))
    (when device (?.Dispose device))))

(defun tutorial3 ()
  ;; The *coerce-double-floats* flag works around a problem calling
  ;; .NET methods with floating point arguments. LispWorks for Windows
  ;; has only double floats, so it's impossible to call methods that
  ;; want single float arguments.
  (let* (#+(and :lispworks :win32) (*coerce-double-floats-to-single* t)
         (frm (make-instance 'matrices)))
    (unwind-protect  ; Makes sure we kill the window!
         (progn
           (unless (initialize-graphics frm)
             (?.Show '?MessageBox
                     "Could not initialize Direct3D. This tutorial will exit.")
             (return-from tutorial3))
           (show frm)
           (loop
              while (created-p frm)
              do
                (render frm)
                (?.DoEvents '?Application)))
      (release-resources frm))))

(bind-clr-symbols)