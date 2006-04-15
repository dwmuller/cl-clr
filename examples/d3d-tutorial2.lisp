;;;;
;;;; This example is a translation of the Direct3D Tutorial 2 which is
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

(enable-clr-syntax "System"
                   "System.Drawing"
                   "System.Windows.Forms"
                   "Microsoft.DirectX"
                   "Microsoft.DirectX.Direct3D")

(defclass vertices ()
  ((window)
   (device)
   (vertex-buffer)))

(defmethod initialize-instance :after ((this vertices) &key &allow-other-keys)
  (with-slots (window) this
    (setf window (new '?Form))
    (setf (?.ClientSize window) (new '?Size 300 300))
    (setf (?.Text window) "Direct3D Tutorial 2 - Vertices")

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
                             (on-keypress this e))))))

(defmethod initialize-graphics ((this vertices))
  (handler-case
   (with-slots (window device) this
     (let ((presentParams (new '?PresentParameters)))
       (setf (?.Windowed presentParams) t
             (?.SwapEffect presentParams) (?.Discard '?SwapEffect))
       (setf device (new '?Device
                         0
                         (?.Hardware '?DeviceType)
                         window
                         (?.SoftwareVertexProcessing '?CreateFlags)
                         presentParams))
       (on-create-device this device nil)
       t))
    (clr-exception (condition)
      (print (?.ToString condition))
      nil)))

(defmethod on-create-device ((this vertices) sender event-args)
  (with-slots (device vertex-buffer) this
      (setf vertex-buffer
            (new '?VertexBuffer
                 '?CustomVertex+TransformedColored
                 3
                 device
                 (?.None '?Usage)
                 (?.Format '?CustomVertex+TransformedColored)
                 (?.Default '?Pool)))
    (?.add_Created vertex-buffer
                   (new '?EventHandler
                        #'(lambda (sender event-args)
                            (on-create-vertex-buffer this sender event-args))))
    (on-create-vertex-buffer this vertex-buffer nil)))
  
  
(defmethod on-create-vertex-buffer ((this vertices) sender event-args)
  (declare (ignore sender event-args))
  (with-slots (vertex-buffer) this
    (let ((verts (?.CreateInstance
                  '?Array
                  '?CustomVertex+TransformedColored
                  3)))
      (setf (aref* verts 0) (new '?CustomVertex+TransformedColored
                                 150  50 0.5f0 1
                                 (?.ToArgb (?.Aqua '?Color)))
            (aref* verts 1) (new '?CustomVertex+TransformedColored
                                 250 250 0.5f0 1
                                 (?.ToArgb (?.Brown '?Color)))
            (aref* verts 2) (new '?CustomVertex+TransformedColored
                                  50 250 0.5f0 1
                                  (?.ToArgb (?.LightPink '?Color))))
      
      ;; Note: In the C# code, stm is locked earlier than necessary.
      (let ((stm (?.Lock vertex-buffer 0 0 (?.None '?LockFlags))))
        ;; We use unwind-protect to make sure the Unlock happens.
        (unwind-protect 
             (?.Write stm verts)
          (?.Unlock vertex-buffer)))
      (values))))

(defmethod render ((this vertices))
  (with-slots (device vertex-buffer) this
    (?.Clear device (?.Target '?ClearFlags) (?.Blue '?Color) 1.0f0 0)
    (?.BeginScene device)
    (?.SetStreamSource device 0 vertex-buffer 0)
    (setf (?.VertexFormat device)
          (?.Format '?CustomVertex+TransformedColored))
    (?.DrawPrimitives device (?.TriangleList '?PrimitiveType) 0 1)
    (?.EndScene device)
    (?.Present device)))

(defmethod on-paint ((this vertices) e)
  (declare (ignore e))
  (render this))

(defmethod on-keypress ((this vertices) e)
  (when (eql (char-code (?.KeyChar e)) (enum-value ?.Escape '?Keys))
    (with-slots (window) this
      (?.Close window))))

;; Again, since we're not actually derived from Form, let's provide a
;; couple of methods that clients would use on us if we were.
(defmethod show ((this vertices))
  (?.Show (slot-value this 'window)))

(defmethod created-p ((this vertices))
  (?.Created (slot-value this 'window)))

(defmethod release-resources ((this vertices))
  (with-slots (window device) this
    (when window (?.Dispose window))
    (when device (?.Dispose device))))

(defun tutorial2 ()
  ;; The *coerce-double-floats* flag works around a problem calling
  ;; .NET methods with floating point arguments. LispWorks for Windows
  ;; has only double floats, so it's impossible to call methods that
  ;; want single float arguments.
  (let* (#+(and :lispworks :win32) (*coerce-double-floats-to-single* t)
         (frm (make-instance 'vertices)))
    (unwind-protect  ; Makes sure we kill the window!
         (progn
           (unless (initialize-graphics frm)
             (?.Show '?MessageBox
                     "Could not initialize Direct3D. This tutorial will exit.")
             (return-from tutorial2))
           (show frm)
           (loop
              while (created-p frm)
              do
                (render frm)
                (?.DoEvents '?Application)))
      (release-resources frm))))

(bind-clr-symbols)