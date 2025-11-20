;;; zoom.el --- Fixed and automatic balanced window layout  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Andrea Cardaci <cyrus.and@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Andrea Cardaci <cyrus.and@gmail.com>
;; Version: 0.4.0
;; URL: https://github.com/cyrus-and/zoom
;; Package-Requires: ((emacs "24.4"))
;; Keywords: frames

;;; Commentary:

;; This minor mode takes care of managing the window sizes by enforcing a fixed
;; and automatic balanced layout where the currently selected window is resized
;; according to `zoom-size' which can be an absolute value in lines/columns, a
;; ratio between the selected window and frame size or even a custom callback.

;;; Code:

(defgroup zoom nil
  "Enforce a fixed and automatic balanced window layout."
  :group 'windows)

(defcustom zoom-size '(80 . 24)
  "Size hint for the selected window.

It can be either a cons or a function.

Each component of the cons can be either an absolute value in
lines/columns or a ratio between the selected window and the
frame size.  In the former case the window is resized according
to its body size, i.e., the total window size can be much larger.
In any case, windows are never shrinked if they are already
larger than the resulting size.

The function takes no arguments and returns a cons as specified
above."
  :type '(choice (function :tag "Custom")
                 (cons :tag "Fixed"
                       (choice (integer :tag "Columns")
                               (float :tag "Width ratio"))
                       (choice (integer :tag "Lines")
                               (float :tag "Height ratio"))))
  :safe #'consp
  :group 'zoom)

(defcustom zoom-use-total-window-size t
  "If nil the window body only is used to compute the sizes.

Otherwise also consider, margins, fringes, header line, etc."
  :type 'boolean
  :group 'zoom)

(defcustom zoom-ignored-major-modes nil
  "List of ignored major modes.

Selected windows displaying a buffer with a major mode that is
derived from any of these major modes should not be enlarged.
Additionally, if ANY window in the frame matches these criteria,
automatic window balancing will be skipped to preserve custom
layouts (e.g., for ediff)."
  :type '(repeat symbol)
  :group 'zoom)

(defcustom zoom-ignored-buffer-names nil
  "List of ignored buffer names.

Selected windows displaying any of these buffers should not be
enlarged.  Additionally, if ANY window in the frame matches these
criteria, automatic window balancing will be skipped to preserve
custom layouts (e.g., for ediff)."
  :type '(repeat string)
  :group 'zoom)

(defcustom zoom-ignored-buffer-name-regexps nil
  "List of ignored buffer name regexps.

Selected windows displaying buffers matching any of these regexps
should not be enlarged.  Additionally, if ANY window in the frame
matches these criteria, automatic window balancing will be skipped
to preserve custom layouts (e.g., for ediff)."
  :type '(repeat regexp)
  :group 'zoom)

(defcustom zoom-ignore-predicates nil
  "List of additional predicates that allow to ignore windows.

These functions are called (in order) to decide whether the
selected window should be ignored or not.  Predicates take no
parameters and as soon as one function returns a non-nil value,
the selected window is ignored and the others are not called.

When a window is ignored, it will not be enlarged.  Additionally,
if ANY window in the frame is ignored, automatic window balancing
will be skipped to preserve custom layouts (e.g., for ediff).

Example for ediff integration:
  (add-to-list 'zoom-ignore-predicates
               (lambda () (and (boundp 'ediff-this-buffer-ediff-sessions)
                          ediff-this-buffer-ediff-sessions)))"
  :type '(repeat function)
  :group 'zoom)

(defcustom zoom-minibuffer-preserve-layout t
  "Non-nil means that the layout is retained when the minubuffer is entered.

Otherwise, since the minibuffer cannot be zoomed, all the other
windows are simply balanced.  Setting this variable to nil can be
useful when third-party modes use the minibuffer to display more
than few lines."
  :type 'boolean
  :group 'zoom)

(defvar zoom-disabled nil
  "When non-nil, temporarily disable all zoom operations.
This can be used to bypass zoom for specific operations without
turning off zoom-mode entirely.  Useful for modes that manage
their own window layout.")

;;;###autoload
(define-minor-mode zoom-mode
  "Perform `zoom' automatically as the selected window changes."
  :global t
  :lighter " Z"
  :require 'zoom
  (if zoom-mode
      (zoom--on)
    (zoom--off)))

;;;###autoload
(defun zoom ()
  "Zoom the current window and balance the others according to `zoom-size'."
  (interactive)
  ;; manual invocation only works when this mode is disabled
  (if zoom-mode
      (message "Window zooming is automatic (M-x zoom-mode to disable)")
    (zoom--update)))

(defun zoom--on ()
  "Enable hooks and advices and update the layout."
  ;; register the zoom handlers
  (add-hook 'window-selection-change-functions #'zoom--window-selection-change)
  (add-hook 'window-configuration-change-hook #'zoom--configuration-change)
  (add-hook 'window-buffer-change-functions #'zoom--buffer-change)
  ;; disable mouse resizing
  (advice-add #'mouse-drag-mode-line :override #'ignore)
  (advice-add #'mouse-drag-vertical-line :override #'ignore)
  (advice-add #'mouse-drag-header-line :override #'ignore)
  ;; update the layout once loaded
  (zoom--do-update t))

(defun zoom--off ()
  "Disable hooks and advices and evenly balance the windows."
  ;; unregister the zoom handlers
  (remove-hook 'window-selection-change-functions #'zoom--window-selection-change)
  (remove-hook 'window-configuration-change-hook #'zoom--configuration-change)
  (remove-hook 'window-buffer-change-functions #'zoom--buffer-change)
  ;; cleanup timer
  (when zoom--update-timer
    (cancel-timer zoom--update-timer)
    (setq zoom--update-timer nil))
  ;; enable mouse resizing
  (advice-remove #'mouse-drag-mode-line #'ignore)
  (advice-remove #'mouse-drag-vertical-line #'ignore)
  (advice-remove #'mouse-drag-header-line #'ignore)
  ;; leave with a clean layout
  (dolist (frame (frame-list))
    (balance-windows frame)))

(defvar zoom--last-window nil
  "Keep track of the currently selected window.")

(defvar zoom--update-timer nil
  "Timer for debouncing zoom updates.")

(defun zoom--window-selection-change (frame)
  "Handle window selection change in FRAME."
  (when (and zoom-mode (eq frame (selected-frame)))
    (zoom--schedule-update)))

(defun zoom--configuration-change ()
  "Handle window configuration change."
  (when zoom-mode
    (zoom--schedule-update 'balance)))

(defun zoom--buffer-change (window)
  "Handle buffer change in WINDOW."
  (when (and zoom-mode (eq window (selected-window)))
    (set-window-parameter window 'zoom--ignored 'unknown)
    (zoom--schedule-update)))

(defun zoom--schedule-update (&optional balance)
  "Schedule a zoom update, optionally with BALANCE."
  (when zoom--update-timer
    (cancel-timer zoom--update-timer))
  (setq zoom--update-timer
        (run-with-idle-timer 0.01 nil
                           (lambda ()
                             (setq zoom--update-timer nil)
                             (zoom--do-update balance)))))

(defun zoom--do-update (&optional balance)
  "Perform the actual update, optionally BALANCE windows first."
  (when (window-valid-p (selected-window))
    (let ((window (selected-window)))
      (unless (equal window zoom--last-window)
        (setq zoom--last-window window))
      (zoom--update balance))))

(defun zoom--update (&optional balance)
  "Update the window layout in the current frame.
If BALANCE is non-nil, balance windows first."
  (let (;; temporarily disables this mode during resize to avoid infinite
        ;; recursion (probably not needed thanks to the following)
        (zoom-mode nil)
        ;; temporarily disable all (even external) hooks about window
        ;; configuration changes to try to avoid potential flickering since
        ;; `balance-windows' calls them
        (window-configuration-change-hook nil)
        ;; make sure that other windows are resized nicely after resizing the
        ;; selected one
        (window-combination-resize t)
        ;; make sure that the exact same amount of pixels is assigned to all the
        ;; siblings
        (window-resize-pixelwise t))
    ;; only balance when explicitly requested (e.g., configuration changes)
    ;; and when no ignored windows are present
    (when (and balance (not (zoom--any-window-ignored-p)))
      (balance-windows))
    ;; check if the selected window is not ignored
    (unless (zoom--window-ignored-p)
      (zoom--resize)
      ;; fix the scrolling but not for image buffers
      (unless (derived-mode-p 'image-mode)
        (zoom--fix-scroll)))))

(defun zoom--window-ignored-p ()
  "Check whether the selected window will be ignored or not."
  (or
   ;; check global disable flag first
   zoom-disabled
   ;; check cache and predicates
   (let ((cached (window-parameter nil 'zoom--ignored)))
     ;; treat nil (unset) and 'unknown the same: evaluate predicates
     (if (or (null cached) (eq cached 'unknown))
         (let ((ignored
                (or
                 ;; `one-window-p' does not work well with the completion buffer
                 ;; when emacsclient is used
                 (frame-root-window-p (selected-window))
                 ;; never attempt to zoom the minibuffer
                 (window-minibuffer-p)
                 ;; check against the major mode (or its parents) (XXX this way of invoking
                 ;; `derived-mode-p' has been deprecated in Emacs 30 but it still works)
                 (apply 'derived-mode-p zoom-ignored-major-modes)
                 ;; check against the buffer name
                 (member (buffer-name) zoom-ignored-buffer-names)
                 ;; check against the buffer name (using a regexp)
                 (catch 'ignored
                   (dolist (regex zoom-ignored-buffer-name-regexps)
                     (when (string-match regex (buffer-name))
                       (throw 'ignored t))))
                 ;; check user-defined predicates
                 (catch 'ignored
                   (dolist (predicate zoom-ignore-predicates)
                     (when (funcall predicate)
                       (throw 'ignored t)))))))
           (set-window-parameter nil 'zoom--ignored ignored)
           ignored)
       cached))))

(defun zoom--any-window-ignored-p ()
  "Check whether any window in the current frame/tab would be ignored.
This function always re-evaluates predicates without using the cache
to avoid stale data when buffers change in non-selected windows.
It's also tab-aware to prevent cross-tab contamination."
  (or
   ;; check global disable flag first
   zoom-disabled
   ;; check all windows in the current tab's configuration, forcing fresh evaluation
   (catch 'found
     (dolist (window (window-list nil 'no-minibuffer))
       (with-selected-window window
         ;; Temporarily mark cache as unknown to force re-evaluation
         (let ((old-cache (window-parameter window 'zoom--ignored)))
           (unwind-protect
               (progn
                 (set-window-parameter window 'zoom--ignored 'unknown)
                 (when (zoom--window-ignored-p)
                   (throw 'found t)))
             ;; Don't restore old cache - let it be re-evaluated next time
             ))))
     nil)))

(defmacro zoom-with-disable (&rest body)
  "Execute BODY with zoom temporarily disabled.
This sets `zoom-disabled' to t, executes BODY, and restores the
previous value of `zoom-disabled'."
  (declare (indent 0) (debug t))
  `(let ((zoom-disabled t))
     ,@body))

(defun zoom--resize ()
  "Resize the selected window according to the user preference."
  (let ((size-hint-cons
         ;; either use the cons as is or call the custom function
         (if (functionp zoom-size) (funcall zoom-size) zoom-size)))
    (zoom--resize-one-dimension size-hint-cons t)
    (zoom--resize-one-dimension size-hint-cons nil)))

(defun zoom--resize-one-dimension (size-hint-cons horizontal)
  "Resize one dimension of the selected window according to the user preference.

Argument SIZE-HINT-CONS is the size hint provided by the user.

Argument HORIZONTAL determines whether the window should be
resized horizontally or vertically."
  (let* ((size-hint
          (if horizontal (car size-hint-cons) (cdr size-hint-cons)))
         (frame-size
          (if horizontal (frame-width) (frame-height)))
         ;; use the total size (including fringes, scroll bars, etc.) for ratios
         ;; and the body size for absolute values
         (window-size
          (if zoom-use-total-window-size
              (if horizontal (window-total-width) (window-total-height))
            (if horizontal (window-body-width) (window-body-height))))
         ;; either use an absolute value or a ratio
         (min-window-size
          (if (floatp size-hint) (round (* size-hint frame-size)) size-hint))
         ;; do not shrink the window if it is already large enough
         (desired-delta (max (- min-window-size window-size) 0))
         ;; fall back to the maximum available if the windows are too small
         (delta (window-resizable nil desired-delta horizontal)))
    ;; actually resize the window, but only if it's safe to do so
    (when (and (> delta 0)
               (not (frame-root-window-p (selected-window)))
               (not (window-minibuffer-p)))
      (condition-case err
          (window-resize nil delta horizontal)
        (error
         ;; silently ignore resize errors to prevent timer errors
         nil)))))

(defun zoom--fix-scroll ()
  "Fix the horizontal scrolling if needed."
  ;; scroll all the way to the left border
  (scroll-right (window-hscroll))
  ;; if the window is not wide enough to contain the point scroll to center
  ;; unless lines are not truncated
  (when (and truncate-lines
             (> (current-column) (- (window-body-width) hscroll-margin)))
    (scroll-left (- (current-column) (/ (window-body-width) 2)))))

(provide 'zoom)

;;; zoom.el ends here
