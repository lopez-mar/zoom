## Main Memory Issues

**1. Excessive `zoom--get-frame-snapshot()` calls**
The function creates a string representation of the entire window layout on every redisplay event:

```elisp
(defun zoom--get-frame-snapshot ()
  (format "%s" (list (default-value 'track-mouse)
                     (mapcar (lambda (window) (list window
                                                    (window-total-width)
                                                    (window-total-height)))
                             (window-list)))))
```

This function:

- Calls `(window-list)` which creates a new list of all windows
- Maps over every window creating nested lists with window objects and dimensions
- Converts the entire structure to a string with `format "%s"`
- Does this on **every single redisplay** (which can be dozens of times per second)

**2. Constant window resizing triggering `balance-windows`**
The zoom handler runs on `pre-redisplay-function`, meaning it executes before every screen refresh. When it detects changes, it:

- Calls `balance-windows` (expensive operation)
- Triggers `window-resize` operations
- These operations can trigger more redisplay events, creating a cycle

**3. Frame parameter overhead**
Every handler call sets `zoom--frame-snapshot` as a frame parameter, which involves string comparison of potentially large layout descriptions.

## Solutions to Reduce Memory Usage

**1. Optimize the snapshot function:**

```elisp
(defun zoom--get-frame-snapshot ()
  ;; Use a more efficient representation
  (cons (default-value 'track-mouse)
        (mapcar (lambda (w) (cons (window-total-width w) (window-total-height w)))
                (window-list))))
```

**2. Add debouncing/throttling:**
Instead of running on every redisplay, add a timer-based approach:

```elisp
(defvar zoom--update-timer nil)

(defun zoom--handler (&optional ignored)
  (when zoom--update-timer
    (cancel-timer zoom--update-timer))
  (setq zoom--update-timer
        (run-with-idle-timer 0.1 nil #'zoom--do-update)))
```

**3. Use a less expensive comparison:**
Instead of string comparison, use `equal` on the actual data structure.

**4. Cache window dimensions:**
Only recalculate when the frame size actually changes, not on every window selection.
