# Zoom.el Fork Changes and Improvements

## Performance and Memory Issues (Original Problems)

### Main Memory Issues

**1. Excessive `zoom--get-frame-snapshot()` calls**
The original function created a string representation of the entire window layout on every redisplay event:

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
The zoom handler ran on `pre-redisplay-function`, meaning it executed before every screen refresh. When it detected changes, it:

- Called `balance-windows` (expensive operation)
- Triggered `window-resize` operations
- These operations could trigger more redisplay events, creating a cycle

**3. Frame parameter overhead**
Every handler call set `zoom--frame-snapshot` as a frame parameter, which involved string comparison of potentially large layout descriptions.

### Solutions Implemented

**1. Replaced redisplay hooks with event-based hooks:**
The fork now uses specific window event hooks instead of `pre-redisplay-function`:

```elisp
(add-hook 'window-selection-change-functions #'zoom--window-selection-change)
(add-hook 'window-configuration-change-hook #'zoom--configuration-change)
(add-hook 'window-buffer-change-functions #'zoom--buffer-change)
```

This ensures updates only happen when windows actually change, not on every screen refresh.

**2. Added debouncing/throttling:**
Implemented timer-based debouncing to prevent excessive updates:

```elisp
(defvar zoom--update-timer nil)

(defun zoom--schedule-update (&optional balance)
  (when zoom--update-timer
    (cancel-timer zoom--update-timer))
  (setq zoom--update-timer
        (run-with-idle-timer 0.01 nil
                           (lambda ()
                             (setq zoom--update-timer nil)
                             (zoom--do-update balance)))))
```

**3. Window parameter caching:**
Cache ignore predicate results per window to avoid re-evaluating on every update:

```elisp
(set-window-parameter nil 'zoom--ignored ignored)
```

**4. Removed frame snapshot overhead:**
Eliminated the `zoom--get-frame-snapshot` mechanism entirely by relying on event hooks instead of polling.

## Ignore Predicates and Window Balancing (Latest Changes)

### Problem: Balancing Destroyed Custom Layouts

Previously, the ignore predicates (`zoom-ignored-major-modes`, `zoom-ignored-buffer-names`, `zoom-ignored-buffer-name-regexps`, and `zoom-ignore-predicates`) only prevented the selected window from being zoomed/resized. However, `balance-windows` was still called unconditionally during window configuration changes, which would destroy custom window layouts like ediff's three-panel setup.

For example, with ediff:
- Ediff creates a specific layout (two large windows side-by-side, small control panel at bottom)
- Even with ediff windows marked as ignored, `balance-windows` would equalize all window sizes
- Result: ediff control panel takes up 1/3 of the screen instead of staying small

### Solution: Conditional Balancing

Modified `zoom--update` to check for ignored windows before calling `balance-windows`:

```elisp
;; only balance when explicitly requested (e.g., configuration changes)
;; and when no ignored windows are present
(when (and balance (not (zoom--any-window-ignored-p)))
  (balance-windows))
```

Added `zoom--any-window-ignored-p` function that:
- Iterates through all windows in the current frame/tab
- Forces fresh predicate evaluation (bypassing cache) to avoid stale data
- Returns `t` if ANY window would be ignored according to the predicates

**Key insight:** If any window should be ignored (e.g., ediff control panel), the entire frame likely has a custom layout that shouldn't be automatically balanced.

### Cache Invalidation for Tab-Bar-Mode

**Problem:** Window parameters (`zoom--ignored`) could become stale when:
1. Windows are reused after ediff exits with different buffers
2. Tab switching reuses window objects with different buffers
3. Cross-tab contamination: ediff in Tab A affects zoom in Tab B

**Solution:** Force re-evaluation in `zoom--any-window-ignored-p`:

```elisp
(dolist (window (window-list nil 'no-minibuffer))
  (with-selected-window window
    ;; Temporarily mark cache as unknown to force re-evaluation
    (set-window-parameter window 'zoom--ignored 'unknown)
    (when (zoom--window-ignored-p)
      (throw 'found t))))
```

This ensures predicates are always evaluated against current buffer state, not cached state from previous tab configurations.

### Ediff Integration Best Practices

For proper ediff integration with tab-bar-mode, use a **buffer-local** predicate:

```elisp
(defun my/ediff-in-progress-p ()
  "Return non-nil if current buffer is part of an active ediff session."
  (or (and (boundp 'ediff-this-buffer-ediff-sessions)
           ediff-this-buffer-ediff-sessions)
      (eq major-mode 'ediff-mode)))

(add-to-list 'zoom-ignore-predicates #'my/ediff-in-progress-p)
```

**Important:** Don't use global buffer checks like this:

```elisp
;; BAD - affects all tabs when ediff is active anywhere
(defun my/ediff-in-progress-p ()
  (let ((ediff-active nil))
    (dolist (buf (buffer-list))  ; ← Checks ALL buffers globally
      (with-current-buffer buf
        (when (and (boundp 'ediff-control-buffer)
                   ediff-control-buffer
                   (buffer-live-p ediff-control-buffer))
          (setq ediff-active t))))
    ediff-active))
```

The problem with the global approach:
- `(buffer-list)` returns ALL buffers across ALL tabs
- When ediff runs in Tab A, the control buffer exists globally
- Predicate returns `t` even in Tab B where ediff isn't visible
- Result: zoom disabled in Tab B even though ediff isn't there

The buffer-local approach:
- Only checks the current buffer (the one in the window being evaluated)
- Tab A with ediff: windows with ediff buffers return `t` → zoom disabled
- Tab B without ediff: windows with normal buffers return `nil` → zoom works normally

### Updated Documentation

The custom variable docstrings now reflect that ignore predicates affect both zooming and balancing:

```elisp
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
```
