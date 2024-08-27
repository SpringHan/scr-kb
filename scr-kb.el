;;; scr-kb.el --- display an on-screen keyboard, for typing without a real one -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'posframe)

(defgroup scr-kb nil
  "Display a visual on-screen keyboard."
  :prefix "scr-kb-"
  :group 'convenience)

(defcustom scr-kb-buffer-name " *On-screen keyboard*"
  "Name of the scr-kb buffer."
  :type 'string
  :group 'scr-kb)

(defcustom scr-kb-main-frame (selected-frame)
  "Main frame."
  :group 'scr-kb)

(defcustom scr-kb-displayed nil
  "Whether the keyboard is showing."
  :type 'boolean
  :group 'scr-kb)

(defcustom scr-kb-insert-key nil
  "The key to be inserted."
  :type 'number
  :group 'scr-kb)

(defcustom scr-kb-shift nil
  "Shift mode.
It'll keep inserting upper case letters when its value is lock.
Just one upper case letter will be inserted when its value is t."
  :type 'symbol
  :group 'scr-kb)

(defcustom scr-kb-position nil
  "A cons, which is the position of floating keyboard."
  :type 'list
  :group 'scr-kb)

(defcustom scr-kb-height 0
  "The height of keyboard."
  :type 'number
  :group 'scr-kb)

(defcustom scr-kb-width 0
  "The width of keyboard."
  :type 'number
  :group 'scr-kb)

(defvar scr-kb-keymap-regular
  '((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 -1 -1)
    (?q ?w ?e ?r ?t ?y ?u ?i ?o ?p -1 -3)
    (?a ?s ?d ?f ?g ?h ?j ?k ?l -1 -4)
    (-5 ?z ?x ?c ?v ?b ?n ?m -2))
  "On-screen keyboard keymap.")

(defvar scr-kb-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ignore)
    (define-key map [down-mouse-1] 'push-button)
    (define-key map [double-down-mouse-1] 'push-button)
    (define-key map [triple-down-mouse-1] 'push-button)
    map)
  "The button map of the on-screen keyboard keys.")

(defmacro scr-kb-insert-button (key)
  "Insert button."
  (declare (indent 1))
  `(insert-button ,key
                  'type 'scr-kb-base
                  'action (lambda (_button)
                            (scr-kb-other-buffer-insert ,key))))

(define-button-type 'scr-kb-base
  'help-echo "mouse-1, RET: Push this button"
  'keymap scr-kb-button-map
  'face 'tool-bar
  'mouse-face 'tool-bar)

(define-button-type 'scr-kb-key
  :supertype 'scr-kb-base
  'action (lambda (_button) (scr-kb-other-buffer-do (lambda () (insert key)))))

(define-button-type 'scr-kb-backspace
  :supertype 'scr-kb-base
  'action (lambda (_button) (scr-kb-other-buffer-do (lambda () (backward-delete-char-untabify 1)))))

(define-button-type 'scr-kb-return
  :supertype 'scr-kb-base
  'action (lambda (_button) (scr-kb-other-buffer-do #'newline)))

(define-button-type 'scr-kb-space
  :supertype 'scr-kb-base
  'action (lambda (_button) (scr-kb-other-buffer-do (lambda () (insert ?\s)))))

(define-button-type 'scr-kb--shift
  :supertype 'scr-kb-base
  'action #'scr-kb--shift-switch)

(defun scr-kb--shift-switch (_button)
  "Shift switch func."
  (setq scr-kb-shift (pcase scr-kb-shift
                       ('t 'lock)
                       ('lock 'nil)
                       ('nil t))))

(defun scr-kb-other-buffer-do (func)
  "Switch to the last window, call FUNC, go back."
  (with-selected-frame scr-kb-main-frame
    (funcall func)))

(defun scr-kb-other-buffer-insert (key)
  "Switch to the last window and insert KEY."
  (with-selected-frame scr-kb-main-frame
    (when scr-kb-shift
      (setq key (upcase key))

      (when (eq scr-kb-shift t)
        (setq scr-kb-shift nil)))
    (insert key)))

(defun scr-kb-insert-key-button (key)
  "Insert KEY into current buffer, with corresponding action.
If KEY is -1 (literal byte value), insert a space.
If KEY is -2, insert a backspace button.
If KEY is -3, insert a return button.
If KEY is -4, insert a space button.
Otherwise, insert the button corresponding to KEY."
  (cond ((= key -1) (insert " "))
	      ((= key -2) (insert-button "BKSP" :type 'scr-kb-backspace))
	      ((= key -3) (insert-button "RTRN" :type 'scr-kb-return))
	      ((= key -4) (insert-button "SPAC" :type 'scr-kb-space))
        ((= key -5) (insert-button "SHIF" :type 'scr-kb--shift))
	      (t (scr-kb-insert-button key))))

(defun scr-kb-double-push-button (&optional pos use-mouse-action)
  "Double pushing button."
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)))
  (push-button pos use-mouse-action)
  (push-button pos use-mouse-action))

(defun scr-kb-display (keymap)
  "Display the KEYMAP."
  (let ((width (window-text-width)))
    (dolist (ele keymap)
      (let* ((key-num (length ele))
             (space-times (/ (- width key-num) (1+ key-num)))
             (space (scr-kb--repeat-string " " space-times)))
        (insert space)
        (dolist (key ele)
          (scr-kb-insert-key-button key)
          (insert space)))
      (insert "\n")))
  (backward-delete-char 1))

(defun scr-kb--repeat-string (string times)
  "Repeat string over TIMES."
  (let ((result string))
    (dotimes (i (1- times))
      (setq result (concat result string)))
    result))

(defun scr-kb-center-window ()
  "Return a cons, storing the center position for the keyboard."
  (let ((x 0)
        (y (/ (- (frame-text-height scr-kb-main-frame)
                 (round (* 0.4 (frame-text-height scr-kb-main-frame)))
                 ;; scr-kb-height
                 )
              2)))
    (cons x y)))

(defun scr-kb-set-default-size ()
  "Calculate and set the default size of keyboard."
  (let ((width (frame-width scr-kb-main-frame))
        (height (round (* 0.4 (frame-height scr-kb-main-frame)))))
    (setq scr-kb-width width
          scr-kb-height height)))

(defun scr-kb ()
  "Display a visual on-screen keyboard."
  (interactive)

  (if scr-kb-displayed
      (progn
        (posframe-delete-frame scr-kb-buffer-name)
        (setq scr-kb-displayed nil))

    ;; Initialize default size and position.
    (when (or (= scr-kb-width 0)
              (= scr-kb-height 0))
      (scr-kb-set-default-size))
    (unless scr-kb-position
      (setq scr-kb-position (scr-kb-center-window)))

    (let ((buffer scr-kb-buffer-name))
      ;; (pop-to-buffer buffer)
      ;; TODO: Note here when adding DIY things.
      (unless (get-buffer scr-kb-buffer-name)
        (with-current-buffer (get-buffer-create scr-kb-buffer-name)
          (scr-kb-display scr-kb-keymap-regular)
          (goto-char (point-min))
          (setq buffer-read-only t)
          (setq-local cursor-type nil)))

      (setq scr-kb-displayed t)

      (posframe-show scr-kb-buffer-name
                     :position scr-kb-position
                     :width scr-kb-width)
      ;; (set-frame-parameter (nth 1 (visible-frame-list)) 'background-color "#000000")
      ;; (fit-frame-to-buffer (nth 1 (visible-frame-list))
      ;;                      nil nil nil nil 'vertically)

      ;; (with-selected-frame)
      ;; (fit-window-to-buffer)
      ;; (goto-char (point-min))
      ;; (setq buffer-read-only t)
      ;; (setq-local cursor-type nil)
      )))

(global-set-key (kbd "<f4>") #'scr-kb)

(provide 'scr-kb)

;;; scr-kb.el ends here
