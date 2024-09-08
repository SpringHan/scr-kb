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
  '((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
    (?q ?w ?e ?r ?t ?y ?u ?i ?o ?p)
    (?a ?s ?d ?f ?g ?h ?j ?k ?l)
    (shift ?z ?x ?c ?v ?b ?n ?m backspace)
    (fn space return))
  "On-screen keyboard keymap.")

(defvar scr-kb-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ignore)
    (define-key map [down-mouse-1] 'push-button)
    (define-key map [double-down-mouse-1] 'push-button)
    (define-key map [triple-down-mouse-1] 'push-button)
    map)
  "The button map of the on-screen keyboard keys.")

(defmacro scr-kb-insert-button (key &optional display)
  "Insert button."
  (declare (indent 1))
  `(insert-button ,(if display
                       display
                     key)
                  'type 'scr-kb-base
                  'action (lambda (_button)
                            (scr-kb-other-buffer-do ,key))))

(define-button-type 'scr-kb-base
  'keymap scr-kb-button-map
  'face 'tool-bar
  'mouse-face 'tool-bar)

(define-button-type 'scr-kb-key
  :supertype 'scr-kb-base
  'action (lambda (_button) (scr-kb-origin-frame-do (lambda () (insert key)))))

;; (define-button-type 'scr-kb-backspace
;;   :supertype 'scr-kb-base
;;   'action (lambda (_button) (scr-kb-origin-frame-do (lambda () (backward-delete-char-untabify 1)))))

;; (define-button-type 'scr-kb-return
;;   :supertype 'scr-kb-base
;;   'action (lambda (_button) (scr-kb-origin-frame-do #'newline)))

;; (define-button-type 'scr-kb-space
;;   :supertype 'scr-kb-base
;;   'action (lambda (_button) (scr-kb-origin-frame-do (lambda () (insert ?\s)))))

(define-button-type 'scr-kb-fn
  :supertype 'scr-kb-base
  'action (lambda (_button) (scr-kb-origin-frame-do (lambda () (insert ?\s)))))

(define-button-type 'scr-kb--shift
  :supertype 'scr-kb-base
  'action #'scr-kb--shift-switch)

(defun scr-kb--shift-switch (_button)
  "Shift switch func."
  (setq scr-kb-shift (pcase scr-kb-shift
                       ('t 'lock)
                       ('lock 'nil)
                       ('nil t))))

;; (defun scr-kb-origin-frame-do (func)
;;   "Operating in the original frame."
;;   (with-selected-frame scr-kb-main-frame
;;     (funcall func)))

(defun scr-kb-other-buffer-do (key)
  "Switch to the last window and insert KEY."
  (with-selected-frame scr-kb-main-frame
    (when (numberp key)
      (when scr-kb-shift
        (setq key (upcase key))

        (when (eq scr-kb-shift t)
          (setq scr-kb-shift nil))))

    (let ((func (key-binding (read-kbd-macro (if (numberp key)
                                                 (char-to-string key)
                                               key)))))
      (if (eq func 'self-insert-command)
          (insert (pcase key
                    ("SPC" " ")
                    ("RET" "\n")
                    (_ key)))
        (call-interactively func)))))

(defun scr-kb-insert-key-button (key)
  "Insert KEY into current buffer, with corresponding action."
  (pcase key
    ;; ( (insert " "))
    ('shift (insert-button "" :type 'scr-kb--shift))
    ('fn (insert-button "" :type 'scr-kb-fn))
	  ('backspace (scr-kb-insert-button "DEL" "")
                ;; (insert-button  :type 'scr-kb-backspace)
                )
	  ('return (scr-kb-insert-button "RET" "")
             ;; (insert-button "" :type 'scr-kb-return)
             )
	  ('space (scr-kb-insert-button "SPC" "")
            ;; (insert-button "" :type 'scr-kb-space)
            )
	  (_ (scr-kb-insert-button key))))

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
        ;; (insert space)
        (dolist (key ele)
          (scr-kb-insert-key-button key)
          (insert " ")
          ))
      (backward-delete-char 1)    
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
          (let* ((origin-height (frame-char-height))
                 (abs-height (/ (float (* scr-kb-height origin-height))
                                (length scr-kb-keymap-regular)))
                 (relative (/ (float abs-height) origin-height)))
            (scr-kb-display scr-kb-keymap-regular)
            (text-scale-increase relative))
          (goto-char (point-min))
          (center-region (point-min) (point-max))
          (setq buffer-read-only t)
          (setq-local cursor-type nil)))

      (setq scr-kb-displayed t)

      (posframe-show scr-kb-buffer-name
                     :position scr-kb-position
                     :width scr-kb-width
                     )
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
