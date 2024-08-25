;;; scr-kb.el --- display an on-screen keyboard, for typing without a real one -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defgroup scr-kb nil
  "Display a visual on-screen keyboard."
  :prefix "scr-kb-"
  :group 'convenience)

(defcustom scr-kb-buffer-name "*On-screen keyboard*"
  "Name of the scr-kb buffer."
  :type 'string
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

(defvar scr-kb-keymap-regular
  '((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?= -1 -2)
    (?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?[ ?] -1 -3)
    (?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?' ?\\ -1 -4)
    (-1 ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/))
  "On-screen keyboard keymap.")

(defvar scr-kb-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'push-button)
    map)
  "The button map of the on-screen keyboard keys.")

(defmacro scr-kb-insert-button (key)
  "Insert button."
  (declare (indent 1))
  `(insert-button ,key
                  'supertype 'scr-kb-base
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

(define-button-type 'scr-kb-shift
  :supertype 'scr-kb-base
  'action (lambda (_button) (scr-kb-other-buffer-do (lambda () (insert ?\s)))))

(defun scr-kb-other-buffer-do (func)
  "Switch to the last window, call FUNC, go back."
  (other-window 1)
  (funcall func)
  (other-window -1))

(defun scr-kb-other-buffer-insert (key)
  "Switch to the last window and insert KEY."
  (other-window 1)
  (insert key)
  (other-window -1))

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
	      (t (scr-kb-insert-button key))))

(defun scr-kb-display (keymap)
  "Display the KEYMAP."
  (dolist (ele keymap)
    (dolist (key ele)
      (scr-kb-insert-key-button key)
      (insert " "))
    (insert "\n"))
  (backward-delete-char 1))

(defun scr-kb ()
  "Display a visual on-screen keyboard."
  (interactive)
  (let ((buffer (get-buffer-create scr-kb-buffer-name)))
    (pop-to-buffer buffer)
    (scr-kb-display scr-kb-keymap-regular)
    (fit-window-to-buffer)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(provide 'scr-kb)

;;; scr-kb.el ends here
