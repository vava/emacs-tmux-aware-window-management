(defun tmux-fm-one-window-per-frame-mode-on ()
  "Turns on one window per frame mode. After switching it on,
emacs will not split your frames, instead it will reuse them, in
more or less sensible manner. It will not reuse frames from
invisible workspaces either and will prefer to replace special
kind of buffers or least recently used ones. Works only in Emacs 24."
  (interactive)
  (tmux-fm-one-window-per-frame-mode t))

(defun tmux-fm-one-window-per-frame-mode-off ()
  "Turns off one window per frame mode. This is the default."
  (interactive)
  (tmux-fm-one-window-per-frame-mode nil))

;;; Internal functions

(defun tmux-fm-one-window-per-frame-mode (turn-on)
  (if turn-on (progn (advice-add 'select-frame
                                 :before #'tmux-fm-timestamp-frame-selection)
                     (advice-add 'pop-to-buffer-same-window
                                 :override #'tmux-fm-pop-to-buffer-same-window)
                     (cl-pushnew 'tmux-fm-display-buffer-use-some-frame (car
                                                                         display-buffer-overriding-action)))
    (advice-remove 'select-frame #'tmux-fm-timestamp-frame-selection)
    (advice-remove 'pop-to-buffer-same-window #'tmux-fm-pop-to-buffer-same-window)
    (cl-callf2 delq 'tmux-fm-display-buffer-use-some-frame (car display-buffer-overriding-action))))

;;; Advices
(defun tmux-fm-pop-to-buffer-same-window (buffer &optional norecord)
  (let ((display-buffer-overriding-action '(nil . nil)))
    (pop-to-buffer buffer display-buffer--same-window-action norecord)))

(defun tmux-fm-timestamp-frame-selection (frame &rest _)
  (set-frame-parameter frame 'tmux-fm-frame-selected-time (current-time)))

;;; Helper functions

(defun tmux-fm-map-and-filter (function list)
  (delq nil (mapcar function list)))

(defun tmux-fm-get-frame-buffer (frame)
  (car (frame-parameter frame 'buffer-list)))

(defun tmux-fm-get-frame-selected-time (frame)
  (float-time (frame-parameter frame 'tmux-fm-frame-selected-time)))

(defun tmux-fm-filter-all-but-special-buffer-frames (frames)
  (tmux-fm-map-and-filter (lambda (f)
                            (when (not (buffer-file-name (tmux-fm-get-frame-buffer f))) f)) frames))

(defun tmux-fm-filter-frames-by-buffer (buffer frames)
  (tmux-fm-map-and-filter (lambda(f)
                            (when (memq buffer (frame-parameter f 'buffer-list)) f)) frames))

(defun tmux-fm-sort-frames-by-buffer (buffer frames)
  (sort frames (lambda(f1 f2)
                 (< (cl-position buffer (frame-parameter f1 'buffer-list))
                    (cl-position buffer (frame-parameter f2 'buffer-list))))))

(defun tmux-fm-sort-frames-by-selected-time (frames)
  (sort frames (lambda (f1 f2)
                 (< (tmux-fm-get-frame-selected-time f1)
                    (tmux-fm-get-frame-selected-time f2)))))


(defun tmux-fm-get-frame-showing-buffer (buffer frames)
  (cl-find-if (lambda (f)
                (eq (car (frame-parameter f 'buffer-list)) buffer)) frames))

(defun tmux-fm-get-frame-most-recently-displayed-buffer (buffer frames)
  (car (tmux-fm-sort-frames-by-buffer buffer (tmux-fm-filter-frames-by-buffer buffer frames))))

(defun tmux-fm-get-frame-least-recently-used (frames)
  (car (tmux-fm-sort-frames-by-selected-time frames)))

(defun tmux-fm-get-popup-frame-for-buffer (buffer)
  (let* ((frames (tmux-fm-visible-frame-list))
         (frames-no-selected-frame (remove (selected-frame) frames))
         (special-frames (tmux-fm-filter-all-but-special-buffer-frames frames-no-selected-frame)))
    (or (tmux-fm-get-frame-showing-buffer buffer frames)
        (tmux-fm-get-frame-most-recently-displayed-buffer buffer special-frames)
        (tmux-fm-get-frame-least-recently-used special-frames)
        (tmux-fm-get-frame-most-recently-displayed-buffer buffer frames-no-selected-frame)
        (tmux-fm-get-frame-least-recently-used frames-no-selected-frame))))

(defun tmux-fm-get-window-for-frame (frame)
  (let ((selected-window (frame-selected-window frame)))
    (if (window-minibuffer-p selected-window)
        (next-window selected-window) selected-window)))

(defun tmux-fm-display-buffer-use-some-frame (buffer alist)
  (ignore alist)
  (when (and (not (display-graphic-p))
             (not (or (member (buffer-name buffer)
                              '("*Completions*" " *undo-tree*"))
                      (string-match-p "\\`[*][Hh]elm.*[*]\\'" (buffer-name buffer)))))
    (let* ((frame (tmux-fm-get-popup-frame-for-buffer buffer))
           (window (tmux-fm-get-window-for-frame frame)))
      (when (and frame
                 window)
        (window--display-buffer buffer window 'reuse)))))

(defun tmux-fm-all-tmux-panes ()
  (let ((lines (split-string (shell-command-to-string
                              "tmux list-panes -a -F '#{pane_tty} #{window_id}'") "\n" t))
        (hash (make-hash-table :test 'equal)))
    (dolist (s lines hash)
      (let ((l (split-string s " " t)))
        (puthash (elt l 0)
                 (elt l 1) hash)))))

(defun tmux-fm-selected-tmux-window-id (tmux-panes-hash)
  (let ((selected-pty (frame-parameter (selected-frame) 'tty)))
    (gethash selected-pty tmux-panes-hash)))

(defun tmux-fm-filter-frames-from-same-tmux-window (tmux-panes-hash frames)
  (let ((window-id (tmux-fm-selected-tmux-window-id tmux-panes-hash)))
    (seq-filter (lambda (f)
                  (let* ((frame-pty (frame-parameter f 'tty))
                         (frame-window-id (gethash frame-pty tmux-panes-hash)))
                    (and frame-window-id
                         (string= window-id frame-window-id)))) frames)))

(defun tmux-fm-visible-frame-list ()
  (tmux-fm-filter-frames-from-same-tmux-window (tmux-fm-all-tmux-panes)
                                               (frame-list)))

(provide 'tmux-fm-integration)
