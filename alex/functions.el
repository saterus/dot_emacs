(defun dot-emacs ()
  "opening-dot-emacs"
  (interactive)
  (dired "~/.emacs.d/alex*" "-Rl")
  (select-window (get-buffer-window "alex*"))
  (search-forward "alex")
  (dired-maybe-insert-subdir "alex")
  (search-backward "alex.el"))

(defun reload-dot-emacs ()
  "reload ~/.emacs"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun copy-line ()
  "Copy 1 Line"
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(defun prev-buffer (&optional arg)
  "Analog to C-x-o. Moves back a buffer."
  (interactive "p")
  (let ((step (if arg arg 1)))
    (other-window (- 0 step))))

;; Beautiful little gem from emacs.wordpress.com
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun craft-regexp ()
  "Uses regexp-opt to create a regexp from a list of strings."
  (interactive)
  (let ((collect-arg (lambda ()
                      (read-regexp "Enter regex (blank to insert): ")))
        (collect-args (lambda (&optional acc)
                        (let ((val (collect-arg))
                              (lst (if (nil? acc) '() acc))))
                        (if (nil? val)
                            lst
                          (collect-args (cons val lst)))))
        (args (collect-args))))
  (if (nil? args)
      (error (message "You must enter at least one regexp."))
    (regexp-opt args)))

;; Kill buffer, window, and go back to previous buffer. Handy for temporary buffers or dired-open buffers.
(defun kill-buffer-and-window ()
  "Kill current buffer, then the current window, then move back one buffer."
  (interactive)
  (kill-buffer)
  (delete-window)
  (prev-buffer))

;; Insert debug statement.
(defun ruby-debug-statement ()
  (interactive)
  (insert "(require 'ruby-debug'; debugger;) # DEBUG-TODO: REMOVE DEBUG STATEMENT")
  (search-backward "#"))

;Don't bother entering search and replace args if the buffer is
;read-only. Duh.
(defadvice query-replace-read-args (before barf-if-buffer-read-only
                                           activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

(defun insert-regexp-group () 
  (interactive)
  (insert "\\(.+\\)"))

(defun wrap-region (b e obracket cbracket)
  "'tag' a region"
  (interactive "r\nMWrap region open bracket: \nMWrap region close bracket: ")
  (save-excursion
    (goto-char e)
    (insert (format "%s" cbracket))
    (goto-char b)
    (insert (format "%s" obracket))))


(defun wrap-xml-brackets (b e tag)
  "wrap region with <sometag>YOUR CONTENT</sometag"
  (interactive "r\nMTag: ")
  (wrap-region b e (format "<%s>" tag) (format "</%s>" tag)))


(defun wrap-with-quotes (b e &optional arg)
  "Wraps a region with double quotes. If passed argument, uses single quotes instead."
  (interactive "r\nP")
  (if arg
      (wrap-region b e "'" "'")
    (wrap-region b e "\"" "\"")))


(defun delete-horizontal-whitespace-forward ()
     "Deletes all spaces and tabs between point and next non-whitespace char."
     (interactive "*")
     (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
 (t
  (setq i 1)
  (setq numWindows (count-windows))
  (while  (< i numWindows)
    (let* (
           (w1 (elt (window-list) i))
           (w2 (elt (window-list) (+ (% i numWindows) 1)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2))
           )
      (set-window-buffer w1  b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)
      (setq i (1+ i)))))))


;; find-alternative-file-with-sudo from http://www.emacswiki.org/emacs/TrampMode
(defun find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))


