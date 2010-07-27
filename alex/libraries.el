
(autoload 'browse-kill-ring "browse-kill-ring.el" "" t)
;; (autoload ' "frame-cmds.el" "" t)
(autoload 'unbound "unbound.el" "" t)

(autoload 'tramp-mode "tramp" "" t)
(eval-after-load 'tramp
  (setq tramp-default-method "ssh"))

;; Setup Haml Mode
(autoload 'haml-mode "haml-mode.el" "" t)
(autoload 'sass-mode "sass-mode.el" "" t)
(add-to-list 'auto-mode-alist '("\\.haml$" . eruby-haml-mumamo))
(add-to-list 'auto-mode-alist '("\\*\\.haml$" . eruby-haml-mumamo))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(autoload 'haskell-mode "haskell-mode.el" "" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(eval-after-load 'haskell
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'font-lock-mode)
    (setq haskell-program-name "ghci")
    (add-hook 'haskell-mode-hook
              (lambda ()
                ;; TODO: check prev char and if its a space, don't insert a space
                ;; if not a space, insert a space and then the arrow
                (define-key haskell-mode-map [(control c) (\.)]
                  (lambda ()
                    (interactive)
                    (insert "-> ")))))))

;; Setup Color Scheme for Emacs
(require 'color-theme)
(setq color-theme-is-global t)
;; (color-theme-initialize)
;; (require 'color-theme-anb-dark)
;; (require 'color-theme-anb-light)
;; (color-theme-anb-dark)
;; (color-theme-anb-light)
;; (set-frame-font "Consolas-13")
;; (set-frame-font "Andale Mono")
(set-face-attribute 'default nil :height 80)

;; ruby mode
(autoload 'ruby-mode "ruby-mode.el" ""  t)
(autoload 'rinari "rinari.el" "" t)

;; (load "~/.emacs.d/elisp/nxhtml/autostart.el")
;; (setq nxhtml-global-minor-mode t
;;       mumamo-chunk-coloring 'submode-colored
;;       nxhtml-skip-welcome t
;;       indent-region-mode t
;;       rng-nxml-auto-validate-flag nil
;;       nxml-degraded t)
;; (add-to-list 'auto-mode-alist '("\\.*html\\.erb\\'" . eruby-nxhtml-mumamo))
;; (add-to-list 'auto-mode-alist '("\\.rhtml'" . eruby-nxhtml-mumamo))
;; (add-hook 'eruby-nxhtml-mumamo-mode-hook
;;           (lambda ()
;;             (local-unset-key [(control enter)] )))

;; ;;Push the mouse out of the way when the cursor approaches.
;; (if window-system
;;     (progn
;;       (autoload 'avoid "avoid" "Avoid mouse and cursor being near each other")
;;       (eval-after-load 'avoid (mouse-avoidance-mode 'jump))))


;; Interactively Do Things
(require 'ido)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;; Setup Ido-Mode for M-x COMMAND
(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
	 (mapatoms (lambda (s)
		     (when (commandp s)
		       (setq ido-execute-command-cache
			     (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))
(add-hook 'ido-setup-hook
	  (lambda ()
	    (setq ido-enable-flex-matching t)
	    (global-set-key "\M-x" 'ido-execute-command)))
(ido-load-history t)
(ido-mode t)

;; From: http://stackoverflow.com/questions/905338/can-i-use-ido-completing-read-instead-of-completing-read-everywhere/907060
(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

  Set it to nil using let in around-advice for functions where the
  original completing-read is required.  For example, if a function
  foo absolutely must use the original completing-read, define some
  advice like this:

  (defadvice foo (around original-completing-read-only activate)
    (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                               allcomp
                               nil require-match initial-input hist def))
        ad-do-it))))

(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))
(global-set-key "\M-p" 'ido-goto-symbol)


