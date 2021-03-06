;; variables
(setq vc-follow-symlinks t
      line-number-mode    t
      column-number-mode  t
      default-truncate-lines t
      completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".svn" ".obj" ".map" ".a" ".ln" ".class")
      ido-ignore-directories '(".git")
      backup-directory-alist `((".*" . ,"~/.emacs.d/backups/"))
      auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backups/" t))
      hl-line-face 'hl-line
      redisplay-dont-pause t)

(setq-default show-trailing-whitespace t
              cursor-type 'bar
              automatic-hscrolling t
              indent-tabs-mode nil)
(set-face-background 'trailing-whitespace "#ffcccc")
(defun interpreter-hook ()
  (progn
     (set-face-background 'trailing-whitespace nil)))
(add-hook 'comint-mode-hook 'interpreter-hook)
(add-hook 'repl-mode-hook 'interpreter-hook)
(add-hook 'slime-mode-hook 'interpreter-hook)
(add-hook 'slime-repl-mode-hook 'interpreter-hook)

(put 'set-goal-column 'disabled nil)
     ;; 'dired-find-alternate-file 'disabled nil)


;; global modes & hooks
(global-hl-line-mode t)
(global-linum-mode t)
(add-hook 'after-change-major-mode-hook 'linum-on)

;; no tooltip when trying to use the mode-bar's commands
(tooltip-mode nil)

;; automatically reload files that have changed on disk
;; note for pairing: ":set autoread" in VIM
(global-auto-revert-mode 1)

;; Open all dired directories in the same buffer
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "<return>")
    'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;; auto mode lists
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js.rjs" . ruby-mode))
(add-to-list 'auto-mode-alist '("irb_tempfile\\.*" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$*" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thrift$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(c-add-style "openbsd"
  '("bsd"
    (indent-tabs-mode . t)
    (defun-block-intro . 8)
    (statement-block-intro . 8)
    (statement-case-intro . 8)
    (substatement-open . 4)
    (substatement . 8)
    (arglist-cont-nonempty . 4)
    (inclass . 8)
    (knr-argdecl-intro . 8)))

(setq c-default-style "openbsd")


;; additional files to load
;; (load-file "~/.emacs.d/alex/functions.el")
;; (load-file "~/.emacs.d/alex/keys.el")

;; (set-frame-font "Inconsolata-g-9")
;; (set-frame-font "Liberation Mono-9")
;; (set-default-font "DejaVu Sans Mono-9")

;; Invaluable for debugging. Keeping here so I remember it.
;; (setq debug-on-error t)
