;; Open the .emacs file
(defun dot-emacs ()
  "opening-dot-emacs"
  (interactive) ;this makes the function a command too
  (find-file "~/.emacs"))
(global-set-key [(control \#)] 'dot-emacs)

;; Reload .emacs
(defun reload-dot-emacs ()
  "reload ~/.emacs"
  (interactive)
  (load-file (expand-file-name "~/.emacs")))
(global-set-key [(meta \#)] 'reload-dot-emacs)

;; Make emacs automatically open the symlink
(setq vc-follow-symlinks t)

;; Make all “yes or no” prompts show “y or n” instead.
(fset 'yes-or-no-p 'y-or-n-p)

; Do without annoying startup msg.
(setq inhibit-startup-message t)

;; Hide Toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Transient Mark Mode
(transient-mark-mode t)

;; Line-wrapping
(set-default 'fill-column 80)
;;(auto-fill-mode t)

;; Display line numberss, column numbers, and trailing whitespace
(setq line-number-mode    t)
(setq column-number-mode  t)
(setq show-trailing-whitespace t)
(global-set-key [(hyper w)] 'delete-trailing-whitespace)

;; Tabs expand into spaces.
;; Type C-q C-i to insert a horizontal tab character.
(setq-default indent-tabs-mode nil)

;; Avoid errors with config files requiring a newline at the eof.
(setq require-final-newline t)

;; Set-Goal-Column is disabled by default.
;; Use C-u C-x C-n to remove goal column
(put 'set-goal-column 'disabled nil)

;; ignore
(setq completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".svn" ".obj" ".map" ".a" ".ln" ".class"))

(defvar site-lisp
  (if (eq system-type 'darwin)
      (concat (getenv "EMACS_HOME") "/../Resources/site-lisp")
    "/usr/share/emacs-snapshot/site-lisp"))
(defvar clojure-home
  (if (eq system-type 'darwin)
      "~/Library/Clojure/"
    nil)) ;; todo: download clojure for linux.

;; Recursively add subdirectories to the path.
(progn (let ((current-dir (substring (pwd) 10 -1)))
             (cd "~/.emacs.d/elisp")
             (normal-top-level-add-subdirs-to-load-path)
             (cd site-lisp)
             (normal-top-level-add-subdirs-to-load-path)
             (cd current-dir)))

;; Line Numbers
;;(require 'linum)
(global-linum-mode 1)

;; Fix meta/hyper keys for osx
;; TODO: add check for window system so i dont have to reload .emacs after initialization
(if (eq system-type 'darwin)
    (progn
      (setq mac-pass-command-to-system nil)
      (global-set-key "\C-q" 'quoted-insert)
      (setq mac-command-modifier 'control)
      (setq mac-control-modifier 'meta)
      (setq mac-option-modifier 'hyper)
      (global-set-key [(control \`)] 'other-frame)
      (global-set-key [(control \~)] '(lambda () (interactive)(other-frame -1)))))

;; Fix Copy/Paste from outside Emacs
(global-set-key [(super x)] 'clipboard-kill-region)
(global-set-key [(super c)] 'clipboard-kill-ring-save)
(global-set-key [(super v)] 'clipboard-yank)

;; Copy 1 Line (M-f)
(defun copy-line ()
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

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
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Kill buffer, window, and go back to previous buffer. Handy for temporary buffers or dired-open buffers.
(defun kill-buffer-and-window ()
  "Kill current buffer, then the current window, then move back one buffer."
  (interactive)
  (kill-buffer)
  (delete-window)
  (prev-buffer))
(global-set-key "\C-x9" 'kill-buffer-and-window)

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

(add-to-list 'load-path "~/.emacs.d/elisp/ruby-mode/")
;; (require 'ruby-mode)
;; (require 'ruby-electric)
;; (require 'ri-emacs)
(autoload 'ruby-mode "ruby-mode.el" "ruby mode"  t)
(autoload 'ruby-electric-mode "ruby-electric.el" "completes braces and such"  t)
(setq ri-ruby-script "~/.emacs.d/elisp/ruby-mode/ri-emacs.rb")
(autoload 'ri "ri-ruby.el" "ri docs inside emacs!"  t)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook (lambda ()
                            (local-set-key [(meta i)] 'ri)
                            (local-set-key [(meta o)] 'ri-ruby-complete-symbol)
                            (local-set-key [(meta I)] 'ri-ruby-show-args)))
(eval-after-load "ruby-electric"
  '(progn
     (define-key ruby-mode-map [(control j)] 'backward-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement Key Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar movement-key-mode-map (make-keymap) "Better movement keymap.")

(define-key movement-key-mode-map "\C-h" 'backward-word)
(define-key movement-key-mode-map [(control \')] 'forward-word)

(define-key movement-key-mode-map [(meta \h)] help-map)

;; Change Movement Keys to j-k-l-;
;; old: C-j: newline-and-indent
;; old: M-j: indent-new-comment-line
(define-key movement-key-mode-map [(control j)] 'backward-char)


;; old: C-k: kill-line
;; old: M-k: kill-paragraph
(define-key movement-key-mode-map "\C-k" 'next-line)

;; old: C-l: recenter-top-bottom
;; old: M-l: downcase-word
(define-key movement-key-mode-map "\C-l" 'previous-line)

;; old: C-;: undefined
;; old: M-;: comment-dwim
(define-key movement-key-mode-map [(control \;)] 'forward-char)
;;(define-key movement-key-mode-map [(meta \;)] 'forward-word)

;; old: C-f: forward-char
;; old: M-f: forward-word
(define-key movement-key-mode-map "\C-f" 'kill-line)
(define-key movement-key-mode-map "\M-f" 'copy-line)

;; old: C-b: backward-char
;; old: M-b: backward-word
(define-key movement-key-mode-map "\C-b" 'newline-and-indent)
(define-key movement-key-mode-map "\M-b" 'indent-new-comment-line)

;; old: C-p: previous-line
;; old: M-p: undefined
;;(if (eq system-type 'darwin)
;;    (global-set-key "\C-p" 'recenter)
;;  (global-set-key "\C-p" 'recenter-top-bottom))
(define-key movement-key-mode-map "\C-p" 'recenter-top-bottom)

;; old: C-n: next-line
;; old: M-n: undefined
(define-key movement-key-mode-map "\C-n" 'comment-dwim)

;; Ruby Mode likes to clobber this.
(define-key movement-key-mode-map "\C-x\C-t" 'transpose-lines)

;; Make iBuffer the fefault buffer view.
(define-key movement-key-mode-map "\C-x\C-b" 'ibuffer)

;; find-grep-dired is too useful to not have a hotkey
(define-key movement-key-mode-map "\C-xf" 'find-grep-dired)

;; I think id-mode overrides repeat
(define-key movement-key-mode-map "\C-xz" 'repeat)

(define-minor-mode movement-key-mode
  "Rebind movement keys to home row in all modes."
  t " Custom-keys" 'movement-key-mode-map)
(defun turn-on-movement-key-mode () (movement-key-mode 1))
(defun turn-off-movement-key-mode () (movement-key-mode -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End Movement Key Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Analog to C-x-o. Moves back a buffer.
(defun prev-buffer (&optional arg)
  (interactive "p")
  (let ((step (if arg arg 1)))
    (other-window (- 0 step))))
(global-set-key "\C-xi" 'prev-buffer)
(global-set-key "\C-xI" (lambda () (interactive)(prev-buffer 2)))
(global-set-key "\C-xO" (lambda () (interactive)(prev-buffer -2)))

;; Shows unbound keys - (describe-unbound-keys)
(add-to-list 'load-path "~/.emacs.d/elisp/unbound/")
(require 'unbound)
;;(describe-unbound-keys 6)

;; Setup Color Scheme for Emacs
(defvar color-theme-already-setup nil)
(unless color-theme-already-setup
  (require 'color-theme)
  (setq color-theme-is-global t)
  ;;    (set-frame-font "Consolas-13")
  (set-face-attribute 'default nil :height 100)
  (defun color-theme-custom-dark ()
    (interactive)
    (color-theme-install
     '(color-theme-custom-dark
       ((foreground-color . "#DDDDDD")
        (background-color . "#0F0F0F")
        (background-mode . light)
        (cursor-color . "#444444"))
       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (default ((t (nil))))
       (font-lock-builtin-face ((t (:italic t :foreground "#a96da0"))))
       (font-lock-comment-face ((t (:italic t :foreground "#666666"))))
       (font-lock-comment-delimiter-face ((t (:foreground "#666666"))))
       (font-lock-constant-face ((t (:bold t :foreground "#197b6e"))))
       (font-lock-doc-string-face ((t (:foreground "#666666"))))
       (font-lock-doc-face ((t (:foreground "#666666"))))
       (font-lock-reference-face ((t (:foreground "white"))))
       (font-lock-function-name-face ((t (:foreground "#883333"))))
       (font-lock-keyword-face ((t (:bold t :foreground "#bb4400"))))
       (font-lock-preprocessor-face ((t (:foreground "#e3ea94"))))
       (font-lock-string-face ((t (:foreground "#33CC44"))))
       (font-lock-type-face ((t (:bold t :foreground "#888822"))))
       (font-lock-variable-name-face ((t (:foreground "#2070B8"))))
       (font-lock-warning-face ((t (:bold t :italic nil :underline nil :background "#AA0000"))))
       (hl-line ((t (:foreground nil :background "#000000"))))
       (mode-line ((t (:foreground "#ffffff" :background "#333333"))))
       (mode-line-highlight ((t (:foreground "#cc5500" :background "#333333"))))
       (mode-line-inactive ((t (:foreground "#bbbbbb" :background "#111111"))))
       (region ((t (:foreground nil :background "#333333"))))
       (isearch ((t (:bold t :background "#163B65" :foreground "#FFFF00"))))
       (isearch-lazy-highlight-face ((t (:background "#163B65" :foreground "#FFFFFF"))))
       (ido-first-match ((t (:foreground "#eeee33"))))
       (ido-only-match ((t (:foreground "#cc5500"))))
       (w3m-anchor-face ((t (:foreground "#803A00"))))
       (w3m-arrived-anchor-face ((t (:foreground "#501A00"))))
       (w3m-header-line-location-content-face ((t (:background "#0F0F0F"))))
       (w3m-header-line-location-title-face ((t (:background "#000000"))))
       (ascii-non-ascii-face ((t (:background "#FFFF00"))))
       (mumamo-background-chunk-major ((f nil)))
       (mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) (:background "#101010"))))
       (minibuffer-prompt ((t (:foreground "#2070B8"))))
       (erc-prompt-face ((t (:bolt t :foreground "#ffffff" :background "#116611"))))
       ;; (erc-current-nick-face ((t (:bold t :foreground "yellow" :weight bold))))
       ;; (erc-default-face ((t (nil))))
       ;; (erc-direct-msg-face ((t (:foreground "pale green"))))
       ;; (erc-error-face ((t (:bold t :foreground "IndianRed" :weight bold))))
       ;; (erc-highlight-face ((t (:bold t :foreground "pale green" :weight bold))))
       ;; (erc-input-face ((t (:foreground "light blue"))))
       ;; (erc-inverse-face ((t (:background "steel blue"))))
       ;; (erc-notice-face ((t (:foreground "light salmon"))))
       ;; (erc-pal-face ((t (:foreground "pale green"))))
       ;; (erc-prompt-face ((t (:bold t :foreground "light blue" :weight bold))))
       ;; (erc-underline-face ((t (:underline t))))
       (show-paren-match-face ((t (:bold t :foreground "#ffffff"
                                         :background "#050505")))))))
  (color-theme-custom-dark)
  (setq color-theme-already-setup t))
;; TODO: Add commented out light color scheme so I can use projectors with normal people.

;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t) ; turn it on for all modes by default

;; Goto line
;; Please note, that this will override the function
;; ``facemenu-keymap'' normally used to change fonts.
;; M-g runs `M-x goto-line'
(global-set-key "\M-g" 'goto-line)

;; Insert group
(fset 'InsertRegexGroup
   (lambda (&optional arg) "Inserts a plain regex group. \(.+\)" (interactive "p") (kmacro-exec-ring-item (quote ("\\(.+\\)" 0 "%d")) arg)))
(global-set-key "\M-G" 'InsertRegexGroup)

;; Insert debug statement.
;; (defun ruby-debug-statement ()
;;   (interactive)
;;   (insert "(require 'ruby-debug'; debugger;) # DEBUG-TODO: REMOVE DEBUG STATEMENT")
;;   (search-backward "#"))
;; (define-key ruby-mode-map "\C-cr" 'ruby-debug-statement)

;; Woo, in place thrift struct conversion!
(defun convert-thrift-struct (&optional use-file)
  "Run replacement for data units and structs"
  (interactive "P")
  (let ((current-dir (substring (pwd) 10 -1))
        (filename (if use-file (read-from-minibuffer "Which test profile (add test_profiles/)? "))))
    (cd "/Users/alex/rapleaf/spider/union/")
    (shell-command-on-region (region-beginning) (region-end) (concat "ruby /Users/alex/rapleaf/spider/union/script/convert_structs.rb " filename) "*Messages*" 1)
    (cd current-dir)))
(global-set-key "\C-ct" 'convert-thrift-struct)

(defun wrap-region (b e obracket cbracket)
  "'tag' a region"
  (interactive "r\nMWrap region open bracket: \nMWrap region close bracket: ")
  (save-excursion
    (goto-char e)
    (insert (format "%s" cbracket))
    (goto-char b)
    (insert (format "%s" obracket))))
(global-set-key "\C-cw" 'wrap-region)

(defun wrap-xml-brackets (b e tag)
  "wrap region with <sometag>YOUR CONTENT</sometag"
  (interactive "r\nMTag: ")
  (wrap-region b e (format "<%s>" tag) (format "</%s>" tag)))
(global-set-key "\C-xx" 'wrap-xml-brackets)

(defun wrap-thrift-replacement-brackets (b e)
  "wrap region with {-- --}"
  (interactive "r")
  (wrap-region b e "'{--" "--}'"))
(global-set-key "\C-cj" 'wrap-thrift-replacement-brackets)

(defun wrap-with-quotes (b e &optional arg)
  "Wraps a region with double quotes. If passed argument, uses single quotes instead."
  (interactive "r\nP")
  (if arg
      (wrap-region b e "'" "'")
    (wrap-region b e "\"" "\"")))
(global-set-key [(control \")] 'wrap-with-quotes)

;; Move backups (file.ext~) to a central location.
(setq backup-directory-alist
      `((".*" . ,"~/.backups.emacs/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.backups.emacs/" t)))

;; Setup Tramp to allow opening files as root.
;; Use C-x C-f "/su::/path/to/file".
(require 'tramp)
(setq tramp-default-method "ssh")

;; Open all dired directories in the same buffer
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "<return>")
    'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))
(put 'dired-find-alternate-file 'disabled nil)


;; Rake files are ruby, too, as are gemspecs.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js.rjs" . ruby-mode))
(add-to-list 'auto-mode-alist '("irb_tempfile\\..*" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$*" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thrift$" . c-mode))


;; Setup Rinari: Rails editing mode.
;; (add-to-list 'load-path "/emacs/elisp/rinari")
(require 'rinari)

;; Setup js2 mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Setup nXhtml mode for editing r/html.
(if (eq system-type 'darwin)
    (load "/opt/local/var/macports/software/emacs-app/23.1_0/Applications/MacPorts/Emacs.app/Contents/Resources/site-lisp/nxml/autostart.el")
    (load "~/.emacs.d/elisp/nxhtml/autostart.el"))
(setq nxhtml-global-minor-mode t
      mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil
      nxml-degraded t)
     (add-to-list 'auto-mode-alist '("\\.*html\\.erb\\'" . eruby-nxhtml-mumamo))

;; This adds an extra keybinding to interactive search (C-s).
;; That runs occur on the current search string/regexp.
;; Shows all hits immediately in the entire buffer.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; Setup Haskell Mode
(load "~/.emacs.d/elisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hoook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(setq haskell-program-name "ghci")


;; (load "~/.emacs.d/elisp/thingatpt+")
;;(require 'browse-apropos-url)

;; (add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/jde/lisp"))
;; (add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/cedet/common"))
;; (load-file (expand-file-name "/usr/share/emacs/site-lisp/cedet/common/cedet.el"))
;; (add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/elib"))
;; (require 'jde)

;; Idea: Quote Line
;; C-': From cursor to end of line, wrap in quotes.
;; C-": Wrap region in quotes.


;; Java Mode Stuff
(defun my-java-mode-hook ()
  (setq c-basic-offset 2))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Jabber Mode
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-jabber")
;;(require 'jabber-autoloads)

(autoload 'wikipedia-mode
  "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)
(eval-after-load "wikipedia-mode.el"
  '(progn
     (define-key wikipedia-mode-map [(meta u)] 'upcase-word)))
(add-to-list 'auto-mode-alist '("\\.wiki$" . wikipedia-mode))

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-use-fast-todo-selection t)
(setq org-return-follows-link t)

(setq org-agenda-files '("~/notes/work.org" "~/notes/home.org" ))
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "DONE(d!/!)")
 (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("STARTED" :foreground "blue" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "orange" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "forest green" :weight bold)
 ("OPEN" :foreground "blue" :weight bold))))

;; clojure-mode
(unless (eq clojure-home nil)
  (add-to-list 'load-path "~/Library/Clojure/clojure-mode/clojure-mode")
  (require 'clojure-mode)

  ;; swank-clojure
  (add-to-list 'load-path "~/Library/Clojure/swank/swank-clojure")
  (require 'swank-clojure-autoload)
  (swank-clojure-config
   (setq swank-clojure-jar-path "~/Library/Clojure/lib/clojure.jar")
   (setq swank-clojure-extra-classpaths
         (list "~/Library/Clojure/lib/clojure-contrib.jar" "~/rapleaf/jars/dev")))

  ;; slime
  (eval-after-load "slime"
    '(progn (slime-setup '(slime-repl))))

  (add-to-list 'load-path "~/Library/Clojure/slime/slime")
  (require 'slime)
  (slime-setup))

;; w3m
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
(if window-system
    (autoload 'w3m "w3m-load.el" "w3m Browser." t))
(eval-after-load "w3m-load.el"
  '(progn
     (global-set-key "\C-xm" 'browse-url-at-point)
     (setq browse-url-browser-function 'w3m-browse-url)
     (setq w3m-use-cookies t)
     (setq w3m-use-title-buffer-name t)
     (setq w3m-use-tab nil)
     (setq w3m-use-tab-menubar nil)
     (setq w3m-default-display-inline-images nil)
     (setq w3m-use-favicon nil)
     (defvar w3m-keys-already-setup nil)
     (add-hook 'w3m-mode
               (lambda ()
                 (unless w3m-keys-already-setup
                     (define-key w3m-mode-map "j" 'backward-char)
                     (define-key w3m-mode-map ";" 'forward-char)
                     (define-key w3m-mode-map "h" 'backward-word)
                     (define-key w3m-mode-map "'" 'forward-word)
                     (define-key w3m-mode-map "k" 'next-line)
                     (define-key w3m-mode-map "l" 'previous-line)
                     (define-key w3m-mode-map "\C-ts" 'w3m-search-new-session)
                     (setq w3m-keys-already-setup t))))))

(message "w3m mode loaded.")

;; Highlight changes mode
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)
(global-set-key [(hyper h)] 'highlight-changes-visible-mode)
(global-set-key [(hyper H)] 'highlight-changes-mode)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#201010")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#201010")

(message "hlc mode loaded.")

;; Stickies
(add-to-list 'load-path "~/.emacs.d/elisp/stickies/")
(autoload 'stickies "stickies" "Sticky notes." t)
(eval-after-load "stickies"
  '(progn
     (if window-system
         (setq stickies-frame-alist
               '( (background-color . "#223388") (foreground-color . "white") (top . 80) (left . 70) (width . 60) (height . 20) (alpha . 90))))
;;; Save stickies when emacs is terminated.
     (add-hook 'kill-emacs-hook 'stickies-save-all-stickies)
;;; Automatically save stickies by 10 minutes.
     (setq auto-save-stickies-timer
           (run-at-time t 600 'stickies-save-all-stickies))
;;; Open all stickies when emacs starts.
     (defvar stickies-open-with-emacs nil)
     (unless stickies-open-with-emacs
       (stickies-open-all-saved-stickies)
       (setq stickies-open-with-emacs t))
     ;; Stickies Keybindings
     (defvar stickies-keys-already-setup nil)
     (unless stickies-keys-already-setup
       (define-prefix-command 'ctl-cs-stickies-mode-prefix)
       (define-key 'ctl-cs-stickies-mode-prefix "n" 'stickies-open-sticky)
       (define-key 'ctl-cs-stickies-mode-prefix "o" 'stickies-open-all-saved-stickies)
       (define-key  'ctl-cs-stickies-mode-prefix "d" '(lambda ()
                                                        (interactive)
                                                        (if (and (stickies-is-sticky-buffer (current-buffer))
                                                                 (y-or-n-p "Delete the sticky file?"))
                                                            (stickies-delete-sticky-file))))
       (define-key 'ctl-cs-stickies-mode-prefix "k" 'stickies-kill-sticky)
       (define-key  'ctl-cs-stickies-mode-prefix "K" '(lambda ()
                                                        (interactive)
                                                        (if (y-or-n-p "Kill all stickies?")
                                                            (stickies-kill-all-stickies))))
       (global-set-key "\C-cs" 'ctl-cs-stickies-mode-prefix)
       (setq stickies-keys-already-setup t))))

(message "Stickies mode loaded.")

;; Zen Coding
(add-to-list 'load-path "~/.emacs.d/elisp/zencoding/")
(require 'zencoding-mode)
(add-hook 'eruby-nxhtml-mumamo-mode-hook 'zencoding-mode)
(add-hook 'nxhtml-mode-hook 'zencoding-mode)
;; Usage: C-RET => expand

;; FlySpell Mode
;; (autoload 'flyspell-mode "flyspell-m
(add-hook 'text-mode-hook 'flyspell-mode)
(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mode-map [(control \;)] 'forward-char)
       (define-key flyspell-mode-map [(control \")] 'forward-word)
       (define-key flyspell-mode-map [(control \.)] 'flyspell-auto-correct-previous-word)
       (define-key flyspell-mode-map [(control meta e)] 'flyspell-goto-next-error)
       (message "Flyspell mode loaded. Keybindings fixed.")))


(defun start-erc ()
  "Start ERC in new frame."
  (interactive)
  ;; (select-frame (make-frame '((name . "ERC")
  ;;                             (minibuffer . t))))
  (erc :server "irc.freenode.net" :port 6667 :nick "saterus")
  (bury-buffer))

(defmacro erc-autojoin (&rest args)
  `(add-hook 'erc-after-connect
             '(lambda (server nick)
                (cond
                 ,@(mapcar (lambda (servers+channels)
                             (let ((servers (car servers+channels))
                                   (channels (cdr servers+channels)))
                               `((member erc-session-server ',servers)
                                 (mapc 'erc-join-channel ',channels))))
                           args)))))

(erc-autojoin
 (("irc.freenode.net")
  "#emacs" "#haskell" "#clojure" "#bash"))
(setq erc-max-buffer-size 20000)
(setq erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))
(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 600)
(setq erc-auto-set-away t)
(setq erc-autoaway-use-emacs-idle t)
(setq erc-autoaway-message "out")

;; Ascii Mode
;;(require 'ascii)
;;(add-to-list 'load-path "~/.emacs.d/elisp/ascii.el")


;; emacs.el todo:
;; * delay define keybindings for ruby-mode and w3m mode until after autoload runs
;; * fix unbound.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;             PUT NOTHING BELOW THIS POINT!                  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Activate Movement Key Mode Last so it will always override all other minor modes.
(define-globalized-minor-mode global-movement-key-mode movement-key-mode turn-on-movement-key-mode)
(global-movement-key-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;             PUT NOTHING BELOW THIS POINT!                  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; TODO: Add ruby keybinding to a generalized version of this. seeing the stack trace is handy.
;;
;; $URL_COUNTER = $URL_COUNTER ? $URL_COUNTER+1 : 0 if data_unit.data.profile_url
;; begin
;;   raise Exception.new("URL COUNTER: #{$URL_COUNTER}") if data_unit.data.profile_url
;; rescue Exception => ex
;;   puts ex.message << "\n===Backtrace: #{ex.backtrace.pretty_inspect}\n===END Backtrace\n"
;; end
;; (require 'ruby-debug'; debugger;) if $URL_COUNTER == 25 # DEBUG-TODO: REMOVE DEBUG STATEMENT
