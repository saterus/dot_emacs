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

(defvar site-lisp
  (if (eq system-type 'darwin)
      (concat (getenv "EMACS_HOME") "/../Resources/site-lisp")
    "/usr/share/emacs-snapshot/site-lisp"))
(defvar clojure-home
  (if (eq system-type 'darwin)
      "~/Library/Clojure/"
    nil)) ;; todo: download clojure for linux.

;; Recursively add subdirectories to the path.
(progn (cd "~/.emacs.d/elisp")
       (normal-top-level-add-subdirs-to-load-path)
       (cd site-lisp)
       (normal-top-level-add-subdirs-to-load-path)
       (cd "~/"))

;; Line Numbers
;;(require 'linum)
(global-linum-mode 1)

;; Fix meta/hyper keys for osx
(if (eq system-type 'darwin)
    (progn
      (setq mac-pass-command-to-system nil)
      (global-set-key "\C-q" 'quoted-insert)
      (setq mac-command-modifier 'control)
      (setq mac-control-modifier 'meta)
      (setq mac-option-modifier 'hyper)
      (global-set-key [(control \`)] 'other-frame)))

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

;; Interactively Do Things (highly recommended, but not strictly required)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement Key Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar movement-key-mode-map (make-keymap) "Better movement keymap.")
;; ;
;; ;  M-h: mark paragraph
;; ;  M-j: "M-j runs the command backward-word"
;; ;  M-k: "M-k runs the command kill-sentence"
;; ;  M-l: "M-l runs the command downcase-word"
;; ;  M-;: "M-; runs the command forward-word"
;; ;  M-': "M-' runs the command abbrev-prefix-mark"
;; ;; Change Movement Keys to M- (j-k-l-;)

;; (define-key movement-key-mode-map "\M-h" 'backward-word)
;; (define-key movement-key-mode-map "\M-j" 'backward-char)
;; (define-key movement-key-mode-map "\M-k" 'next-line)
;; (define-key movement-key-mode-map "\M-l" 'previous-line)
;; (define-key movement-key-mode-map [(meta \;)] 'forward-char)
;; (define-key movement-key-mode-map [(meta \')] 'forward-word)

;; ;;(define-key movement-key-mode-map "\C-h" 'backward-word)
;; (define-key movement-key-mode-map "\C-j" 'mark-paragraph)
;; (define-key movement-key-mode-map "\C-k" 'kill-sentence)
;; (define-key movement-key-mode-map "\C-l" 'downcase-word)
;; (define-key movement-key-mode-map [(control \;)] nil)
;; (define-key movement-key-mode-map [(control \')] nil)

;; (define-key movement-key-mode-map "\C-a" 'backward-sentence)
;; (define-key movement-key-mode-map "\C-e" 'forward-sentence)
;; (define-key movement-key-mode-map "\M-a" 'move-beginning-of-line)
;; (define-key movement-key-mode-map "\M-e" 'move-end-of-line)

;; "M-a runs the command backward-sentence"
;; "M-e runs the command forward-sentence"
;; "C-a runs the command move-beginning-of-line"
;; "C-e runs the command move-end-of-line"
(define-key movement-key-mode-map "\C-h" 'backward-word)
(define-key movement-key-mode-map [(control \')] 'forward-word)

(define-key movement-key-mode-map [(meta \h)] help-map)

;; Change Movement Keys to j-k-l-;
;; old: C-j: newline-and-indent
;; old: M-j: indent-new-comment-line
(define-key movement-key-mode-map "\C-j" 'backward-char)
;;(define-key movement-key-mode-map "\M-j" 'backward-word)
;; Fix C-j. Every minor mode and it's brother love to override it.
;; (define-key slime-repl-mode-map (kbd "C-j") 'backward-char) ;; repl mode
;; (define-key ruby-mode-map (kbd "C-j") 'backward-char) ;; ruby mode
;; (define-key yaml-mode-map (kbd "C-j") 'backward-char) ;; yaml mode
;; (define-key view-mode-map (kbd "C-j") 'backward-char) ;; View mode

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
(defun prev-buffer ()
  (interactive)
  (other-window -1))
(global-set-key "\C-xi" 'prev-buffer)

;; Shows unbound keys - (describe-unbound-keys)
;;(require 'unbound)
;;(describe-unbound-keys 5)

;; Setup Color Scheme for Emacs
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
       (font-lock-warning-face ((t (:bold t :italic nil :underline nil
                                     :background "#AA0000"))))
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
       (highlight-changes-face ((t (:background "#201010"))))
       (highlight-changes-delete-face ((t (nil))))
       (ascii-non-ascii-face ((t (:background "#FFFF00"))))
       (mumamo-background-chunk-major ((f nil)))
       (mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) (:background "#101010"))))
       (minibuffer-prompt ((t (:foreground "#2070B8"))))
       (show-paren-match-face ((t (:bold t :foreground "#ffffff"
                                    :background "#050505")))))))
(color-theme-custom-dark)

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

(defun convert-thrift (arg filename)
  "Run replacement for data units and structs"
  (interactive "M\Which test profile? ")
  (let ((current-dir (pwd))
        (type (if (eq arg nil) "struct" arg)))
    (cd "/Users/alex/rapleaf/spider/union/")
    (shell-command-on-region (region-beginning) (region-end) (concat "ruby script/convert_structs.rb " type " " filename) "*Messages*" 1)
    (cd current-dir)))
(defun convert-du (filename)
  (interactive "M\Which test profile? ")
  (convert-thrift "du" filename))
(defun convert-struct (filename)
  (interactive "M\Which test profile? ")
  (convert-thrift "struct" filename))
(defun convert-pedigree (filename)
  (interactive "M\Which test profile? ")
  (convert-thrift "pedigree" filename))
(global-set-key "\C-cd" 'convert-du)
(global-set-key "\C-cs" 'convert-struct)
(global-set-key "\C-cp" 'convert-pedigree)

;; Move backups (file.ext~) to a central location.
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.backups/" t)))

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
   (require 'w3m-load))
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)
(setq w3m-use-title-buffer-name t)
(setq w3m-use-tab nil)
(setq w3m-use-tab-menubar nil)
(setq w3m-default-display-inline-images nil)
(setq w3m-use-favicon nil)

;; Highlight changes mode
(global-highlight-changes-mode t)
(global-set-key [(hyper h)] 'highlight-changes-mode)

;; Ascii Mode
;;(require 'ascii)
;;(add-to-list 'load-path "~/.emacs.d/elisp/ascii.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;             PUT NOTHING BELOW THIS POINT!                  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Activate Movement Key Mode Last so it will always override all other minor modes.
(define-globalized-minor-mode global-movement-key-mode movement-key-mode turn-on-movement-key-mode)
(global-movement-key-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;             PUT NOTHING BELOW THIS POINT!                  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
