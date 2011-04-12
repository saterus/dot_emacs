;; Recursively add library subdirectories to the path.
(defun add-recursive-load-path (path)
  "Decends through all subdirectories and adds them to the load path."
  (progn (let ((current-dir (substring (pwd) 10 -1)))
           (cd path)
           (normal-top-level-add-subdirs-to-load-path)
           (cd current-dir)
           (message (concat "Added " path)))))

(add-recursive-load-path (concat dotfiles-dir "/non-elpa-libs"))
(add-recursive-load-path (concat dotfiles-dir "/alex"))

;; Beginning of the el4r block:
;; RCtool generated this block automatically. DO NOT MODIFY this block!
;; (add-to-list 'load-path "/home/alex/.rvm/rubies/ruby-1.8.7-p299/share/emacs/site-lisp")
;; (require 'el4r)
;; (el4r-boot)
;; End of the el4r block.


(autoload 'scratch "scratch" "" t)

;; Resolve CPP Mode
(autoload 'rcpp-mode "rcpp-mode.el" "" t)
(eval-after-load 'rcpp-mode
  (progn
    (setq tab-width 2)
    ))
;;   (local-set-key [(f5)] compile))
(add-to-list 'auto-mode-alist '("\\.cpp$" . rcpp-mode))

(autoload 'browse-kill-ring "browse-kill-ring" "" t)
;; (autoload ' "frame-cmds.el" "" t)
(autoload 'unbound "unbound.el" "" t)

(autoload 'tramp-mode "tramp" "" t)
(eval-after-load 'tramp
  (progn
    (setq tramp-default-method "ssh2")
    ;; (defun tramp-find-file-timeout ()
    ;;   (let* ( ;; We bind the variable `file-name-history' locally so we can
    ;;          ;; use a separate history list for "root" files.
    ;;          (name (or buffer-file-name default-directory))
    ;;          (tramp (and (tramp-tramp-file-p name)
    ;;                      (tramp-dissect-file-name name)))
    ;;          path dir file)
    ;;   (when tramp
    ;;     (with-timeout (4)
    ;;       (keyboard-quit)))))
    ;; (add-hook 'find-file-hook 'tramp-find-file-timeout)
    ;; (setq tramp-verbose 10)
    ;; (setq tramp-debug-buffer nil)
    ))



;; Setup Haml Mode
;; (require 'haml-mode-autoloads)
;; (require 'sass-mode-autoloads)
;; (autoload 'haml-mode "haml-mode.el" "" t)
;; (autoload 'sass-mode "sass-mode.el" "" t)
;; (add-to-list 'auto-mode-alist '("\\.haml$" . eruby-haml-mumamo))
;; (add-to-list 'auto-mode-alist '("\\*\\.haml$" . eruby-haml-mumamo))
;; (add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
;; (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(autoload 'haskell-mode "haskell-mode.el" "" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[hg]s$"  . literate-haskell-mode))
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
(require 'color-theme-anb-light)
;; (color-theme-anb-dark)
(color-theme-anb-light)
;; (set-frame-font "Consolas-13")
(set-frame-font "Andale Mono")
(set-face-attribute 'default nil :height 80)

;; ruby mode
(autoload 'ruby-mode "ruby-mode.el" ""  t)
(autoload 'rinari "rinari.el" "" t)

;; (load "~/.emacs.d/non-elpa-libs/nxhtml/autostart.el")
(setq nxhtml-global-minor-mode t
      mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil
      nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.*html\\.erb\\'" . eruby-nxhtml-mumamo))
(add-to-list 'auto-mode-alist '("\\.rhtml'" . eruby-nxhtml-mumamo))
(add-hook 'eruby-nxhtml-mumamo-mode-hook
          (lambda ()
            (local-unset-key [(control enter)] )))

;; Zen Coding
(autoload 'zencoding-mode "zencoding-mode.el" "" t)
(add-hook 'sgml-mode-hook (lambda () (zencoding-mode t)))
(add-hook 'eruby-nxhtml-mumamo-mode-hook 'zencoding-mode)
(add-hook 'nxhtml-mode-hook 'zencoding-mode)
;; Usage: C-RET => expand


;; Setup js2 mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Java Mode Stuff
(defun anb-java-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'anb-java-mode-hook)
(add-hook 'java-mode-hook 'coding-hook)

;; yaml mode
(autoload 'yaml-mode "yaml-mode.el" "" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(autoload 'wikipedia-mode
  "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)
(eval-after-load "wikipedia-mode.el"
  '(progn
     (define-key wikipedia-mode-map [(meta u)] 'upcase-word)))
(add-to-list 'auto-mode-alist '("\\.wiki$" . wikipedia-mode))

;; Highlight changes mode
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)
(global-set-key [(hyper h)] 'highlight-changes-visible-mode)
(global-set-key [(hyper H)] 'highlight-changes-mode)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#FFFFDD") ; "#201010")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#FFFFDD") ; "#201010")


;; w3m
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")

(autoload 'w3m "w3m-load.el" "w3m Browser." t)
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

;; clojure-mode
;; (unless (eq clojure-home nil)
;;   (add-to-list 'load-path "~/Library/Clojure/clojure-mode/clojure-mode")
;;   (require 'clojure-mode)

;;   ;; swank-clojure
;;   (add-to-list 'load-path "~/Library/Clojure/swank/swank-clojure")
;;   (require 'swank-clojure-autoload)
;;   (swank-clojure-config
;;    (setq swank-clojure-jar-path "~/Library/Clojure/lib/clojure.jar")
;;    (setq swank-clojure-extra-classpaths
;;          (list "~/Library/Clojure/lib/clojure-contrib.jar" "~/rapleaf/jars/dev")))

;;   ;; slime
;;   (eval-after-load "slime"
;;     '(progn (slime-setup '(slime-repl))))

;;   (add-to-list 'load-path "~/Library/Clojure/slime/slime")
;;   (require 'slime)
;;   (slime-setup))

;; ;;Push the mouse out of the way when the cursor approaches.
;; (if window-system
;;     (progn
;;       (autoload 'avoid "avoid" "Avoid mouse and cursor being near each other")
;;       (eval-after-load 'avoid (mouse-avoidance-mode 'jump))))


;; Interactively Do Things
(require 'ido)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;; Setup Ido-Mode for M-x COMMAND
;; (setq ido-execute-command-cache nil)
;; (defun ido-execute-command ()
;;   (interactive)
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (progn
;;        (unless ido-execute-command-cache
;; 	 (mapatoms (lambda (s)
;; 		     (when (commandp s)
;; 		       (setq ido-execute-command-cache
;; 			     (cons (format "%S" s) ido-execute-command-cache))))))
;;        ido-execute-command-cache)))))
;; (add-hook 'ido-setup-hook
;; 	  (lambda ()
;; 	    (setq ido-enable-flex-matching t)
;; 	    (global-set-key "\M-x" 'ido-execute-command)))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        ;; doesn't place nice with tramp mode.
      (let* ((name ido-current-directory)
             (tramp (and (tramp-tramp-file-p name)
                         (tramp-dissect-file-name name)))
             path dir file)
        (if (or tramp (string= ido-current-directory "/"))
            ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                        (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                    (if (= (nth 0 ta) (nth 0 tb))
                        (> (nth 1 ta) (nth 1 tb))
                      (> (nth 0 ta) (nth 0 tb)))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (and (not (string-equal x ".")) (string-equal (substring x 0 1) ".")) x))
              ido-temp-list))))


(ido-load-history t)
(ido-mode t)

;; From: http://stackoverflow.com/questions/905338/can-i-use-ido-completing-read-instead-of-completing-read-everywhere/907060
;; (defvar ido-enable-replace-completing-read t
;; "If t, use ido-completing-read instead of completing-read if possible.

;; Set it to nil using let in around-advice for functions where the
;; original completing-read is required.  For example, if a function
;; foo absolutely must use the original completing-read, define some
;; advice like this:

;; (defadvice foo (around original-completing-read-only activate)
;;   (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
;; (defadvice completing-read
;;   (around use-ido-when-possible activate)
;;   (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;;           (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
;;       ad-do-it
;;     (let ((allcomp (all-completions "" collection predicate)))
;;       (if allcomp
;;           (setq ad-return-value
;;                 (ido-completing-read prompt
;;                                allcomp
;;                                nil require-match initial-input hist def))
;;         ad-do-it))))

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


(require 'smex)
(smex-initialize)

;; (require 'anything)
;; (require 'anything-config)
;; (require 'anything-match-plugin)
;; (require 'anything-complete)
;; (require 'descbinds-anything)
;; (descbinds-anything-install)


(setq inferior-lisp-program "/usr/bin/clisp") ; your Lisp system
;; (add-to-list 'load-path "~/hacking/lisp/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup)
