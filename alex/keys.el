;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keybindings for functions without keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .emacs
(global-set-key [(control \#)] 'dot-emacs)
(global-set-key [(meta \#)] 'reload-dot-emacs)

;; buffers
(global-set-key "\C-xi" 'prev-buffer)
(global-set-key "\C-xI" (lambda () (interactive)(prev-buffer 2)))
(global-set-key "\C-xO" (lambda () (interactive)(prev-buffer -2)))
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)
(global-set-key (kbd "C-x C-S-r") 'ido-find-file-read-only)

;; clipboard
(global-set-key [(super x)] 'clipboard-kill-region)
(global-set-key [(super c)] 'clipboard-kill-ring-save)
(global-set-key [(super v)] 'clipboard-yank)

;; frame/window commands
(global-set-key [(control \`)] 'other-frame)
(global-set-key [(control \~)] '(lambda () (interactive)(other-frame -1)))
(global-set-key [(hyper f)] 'rename-frame)
(global-set-key [(hyper n)] 'new-frame)
(global-set-key "\C-x9" 'kill-buffer-and-window)
(global-set-key [(hyper r)] 'rotate-windows)
(global-set-key [(hyper c)] 'calc)
(global-set-key [(hyper b)] 'w3m)
(global-set-key [(hyper h)] 'highlight-changes-visible-mode)
(global-set-key [(hyper right)] 'enlarge-window-horizontally)
(global-set-key [(hyper left)]  'shrink-window-horizontally)
(global-set-key [(hyper up)]    'shrink-window)
(global-set-key [(hyper down)]  'enlarge-window)

;; wrapping
(global-set-key "\C-cw" 'wrap-region)
(global-set-key "\M-n" 'wrap-xml-brackets)
(global-set-key [(control \")] 'wrap-with-quotes)

;; misc
(global-set-key [(hyper w)] 'delete-trailing-whitespace)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-G" 'insert-regexp-group)
(global-set-key "\C-z" 'repeat-complex-command)
(global-set-key (kbd "M-S-<SPC>") 'delete-horizontal-whitespace-forward)
(global-set-key (kbd "C-x C-<tab>") 'indent-code-rigidly)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix various minor modes keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fix emacs lisp mode's return
(define-key emacs-lisp-mode-map (kbd "<return>") 'newline)

;; truncate shortcut
(global-set-key [(hyper t)] 'toggle-truncate-lines)

;; fix view mode
;; (define-key view-mode-map (kbd "C-j") 'backward-char)
;; (define-key view-mode-map (kbd "C-k") 'next-line)
;; (define-key view-mode-map (kbd "C-l") 'previous-line)
;; (define-key view-mode-map (kbd "k") 'View-scroll-line-forward)
;; (define-key view-mode-map (kbd "l") 'View-scroll-line-backward)

;; This adds an extra keybinding to interactive search (C-s).
;; That runs occur on the current search string/regexp.
;; Shows all hits immediately in the entire buffer.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "\C-x\C-t" 'transpose-lines)
     (define-key ruby-mode-map "\C-c\C-r" 'ruby-debug-statement)))

(eval-after-load 'ruby-electric-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-j") 'backward-char)))

(eval-after-load 'ruby-electric-mode
  '(progn
     (define-key ruby-compilation-minor-mode-map "\C-x\C-t" 'transpose-lines)
     (define-key ruby-compilation-minor-mode-map [(f5)] 'ruby-compilation-this-test)))

;; Fix paredit mode bindings
(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(progn
     (define-key paredit-mode-map (kbd "C-k") 'next-line)
     (define-key paredit-mode-map (kbd "C-f") 'paredit-kill)
     (define-key paredit-mode-map (kbd "C-j") 'backward-char)
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward)
     (define-key paredit-mode-map (kbd "C-M-'") 'paredit-forward)
     (define-key paredit-mode-map (kbd "C-M-k") 'up-list)
     (define-key paredit-mode-map (kbd "C-M-l") 'backward-down-list)))

(eval-after-load 'smex
  '(progn
     (global-set-key (kbd "M-x") 'smex)
     (global-set-key (kbd "M-X") 'smex-major-mode-commands)
     ;; This is your old M-x.
     (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
))

;; uses C-o as the anything-prefix key.
;; (eval-after-load 'anything
;;   '(progn
;;      (define-key movement-key-mode-map [(control \o)] 'anything-command-map)
;;      (global-unset-key (kbd "C-o"))
;;      (global-set-key "\C-oo" 'open-line)
;;      (global-set-key "\C-o\C-o" 'anything)
;;      ;; (global-set-key "\C-xb" 'anything-mini)
;;      (eval-after-load 'anything-dabbrev
;;        '(progn
;;           (global-set-key (kbd "M-/") 'anything-dabbrev-expand)
;;           (define-key anything-dabbrev-map "\M-/" 'anything-dabbrev-find-all-buffers)))
;;      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOVEMENT Key Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar movement-key-mode-map (make-keymap) "Better movement keymap.")

(define-key movement-key-mode-map "\C-h" 'backward-word)
(define-key movement-key-mode-map [(control \')] 'forward-word)
(define-key movement-key-mode-map [(control meta j)] 'backward-sexp)
(define-key movement-key-mode-map [(control meta \;)] 'forward-sexp)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;             PUT NOTHING BELOW THIS POINT!                  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Activate Movement Key Mode Last so it will always override all other minor modes.
(define-globalized-minor-mode global-movement-key-mode movement-key-mode turn-on-movement-key-mode)
(global-movement-key-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;             PUT NOTHING BELOW THIS POINT!                  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
