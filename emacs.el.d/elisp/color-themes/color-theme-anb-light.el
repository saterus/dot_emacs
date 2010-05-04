(defun color-theme-anb-light ()
  (interactive)
  (color-theme-install
   '(color-theme-anb-light
      ((background-color . "#ffffff")
      (background-mode . light)
      (border-color . "#f5f5f5")
      (cursor-color . "#3fab65")
      (foreground-color . "#000000")
      (mouse-color . "black"))
     (fringe ((t (:background "#f5f5f5"))))
     (mode-line ((t (:foreground "#000000" :background "#f3f2f2"))))
     (region ((t (:background "#dbf3f5"))))

       (font-lock-builtin-face ((t (:italic t :foreground "#A864FF"))))
       (font-lock-comment-face ((t (:italic t :foreground "#666666"))))
       (font-lock-comment-delimiter-face ((t (:foreground "#666666"))))
       (font-lock-constant-face ((t (:bold t :foreground "#197b6e"))))
       (font-lock-doc-string-face ((t (:foreground "#666666"))))
       (font-lock-doc-face ((t (:foreground "#666666"))))
       (font-lock-reference-face ((t (:foreground "#000000"))))
       (font-lock-function-name-face ((t (:bold t :foreground "#891200"))))
       (font-lock-keyword-face ((t (:foreground "#ff7e05")))) ;; bb4400
       (font-lock-preprocessor-face ((t (:foreground "#999999"))))
       (font-lock-string-face ((t (:foreground "#298e3c"))))
       (font-lock-type-face ((t (:foreground"#2a4fbb"))))
       (font-lock-variable-name-face ((t (:foreground "#2070B8"))))


       (mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) (:background "#FFFFFF"))))
       (mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background light)) (:background "#F0E0E0"))))
       (mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background light)) (:background "#F0E0E0"))))
       (mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background light)) (:background "#F0E0E0"))))
       (mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background light)) (:background "#E0F0F0"))))

     ;; (font-lock-builtin-face ((t (:foreground "#c27100"))))
     ;; (font-lock-comment-face ((t (:foreground "#c2c2c2"))))
     ;; (font-lock-keyword-face ((t (:foreground "#ff7e05"))))
     ;; (font-lock-variable-name-face ((t (:foreground "#a79844"))))
     (font-lock-warning-face ((t (:bold t :italic nil :underline nil :background "#AA0000"))))
     (minibuffer-prompt ((t (:foreground "#7299ff" :bold t))))
     )))
(provide 'color-theme-anb-light)
