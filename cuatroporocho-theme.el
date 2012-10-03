;; ============================================================================
;; File:            cuatroporocho-theme.el
;; Last changed by: Frank Ruben on 03-10-2012 21:38:51
;; Purpose:         Color theme for Emacs,
;;                  technically and color-wise initially based on zenburn,
;;                  but changed quite drastically from there, both WRT colors
;;                  and implementation.
;;                  The color theme does currently contain colors
;;                  for win32 graphics mode and win32 console mode.
;; Note:            As zenburn, this still requires a special load:
;;       (progn (load-theme 'cuatroporocho t) (load "cuatroporocho-theme"))
;; ============================================================================

(deftheme cuatroporocho "The cuatro por ocho color theme")

(defvar cuatroporocho-background-mode 'dark)
;; (defvar cuatroporocho-background-mode 'light)

(defun my-get-color (which-color &optional light-mode for-terminal)
  ;;                               win32          win32
  ;;                               console        graphics
  ;; colors with +x are lighter, colors with -x are darker
  ;;  display-visual-class -> win32 UI: true-color ; win32 console: static-color
  ;;  display-color-cells  -> win32 UI: 16777216   ; win32 console: 16
  (let ((dark-colors '((my-bg+2    "brown"        "#4f4f4f") ; fringe and highlighted background
                       (my-bg+1    "darkgray"     "#2f2f2f") ; string background
                       (my-bg      "black"        "#1f1f1f") ; default background
                       (my-bg-1    "black"        "#0b0b0b") ; match background
                       (my-bg-X    "magenta"      "#4F3D3D") ; active modeline background; touch of red
                       (my-blue    "lightblue"    "#AFC2F2") ; link, directory
                       (my-blue-1  "blue"         "#90A8E5") ; types, functions
                       (my-blue-2  "blue"         "#7C8AAC") ; variable and parameter names
                       (my-cyan    "cyan"         "#93e0e3") ; cursor
                       (my-cyan-1  "cyan"         "#7DA8AA") ; match foreground
                       (my-fg+1    "white"        "#EEEEE4")
                       (my-fg      "lightgray"    "#dcdccc") ; default foreground
                       (my-fg-1    "lightgray"    "#A5A59C") ; comment
                       (my-fg-2    "lightgray"    "#5b6555") ; comment delimiter; touch of green
                       (my-green+4 "lightgreen"   "#D3ECD3") ; string foreground
                       (my-green+3 "lightgreen"   "#afd8af") ; success
                       (my-green+2 "lightgreen"   "#9fc59f")
                       (my-green+1 "green"        "#8fb28f") ; documentation
                       (my-green   "green"        "#7f9f7f") ; inactive modeline foreground
                       (my-magenta "magenta"      "#dc8cc3") ; unknown faces
                       (my-orange  "lightmagenta" "#f7d788") ; header/context/message/...
                       (my-red+1   "lightred"     "#EEBFaF") ; warning
                       (my-red     "red"          "#cb8373") ; error
                       (my-yellow  "yellow"       "#fbfba2") ; constant/line/info/...
                       ))
        ;; colors with +x are darker, colors with -x are lighter
        (lite-colors '((my-bg+2    "lightgray"    "#BcBaB0")
                       (my-bg+1    "lightgray"    "#DcDaD0")
                       (my-bg      "white"        "#F4F2E2")
                       (my-bg-1    "white"        "#FCF8F2")
                       (my-bg-X    "lightmagenta" "#BFAFAF")
                       (my-blue    "blue"         "#0A1A70")
                       (my-blue-1  "blue"         "#283D70")
                       (my-blue-2  "lightblue"    "#385490")
                       (my-cyan    "cyan"         "#93e0e3")
                       (my-cyan-1  "lightcyan"    "#C2EFF1")
                       (my-fg+1    "darkgray"     "#0b0b0b")
                       (my-fg      "darkgray"     "#1f1f1f")
                       (my-fg-1    "black"        "#2f2f2f")
                       (my-fg-2    "black"        "#4f4f4f")
                       (my-green+4 "green"        "#183808")
                       (my-green+3 "green"        "#1a4a0a")
                       (my-green+2 "green"       "#104a20")
                       (my-green+1 "lightgreen"   "#28542b")
                       (my-green   "lightgreen"   "#195E19")
                       (my-magenta "magenta"      "#8F2D72")
                       (my-orange  "brown"        "#bd7500")
                       (my-red+1   "red"          "#8b3626")
                       (my-red     "lightred"     "#cd4f39")
                       (my-yellow  "yellow"       "#a0522d")
                       )))
    (let* ((which-color-list (if light-mode lite-colors dark-colors))
           (matching-color-set (cdr (assoc which-color which-color-list)))
           (m-or-d-color-set (if matching-color-set matching-color-set '("magenta" "#ee82ee"))))
      (if for-terminal (car m-or-d-color-set) (cadr m-or-d-color-set)))))

(let ((cld88 '((class color) (min-colors 88) (background dark)))
      (cll88 '((class color) (min-colors 88) (background light)))
      (cld16 '((class color) (min-colors 16) (background dark)))
      (cll16 '((class color) (min-colors 16) (background light))))
  (flet ((fd88 (c) (my-get-color c      ))
         (fl88 (c) (my-get-color c t    ))
         (fd16 (c) (my-get-color c nil t))
         (fl16 (c) (my-get-color c t   t))
         (fc   (c) (my-get-color c (eq cuatroporocho-background-mode 'dark)))) ; TODO: 16/88
    (custom-theme-set-faces
     'cuatroporocho
     ;; basic coloring (most faces inherit from 'success, 'error, 'warning etc.)
     `(default      ((default :underline nil :slant normal :weight normal :overline nil :width normal
                       :inverse-video nil :box nil :strike-through nil :stipple nil)
                     (,cld88 (:foreground ,(fd88 'my-fg) :background ,(fd88 'my-bg)))
                     (,cll88 (:foreground ,(fl88 'my-fg) :background ,(fl88 'my-bg)))
                     (,cld16 (:foreground ,(fd16 'my-fg) :background ,(fd16 'my-bg)))
                     (,cll16 (:foreground ,(fl16 'my-fg) :background ,(fl16 'my-bg)))
                     (t :foreground "white" :background "black")))
     `(error        ((default :weight bold :underline t)
                     (,cld88 (:foreground ,(fd88 'my-red)))
                     (,cll88 (:foreground ,(fl88 'my-red)))
                     (,cld16 (:foreground ,(fd16 'my-red)))
                     (,cll16 (:foreground ,(fl16 'my-red)))
                     (t :foreground "red")))
     `(warning      ((default :slant italic)
                     (,cld88 (:foreground ,(fd88 'my-red+1)))
                     (,cll88 (:foreground ,(fl88 'my-red+1)))
                     (,cld16 (:foreground ,(fd16 'my-red+1)))
                     (,cll16 (:foreground ,(fl16 'my-red+1)))
                     (t :foreground "yellow")))
     `(success      ((default :underline t)
                     (,cld88 (:foreground ,(fd88 'my-green+3)))
                     (,cll88 (:foreground ,(fl88 'my-green+3)))
                     (,cld16 (:foreground ,(fd16 'my-green+3)))
                     (,cll16 (:foreground ,(fl16 'my-green+3)))
                     (t :foreground "green")))
     `(link         ((default :underline t :weight bold)
                     (,cld88 (:foreground ,(fd88 'my-blue)))
                     (,cll88 (:foreground ,(fl88 'my-blue)))
                     (,cld16 (:foreground ,(fd16 'my-blue)))
                     (,cll16 (:foreground ,(fl16 'my-blue)))
                     (t :foreground "blue")))
     `(link-visited ((default :underline t :weight normal :slant italic)
                     (,cld88 (:foreground ,(fd88 'my-blue)))
                     (,cll88 (:foreground ,(fl88 'my-blue)))
                     (,cld16 (:foreground ,(fd16 'my-blue)))
                     (,cll16 (:foreground ,(fl16 'my-blue)))
                     (t :foreground "blue")))
     `(button       ((t (:inherit link))))
     `(match        ((,cld88 (:foreground ,(fd88 'my-cyan-1) :background ,(fd88 'my-bg-1)))
                     (,cll88 (:foreground ,(fd88 'my-cyan-1) :background ,(fl88 'my-bg-1)))
                     (,cld16 (:foreground ,(fd88 'my-cyan-1) :background ,(fd16 'my-bg-1)))
                     (,cll16 (:foreground ,(fd88 'my-cyan-1) :background ,(fl16 'my-bg-1)))
                     (t :background "cyan"))) ; e.g. grep match
     `(highlight    ((,cld88 (:background ,(fd88 'my-bg+2)))
                     (,cll88 (:background ,(fl88 'my-bg+2)))
                     (,cld16 (:background ,(fd16 'my-bg+2)))
                     (,cll16 (:background ,(fl16 'my-bg+2)))
                     (t :background "blue")))
     `(cursor            ((,cld88 (:background ,(fd88 'my-cyan)))
                          (,cll88 (:background ,(fl88 'my-cyan)))
                          (,cld16 (:background ,(fd16 'my-cyan)))
                          (,cll16 (:background ,(fl16 'my-cyan)))
                          (t :background "cyan")))
     `(pointer           ((t (:inherit cursor))))
     `(mouse             ((t (:inherit cursor))))
     `(minibuffer-prompt ((default :weight bold)
                          (,cld88 (:foreground ,(fd88 'my-fg-1)))
                          (,cll88 (:foreground ,(fl88 'my-fg-1)))
                          (,cld16 (:foreground ,(fd16 'my-fg-1)))
                          (,cll16 (:foreground ,(fl16 'my-fg-1)))
                          (t :foreground "blue")))
     `(minibuffer-noticeable-prompt ((default :weight bold :slant italic)
                                     (,cld88 (:foreground ,(fd88 'my-fg)))
                                     (,cll88 (:foreground ,(fl88 'my-fg)))
                                     (,cld16 (:foreground ,(fd16 'my-fg)))
                                     (,cll16 (:foreground ,(fl16 'my-fg)))
                                     (t :foreground "orange")))
     `(region              ((t (:inherit highlight)))) ; text highlight
     `(cua-rectangle       ((t (:inherit highlight)))) ; CUA rectangle
     `(primary-selection   ((,cld88 (:background ,(fd88 'my-bg+2)))
                            (,cll88 (:background ,(fl88 'my-bg+2)))
                            (,cld16 (:background ,(fd16 'my-bg+2)))
                            (,cll16 (:background ,(fl16 'my-bg+2)))
                            (t :background "blue")))
     `(secondary-selection ((,cld88 (:background ,(fd88 'my-bg-X)))
                            (,cll88 (:background ,(fl88 'my-bg-X)))
                            (,cld16 (:background ,(fd16 'my-bg-X)))
                            (,cll16 (:background ,(fl16 'my-bg-X)))
                            (t :background "brown")))

     ;; isearch and query
     `(isearch           ((t (:inherit match :weight bold))))
     `(lazy-highlight    ((t (:inherit match))))
     `(isearch-secondary ((t (:inherit match :slant italic))))
     `(isearch-fail      ((t (:inherit error))))
     `(query-replace     ((t (:inherit isearch))))

     ;; grep
     `(grep-context-face ((,cld88 (:foreground ,(fd88 'my-orange) :background ,(fd88 'my-bg-1)))
                          (,cll88 (:foreground ,(fl88 'my-orange) :background ,(fl88 'my-bg-1)))
                          (,cld16 (:foreground ,(fd16 'my-orange) :background ,(fd16 'my-bg-1)))
                          (,cll16 (:foreground ,(fl16 'my-orange) :background ,(fl16 'my-bg-1)))
                          (t :foreground "magenta")))
     `(grep-error-face   ((t (:inherit error))))
     `(grep-hit-face     ((t (:inherit success))))
     `(grep-match-face   ((t (:inherit match))))

     ;; compilation and comint
     `(compilation-column-face ((,cld88 (:foreground ,(fd88 'my-yellow)))
                                (,cll88 (:foreground ,(fl88 'my-yellow)))
                                (,cld16 (:foreground ,(fd16 'my-yellow)))
                                (,cll16 (:foreground ,(fl16 'my-yellow)))
                                (t :foreground "yellow")))
     `(compilation-enter-directory-face ((,cld88 (:foreground ,(fd88 'my-blue)))
                                         (,cll88 (:foreground ,(fl88 'my-blue)))
                                         (,cld16 (:foreground ,(fd16 'my-blue)))
                                         (,cll16 (:foreground ,(fl16 'my-blue)))
                                         (t :foreground "blue")))
     `(compilation-error ((t (:inherit error))))
     `(compilation-face ((,cld88 (:foreground ,(fd88 'my-fg)))
                         (,cll88 (:foreground ,(fl88 'my-fg)))
                         (,cld16 (:foreground ,(fd16 'my-fg)))
                         (,cll16 (:foreground ,(fl16 'my-fg)))))
     `(compilation-info ((default :underline t)
                         (,cld88 (:foreground ,(fd88 'my-green+3)))
                         (,cll88 (:foreground ,(fl88 'my-green+3)))
                         (,cld16 (:foreground ,(fd16 'my-green+3)))
                         (,cll16 (:foreground ,(fl16 'my-green+3))))) ; file name (also in grep)
     `(compilation-leave-directory-face ((t (:inherit compilation-enter-directory-face))))
     `(compilation-line-face ((,cld88 (:foreground ,(fd88 'my-yellow)))
                              (,cll88 (:foreground ,(fl88 'my-yellow)))
                              (,cld16 (:foreground ,(fd16 'my-yellow)))
                              (,cll16 (:foreground ,(fl16 'my-yellow)))))
     `(compilation-line-number ((t (:inherit compilation-line-face :underline t)))) ; file line number (also in grep)
     `(compilation-message-face ((,cld88 (:foreground ,(fd88 'my-orange)))
                                 (,cll88 (:foreground ,(fl88 'my-orange)))
                                 (,cld16 (:foreground ,(fd16 'my-orange)))
                                 (,cll16 (:foreground ,(fl16 'my-orange)))
                                 (t :foreground "magenta")))
     `(compilation-warning ((t (:inherit warning))))
     `(comint-highlight-input  ((t (:inherit highlight))))
     `(comint-highlight-prompt ((t (:inherit minibuffer-prompt))))
     `(next-error ((t (:inherit (highlight)))))

     ;; header line and mode line
     `(header-line ((default :box (:line-width -1 :style released-button))
                    (,cld88 (:foreground ,(fd88 'my-orange) :background ,(fd88 'my-bg-1)))
                    (,cll88 (:foreground ,(fl88 'my-orange) :background ,(fl88 'my-bg-1)))
                    (,cld16 (:foreground ,(fd16 'my-orange) :background ,(fd16 'my-bg-1)))
                    (,cll16 (:foreground ,(fl16 'my-orange) :background ,(fl16 'my-bg-1)))
                    (t :foreground "magenta")))
     `(mode-line ((default :box (:line-width -1 :style released-button))
                  (,cld88 (:foreground ,(fd88 'my-green+1) :background ,(fd88 'my-bg-X)))
                  (,cll88 (:foreground ,(fl88 'my-green+1) :background ,(fl88 'my-bg-X)))
                  (,cld16 (:foreground ,(fd16 'my-green+1) :background ,(fd16 'my-bg-X)))
                  (,cll16 (:foreground ,(fl16 'my-green+1) :background ,(fl16 'my-bg-X)))))
     `(mode-line-highlight ((default :box (:line-width 2 :style released-button))
                            (,cld88 (:foreground ,(fd88 'my-green+2) :background ,(fd88 'my-bg+1)))
                            (,cll88 (:foreground ,(fl88 'my-green+2) :background ,(fl88 'my-bg+1)))
                            (,cld16 (:foreground ,(fd16 'my-green+2) :background ,(fd16 'my-bg+1)))
                            (,cll16 (:foreground ,(fl16 'my-green+2) :background ,(fl16 'my-bg+1)))))
     `(mode-line-inactive ((default :box (:line-width -1 :style released-button))
                           (,cld88 (:foreground ,(fd88 'my-green) :background ,(fd88 'my-bg+1)))
                           (,cll88 (:foreground ,(fl88 'my-green) :background ,(fl88 'my-bg+1)))
                           (,cld16 (:foreground ,(fd16 'my-green) :background ,(fd16 'my-bg+1)))
                           (,cll16 (:foreground ,(fl16 'my-green) :background ,(fl16 'my-bg+1)))))
     `(mode-line-buffer-id ((default :weight bold)
                            (,cld88 (:foreground ,(fd88 'my-yellow)))
                            (,cll88 (:foreground ,(fl88 'my-yellow)))
                            (,cld16 (:foreground ,(fd16 'my-yellow)))
                            (,cll16 (:foreground ,(fl16 'my-yellow))))) ; file name

     ;; etc
     `(menu ((t (:inherit default))))
     `(toolbar ((t (:inherit default))))
     `(escape-glyph-face ((t (:inherit font-lock-negation-char-face))))
     `(info-xref ((t (:inherit link))))
     `(info-xref-visited ((t (:inherit link-visited))))
     `(info-menu-star ((t (:inherit highlight))))
     `(fringe ((,cld88 (:foreground ,(fd88 'my-fg) :background ,(fd88 'my-bg+2)))
               (,cll88 (:foreground ,(fl88 'my-fg) :background ,(fl88 'my-bg+2)))
               (,cld16 (:foreground ,(fd16 'my-fg) :background ,(fd16 'my-bg+2)))
               (,cll16 (:foreground ,(fl16 'my-fg) :background ,(fl16 'my-bg+2)))))
     `(vertical-border ((t (:inherit fringe))))

     ;; font lock
     `(font-lock-warning-face ((t (:inherit warning))))
     `(font-lock-keyword-face ((default :weight bold)
                               (,cld88 (:foreground ,(fd88 'my-orange)))
                               (,cll88 (:foreground ,(fl88 'my-orange)))
                               (,cld16 (:foreground ,(fd16 'my-orange)))
                               (,cll16 (:foreground ,(fl16 'my-orange)))))
     `(font-lock-special-keyword-face ((default :weight bold :slant italic)
                                       (,cld88 (:foreground ,(fd88 'my-orange)))
                                       (,cll88 (:foreground ,(fl88 'my-orange)))
                                       (,cld16 (:foreground ,(fd16 'my-orange)))
                                       (,cll16 (:foreground ,(fl16 'my-orange)))
                                       (t :foreground "magenta")))
     `(font-lock-builtin-face ((default :weight bold)
                               (,cld88 (:foreground ,(fd88 'my-yellow)))
                               (,cll88 (:foreground ,(fl88 'my-yellow)))
                               (,cld16 (:foreground ,(fd16 'my-yellow)))
                               (,cll16 (:foreground ,(fl16 'my-yellow))))) ; e.g. elisp :zzz
     `(font-lock-constant-face ((,cld88 (:foreground ,(fd88 'my-yellow)))
                                (,cll88 (:foreground ,(fl88 'my-yellow)))
                                (,cld16 (:foreground ,(fd16 'my-yellow)))
                                (,cll16 (:foreground ,(fl16 'my-yellow))))) ; e.g. elisp 'zzz, C macro constants
     `(font-lock-color-constant-face ((default :slant italic)
                                      (,cld88 (:foreground ,(fd88 'my-yellow)))
                                      (,cll88 (:foreground ,(fl88 'my-yellow)))
                                      (,cld16 (:foreground ,(fd16 'my-yellow)))
                                      (,cll16 (:foreground ,(fl16 'my-yellow)))))
     `(font-lock-preprocessor-face ((default :slant italic)
                                    (,cld88 (:foreground ,(fd88 'my-yellow)))
                                    (,cll88 (:foreground ,(fl88 'my-yellow)))
                                    (,cld16 (:foreground ,(fd16 'my-yellow)))
                                    (,cll16 (:foreground ,(fl16 'my-yellow)))))
     `(font-lock-exit-face ((,cld88 (:foreground ,(fd88 'my-magenta)))
                            (,cll88 (:foreground ,(fl88 'my-magenta)))
                            (,cld16 (:foreground ,(fd16 'my-magenta)))
                            (,cll16 (:foreground ,(fl16 'my-magenta))))) ; TODO: ???
     `(font-lock-other-emphasized-face ((,cld88 (:foreground ,(fd88 'my-magenta)))
                                        (,cll88 (:foreground ,(fl88 'my-magenta)))
                                        (,cld16 (:foreground ,(fd16 'my-magenta)))
                                        (,cll16 (:foreground ,(fl16 'my-magenta))))) ; TODO: ???
     `(c-annotation-face ((t (:inherit font-lock-constant-face))))

     `(font-lock-type-face ((default :weight bold)
                            (,cld88 (:foreground ,(fd88 'my-blue-1)))
                            (,cll88 (:foreground ,(fl88 'my-blue-1)))
                            (,cld16 (:foreground ,(fd16 'my-blue-1)))
                            (,cll16 (:foreground ,(fl16 'my-blue-1)))))
     `(font-lock-other-type-face ((default :weight bold :slant italic)
                                  (,cld88 (:foreground ,(fd88 'my-blue-1)))
                                  (,cll88 (:foreground ,(fl88 'my-blue-1)))
                                  (,cld16 (:foreground ,(fd16 'my-blue-1)))
                                  (,cll16 (:foreground ,(fl16 'my-blue-1)))))
     `(font-lock-function-name-face ((,cld88 (:foreground ,(fd88 'my-blue-1)))
                                     (,cll88 (:foreground ,(fl88 'my-blue-1)))
                                     (,cld16 (:foreground ,(fd16 'my-blue-1)))
                                     (,cll16 (:foreground ,(fl16 'my-blue-1)))))
     `(font-lock-variable-name-face ((,cld88 (:foreground ,(fd88 'my-blue-2)))
                                     (,cll88 (:foreground ,(fl88 'my-blue-2)))
                                     (,cld16 (:foreground ,(fd16 'my-blue-2)))
                                     (,cll16 (:foreground ,(fl16 'my-blue-2)))))
     `(font-lock-reference-face ((,cld88 (:foreground ,(fd88 'my-magenta)))
                                 (,cll88 (:foreground ,(fl88 'my-magenta)))
                                 (,cld16 (:foreground ,(fd16 'my-magenta)))
                                 (,cll16 (:foreground ,(fl16 'my-magenta))))) ;TODO: ???
     `(font-lock-string-face ((,cld88 (:background ,(fd88 'my-bg+1) :foreground ,(fd88 'my-green+4)))
                              (,cll88 (:background ,(fl88 'my-bg+1) :foreground ,(fl88 'my-green+4)))
                              (,cld16 (:background ,(fd16 'my-bg+1) :foreground ,(fd16 'my-green+4)))
                              (,cll16 (:background ,(fl16 'my-bg+1) :foreground ,(fl16 'my-green+4)))))
     `(font-lock-negation-char-face ((,cld88 (:foreground ,(fd88 'my-orange) :background ,(fd88 'my-bg+1)))
                                     (,cll88 (:foreground ,(fl88 'my-orange) :background ,(fl88 'my-bg+1)))
                                     (,cld16 (:foreground ,(fd16 'my-orange) :background ,(fd16 'my-bg+1)))
                                     (,cll16 (:foreground ,(fl16 'my-orange) :background ,(fl16 'my-bg+1)))
                                     (t :foreground "magenta"))) ; e.g. CPP '!' or C op '!'

     `(font-lock-comment-face ((,cld88 (:foreground ,(fd88 'my-fg-1)))
                               (,cll88 (:foreground ,(fl88 'my-fg-1)))
                               (,cld16 (:foreground ,(fd16 'my-fg-1)))
                               (,cll16 (:foreground ,(fl16 'my-fg-1)))))
     `(font-lock-comment-delimiter-face ((,cld88 (:foreground ,(fd88 'my-fg-2)))
                                         (,cll88 (:foreground ,(fl88 'my-fg-2)))
                                         (,cld16 (:foreground ,(fd16 'my-fg-2)))
                                         (,cll16 (:foreground ,(fl16 'my-fg-2)))))
     `(font-lock-doc-face ((default :slant italic)
                           (,cld88 (:foreground ,(fd88 'my-green+1)))
                           (,cll88 (:foreground ,(fl88 'my-green+1)))
                           (,cld16 (:foreground ,(fd16 'my-green+1)))
                           (,cll16 (:foreground ,(fl16 'my-green+1))))) ; e.g. elisp variable doc
     `(font-lock-doc-string-face ((,cld88 (:foreground ,(fd88 'my-green+1)))
                                  (,cll88 (:foreground ,(fl88 'my-green+1)))
                                  (,cld16 (:foreground ,(fd16 'my-green+1)))
                                  (,cll16 (:foreground ,(fl16 'my-green+1)))))

     `(font-lock-regexp-grouping-backslash ((,cld88 (:foreground ,(fd88 'my-red)))
                                            (,cll88 (:foreground ,(fl88 'my-red)))
                                            (,cld16 (:foreground ,(fd16 'my-red)))
                                            (,cll16 (:foreground ,(fl16 'my-red)))))
     `(font-lock-regexp-grouping-construct ((,cld88 (:foreground ,(fd88 'my-red+1)))
                                            (,cll88 (:foreground ,(fl88 'my-red+1)))
                                            (,cld16 (:foreground ,(fd16 'my-red+1)))
                                            (,cll16 (:foreground ,(fl16 'my-red+1)))))

     ;; show-paren
     `(show-paren-mismatch ((t (:inherit error))))
     `(show-paren-match ((default :weight bold)
                         (,cld88 (:foreground ,(fd88 'my-cyan)))
                         (,cll88 (:foreground ,(fl88 'my-cyan)))
                         (,cld16 (:foreground ,(fd16 'my-cyan)))
                         (,cll16 (:foreground ,(fl16 'my-cyan)))))

     ;; ido-mode
     `(ido-first-match ((t (:inherit match))))
     `(ido-only-match  ((t (:inherit match :weight bold :underline t))))
     `(ido-subdir ((,cld88 (:foreground ,(fd88 'my-blue)))
                   (,cll88 (:foreground ,(fl88 'my-blue)))
                   (,cld16 (:foreground ,(fd16 'my-blue)))
                   (,cll16 (:foreground ,(fl16 'my-blue)))))

     ;; hl-line-mode
     `(hl-line      ((t (:inherit fringe))))
     `(hl-line-face ((t (:inherit fringe))))

     ;; linum-mode
     `(linum ((,cld88 (:foreground ,(fd88 'my-yellow) :background ,(fd88 'my-bg+1)))
              (,cll88 (:foreground ,(fl88 'my-yellow) :background ,(fl88 'my-bg+1)))
              (,cld16 (:foreground ,(fd16 'my-yellow) :background ,(fd16 'my-bg+1)))
              (,cll16 (:foreground ,(fl16 'my-yellow) :background ,(fl16 'my-bg+1)))))

     ;; column-marker-1 [ .. 4 ] ; supported by column-marker and python-mode
     `(column-marker-1 ((t (:background "red" :underline t))))

     ;; python-mode - seems to currently font-lock incorrectly (see *Messages*, also w/ other themes)
     `(py-XXX-tag-face        ((t (:inherit font-lock-warning-face))))
     `(py-builtins-face       ((t (:inherit font-lock-builtin-face))))
     `(py-class-name-face     ((t (:inherit font-lock-function-name-face :weight bold))))
     `(py-decorators-face     ((t (:inherit font-lock-function-name-face :slant italic))))
     `(py-exception-name-face ((t (:inherit py-class-name-face :underline t))))
     `(py-number-face         ((,cld88 (:background ,(fd88 'my-bg+1) :foreground ,(fd88 'my-fg)))
                               (,cll88 (:background ,(fl88 'my-bg+1) :foreground ,(fl88 'my-fg)))
                               (,cld16 (:background ,(fd16 'my-bg+1) :foreground ,(fd16 'my-fg)))
                               (,cll16 (:background ,(fl16 'my-bg+1) :foreground ,(fl16 'my-fg)))))
     `(py-pseudo-keyword-face ((t (:inherit font-lock-keyword-face :weight normal))))
     `(py-variable-name-face  ((t (:inherit font-lock-variable-name-face))))

     ;; js2-mode
     `(js2-warning-face           ((t (:inherit warning))))
     `(js2-error-face             ((t (:inherit error))))
     `(js2-external-variable-face ((t (:inherit font-lock-variable-name-face))))
     `(js2-function-param-face    ((t (:inherit font-lock-variable-name-face :slant italic))))
     `(js2-jsdoc-tag-face         ((t (:inherit font-lock-doc-face))))
     `(js2-jsdoc-type-face        ((t (:inherit font-lock-doc-face))))
     `(js2-jsdoc-value-face       ((t (:inherit font-lock-doc-face))))

     ;; diff
     `(diff-added       ((,cld88 (:foreground ,(fd88 'my-green+3)))
                         (,cll88 (:foreground ,(fl88 'my-green+3)))
                         (,cld16 (:foreground ,(fd16 'my-green+3)))
                         (,cll16 (:foreground ,(fl16 'my-green+3)))))
     `(diff-changed     ((,cld88 (:foreground ,(fd88 'my-red+1)))
                         (,cll88 (:foreground ,(fl88 'my-red+1)))
                         (,cld16 (:foreground ,(fd16 'my-red+1)))
                         (,cll16 (:foreground ,(fl16 'my-red+1)))))
     `(diff-removed     ((,cld88 (:foreground ,(fd88 'my-red)))
                         (,cll88 (:foreground ,(fl88 'my-red)))
                         (,cld16 (:foreground ,(fd16 'my-red)))
                         (,cll16 (:foreground ,(fl16 'my-red)))))
     `(diff-header      ((,cld88 (:foreground ,(fd88 'my-orange) :background ,(fd88 'my-bg-1)))
                         (,cll88 (:foreground ,(fl88 'my-orange) :background ,(fl88 'my-bg-1)))
                         (,cld16 (:foreground ,(fd16 'my-orange) :background ,(fd16 'my-bg-1)))
                         (,cll16 (:foreground ,(fl16 'my-orange) :background ,(fl16 'my-bg-1)))
                         (t :foreground "magenta")))
     `(diff-file-header ((default :bold t)
                         (,cld88 (:foreground ,(fd88 'my-blue) :background ,(fd88 'my-bg-1)))
                         (,cll88 (:foreground ,(fl88 'my-blue) :background ,(fl88 'my-bg-1)))
                         (,cld16 (:foreground ,(fd16 'my-blue) :background ,(fd16 'my-bg-1)))
                         (,cll16 (:foreground ,(fl16 'my-blue) :background ,(fl16 'my-bg-1)))))

     ;; dired ; TODO:

     ;; eshell
     `(eshell-prompt        ((t (:inherit minibuffer-prompt))))
     `(eshell-ls-archive    ((,cld88 (:foreground ,(fd88 'my-fg)   :weight bold))))
     `(eshell-ls-backup     ((,cld88 (:foreground ,(fd88 'my-fg-1)))))
     `(eshell-ls-clutter    ((,cld88 (:foreground ,(fd88 'my-fg-1)))))
     `(eshell-ls-directory  ((,cld88 (:foreground ,(fd88 'my-blue) :weight bold))))
     `(eshell-ls-executable ((,cld88 (:foreground ,(fd88 'my-orange) :weight bold))))
     `(eshell-ls-unreadable ((,cld88 (:foreground ,(fd88 'my-red)))))
     `(eshell-ls-missing    ((,cld88 (:foreground ,(fd88 'my-red+1)))))
     `(eshell-ls-product    ((,cld88 (:foreground ,(fd88 'my-fg+1)))))
     `(eshell-ls-special    ((,cld88 (:foreground ,(fd88 'my-magenta) :weight bold))))
     `(eshell-ls-symlink    ((,cld88 (:foreground ,(fd88 'my-blue) :underline t))))

     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face  ((,cld88 (:foreground ,(fd88 'my-cyan)))))
     `(rainbow-delimiters-depth-2-face  ((,cld88 (:foreground ,(fd88 'my-cyan-1)))))
     `(rainbow-delimiters-depth-3-face  ((,cld88 (:foreground ,(fd88 'my-blue-2)))))
     `(rainbow-delimiters-depth-4-face  ((,cld88 (:foreground ,(fd88 'my-blue-1)))))
     `(rainbow-delimiters-depth-5-face  ((,cld88 (:foreground ,(fd88 'my-blue)))))
     `(rainbow-delimiters-depth-6-face  ((,cld88 (:foreground ,(fd88 'my-green+4)))))
     `(rainbow-delimiters-depth-7-face  ((,cld88 (:foreground ,(fd88 'my-green+3)))))
     `(rainbow-delimiters-depth-8-face  ((,cld88 (:foreground ,(fd88 'my-green+2)))))
     `(rainbow-delimiters-depth-9-face  ((,cld88 (:foreground ,(fd88 'my-green+1)))))
     `(rainbow-delimiters-depth-10-face ((,cld88 (:foreground ,(fd88 'my-green)))))
     `(rainbow-delimiters-depth-11-face ((,cld88 (:foreground ,(fd88 'my-cyan)))))
     `(rainbow-delimiters-depth-12-face ((,cld88 (:foreground ,(fd88 'my-cyan-1)))))

     ;; whitespace and whitespace-mode
     `(trailing-whitespace         ((,cld88 (:background ,(fd88 'my-red)))))
     `(whitespace-space            ((,cld88 (:background ,(fd88 'my-bg+1)))))
     `(whitespace-hspace           ((,cld88 (:background ,(fd88 'my-bg+1)))))
     `(whitespace-tab              ((,cld88 (:background ,(fd88 'my-bg+1) :underline t))))
     `(whitespace-newline          ((,cld88 (:background ,(fd88 'my-bg+2)))))
     `(whitespace-trailing         ((t (:inherit trailing-whitespace))))
     `(whitespace-line             ((,cld88 (:background ,(fd88 'my-bg-1)))))
     `(whitespace-space-before-tab ((,cld88 (:foreground ,(fd88 'my-bg+1)))))
     `(whitespace-indentation      ((,cld88 (:foreground ,(fd88 'my-bg+1) :underline t))))
     `(whitespace-empty            ((,cld88 (:background ,(fd88 'my-bg+1)))))
     `(whitespace-space-after-tab  ((,cld88 (:background ,(fd88 'my-red)))))

     ;; which-func-mode
     `(which-func ((t (:inherit font-lock-function-name-face))))

     ;; Yasnippet
     `(yas/field-debug-face     ((t (:inherid font-lock-negation-char-face)))) ; for debugging some overlays normally hidden
     `(yas/field-highlight-face ((t (:inherit highlight)))) ; highlight the currently active field of a snippet
     )

    (custom-theme-set-variables
     'cuatroporocho
     '(frame-background-mode cuatroporocho-background-mode)
     `(ansi-color-names-vector ; black, red, green, yellow, blue, magenta, cyan, white
       [ ,(fc 'my-bg)   ,(fc 'my-red)     ,(fc 'my-green) ,(fc 'my-yellow)
         ,(fc 'my-blue) ,(fc 'my-magenta) ,(fc 'my-cyan)  ,(fc 'my-fg) ] )
     `(fci-rule-color ,(fc 'my-bg-1))) ; fill-column-indicator

    (eval-after-load 'term
      `(setq ansi-term-color-vector ; colors for the ansi-term; used by shell mode
             [ 'unspecified ,(fc 'my-bg)   ,(fc 'my-red)     ,(fc 'my-green) ,(fc 'my-yellow)
                            ,(fc 'my-blue) ,(fc 'my-magenta) ,(fc 'my-cyan)  ,(fc 'my-fg) ] ))))

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'cuatroporocho)

;; Local Variables:;; no-byte-compile: t
;; End:
