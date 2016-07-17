;;; cuatroporocho-theme.el --- Color theme for Emacs.

;;; Commentary:
;;    Technically and color-wise initially based on zenburn, but changed quite drastically from there, both WRT colors
;;    and implementation. The color theme does currently contain colors for win32 graphics mode and win32 console mode
;;    and Linux/X11.

;; Last changed by: NN  on 13-06-2016 20:17:40
;; Note:            As zenburn, this still requires a special load:
;;       (progn (load-theme 'cuatroporocho t) (load "cuatroporocho-theme"))
;; Note:            Use M-x rainbow-mode to display the color codes and constants
;; Note:            Use M-x list-faces-display to find faces in a buffer
;; Note:            For gallery, see: http://pawelbx.github.io/emacs-theme-gallery/
;; ===================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(unless (>= emacs-major-version 24)
  (error "cuatroporocho-theme requires Emacs 24+."))

(deftheme cuatroporocho "The cuatro por ocho color theme")

;;(defvar cuatroporocho-background-mode 'dark)
(defvar cuatroporocho-background-mode 'light)

(defun my-get-color (which-color &optional light-mode for-terminal)
  ;;                               win32          win32
  ;;                               console        graphics
  ;; colors with +x are lighter, colors with -x are darker
  ;;  display-visual-class -> win32 UI: true-color ; win32 console: static-color
  ;;                          linux/X:  true-color ; linux/ubuntu terminal: static-color
  ;;  display-color-cells  -> win32 UI: 16777216   ; win32 console: 16
  ;;                          linux/X:  16777216   ; linux/ubuntu terminal: 256
  ;;  linux terminal colors do not work nice w/ 8 color terminals,
  ;;  they assume: `TERM=xterm-256color emacsclient -nw -c'
  (let ((dark-colors '((my-bg+2    "#884400"      "#4f4f4f") ; fringe and highlighted background; brown
                       (my-bg+1    "#777777"      "#2f2f2f") ; string background and inactive modeline background; darkgray
                       (my-bg      "black"        "#1f1f1f") ; default background
                       (my-bg-1    "black"        "#0b0b0b") ; match background
                       (my-bg-X    "magenta"      "#4F3D3D") ; active modeline background; touch of red
                       (my-blue    "#0000FF"      "#AFC2F2") ; link, directory; lightblue
                       (my-blue-1  "#0000FF"      "#90A8E5") ; types, functions; lightblue
                       (my-blue-2  "blue"         "#7C8AAC") ; variable and parameter names
                       (my-cyan    "#00FFFF"      "#93e0e3") ; cursor; lightcyan
                       (my-cyan-1  "cyan"         "#3D7575") ; match foreground
                       (my-fg      "#AAAAAA"      "#dcdccc") ; default foreground; lightgray
                       (my-fg-1    "#AAAAAA"      "#A5A59C") ; comment; lightgray
                       (my-fg-2    "#AAAAAA"      "#5b6555") ; comment delimiter; touch of green; lightgray
                       (my-green+4 "#00FF00"      "#D3ECD3") ; string foreground; lightgreen
                       (my-green+3 "#00FF00"      "#afd8af") ; success; lightgreen
                       (my-green+2 "#00FF00"      "#9fc59f") ; lightgreen
                       (my-green+1 "green"        "#8fb28f") ; documentation
                       (my-green   "green"        "#7f9f7f") ; inactive modeline foreground
                       (my-magenta "magenta"      "#dc8cc3") ; unknown faces
                       (my-orange  "#FF00FF"      "#f7d788") ; header/context/message/global-mark/...; lightmagenta
                       (my-red+1   "#FF0000"      "#EEBFaF") ; warning; lightred
                       (my-red     "red"          "#cb8373") ; error
                       (my-yellow  "yellow"       "#fbfba2") ; constant/line/info/...
                       ))
        ;; colors with +x are darker, colors with -x are lighter
        (lite-colors '((my-bg+2    "yellow"       "#F0F0D8") ; #EBE6D1
                       (my-bg+1    "#AAAAAA"      "#FCFBE3") ; #F4F2E6 ; lightgray
                       (my-bg      "white"        "#F9F7F1") ;
                       (my-bg-1    "white"        "#FFFFFC")
                       (my-blue    "blue"         "#0400FA")
                       (my-blue-1  "blue"         "#2C29D1")
                       (my-blue-2  "#0000FF"      "#5452A7") ; lightblue
                       (my-cyan    "cyan"         "#6CDFEA") ; #1693A5 #00AD8E
                       (my-cyan-1  "#00FFFF"      "#ADD8C7") ; #69D2E7 #6ECFBD ; lightcyan
                       (my-fg      "black"        "#060F00")
                       (my-fg-1    "#777777"      "#5D5D5B") ; gray
                       (my-fg-2    "#777777"      "#464C43") ; gray
                       (my-green+4 "green"        "#3D4D33")
                       (my-green+3 "green"        "#3A5926")
                       (my-green+2 "green"        "#376619")
                       (my-green+1 "#00FF00"      "#34730D") ; lightgreen
                       (my-green   "#00FF00"      "#318000") ; lightgreen
                       (my-magenta "magenta"      "#AD0088") ; #BD1550
                       (my-bg-X    "#FF00FF"      "#F6E9F3") ; #EDDFDE #E2D9E0 ; lightmagenta
                       (my-orange  "magenta"      "#FA6900")
                       (my-red+1   "#770000"      "#A40802") ; #DD1F0E #B82214 ; red
                       (my-red     "#FF0000"      "#F02311") ; lightred
                       (my-yellow  "#884400"      "#FBB829") ; #EBC000 ; brown
                       )))
    (let* ((which-color-list (if light-mode lite-colors dark-colors))
           (matching-color-set (cdr (assoc which-color which-color-list)))
           (m-or-d-color-set (if matching-color-set matching-color-set '("magenta" "#ee82ee"))))
      (if for-terminal (car m-or-d-color-set) (cadr m-or-d-color-set)))))

(let ((cld88 '((class color) (min-colors 88) (background dark)))
      (cll88 '((class color) (min-colors 88) (background light)))
      (cld16 '((class color) (min-colors 16) (background dark)))
      (cll16 '((class color) (min-colors 16) (background light))))
  (cl-flet ((fd88 (c) (my-get-color c      ))
            (fl88 (c) (my-get-color c t    ))
            (fd16 (c) (my-get-color c nil t))
            (fl16 (c) (my-get-color c t   t))
            (fc   (c) (my-get-color c (eq cuatroporocho-background-mode 'light)))) ; TODO: 16/88, e.g. display-graphic-p?
    (custom-theme-set-faces
     'cuatroporocho
     `(default      ((default :underline nil :slant normal :weight normal :overline nil :width normal
                       :inverse-video nil :box nil :strike-through nil :stipple nil)
                     (,cld88 (:foreground ,(fd88 'my-fg) :background ,(fd88 'my-bg)))
                     (,cll88 (:foreground ,(fl88 'my-fg) :background ,(fl88 'my-bg)))
                     (,cld16 (:foreground ,(fd16 'my-fg) :background ,(fd16 'my-bg)))
                     (,cll16 (:foreground ,(fl16 'my-fg) :background ,(fl16 'my-bg)))
                     (t :foreground "white" :background "black")))
     `(error        ((default :weight bold)
                     (,cld88 (:foreground ,(fd88 'my-red) :underline "red")) ; only standard color underlines work
                     (,cll88 (:foreground ,(fl88 'my-red) :underline "red"))
                     (,cld16 (:foreground ,(fd16 'my-red) :underline "red"))
                     (,cll16 (:foreground ,(fl16 'my-red) :underline "red"))
                     (t :foreground "red")))
     `(warning      ((default :slant italic)
                     (,cld88 (:foreground ,(fd88 'my-red+1)))
                     (,cll88 (:foreground ,(fl88 'my-red+1)))
                     (,cld16 (:foreground ,(fd16 'my-red+1)))
                     (,cll16 (:foreground ,(fl16 'my-red+1)))
                     (t :foreground "yellow")))
     `(cancel       ((t (:inherit warning :slant italic))))
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
     `(cua-rectangle       ((t (:inherit primary-selection)))) ; CUA highlighting rectangle
     `(cua-rectangle-noselect ((t (:inherit secondary-selection)))) ; CUA highlighting non-selected rectangle lines
     `(cua-global-mark ((,cld88 (:background ,(fd88 'my-cyan-1))) ; CUA highlighting the global mark; background only
                        (,cll88 (:background ,(fl88 'my-cyan-1)))
                        (,cld16 (:background ,(fd16 'my-cyan-1)))
                        (,cll16 (:background ,(fl16 'my-cyan-1)))
                        (t :foreground "lightcyan")))
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
     `(compilation-face ((,cld88 (:foreground ,(fd88 'my-fg)))
                         (,cll88 (:foreground ,(fl88 'my-fg)))
                         (,cld16 (:foreground ,(fd16 'my-fg)))
                         (,cll16 (:foreground ,(fl16 'my-fg)))))
     `(compilation-error ((t (:inherit error))))
     `(compilation-warning-face ((t (:inherit warning))))
     `(compilation-info-face ((default :underline t)
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
     '(compilation-mode-line-exit ((t (:inherit compilation-info-face))))
     '(compilation-mode-line-fail ((t (:inherit compilation-error))))
     '(compilation-mode-line-run ((t (:inherit compilation-line-face))))

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
     `(mode-line ((default :box (:line-width -1 :style pressed-button) :width condensed) ; :height 1.0
                  (,cld88 (:foreground ,(fd88 'my-green+1) :background ,(fd88 'my-bg-X)))
                  (,cll88 (:foreground ,(fl88 'my-green+1) :background ,(fl88 'my-bg-X)))
                  (,cld16 (:foreground ,(fd16 'my-green+1) :background ,(fd16 'my-bg-X)))
                  (,cll16 (:foreground ,(fl16 'my-green+1) :background ,(fl16 'my-bg-X)))))
     `(mode-line-highlight ((default :box (:line-width 2 :style released-button))
                            (,cld88 (:foreground ,(fd88 'my-green+2) :background ,(fd88 'my-bg+1)))
                            (,cll88 (:foreground ,(fl88 'my-green+2) :background ,(fl88 'my-bg+1)))
                            (,cld16 (:foreground ,(fd16 'my-green+2) :background ,(fd16 'my-bg+1)))
                            (,cll16 (:foreground ,(fl16 'my-green+2) :background ,(fl16 'my-bg+1)))))
     `(mode-line-emphasis ((t (:inherit mode-line-highlight :weight bold))))
     `(mode-line-inactive ((default :box (:line-width -1 :style released-button) :width condensed :height 0.9)
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
     `(escape-glyph ((t (:inherit font-lock-negation-char-face)))) ; e.g. ^M
     `(nobreak-space ((t (:inherit font-lock-warning-face))))
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
     `(font-lock-builtin-face ((default :weight bold)
                               (,cld88 (:foreground ,(fd88 'my-yellow)))
                               (,cll88 (:foreground ,(fl88 'my-yellow)))
                               (,cld16 (:foreground ,(fd16 'my-yellow)))
                               (,cll16 (:foreground ,(fl16 'my-yellow))))) ; e.g. elisp :zzz
     `(font-lock-constant-face ((,cld88 (:foreground ,(fd88 'my-yellow)))
                                (,cll88 (:foreground ,(fl88 'my-yellow)))
                                (,cld16 (:foreground ,(fd16 'my-yellow)))
                                (,cll16 (:foreground ,(fl16 'my-yellow))))) ; e.g. elisp 'zzz, C macro constants
     `(font-lock-preprocessor-face ((default :slant italic)
                                    (,cld88 (:foreground ,(fd88 'my-yellow)))
                                    (,cll88 (:foreground ,(fl88 'my-yellow)))
                                    (,cld16 (:foreground ,(fd16 'my-yellow)))
                                    (,cll16 (:foreground ,(fl16 'my-yellow)))))
     `(font-lock-type-face ((default :weight bold)
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
                                 (,cll16 (:foreground ,(fl16 'my-magenta)))))
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
     `(show-paren-match ((default :weight bold :strike-through t)
                         (,cld88 (:foreground ,(fd88 'my-cyan) :background ,(fd88 'my-bg+2)))
                         (,cll88 (:foreground ,(fl88 'my-cyan) :background ,(fl88 'my-bg+2)))
                         (,cld16 (:foreground ,(fd16 'my-cyan) :background ,(fd16 'my-bg+2)))
                         (,cll16 (:foreground ,(fl16 'my-cyan) :background ,(fl16 'my-bg+2)))))

     ;; flycheck
     '(flycheck-error ((t (:inherit error))))
     '(flycheck-warning ((t (:inherit warning))))

     ;; ido-mode
     `(ido-first-match ((t (:inherit match))))
     `(ido-only-match  ((t (:inherit match :weight bold :underline t))))
     `(ido-subdir      ((t (:inherit link :underline t))))

     ;; hl-line-mode
     `(hl-line      ((t (:inherit fringe))))
     `(hl-line-face ((t (:inherit fringe))))

     ;; highlight-changes-mode
     `(highlight-changes ((,cld88 (:underline (:color ,(fd88 'my-orange) :style wave)))
                          (,cld88 (:underline (:color ,(fl88 'my-orange) :style wave)))
                          (,cld88 (:underline (:color ,(fd16 'my-orange) :style wave)))
                          (,cld88 (:underline (:color ,(fl16 'my-orange) :style wave)))
                          (t (:inverse-video t))))
     `(highlight-changes-delete ((,cld88 (:underline (:color ,(fd88 'my-magenta) :style wave)))
                                 (,cld88 (:underline (:color ,(fl88 'my-magenta) :style wave)))
                                 (,cld88 (:underline (:color ,(fd16 'my-magenta) :style wave)))
                                 (,cld88 (:underline (:color ,(fl16 'my-magenta) :style wave)))
                                 (t (:inverse-video t))))
     ;; hi-lock - highlight-symbol-at-point
     `(hi-yellow
       ((,cld88 (:background ,(fd88 'my-yellow)))
        (,cll88 (:background ,(fl88 'my-yellow)))
        (,cld16 (:background ,(fd16 'my-yellow)))
        (,cll16 (:background ,(fl16 'my-yellow)))
        (t (:background "yellow"))))
     `(hi-pink
       ((,cld88 (:background ,(fd88 'my-magenta)))
        (,cll88 (:background ,(fl88 'my-magenta)))
        (,cld16 (:background ,(fd16 'my-magenta)))
        (,cll16 (:background ,(fl16 'my-magenta)))
        (t (:background "pink"))))
     `(hi-green
       ((,cld88 (:background ,(fd88 'my-green)))
        (,cll88 (:background ,(fl88 'my-green)))
        (,cld16 (:background ,(fd16 'my-green)))
        (,cll16 (:background ,(fl16 'my-green)))
        (t (:background "green"))))
     `(hi-blue
       ((,cld88 (:background ,(fd88 'my-blue)))
        (,cll88 (:background ,(fl88 'my-blue)))
        (,cld16 (:background ,(fd16 'my-blue)))
        (,cll16 (:background ,(fl16 'my-blue)))
        (t (:background "blue"))))
     `(hi-black-b
       ((,cld88 (:background ,(fd88 'my-bg-X)))
        (,cll88 (:background ,(fl88 'my-bg-X)))
        (,cld16 (:background ,(fd16 'my-bg-X)))
        (,cll16 (:background ,(fl16 'my-bg-X)))
        (t (:background "pink"))))
     `(hi-blue-b
       ((,cld88 (:background ,(fd88 'my-cyan)))
        (,cll88 (:background ,(fl88 'my-cyan)))
        (,cld16 (:background ,(fd16 'my-cyan)))
        (,cll16 (:background ,(fl16 'my-cyan)))
        (t (:background "cyan"))))
     `(hi-red-b
       ((,cld88 (:background ,(fd88 'my-orange)))
        (,cll88 (:background ,(fl88 'my-orange)))
        (,cld16 (:background ,(fd16 'my-orange)))
        (,cll16 (:background ,(fl16 'my-orange)))
        (t (:background "orange"))))
     `(hi-green-b
       ((,cld88 (:background ,(fd88 'my-green+2)))
        (,cll88 (:background ,(fl88 'my-green+2)))
        (,cld16 (:background ,(fd16 'my-green+2)))
        (,cll16 (:background ,(fl16 'my-green+2)))
        (t (:background "green"))))
     `(hi-black-hb
       ((,cld88 (:background ,(fd88 'my-bg+2)))
        (,cll88 (:background ,(fl88 'my-bg+2)))
        (,cld16 (:background ,(fd16 'my-bg+2)))
        (,cll16 (:background ,(fl16 'my-bg+2)))
        (t (:background "yellow"))))
     ;; linum-mode
     `(linum ((,cld88 (:foreground ,(fd88 'my-yellow) :background ,(fd88 'my-bg+1)))
              (,cll88 (:foreground ,(fl88 'my-yellow) :background ,(fl88 'my-bg+1)))
              (,cld16 (:foreground ,(fd16 'my-yellow) :background ,(fd16 'my-bg+1)))
              (,cll16 (:foreground ,(fl16 'my-yellow) :background ,(fl16 'my-bg+1)))))

     ;; column-marker-1 [ .. 4 ] ; supported by column-marker and python-mode
     `(column-marker-1 ((t (:inherit whitespace-line))))

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

     ;; slime-mode
     `(slime-highlight-edits-face ((default :weight bold)))
     `(slime-repl-input-face ((default (:weight normal :underline nil))))
     `(slime-repl-prompt-face ((t (:inherit minibuffer-prompt))))
     `(slime-repl-result-face ((t (:inherit highlight))))
     `(slime-repl-inputed-output-face ((t (:inherit font-lock-negation-char-face))))
     `(slime-repl-output-face ((t (:inherit font-lock-string-face))))

     ;; bigloo
     ;; there is also: ude-font-lock-face-1 .. ude-font-lock-face-12
     `(ude-font-lock-face-1 ((t (:inherit font-lock-builtin-face))))       ; module, ...
     `(ude-font-lock-face-2 ((t (:inherit font-lock-negation-char-face)))) ; assert
     `(ude-font-lock-face-4 ((t (:inherit font-lock-type-face))))          ; ::type
     `(ude-font-lock-face-8 ((t (:inherit font-lock-builtin-face))))       ; error, trace, with-*
     `(ude-font-lock-face-9 ((t (:inherit font-lock-preprocessor-face))))  ; sqlite-*

     ;; racket
     `(racket-check-syntax-def-face ((t (:inherit font-lock-type-face))))
     `(racket-check-syntax-use-face ((t (:inherit font-lock-type-face :weight normal))))
     `(racket-here-string-face ((t (:inherit sh-heredoc-face))))
     `(racket-keyword-argument-face ((t (:inherit font-lock-keyword-face :weight normal))))
     ;;`(racket-paren-face ((t (:inherit font-lock-preprocessor-face))))
     `(racket-selfeval-face ((t (:inherit font-lock-preprocessor-face))))

     ;; Ocaml: tuareg, merlin, utop
     `(tuareg-font-lock-governing-face ((t (:inherit font-lock-builtin-face))))
     `(tuareg-font-lock-multistage-face ((t (:inherit font-lock-preprocessor-face))))
     `(tuareg-font-lock-line-number-face ((t (:inherit compilation-line-face :underline t))))
     `(tuareg-font-lock-operator-face ((t (:inherit font-lock-keyword-face))))
     `(tuareg-font-lock-module-face ((t (:inherit font-lock-builtin-face))))
     `(tuareg-font-lock-constructor-face ((t (:inherit font-lock-function-name-face :weight bold))))
     `(tuareg-font-lock-error-face ((t (:inherit error))))
     `(tuareg-font-lock-interactive-output-face ((t (:inherit font-lock-string-face))))
     `(tuareg-font-lock-interactive-error-face ((t (:inherit error))))
     `(merlin-type-face ((t (:inherit font-lock-type-face))))
     `(merlin-compilation-warning-face ((t (:inherit compilation-warning-face))))
     `(merlin-compilation-error-face ((t (:inherit compilation-error))))
     `(utop-prompt ((t (:inherit minibuffer-prompt))))
     `(utop-stdout ((t (:inherit font-lock-string-face))))
     `(utop-stderr ((t (:inherit warning))))
     `(utop-frozen ((t (:inherit font-lock-comment-face))))
     `(utop-error ((t (:inherit compilation-error))))

     ;; diff
     `(diff-added       ((,cld88 (:foreground ,(fd88 'my-green+3)))
                         (,cll88 (:foreground ,(fl88 'my-green+3)))
                         (,cld16 (:foreground ,(fd16 'my-green+3)))
                         (,cll16 (:foreground ,(fl16 'my-green+3)))))
     `(diff-changed     ((,cld88 (:foreground ,(fd88 'my-red+1)))
                         (,cll88 (:foreground ,(fl88 'my-red+1)))
                         (,cld16 (:foreground ,(fd16 'my-red+1)))
                         (,cll16 (:foreground ,(fl16 'my-red+1)))))
     `(diff-removed     ((default :slant italic)
                         (,cld88 (:foreground ,(fd88 'my-red)))
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

     ;; Magit
     `(magit-diff-add       ((t (:inherit diff-added))))
     `(magit-diff-del       ((t (:inherit diff-removed))))
     `(magit-diff-none      ((t (:inherit diff-context))))
     `(magit-branch         ((t (:inherit diff-changed))))
     `(magit-header         ((t (:inherit diff-header))))
     `(magit-section-title  ((t (:inherit diff-file-header))))
     `(magit-log-sha1       ((t (:inherit warning))))
     `(magit-item-highlight ((t (:inherit font-lock-negation-char-face))))

     ;; dired
     `(dired-directory      ((t (:inherit link :weight bold))))
     `(dired-flagged        ((t (:inherit font-lock-negation-char-face))))
     `(dired-header         ((t (:inherit diff-header))))
     `(dired-ignored        ((t (:inherit font-lock-comment-face))))
     `(dired-mark           ((t (:inherid font-lock-negation-char-face))))
     `(dired-marked         ((t (:inherit font-lock-keyword-face))))
     `(dired-perm-write     ((t (:inherit font-lock-reference-face))))
     `(dired-symlink        ((t (:inherit link :underline t))))
     `(dired-warning        ((t (:inherit warning))))

     ;; shell, eshell
     `(sh-heredoc-face      ((t (:inherit font-lock-string-face :slant italic))))
     `(eshell-prompt        ((t (:inherit minibuffer-prompt))))
     `(eshell-ls-archive    ((t (:inherit font-lock-comment-face :slant italic))))
     `(eshell-ls-backup     ((t (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter    ((t (:inherit font-lock-comment-delimiter-face))))
     `(eshell-ls-directory  ((t (:inherit dired-directory))))
     `(eshell-ls-executable ((t (:inherit font-lock-keyword-face))))
     `(eshell-ls-unreadable ((t (:inherit error))))
     `(eshell-ls-missing    ((t (:inherit warning))))
     `(eshell-ls-product    ((t (:inherit font-lock-string-face)))) ; for files generated from source files
     `(eshell-ls-special    ((t (:inherit font-lock-negation-char-face))))
     `(eshell-ls-symlink    ((t (:inherit dired-symlink))))

     ;; rainbow-delimiters  - {*3*}: add light classes
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

     ;; outline
     '(outline-1 ((t (:inherit rainbow-delimiters-depth-1-face))))

     ;; whitespace and whitespace-mode
     `(trailing-whitespace         ((,cld88 (:background ,(fd88 'my-bg-X)))
                                    (,cll88 (:background ,(fl88 'my-bg-X)))
                                    (,cld16 (:background ,(fd16 'my-bg-X)))
                                    (,cll16 (:background ,(fl16 'my-bg-X)))))
     `(whitespace-trailing         ((t (:inherit trailing-whitespace))))
     `(whitespace-space            ((,cld88 (:background ,(fd88 'my-bg+1)))
                                    (,cll88 (:background ,(fl88 'my-bg+1)))
                                    (,cld16 (:background ,(fd16 'my-bg+1)))
                                    (,cll16 (:background ,(fl16 'my-bg+1)))))
     `(whitespace-hspace           ((t (:inherit whitespace-space))))
     `(whitespace-tab              ((t (:inherit whitespace-space :underline t))))
     `(whitespace-newline          ((,cld88 (:background ,(fd88 'my-bg+2)))
                                    (,cll88 (:background ,(fl88 'my-bg+2)))
                                    (,cld16 (:background ,(fd16 'my-bg+2)))
                                    (,cll16 (:background ,(fl16 'my-bg+2)))))
     `(whitespace-line             ((,cld88 (:underline (:color ,(fd88 'my-orange) :style wave))) ; long lines
                                    (,cll88 (:underline (:color ,(fl88 'my-orange) :style wave)))
                                    (,cld16 (:underline (:color ,(fd16 'my-orange) :style wave)))
                                    (,cll16 (:underline (:color ,(fl16 'my-orange) :style wave)))))
     `(whitespace-space-before-tab ((t (:inherit whitespace-space))))
     `(whitespace-indentation      ((t (:inherit whitespace-space :underline t))))
     `(whitespace-empty            ((t (:inherit whitespace-space))))
     `(whitespace-space-after-tab  ((t (:inherit trailing-whitespace))))

     ;; which-func-mode
     `(which-func                  ((t (:inherit font-lock-function-name-face))))

     ;; Yasnippet
     `(yas/field-debug-face        ((t (:inherid font-lock-negation-char-face)))) ; hidden overlays during debugging
     `(yas/field-highlight-face    ((t (:inherit highlight)))) ; highlight the currently active field of a snippet

     ;; Ace-jump
     `(ace-jump-face-background    ((,cld88 (:background ,(fd88 'my-bg+1)))
                                    (,cll88 (:background ,(fl88 'my-bg+1)))
                                    (,cld16 (:background ,(fd16 'my-bg+1)))
                                    (,cll16 (:background ,(fl16 'my-bg+1)))
                                    (t :background "darkgray")))
     `(ace-jump-face-foreground    ((default :weight bold :slant italic)
                                    (,cld88 (:foreground ,(fd88 'my-magenta)))
                                    (,cll88 (:foreground ,(fl88 'my-magenta)))
                                    (,cld16 (:foreground ,(fd16 'my-magenta)))
                                    (,cll16 (:foreground ,(fl16 'my-magenta)))))

     ;; Company-mode (auto-complete)
     '(company-tooltip ((t (:inherit highlight))))
     '(company-tooltip-selection ((t (:inherit match))))
     '(company-tooltip-common ((t (:inherit company-tooltip :weight bold))))
     '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold))))
     '(company-echo ((t (:inherit minibuffer-prompt))))
     '(company-echo-common ((t (:inherit minibuffer-noticeable-prompt))))
     '(company-preview ((t (:inherit primary-selection))))
     '(company-preview-common ((t (:inherit company-preview :weight bold))))
     '(company-preview-search ((t (:inherit secondary-selection))))

     ;; Org-mode
     `(org-hide ((,cld88 (:foreground ,(fd88 'my-bg)))
                 (,cll88 (:foreground ,(fl88 'my-bg)))
                 (,cld16 (:foreground ,(fd16 'my-bg)))
                 (,cll16 (:foreground ,(fl16 'my-bg)))
                 (((background light)) (:foreground "white"))
                 (((background dark)) (:foreground "black"))
                 ))
     `(org-agenda-calendar-event ((t (:inherit font-lock-regexp-grouping-backslash))))
     `(org-agenda-calendar-sexp ((t (:inherit font-lock-regexp-grouping-construct))))
     `(org-agenda-clocking ((t (:inherit font-lock-doc-face))))
     `(org-agenda-column-dateline ((t (:inherit font-lock-negation-char-face))))
     `(org-agenda-diary ((t (:inherit font-lock-function-name-face))))
     `(org-agenda-dimmed-todo-face ((t (:inherit diff-added))))
     `(org-agenda-done ((t (:inherit success))))
     `(org-agenda-filter-category ((t (:inherit font-lock-type-face))))
     `(org-agenda-filter-tags ((t (:inherit font-lock-type-face))))
     `(org-agenda-restriction-lock ((t (:inherit font-lock-type-face))))
     `(org-agenda-structure ((t (:inherit font-lock-function-name-face))))
     `(org-archived ((t (:inherit font-lock-comment-face))))
     `(org-block ((t (:inherit font-lock-comment-face))))
     `(org-block-background ((t (:inherit whitespace-space))))
     `(org-checkbox ((t (:inherit font-lock-constant-face))))
     `(org-clock-overlay ((t (:inherit secondary-selection))))
     `(org-code ((t (:inherit font-lock-doc-face))))
     `(org-column ((t (:inherit font-lock-variable-name-face))))
     `(org-column-title ((t (:inherit org-column :weight bold))))
     `(org-date ((t (:inherit font-lock-doc-face))))
     `(org-date-selected ((t (:inherit font-lock-doc-face))))
     `(org-document-info ((t (:inherit font-lock-comment-face))))
     `(org-document-info-keyword ((t (:inherit font-lock-comment-delimiter-face))))
     `(org-document-title ((t (:inherit font-lock-negation-char-face))))
     `(org-done ((t (:inherit success))))
     `(org-drawer ((t (:inherit font-lock-function-name-face))))
     `(org-ellipsis ((t (:inherit whitespace-line))))
     `(org-footnote ((t (:inherit font-lock-comment-face))))
     `(org-formula ((t (:inherit font-lock-function-name-face))))
     `(org-headline-done ((t (:inherit success :weight bold))))
     `(org-latex-and-export-specials ((t (:inherid font-lock-negation-char-face))))
     `(org-level-1 ((t (:inherit font-lock-negation-char-face))))
     `(org-level-2 ((t (:inherit font-lock-keyword-face))))
     `(org-level-3 ((t (:inherit font-lock-builtin-face))))
     `(org-level-4 ((t (:inherit font-lock-preprocessor-face))))
     `(org-level-5 ((t (:inherit font-lock-constant-face))))
     `(org-level-6 ((t (:inherit font-lock-type-face))))
     `(org-level-7 ((t (:inherit font-lock-type-face))))
     `(org-level-8 ((t (:inherit font-lock-function-name-face))))
     `(org-link ((t (:inherit link))))
     `(org-list-dt ((t (:inherit font-lock-constant-face))))
     `(org-meta-line ((t (:inherit font-lock-type-face))))
     `(org-property-value ((t (:inherit font-lock-string-face))))
     `(org-scheduled ((t (:inherit diff-changed))))
     `(org-scheduled-previously ((t (:inherit diff-added))))
     `(org-scheduled-today ((t (:inherit diff-removed))))
     `(org-sexp-date ((t (:inherit font-lock-regexp-grouping-construct))))
     `(org-special-keyword ((t (:inherit font-lock-negation-char-face))))
     `(org-table ((t (:inherit  font-lock-function-name-face))))
     `(org-tag ((t (:inherit (highlight)))))
     `(org-target ((t (:inherit link-visited))))
     `(org-time-grid ((t (:inherit font-lock-variable-name-face))))
     `(org-todo ((t (:inherit warning))))
     `(org-upcoming-deadline ((t (:inherit diff-changed))))
     `(org-verbatim ((t (:inherit font-lock-comment-delimiter-face))))
     `(org-warning ((t (:inherit warning))))

     ;; term - colors defined here will be used in `ansi-term-color-vector'
     `(term-color-black ((,cld88 (:foreground ,(fd88 'my-bg) :background ,(fd88 'my-bg-1)))
                         (,cll88 (:foreground ,(fl88 'my-bg) :background ,(fl88 'my-bg-1)))
                         (,cld16 (:foreground ,(fd16 'my-bg) :background ,(fd16 'my-bg-1)))
                         (,cll16 (:foreground ,(fl16 'my-bg) :background ,(fl16 'my-bg-1)))
                         (t (:foreground "black" :background "darkgray"))))
     `(term-color-red ((,cld88 (:foreground ,(fd88 'my-red+1) :background ,(fd88 'my-red)))
                       (,cll88 (:foreground ,(fl88 'my-red+1) :background ,(fl88 'my-red)))
                       (,cld16 (:foreground ,(fd16 'my-red+1) :background ,(fd16 'my-red)))
                       (,cll16 (:foreground ,(fl16 'my-red+1) :background ,(fl16 'my-red)))
                       (t (:foreground "orange" :background "red"))))
     `(term-color-green ((,cld88 (:foreground ,(fd88 'my-green) :background ,(fd88 'my-green+2)))
                         (,cll88 (:foreground ,(fl88 'my-green) :background ,(fl88 'my-green+2)))
                         (,cld16 (:foreground ,(fd16 'my-green) :background ,(fd16 'my-green+2)))
                         (,cll16 (:foreground ,(fl16 'my-green) :background ,(fl16 'my-green+2)))
                         (t (:foreground "green" :background "lightgreen"))))
     `(term-color-yellow ((,cld88 (:foreground ,(fd88 'my-orange) :background ,(fd88 'my-yellow)))
                          (,cll88 (:foreground ,(fl88 'my-orange) :background ,(fl88 'my-yellow)))
                          (,cld16 (:foreground ,(fd16 'my-orange) :background ,(fd16 'my-yellow)))
                          (,cll16 (:foreground ,(fl16 'my-orange) :background ,(fl16 'my-yellow)))
                          (t (:foreground "orange" :background "yellow"))))
     `(term-color-blue ((,cld88 (:foreground ,(fd88 'my-blue) :background ,(fd88 'my-blue-2)))
                        (,cll88 (:foreground ,(fl88 'my-blue) :background ,(fl88 'my-blue-2)))
                        (,cld16 (:foreground ,(fd16 'my-blue) :background ,(fd16 'my-blue-2)))
                        (,cll16 (:foreground ,(fl16 'my-blue) :background ,(fl16 'my-blue-2)))
                        (t (:foreground "blue" :background "lightblue"))))
     `(term-color-magenta ((,cld88 (:foreground ,(fd88 'my-magenta) :background ,(fd88 'my-red)))
                           (,cll88 (:foreground ,(fl88 'my-magenta) :background ,(fl88 'my-red)))
                           (,cld16 (:foreground ,(fd16 'my-magenta) :background ,(fd16 'my-red)))
                           (,cll16 (:foreground ,(fl16 'my-magenta) :background ,(fl16 'my-red)))
                           (t (:foreground "magenta" :background "red"))))
     `(term-color-cyan ((,cld88 (:foreground ,(fd88 'my-cyan) :background ,(fd88 'my-blue)))
                        (,cll88 (:foreground ,(fl88 'my-cyan) :background ,(fl88 'my-blue)))
                        (,cld16 (:foreground ,(fd16 'my-cyan) :background ,(fd16 'my-blue)))
                        (,cll16 (:foreground ,(fl16 'my-cyan) :background ,(fl16 'my-blue)))
                        (t (:foreground "cyan" :background "blue"))))
     `(term-color-white ((,cld88 (:foreground ,(fd88 'my-fg) :background ,(fd88 'my-fg-1)))
                         (,cll88 (:foreground ,(fl88 'my-fg) :background ,(fl88 'my-fg-1)))
                         (,cld16 (:foreground ,(fd16 'my-fg) :background ,(fd16 'my-fg-1)))
                         (,cll16 (:foreground ,(fl16 'my-fg) :background ,(fl16 'my-fg-1)))
                         (t (:foreground "white" :background "lightgray"))))
     '(term-default-fg-color ((t (:inherit term-color-white))))
     '(term-default-bg-color ((t (:inherit term-color-black))))

     ;; others
     `(c-annotation-face ((t (:inherit font-lock-constant-face))))
     `(csv-separator-face ((t (:inherit diff-header))))
     )

    (custom-theme-set-variables
     'cuatroporocho
     '(frame-background-mode cuatroporocho-background-mode)
     `(ansi-color-names-vector          ; black, red, green, yellow, blue, magenta, cyan, white
       [ ,(fc 'my-bg)   ,(fc 'my-red)     ,(fc 'my-green) ,(fc 'my-yellow)
         ,(fc 'my-blue) ,(fc 'my-magenta) ,(fc 'my-cyan)  ,(fc 'my-fg) ] )
     `(fci-rule-color ,(fc 'my-bg-1))) ; fill-column-indicator

    (eval-after-load 'highlight-symbol
      `(setq highlight-symbol-colors
             '( ,(fc 'my-yellow) ,(fc 'my-red)   ,(fc 'my-cyan) ,(fc 'my-magenta) ,(fc 'my-green)
                ,(fc 'my-orange) ,(fc 'my-red+1) ,(fc 'my-blue) ,(fc 'my-fg-2) ))))) ; C-x e here

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; --- scratchpad

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html
;; https://github.com/fniessen/color-theme-leuven/blob/master/color-theme-leuven.el
;; http://edward.oconnor.cx/config/elisp/hober2-theme.el -> tricks with custom-theme variables
;; http://emacsfodder.github.com/DeepBlueDay-theme.el -> tricks with class for different min-colors / backgrounds
;; https://github.com/m00natic/dot-emacs/blob/master/init.el -> with defaults, colors 89, minimal, default/class/t settings
;; nice: http://img119.imageshack.us/my.php?image=gvimeveningqu9.png,
;;   see here: http://www.reddit.com/r/programming/comments/760yt/ask_reddit_help_me_find_a_vim_color_scheme
;; many faces: http://emacswiki.org/emacs/color-theme-ahei.el
;; visualize: http://chriskempson.github.io/base16/#tomorrow
;; brewer palettes for emacs: https://github.com/jrnold/color-theme-brewer/blob/master/colorbrewer.el
;; palette / colorschemes:http://tools.medialab.sciences-po.fr/iwanthue/ http://colorschemedesigner.com/
;;   http://www.workwithcolor.com/hsl-color-schemer-01.htm http://www.colourlovers.com/colors/most-loved/all-time

;; Local Variables:
;; no-byte-compile: t
;; End:
(provide-theme 'cuatroporocho)
;;; cuatroporocho-theme.el ends here
