;; ============================================================================
;; File:            init.el
;; Last changed by: Frank Ruben   on 07-07-2013 20:23:25
;; Purpose:         Emacs initialisation
;;
;; L1-Outline:      (occur "^;;; --- +")
;; L2-Outline:      (occur "^;; --- +")
;; ============================================================================

;;; ###########################################################################
;;; --- Debugging, init, pre-checks
;;; ###########################################################################

;; use this to byte-compile all el's even if they don't exist as elc's before:
;;  (byte-recompile-directory (concat user-emacs-directory "src") 0)
;;  (byte-recompile-directory (getenv "emacs_dir") 0)
;; use this to unbind a variable / function:
;;   (makunbound 'my-variable) (fmakunbound 'my-function)

(setq debug-on-error t) ; log detailed debug output to *Backtrace*
;; debug-on-entry / cancel-debug-on-entry / see alias 'doe below
;; eval-expression-debug-on-error ; If non-nil set `debug-on-error' to t in `eval-expression'

(setq dotfile-name (buffer-file-name)) ; (setq dotfile-name (or load-file-name (buffer-file-name)))

(message "***starting to load %s." dotfile-name)
(setq my-emacs-load-start-time (current-time))

(add-to-list 'load-path (concat user-emacs-directory "src")) ; some el's are directly copied to src root
                                        ; for all others, I'll define their path before loading them

(eval-after-load 'info '(progn (push "~/.emacs.d/info" Info-default-directory-list)))
;; add new info files to ~/emacs-xxx/info/dir, e.g.:
;; Languages
;; * C-library: (libc).            GNU C library functions and macros.
;; * Python: (python).             The Python Documentation.

(setq my-use-shortcut-alias nil)        ; might become useless w/ smex
(setq my-use-consistent-alias t)        ; not useless even w/ smex

;; ============================================================================
;; --- Keydef macros

(defvar my-key-defining-minor-mode-map (make-keymap)
  "My minor-mode keymap. Will be used by `my-define-key' et al.")

(define-minor-mode my-key-defining-minor-mode
  "A minor mode so that my key settings aren't shadowed by other major/minor modes."
  t " keydef" 'my-key-defining-minor-mode-map)

(my-key-defining-minor-mode 1)          ; Make sure our minor mode is the first one in minor-mode-map-alist
(add-hook 'minibuffer-setup-hook '(lambda () (my-key-defining-minor-mode 0)))

;; (defadvice load (after give-my-keybindings-priority)
;;   "Try to ensure that my keybindings always have priority."
;;   (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
;;       (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
;;         (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
;;         (add-to-list 'minor-mode-map-alist mykeys))))
;; (ad-activate 'load)

(defmacro my-define-key (key &rest body)
  "Define BODY with KEY for local keymap `my-key-defining-minor-mode-map'."
  `(define-key my-key-defining-minor-mode-map ,key ,@body))

(defmacro my-define-key-interactive (map key &rest body)
  "Define BODY with KEY as interactive lambda for keymap MAP."
  `(define-key ,map ,key (my-defun-interactive ,@body)))

(defmacro my-define-interactive (key &rest body)
  "Define BODY with KEY as interactive lambda for local keymap `my-key-defining-minor-mode-map'."
  (my-define-key-interactive my-key-defining-minor-mode-map ,key ,@body))

(defmacro my-defun-interactive (&rest body)
  "Define BODY as interactive lambda for use in keymaps."
  `(lambda() (interactive) ,@body))

(defmacro my-safe-interactive (&rest body) ; like ignore-errors, just displays error message
  "Safely execute BODY; don't fail, just display error message."
  `(lambda () (interactive)
     (condition-case err
         (progn ,@body)
       (error (message "%s" (error-message-string err))))))

;;; ###########################################################################
;;; --- General packages
;;; ###########################################################################

;; ============================================================================
;; --- encoding
;;     TODO: sort this out: http://www.gnu.org/software/emacs/manual/html_node/emacs/International.html#International

(unless current-language-environment
  (set-language-environment "Latin-1")) ; "UTF-8" ; effects apply globally to the Emacs session

;; (prefer-coding-system 'iso-8859-1-dos) ; also sets:
;;     `file-name-coding-system'
;;     `set-terminal-coding-system' (which has no effect on graphical terminals)
;;     `set-keyboard-coding-system' (which has no effect on graphical terminals)
;; `file-name-coding-system' defaults to `default-file-name-coding-system', which is auto-set to `cp1252' for win32.
;; `locale-coding-system' is auto-set to `cp1252' for win32.
;; `w32-set-console-codepage' for keyboard input in tty mode,
;; `w32-set-console-output-codepage' to make Windows codepage CP be the codepage for Emacs console output.
;;  set-buffer-file-coding-system ; set-buffer-process-coding-system
;;  Mode variable encoding sets 'file-coding-system-alist'
;;  On win32, Emacs queries the OS about the character set supported by the display,
;;  and sets up the required terminal coding system automatically.

;; ============================================================================
;; --- load cua-mode and transient-mark-mode
;;     CUA bindings are only active when mark is active.
;;     When mark is active use C-S-x (or two rapid C-x C-x) instead C-x for non-CUA binding, same for other CUA keys.

(cua-mode t)                            ; CUA will disable delete-selection-mode
(setq cua-keep-region-after-copy t)     ; selection remains after C-c, standard win32 behaviour
(transient-mark-mode t)                 ; highlight region between point and mark

;;; ###########################################################################
;;; --- Init frame and server mode
;;;     Note: Defined here, but to be called at end of init file
;;; ###########################################################################

(defun my-init-graphic-frame (first-frame frame)
  (if first-frame
      (progn ; started w/ "runemacs"

        ;; --- start the server for emacsclient use (clear the server file w/ server-force-delete)
        (message "***server started.")
        (server-start)

        ;; --- desktop load/save, load recent file, and cursor position handling
        (desktop-save-mode)

        (when (recentf-mode 1)
          (setq recentf-max-saved-items 100)
          (defun my-ido-choose-from-recentf ()
            "Use ido to select a recently opened file from the `recentf-list' shown in minibuffer."
            (interactive)
            (find-file (ido-completing-read "Open file: " recentf-list nil t)))
          (when (fboundp 'my-ido-choose-from-recentf)
            (my-define-key [C-f3]              'my-ido-choose-from-recentf)))

        (require 'saveplace) ; Save cursor position to file save-place-file
        (setq-default save-place t)
        (setq save-place-file (concat user-emacs-directory ".emacs-places")) ; per default in ~/

        (when (require 'workgroups nil t) ; save windows/buffer configuration
          (let ((workgroups-file (concat user-emacs-directory ".emacs-workgroups")))
            (setq wg-prefix-key (kbd "C-c w") ; TODO: better key? ; TODO: move down to other key settings?
                  wg-no-confirm t
                  my-workgroups-file workgroups-file)

            (define-key wg-map (kbd "l") 'my-wg-load-default) ; <prefix> C-l is wg-load, which loads the base config
            (define-key wg-map (kbd "s") 'my-wg-save-default) ; <prefix> C-s is wg-save, which saves the base config
                                        ; use <prefix> U to update the base config from the current config
            (workgroups-mode 1)

            (defun my-wg-load-default ()
              "Run `wg-load' on the default workgroups save file."
              (interactive)
              (when (file-exists-p my-workgroups-file)
                (wg-load my-workgroups-file)))

            (defun my-wg-save-default ()
              "Run `wg-save' on the default workgroups save file."
              (interactive)
              (wg-save my-workgroups-file))

            ;;(add-hook 'auto-save-hook 'my-wg-save-default)
            (add-hook 'kill-emacs-hook 'my-wg-save-default)

            (my-wg-load-default) ))

        ;; --- init display & frame: font and color scheme
        ;;     more font here: http://www.fontsquirrel.com/fonts/list/style/Monospaced
        (condition-case nil
            (set-frame-font "Ubuntu Mono-11") ; quite nice - and narrow (same height as the 2 below)
          (error (set-frame-font "Consolas-12"))) ; Win7+ system font, nicer - but more wide
        ;; (set-frame-font "Source Code Pro-11") ; nice too, but also more wide (same size as "Consolas-12")

        (let ((themes-dir (concat user-emacs-directory "src/themes/")))
          (add-to-list 'load-path themes-dir)
          (add-to-list 'custom-theme-load-path themes-dir)
          (if nil
              (progn (let ((solarized-dir (concat themes-dir "solarized/")))
                       (add-to-list 'load-path solarized-dir)
                       (add-to-list 'custom-theme-load-path solarized-dir))
                     (load-theme 'solarized-light t)) ; (load-theme 'solarized-dark t)
            (progn (let ((cuatroporocho-dir (concat themes-dir "cuatroporocho/")))
                     (add-to-list 'load-path cuatroporocho-dir)
                     (add-to-list 'custom-theme-load-path cuatroporocho-dir))
                   (load-theme 'cuatroporocho t) (load "cuatroporocho-theme")) )) ; (load...) required, see zenburn
        ;; (load-theme 'wombat t) ; (load-theme 'tomorrow-night t) ; (load-theme 'twilight t)
        ;; (load-theme 'zenburn t) (load-theme 'inkpot t) ; (load-theme 'monokai t) ; (load-theme 'blackboard t)
        ;; (load-theme 'leuven t) ; (load-theme 'mesa t) ; (load-theme 'dichromacyd t)

        ;; --- final display settings (call these two at end)
        (w32-send-sys-command #xf030)
        (add-hook 'window-setup-hook (lambda ()
                                       (menu-bar-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1)
                                       (set-fringe-mode 4) ; display bugs w/ 0
                                       (balance-windows)))

        (message "*** %s." "1st graphic frame"))

    ;; else, not in first frame:
    ;; started w/ "emacsclientw" (same frame) or "emacsclientw --tty" (new frame) for win32
    ;; Note: init.el is only evaluated once when Emacs starts, not when emacsclient connects.
    (message "*** %s." "nth graphic frame"))) ; defun my-init-graphic-frame

(defun my-init-term-frame (first-frame frame)
  (if first-frame
      (progn ; started w/ "emacs -nw" for win32 or console emacs for Unix
        ;; don't start server for standalone terminal session
        (set-frame-font "Consolas-12")
        (progn (load-theme 'cuatroporocho t) (load "cuatroporocho-theme"))
        (message "*** %s." "1st term frame"))
    ;; else, currently (w32 only) not expected
    (save-buffers-kill-emacs)))

;;; ###########################################################################
;;; --- Vanilla packages and their settings
;;; ###########################################################################

;; ============================================================================
;; --- time stamp and user info

(require 'time-stamp)                                   ; (setq time-stamp-line-limit 5)
(setq time-stamp-active t)                              ; C-x e this to test: (time-stamp-string)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-start  "[lL]ast changed by[:]?\\s-*")  ; (re-search-forward time-stamp-start)
(setq time-stamp-end    "\\s-*$")
(setq time-stamp-format "%U\ton %02d-%02m-%:y %02H:%02M:%02S")
(setq user-full-name "Frank Ruben"
      user-mail-address "frankruben27@gmail.com")

;; ============================================================================
;; --- modeline and other display settings

(blink-cursor-mode 0)                   ; don't blink cursor
(display-time)                          ; display time in the modeline
(global-visual-line-mode -1)            ; no word-wrap, no editing on visual lines
(mouse-avoidance-mode 'animate)         ; drive out the mouse when it's too near to the cursor
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-interval 60)
(setq font-lock-maximum-decoration t
      jit-lock-defer-time 0.1)          ; defer fontification to not slow down continuous scrolling
(when window-system
  (setq frame-title-format
        '( "%b - " invocation-name "@" system-name "; uptime " (:eval (emacs-uptime)) )))
(setq scroll-conservatively 101) ; don't recenter after scroll; pefered over (setq scroll-step 1)
(setq use-file-dialog nil        ; disable use of file dialog
      use-dialog-box nil)        ; disable file selection dialog
(setq-default scroll-margin 2)   ; Recenter window whenever point gets within this many lines of top or bottom of window

(setq scroll-preserve-screen-position 1) ; try to keep screen position when PgDn/PgUp
(setq visible-bell t) ; disable beep

(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil ; use 'xyz-mode to add this for just one mode
             '(("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)))))

;; ============================================================================
;; --- backup and auto save

(let ((backup-dir (concat temporary-file-directory "emacs.backup"))) ; "%LOCALAPPDATA%\Temp\emacs.backup"
  (make-directory backup-dir t)
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq vc-make-backup-files t) ; Make backups of files, even when they're in version control
  (setq auto-save-file-name-transforms `((".*" ,backup-dir) t))) ; not used for auto-save-visited-file-name t

(setq backup-by-copying t ; (backup-buffer)
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 10
      version-control t)
(add-hook 'before-save-hook (lambda() (setq buffer-backed-up nil))) ; backup EVERY save
(auto-save-mode)
(setq auto-save-visited-file-name t     ; auto-save buffer visited file, not under other name
      auto-save-interval 20             ; keystrokes between auto saves
      auto-save-timeout 30)             ; idle seconds between auto saves

;; ============================================================================
;; --- abbrevs

(let ((abbrev-file (concat user-emacs-directory "emacs.abbrevs.el")))
  (if (file-exists-p abbrev-file)
      (progn
        (require 'dabbrev)
        (setq abbrev-file-name abbrev-file
              dabbrev-case-replace nil  ; Preserve case when expanding
              save-abbrevs 'silently)
        (quietly-read-abbrev-file)      ; abbrev-prefix-mark to combine abbrevs w/in works
        (define-abbrev-table 'global-abbrev-table
          '(("xxx"    "" (lambda() (insert comment-start " TODO:" comment-end) (indent-according-to-mode)))
            ("fb"     "" (lambda() (insert (or user-full-name (getenv "USERNAME") (getenv "USER")))))
            ("cutf8"  "# -*- coding: utf-8 -*-" nil 1)
            ("ciso1"  "# -*- coding: iso-8859-1 -*-" nil 1))))
    (setq abbrev-file-name nil)))

;; ============================================================================
;; --- bookmarks; see C-. b keys

(let ((bookmark-file (concat user-emacs-directory "emacs.bookmarks.el")))
  (if (file-exists-p bookmark-file)
      (setq bookmark-default-file bookmark-file
            bookmark-save-flag t) ; autosave each change
    (setq bookmark-default-file nil)))

;; ============================================================================
;; --- tags / gtags / GNU global (ctags, cscope replacement); see C-. t keys
;;     http://www.gnu.org/software/global/globaldoc.html

(when (require 'gtags nil t)
  (setq gtags-suggested-key-mapping nil
        gtags-disable-pushy-mouse-mapping nil
        gtags-path-style 'relative)

  (defun my-cycle-next-gtag ()
    "Find next matching tag, for GTAGS, after doing gtags-find-tag, gtags-find-rtag, or gtags-find-symbol."
    (interactive)
    (let ((latest-gtags-buffer
           (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                   (buffer-list)) ))))
      (cond (latest-gtags-buffer
             (switch-to-buffer latest-gtags-buffer)
             (forward-line)
             (gtags-select-it nil)))))

  (defun my-init-gtags-mode ()  ; not all keys from gtags supported
    (gtags-mode 1)
    (my-define-key [remap find-tag]           'gtags-find-tag)     ; M-. ; find definition of tag
                                        ; C-u M-. / M-- M-.  next tag match / previous tag match
    (my-define-key [remap tags-loop-continue] 'my-cycle-next-gtag) ; M-, ; continue last tags-search or tags-query-replace
    (my-define-key [remap pop-tag-mark]       'gtags-pop-stack)    ; M-* ; jump back to where M-. was last invoked
    (my-define-key (kbd "C-:")                'gtags-find-rtag)    ; find all references of tag
    (when (boundp 'my-alt-o-map)
      (define-key my-alt-o-map (kbd "M-.")    'gtags-find-tag-other-window))
    ;;  If cursor is on a definition, look for usage. If your cursor is on a usage, will look for the definition.
    (my-define-key (kbd "C-M-:")              'gtags-find-tag-from-here)
    ;; (define-key gtags-mode-map (concat gtags-prefix-key "P") 'gtags-find-file) ; open a file that was indexed by gtags
    ;; (define-key gtags-mode-map (concat gtags-prefix-key "f") 'gtags-parse-file)
    ;; (define-key gtags-mode-map (concat gtags-prefix-key "g") 'gtags-find-with-grep)
    ;; (define-key gtags-mode-map (concat gtags-prefix-key "s") 'gtags-find-symbol) ; find all usages of symbol
    ;; (define-key gtags-mode-map (concat gtags-prefix-key "v") 'gtags-visit-rootdir)
    ;; (setq gtags-libpath `((,(expand-file-name "~/.tags/c") . "/usr/include")))
    ))

(defun my-init-ctags-file-or-list (&optional opt-global-tags-file-name opt-project-tags-file-name)
  ;; Note: to set tags-table-list in file local variables, use 'tags-table-list: ("..")'
  (let* ((global-tags-file-name (or opt-global-tags-file-name
                                    (concat (file-name-extension (buffer-file-name)) ".tags")))
         (global-tags-path-name (concat (file-name-as-directory (getenv "HOME")) global-tags-file-name))
         (project-tags-file-name (or opt-project-tags-file-name "TAGS"))
         (this-tags-path-name (concat (file-name-as-directory ".") project-tags-file-name))
         ;; TODO: support in btags.bat, that the project file is only searched and set as env if found, then use here
         (parent-tags-path-name (concat (file-name-as-directory "..") project-tags-file-name)))
    (when tags-file-name
      (when (file-exists-p tags-file-name)

        (defun my-try-expand-tag (old)
          ;; allow to always add this to he-alist, but skip if no TAGS for current buffer/mode
          (when (and tags-file-name (fboundp 'tags-completion-table))
            (unless old
              (he-init-string (my-tag-beg-internal) (point))
              (setq he-expand-list (sort
                                    (all-completions he-search-string 'my-tags-complete-tag) 'string-lessp)))
            (while (and he-expand-list
                        (he-string-member (car he-expand-list) he-tried-table))
              (setq he-expand-list (cdr he-expand-list)))
            (if (null he-expand-list)
                (progn (when old (he-reset-string)) ())
              (he-substitute-string (car he-expand-list))
              (setq he-expand-list (cdr he-expand-list))
              t)))
        (make-local-variable 'hippie-expand-try-functions-list)
        (add-to-list 'hippie-expand-try-functions-list 'my-try-expand-tag t)

        (add-to-list 'tags-table-list tags-file-name t))
      (setq tags-file-name nil))
    (if (file-exists-p this-tags-path-name)
        (add-to-list 'tags-table-list this-tags-path-name t) ; t appends
      (if (file-exists-p parent-tags-path-name)
          (add-to-list 'tags-table-list parent-tags-path-name t)))
    (when (file-exists-p global-tags-path-name)
      (add-to-list 'tags-table-list global-tags-path-name t)))
  (setq tags-revert-without-query t))

;; ============================================================================
;; --- browser

(let ((ff-exe (concat (getenv "LOCAL") "\\ff\\FirefoxPortable.exe")))
  (when (file-executable-p ff-exe)
    (setq browse-url-firefox-program ff-exe
          browse-url-browser-function 'browse-url-firefox
          browse-url-new-window-flag t
          browse-url-firefox-new-window-is-tab t)))

;; ============================================================================
;; --- outline

(when (require 'outline)                               ; standard emacs
  (defun my-outline-minor-mode-hook ()                 ; activate w/ alias omm
    (my-define-key "\C-c\C-o" outline-mode-prefix-map) ; outline-minor-mode-prefix doesn't work...;
                                        ; and this overrides the C-c C-o for override mode
    ;; C-o - hide-other
    ;; C-s - show-subtree
    ;; C-a - show-all
    (when (require 'outline-magic nil t) ; extra package
      ;; outline-cycle defined below w/ other keys
      (define-key outline-mode-prefix-map "\C-y" 'outline-cycle)
      (define-key outline-mode-prefix-map (kbd "<left>") 'outline-promote)
      (define-key outline-mode-prefix-map (kbd "<right>") 'outline-demote)
      (define-key outline-mode-prefix-map (kbd "<up") 'outline-move-subtree-up)
      (define-key outline-mode-prefix-map (kbd "<down") 'outline-move-subtree-down)))
  (add-hook 'outline-minor-mode-hook 'my-outline-minor-mode-hook))

;; ============================================================================
;; --- hippie-expand

(when (require 'hippie-exp)

  (defun my-hippie-unexpand ()
    (interactive)
    (hippie-expand 0))

  (defun my-hippie-smart-tab ()
    "Minibuffer compliant smart tab handling: acts as usual in the minibuffer. Else, if mark is active, indents region.
Else if point is at the end of a symbol, expands it. Else indents the current line."
    (interactive)
    (if (minibufferp)
        (unless (minibuffer-complete)
          (hippie-expand nil))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (if (looking-at "\\_>")
            (hippie-expand nil)
          (indent-for-tab-command)))))

  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially ; more might be added in programming modes
                                           try-complete-file-name           ; yas-hippie-try-expand added below w/ yasnippet
                                           try-expand-all-abbrevs ; search thru defined abbrevs
                                           try-expand-dabbrev ; like dabbrev-expand, dynamic abbrevs
                                           try-expand-dabbrev-visible ; dabbrev for visible parts of all windows
                                           try-expand-dabbrev-all-buffers))) ; dabbrev for all buffers

;; ============================================================================
;; --- yasnippet & autoinsert

(defun my-load-yas ()
  (let ((yas-rootdir (concat user-emacs-directory "src/yasnippet/"))
        (autoinsert-dir (concat user-emacs-directory "autoinsert/")))
    (when (and (file-directory-p yas-rootdir)
               (file-directory-p autoinsert-dir))

      ;; -- yasnippet; will expand w/ hippie-expand with settings below

      (add-to-list 'load-path yas-rootdir)
      (require 'yasnippet)
      (let ((yas-snippet-dirs (list (concat user-emacs-directory "snippets/")
                                    (concat yas-rootdir "snippets/"))))
        (mapc 'yas-load-directory yas-snippet-dirs))
      ;; (yas-global-mode 1) ; don't enable globally, add (yas-minor-mode-on) to mode hook
      (setq hippie-expand-try-functions-list ; to define a key just for yas: (setq yas/trigger-key (kbd "KKK"))
            (cons 'yas-hippie-try-expand hippie-expand-try-functions-list)) ; make yas available via hippie-expand
      (setq yas-wrap-around-region t)
      (setq yas-trigger-key "<C-tab>")
      (setq yas-next-field-key '("C-tab" "<C-tab>"))

      ;; -- autoinsert

      (require 'autoinsert)
      ;; (auto-insert-mode) ; don't enable globally, add "(add-hook 'find-file-hook 'auto-insert)" per supported mode
      (setq auto-insert-directory autoinsert-dir)
      (setq auto-insert-query nil) ; don't be prompted before insertion

      ;; -- define autoinsert w/ yasnippets
      ;;    C-x e: (yas--get-template-by-uuid 'emacs-lisp-mode "car") ; to test, enter: car + C-TAB
      ;;    C-x e: (yas--get-template-by-uuid 'python-mode "autoinsert.py")

      (defun my-yas-expand-by-uuid (mode uuid)
        "Expand snippet template in MODE by its UUID."
        (yas-expand-snippet (yas--template-content (yas--get-template-by-uuid mode uuid))))

      (defun my-other-define-yas-auto-insert (condition mode template &optional after)
        "Set `auto-insert-alist' to expand SNIPPET-KEY at file creation."
        (add-to-list 'auto-insert-alist `(,condition . (lambda () (my-yas-expand-by-uuid ',mode ,template))) after))

      ;; test auto inserts w/ 'M-x auto-insert' in respective buffer
      (my-other-define-yas-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)\\'" 'cc-mode "autoinsert.c")
      (my-other-define-yas-auto-insert "\\.\\([Cc]\\|cc\\|cpp\\)\\'" 'cc-mode "autoinsert.c")
      (my-other-define-yas-auto-insert "\\.el\\'" 'emacs-lisp-mode "autoinsert.el")
      (my-other-define-yas-auto-insert "\\.go\\'" 'go-mode "autoinsert.go")
      (my-other-define-yas-auto-insert "\\.js\\'" 'js-mode "autoinsert.js")
      (my-other-define-yas-auto-insert "\\.py\\'" 'python-mode "autoinsert.py")
      (my-other-define-yas-auto-insert "\\.\\(r\\|r3\\|red\\|reds\\)\\'" 'rebol-mode "autoinsert.r")

      (message "*** autoinsert and yasnippets [%s] support loaded." yas--version)))) ; defun my-load-yas

(unless (and (fboundp 'yas-global-mode) (fboundp 'auto-insert-mode))
  (condition-case nil
      (my-load-yas)
    (error t)))  ; C-x e: (my-load-yas) ; also for reload

;; ============================================================================
;; --- thingatpt - including function definitions

(when (require 'thingatpt nil t)

  (defun my-minibuffer-insert-thing-at-point (thing)
    "Insert the THING at point (from the minibuffer's parent buffer) to minibuffer."
    (let ((thing-at-point ; see minibuffer-insert-file-name-at-point
           (with-current-buffer (window-buffer (minibuffer-selected-window))
             (thing-at-point thing))))
      (when thing-at-point
        (insert thing-at-point))))

  (defun my-isearch-insert-symbol-at-point ()
    "Insert the symbol at point (in current buffer during isearch) to isearch buffer."
    (interactive)
    (isearch-yank-string (thing-at-point 'symbol)))

  (defun my-minibuffer-insert-symbol-at-point ()
    "Insert the symbol at point (from the minibuffer's parent buffer) to minibuffer."
    (interactive)
    (my-minibuffer-insert-thing-at-point 'symbol))

  (defun my-isearch-insert-word-at-point ()
    "Insert the word at point (in current buffer during isearch) to isearch buffer."
    (interactive)
    (isearch-yank-string (thing-at-point 'word)))

  (defun my-minibuffer-insert-word-at-point ()
    "Insert the word at point (from the minibuffer's parent buffer) to minibuffer."
    (interactive)
    (my-minibuffer-insert-thing-at-point 'word)))

;; ============================================================================
;; --- whitespace; delivered w/ emacs 24, so use it instead of delete-trailing-whitespace and column-marker
;;                 switch on per major mode or use alias wsm as defined below

(require 'whitespace)
(setq whitespace-action '(auto-cleanup) ; automatically clean up bad whitespace
      whitespace-style '(face newline-mark space-mark tab-mark lines-tail empty trailing)
      whitespace-line-column nil)       ; use fill-column for long line detection, mark w/ face whitespace-line
                                        ; use whitespace-line-column 9999999 to not mark long lines
(setq whitespace-display-mappings
      (if t
          '((space-mark   ?\     [? ])                   ; adapted version: use space not dot
            (space-mark   ?\xA0  [?\u00A4]     [?_])
            (space-mark   ?\x8A0 [?\x8A4]      [?_])
            (space-mark   ?\x920 [?\x924]      [?_])
            (space-mark   ?\xE20 [?\xE24]      [?_])
            (space-mark   ?\xF20 [?\xF24]      [?_])
            (newline-mark ?\n    [?$ ?\n])
            (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]))
        '((space-mark   ?\     [?\u00B7]     [?.])       ; original version: space - centered dot
          (space-mark   ?\xA0  [?\u00A4]     [?_])       ; hard space - currency
          (newline-mark ?\n    [?$ ?\n])                 ; eol - dollar sign
          (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])) ; tab - left quote mark
        ))

(add-hook 'before-save-hook 'whitespace-cleanup t) ; force whitespace cleanup; set APPEND flag, to run time-stamp 1st

;; ============================================================================
;; --- other vanilla packages w/o autoloads

(ido-mode t) ; ido is an iswitch improvement ;; TODO: check package ido-ubiquitius
(set-default 'imenu-auto-rescan t)
(setq ido-enable-flex-matching t)       ; enable fuzzy matching
(setq ido-ignore-extensions t)          ; make ido use completion-ignored-extensions
(setq ido-use-virtual-buffers t)        ; ido also supports files from already closed buffers; uses recentf
(setq ido-use-filename-at-point 'guess) ; guess the context when using filename at point (ffap)
;;                                        disable ffap w/ ido-use-url-at-point
;; ido-file-extensions-order set per programming mode

(require 'misc)                         ; C-k z zap-up-to-char, see below

;; ============================================================================
;; --- other optional dependencies

(when (require 'ace-jump-mode nil t))   ; C-c j, see below

(when (require 'autopair nil t)
  ;; no (autopair-global-mode), instead set (autopair-mode 1) per mode
  ;; TODO: better than emacs standard modes (electric-pair-mode, skeleton-pair)?
  ;;       and what about the new smartparens: https://github.com/Fuco1/smartparens/wiki
  ;; TODO: should be a my-quote-region replacement, but doesn't work:
  ;; (setq autopair-autowrap t)
  ;; (push '(?` . ?`) (getf autopair-extra-pairs :everywhere)) (modify-syntax-entry ?` "\"" c-mode-syntax-table)
  ;; (push '(?` . ?') (getf autopair-extra-pairs :comment))
  ;; (push '(?` . ?') (getf autopair-extra-pairs :string))
  ;; autopair-insert-or-skip-quote conflicts w/ my-quote-region
  (electric-pair-mode -1))

(when (require 'evil-numbers nil t))    ; C-. + C-. -, see below

(when (require 'hide-lines nil t))      ; C-c # hide-matching-lines, hide-non-matching-lines, see below

(when (require 'jump-char nil t))       ; C-c f, see below

(when (require 'goto-last-change nil t)) ; C-c l, see below

(when (require 'rainbow-mode nil t))    ; M-x rainbow-mode per buffer; helpful in colortheme definition

;; (when (require 'rainbow-delimiters nil t) ; not perfect...
;;   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(when (require 'smex nil t)
  (setq smex-save-file (concat user-emacs-directory ".smex.save")) ; Must be set before initializing smex
  (smex-initialize)
  (setq smex-prompt-string "M-smex "))

(when (require 'switch-window nil t))   ; overrides C-x o; also M-f5, see below

(when (require 'transpose-frame nil t)) ; rotate-frame-clockwise, flip-frame, ..., see below

(when (require 'winner nil 'noerror)    ; winner-undo, winner-redo - restore window configuration
  (setq winner-dont-bind-my-keys t)     ; don't use default bindings, use M-left / M-right, see below
  (winner-mode t))                      ; turn on the global minor mode

;; (unless (require 'wrap-region nil t))   ; replacement for my-quote-region and autopair-extra-pairs
;;                                         ; also not fixing the `' issue, so try to get these running

(let ((er-rootdir (concat user-emacs-directory "src/expand-region/")))
  (when (file-directory-p er-rootdir)
    (add-to-list 'load-path er-rootdir)
    (require 'expand-region)))          ; C-, keys, see below

(let ((mc-rootdir (concat user-emacs-directory "src/multiple-cursors/")))
  (when (file-directory-p mc-rootdir)
    (add-to-list 'load-path mc-rootdir)
    (require 'multiple-cursors)))       ; C-, keys, see below

;;; ###########################################################################
;;; --- OS, files, ...
;;; ###########################################################################

;; ============================================================================
;; --- find, grep, dired

(require 'grep)

(let ((gnuwin32-dir (concat (getenv "GNUWIN_DIR") "\\bin")))
  (when (and (eq system-type 'windows-nt)
             (file-directory-p gnuwin32-dir))
    ;; stuff below does not work; using my pyvimgrep below by setting it via grep-apply-setting in programming modes

    ;; (defadvice shell-quote-argument (after windows-nt-special-quote (argument) activate)
    ;;   "Add special quotes to ARGUMENT in case the system type is 'windows-nt."
    ;;   (when (w32-shell-dos-semantics)
    ;;     (if (string-match "[\\.~]" ad-return-value)
    ;;         (setq ad-return-value
    ;;               (replace-regexp-in-string
    ;;                "\\([\\.~]\\)"
    ;;                "\\\\\\1"
    ;;                ad-return-value))))) ; required, otherwise rgrep will hang

    ;; To add gnuwin32 to path for emacs shell/command calls:
    ;; (setenv "PATH" (concat gnuwin32-dir ";" (getenv "PATH")))

    ;; ;; This might fix some issues and open others: (setq grep-find-command '("findstr /sn *" . 13))
    ;; ;; gnuwin32 should already be in PATH, anyway add absolute path info to avoid any trouble.
    ;; (let ((exe (concat gnuwin32-dir "\\find.exe"))) ; this is slooooooooow
    ;;   (when (file-executable-p exe) (setq find-program exe))) ; (setq grep-command "findstr /n ") ; add /S for recursive search
    ;; (let ((exe (concat gnuwin32-dir "\\grep.exe")))
    ;;   (when (file-executable-p exe) (setq grep-program exe)))
    ;; (let ((exe (concat gnuwin32-dir "\\xargs.exe"))) ; not used by rgrep for grep-find-use-xargs = exec or exec-plus
    ;;   (when (file-executable-p exe) (setq xargs-program exe)))
    ;; (setq grep-find-use-xargs 'exec)  ; default is exec-plus ('+', quicker), but ';' works
    ;; ;; see http://stackoverflow.com/questions/6085156/using-vs-with-exec-in-find for difference of ';', '+'
    ;; (setq find-exec-terminator (shell-quote-argument ";"))) ; same required for find-dired*
    ))

(fset 'dired-find-file 'dired-find-alternate-file) ; visit this file or directory instead of dired buffer
(put 'dired-find-alternate-file 'disabled nil) ; allow dired to reuse its buffer
;; grep-find-ignored-directories grep-find-ignored-files

;; M-x lgrep                    prompted grep for current dir
;; M-x rgrep                    prompted recursive grep; TODO: doesn't work yet, error in find
;; M-x find-dired               Run `find' and go into Dired mode for output
;; M-x find-name-dired          Run `find' for file name matching os pattern and go into Dired mode for output
;; M-x find-grep-dired          Run `find' for file containing regexp and go into Dired mode for output

(when (require 'dired+)  nil t)

;; C-x C-j                      jump to buffer file in dired
;; C-x d *.el                   open dired w/ only *.el files

;; % m                          mark all files
;; A                            search regexp for marked files
;; Q                            query-replace for marked files, so supports replace for multiple buffers
;; RET / o / a                  open file in dired window & new buffer / other window & buffer / dired window & buffer
;; i                            include dir listing in current buffer
;; C-u k                        remove dir listing from current buffer

;;; ###########################################################################
;;; --- Convenience settings
;;; ###########################################################################

(fset 'yes-or-no-p 'y-or-n-p)   ; use y and n instead of 'yes' and 'no
(global-auto-revert-mode) ; reverts buffer when file changes on disk
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-defun  'disabled nil)   ; C-x n d - elisp, cc-mode, go-mode
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)   ; C-x n n - also see my-toggle-narrow
(put 'scroll-left 'disabled t)
(put 'scroll-right 'disabled t)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq backward-delete-char-untabify-method 'untabify) ; backward-delete-char-untabify turns tab to spaces, then deletes one space
(setq column-number-mode 1)                           ; show the column number in each modeline
(setq comment-fill-column nil)                        ; Use `fill-column' as column for `comment-indent'
(setq comment-column 40)                              ; Column to indent right-margin comments to
(setq completion-auto-help 1)           ; only show auto-completions buffer on 2nd tab-press if no match is found
(setq completion-ignored-extensions (append (list ".exe" ".i" ".s") completion-ignored-extensions))
(setq confirm-nonexistent-file-or-buffer t) ; don't let switch-to-buffer create new buffers w/o asking
(setq cursor-in-non-selected-windows nil) ; turn off cursors in non-selected windows
(setq delete-by-moving-to-trash t) ; use the system's trash can
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)      ; Don't insert instructions in the *scratch* buffer
(setq lazy-highlight-initial-delay 0.5)
(setq line-number-mode 1) ; show the line number in each modeline
;; (setq next-line-add-newlines t)
(setq pop-up-windows nil) ; don't keep splitting windows for things
(setq query-replace-highlight t) ; Highlight during query
(setq read-buffer-completion-ignore-case t) ; Ignore case when completing a buffer name
(setq read-file-name-completion-ignore-case t) ; Ignore case when looking for a file
(setq search-highlight t) ; Highlight incremental search
(setq size-indication-mode t) ; show file size
(setq tab-always-indent t ; for indent-for-tab-command: hitting TAB always just indents the current line
      completion-cycle-threshold 5           ; cycle for less than 5 candidates
      completion-ignore-case t)              ; don't consider case significance for completion
(setq truncate-partial-width-windows nil)    ; same truncate column, if single or multiple windows
(setq view-read-only t) ; activate view-mode (and its hook below) for read-only buffer
(setq w32-get-true-file-attributes nil) ; supposed to stop emacs from pausing all the time...
(setq-default case-fold-search t)       ; make search case-insensitive: Non-nil if search should ignore case
(setq-default fill-column 120) ; text width per line
(setq-default indent-tabs-mode nil) ; spaces, not tabs
(setq-default require-final-newline t) ; make sure file ends with newline
(setq-default sentence-end-double-space nil) ; no two spaces in german
(setq-default sort-fold-case t) ; sort-lines ignoring case
(setq-default truncate-lines t) ; give each line of text just one screen line
(setq-default x-stretch-cursor t) ; when on a tab, make the cursor the tab length
;; paragraph-start and paragraph-separate
;; sentence-end - function or variable

(savehist-mode t) ; keep minibuffer history between session; in savehist-file
(setq savehist-file (concat user-emacs-directory ".emacs.savehist"))
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(show-paren-mode t) ; Show matching parens

(setq kill-emacs-query-functions        ; Add Emacs close confirmation
      (cons (lambda () (yes-or-no-p "Really kill Emacs?"))
            kill-emacs-query-functions))

;; Note: mode for the *scratch* buffer is controlled by the variable initial-major-mode.
(run-with-idle-timer 1 t '(lambda () (get-buffer-create "*scratch*")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eval sql-set-product (quote postgres))))))

;;; ###########################################################################
;;; --- Macros, functions, and defadvice
;;; ###########################################################################

(defun my-align-comment (begin end)
  "Align region to comment start"
  (interactive "r")
  (align-regexp begin end (concat "\\(\\s-*\\)" comment-start)))

(defun my-align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1)) ; TODO: 2nd arg 0 -> no space, 2nd arg 1 -> 2 spaces

(defun my-backup-region-or-buffer ()    ;###nokey
  "Backup text in current region or from whole buffer to a txt file in backup dir."
  (interactive)
  (let* ((backup-dir (concat temporary-file-directory "emacs.backup"))
         (backup-file (format "%s.txt" (float-time)))
         (backup-path (concat backup-dir "/" backup-file)))
    (unless (file-directory-p backup-dir)
      (make-directory backup-dir))
    (if (region-active-p)
        (write-region (region-beginning) (region-end) backup-path)
      (write-region (point-min) (point-max) backup-path))))

(defun my-beginning-of-line-dynamic ()
  "Jumps to beginning of text on line, or if already there, to true beginning of the line."
  (interactive)
  (let ((cur (point)))
    (beginning-of-line-text)
    (if (= cur (point))
        (beginning-of-line))))

(defun my-bury-then-switch-to-buffer (buffer)
  "Bury buffer in all windows, then switch to it in the current window."
  (replace-buffer-in-windows buffer)
  (switch-to-buffer buffer t)) ; t: don't add it to the recent buffer list

(defun my-center-other-window ()
  "Center buffer in other window."
  (interactive)
  (save-selected-window
    (other-window 1)
    (recenter-top-bottom)))

(defun my-close-and-kill-next-window ()
  "Close the other window and kill the buffer in it also."
  (interactive)
  (other-window 1) (kill-buffer) (delete-window))

(defun my-cmd-exec ()                   ;###nokey
  "Run the current BAT file in a compilation buffer."
  (interactive)
  (save-buffer)
  (let ((compilation-buffer-name-function
         (function
          (lambda (ign)
            (concat "*" (buffer-file-name) "*")))))
    (compile (concat (w32-shell-name) " -c " (buffer-file-name)))))

(defun my-copy-from-above-to-char (arg char)
  "Copy all chars from previous line beginning with char currently above cursor up to ARGth occurrence of CHAR."
  (interactive "p\ncCopy to char: ")
  (let* ((col (current-column))
         (n (save-excursion
              (forward-line -1)
              (move-to-column col)
              (search-forward (char-to-string char)
                              (line-end-position) nil arg)
              (- (current-column) col))))
    (copy-from-above-command n)))

(defun my-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)))))

(defun my-delete-word (arg)
  "Delete characters forward until end of word. With ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until beginning of word. With ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun my-delete-to-beginning-of-line ()
  "Delete from current position to beginning of line."
  (interactive)
  (delete-region (point)
                 (save-excursion (beginning-of-line) (point))))

(defun my-delete-to-end-of-line (&optional arg)
  "Delete from current position to end of line."
  (interactive)
  (delete-region (point)
                 (save-excursion (end-of-line) (point)))
  (when arg (delete-char 1)))

(defun my-describe-last-function ()     ;###nokey
  "Bring up the documentation for the thing that immediately happened."
  (interactive)
  (describe-function last-command))

(defun my-downcase-region-or-word ()
  "Downcase region, iff region is marked, otherwise downcase word."
  (interactive)
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word 1)))

(defun my-upcase-region-or-word ()
  "Upcase region, iff region is marked, otherwise upcase word."
  (interactive)
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word 1)))

(defun my-duplicate-current-line ()
  "Duplicate current line."
  (interactive)
  (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (end-of-line) (newline) (insert text))))

(defun my-eval-region-or-sexp (begin end)
  "Eval region, iff region is marked, otherwise eval last sexp."
  (interactive "r")
  (if (region-active-p)
      (eval-region begin end)
    (eval-last-sexp nil)))

(defun my-copy-file-name-to-clipboard () ;###nokey
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun my-google-at-point ()
  "Google a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google query: " (when (fboundp 'thing-at-point) (thing-at-point 'symbol)))))))

(defun my-ido-goto-symbol-definition (&optional symbol-list)
  "Refresh imenu and jump to a given symbol's definition in the buffer using ido."
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
               (my-ido-goto-symbol-definition
                (imenu--make-index-alist)) ; creates index alist for definitions in current buffer
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
          (my-ido-goto-symbol-definition symbol))
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

(defun my-indent-region-or-buffer (&optional noclean)
  "Indent region, iff region is marked, otherwise buffer; unless `noclean' is set, untabify and
delete trailing whitespace."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (unless noclean (untabify (region-beginning) (region-end)))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (unless noclean (whitespace-cleanup-region (region-beginning) (region-end))))))

(defun my-insert-next-line-char ()
  "Insert the same character as in the prior line. Space if none."
  (interactive)
  (let* ((cur (current-column))
         (char (save-excursion
                 (if (or (not (eq 0 (forward-line 1)))
                         (not (eq cur (move-to-column cur)) ))
                     32
                   (char-after)))))
    (insert char)))

(defun my-insert-previous-line-char ()
  "Insert the same character as in the prior line. Space if none."
  (interactive)
  (let* ((cur (current-column))
         (char (save-excursion
                 (if (or (not (eq 0 (forward-line -1)))
                         (not (eq cur (move-to-column cur)) ))
                     32
                   (char-after)))))
    (insert char)))

(defun my-isearch-insert-buffer-name ()
  "Insert the name of the current buffer during isearch to isearch buffer."
  (interactive)
  (isearch-yank-string (buffer-name)))

(defun my-isearch-insert-file-name-at-point ()
  "Get a file name at point in current buffer during isearch and insert it to isearch buffer."
  (interactive)
  (let ((file-name-at-point (ffap-guess-file-name-at-point)))
    (when file-name-at-point (insert file-name-at-point))))

(defun my-isearch-kill-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

(defun my-isearch-exit-before-match (rbeg rend)
  "Exit isearch, but at the other end of the search string. Useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(defun my-isearch-comment-line ()
  "Comment matching line and continue searching."
  (interactive)
  (save-excursion
    (comment-region (point-at-bol) (point-at-eol)))
  (if isearch-forward (isearch-repeat-forward) (isearch-repeat-backward)))

(defun my-isearch-kill-line ()
  "Kill matching line and continue searching."
  (interactive)
  (save-excursion
    (beginning-of-line) (kill-line))
  (if isearch-forward (isearch-repeat-forward) (isearch-repeat-backward)))

(defun my-isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun my-isearch-other-window ()
  "Incremental search in other window."
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward)))

(defun my-isearch-zap-up-to (rbeg rend)
  "Kill region between the mark or isearch start and the closest portion of the isearch match string."
  (interactive "r")
  (let* ((zap-start (if (region-active-p) (mark) isearch-opoint))
         (isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds)))
    (if (< zap-start ismin)
        (kill-region zap-start ismin)
      (if (> zap-start ismax)
          (kill-region ismax zap-start)
        (error "Internal error in isearch kill function.")))
    (isearch-exit)))

(defun my-isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(defun my-join-lines-or-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(defun my-kill-buffer-and-window ()
  "Kills current window and its buffer, if it's saved, leaving point in then current window."
  (interactive)
  (if (or (not buffer-file-name) (not (buffer-modified-p)))
      (progn
        (kill-buffer-and-window)
        (my-other-window-backward))))

(defun my-kill-other-buffer ()
  "Kills the buffer in the other window w/o asking, leaving point in the current buffer."
  (interactive)
  (other-window 1) (kill-buffer nil) (other-window -1))

(defun my-kill-other-buffers-and-windows (prefix-arg)
  "Kill all other buffers and if requested via prefix arg other windows, leaving *scratch* only."
  (interactive "P")
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (when prefix-arg (delete-other-windows)))

(defun my-kill-other-buffer-and-window ()
  "Kills the other window and its buffer, if it's saved, leaving point in the current buffer."
  (interactive)
  (save-selected-window
    (other-window 1)
    (if (or (not buffer-file-name) (not (buffer-modified-p)))
        (progn
          (kill-buffer-and-window)
          (my-other-window-backward)))))

(defun my-kill-current-buffer ()
  "Kills current buffer w/o asking."
  (interactive)
  (kill-buffer nil))

(defun my-kill-word-and-space (arg)
  "Kill word and adjust to one space before next word."
  (interactive "p")
  (kill-word arg) (just-one-space))

(defun my-match-paren (arg)
  "Go to the matching paren if on a paren."
  (interactive "p")
  (cond ((and (region-active-p) (looking-at "[\[\(\{]")) (forward-list 1))
        ((and (region-active-p) (looking-back "[\]\)\}]")) (backward-list 1))
        ((looking-at "[\[\(\{]") (forward-list 1) (backward-char 1))
        ((looking-at "[\]\)\}]") (forward-char 1) (backward-list 1))))

(defun my-minibuffer-insert-buffer-name ()
  "Insert the name of the minibuffer's parent buffer to minibuffer."
  (interactive)
  (let ((parent-buffer-name (with-current-buffer (window-buffer (minibuffer-selected-window))
                              (buffer-name))))
    (when parent-buffer-name
      (insert parent-buffer-name))))

(defun my-move-buffer-file (dir)        ;###nokey
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1) (delete-file filename)
        (set-visited-file-name newname) (set-buffer-modified-p nil) t))))

(defun my-move-region-or-line-internal (arg)
  (cond
   ((region-active-p)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg) (move-to-column column t)
      (set-mark (point)) (insert text) (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun my-move-region-or-line-down (arg)
  "Move region, iff region is marked, otherwise current line arg lines down."
  (interactive "*p")
  (my-move-region-or-line-internal arg))

(defun my-move-region-or-line-up (arg)
  "Move region, iff region is marked, otherwise current line arg lines up."
  (interactive "*p")
  (my-move-region-or-line-internal (- arg)))

(defun my-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun my-next-buffer-other-window ()
  "Switch to next buffer in other window."
  (interactive)
  (save-selected-window
    (other-window 1)
    (next-buffer)))

(defvar my-point-stack nil
  "Stack to store point locations.")

(defun my-exchange-point-and-mark-no-activate () ; ###nokey
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil)) ; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defun my-previous-message (&optional nth)
  "Get last line of *Message* buffer"
  (interactive)
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (setq nth (if nth nth 1))
      (while (> nth 0)
        (previous-line)
        (setq nth (- nth 1)))
      (buffer-substring (line-beginning-position) (line-end-position)))))

(defun my-push-point ()
  "Push the current point onto the stack."
  (interactive)
  (setq my-point-stack (cons (point-marker) my-point-stack)))

(defun my-pop-point ()
  "Try to pop a point from the stack and return to it."
  (interactive)
  (unless (null my-point-stack)
    (let ((m (car my-point-stack)))
      (setq my-point-stack (cdr my-point-stack))
      (if (not (marker-buffer m))
          (my-pop-point)
        (switch-to-buffer (marker-buffer m))
        (goto-char m)))))

(defun my-previous-buffer-other-window ()
  "Switch to previous buffer in other window."
  (interactive)
  (save-selected-window
    (other-window 1)
    (previous-buffer)))

(defun my-open-explorer ()
  "Launch the windows explorer in the current directory and selects current file."
  (interactive) ; see also w32-browser.el for more w32 shell stuff
  (w32-shell-execute
   "open" "explorer" (concat "/e,/select,"
                             (convert-standard-filename buffer-file-name))))

(defun my-open-shell ()
  "Launch the windows cmd in the current directory and selects current file."
  (interactive)
  (w32-shell-execute
   "open" "cmd.exe" (concat "/k for %i in ("
                            (convert-standard-filename buffer-file-name)
                            ") do cd /d %~di%~pi")))

(defun my-insert-next-line (&optional arg) ;###nokey
  "Insert a line below current line w/o changing cursor position."
  (interactive "P")
  (save-excursion
    (end-of-line) (open-line arg) (next-line 1)
    (indent-according-to-mode)))

(defun my-insert-previous-line (&optional arg)    ;###nokey
  "Insert a line above current line w/o changing cursor position."
  (interactive "P")
  (save-excursion
    (beginning-of-line) (open-line arg)
    (indent-according-to-mode)))

(defun my-open-next-line (&optional arg)
  "Move to the next line and open a line; like vi's o command."
  (interactive "P")
  (end-of-line) (open-line arg) (next-line 1)
  (indent-according-to-mode))

(defun my-open-previous-line (&optional arg)
  "Open a new line before the current one; like vi's O command."
  (interactive "P")
  (beginning-of-line) (open-line arg)
  (indent-according-to-mode))

(defun my-other-window-backward (&optional n)
  "Select previous Nth window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun my-prefix-region (s e prefix-arg)
  "Insert prefix PREFIX-ARG at beginning of all lines in region."
  (interactive "r\nsLine-prefix: ")
  (save-excursion
    (save-restriction
      (narrow-to-region s e)
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match prefix-arg nil nil)))))

(defun my-prefix-defun (prefix-arg)
  "Insert prefix PREFIX-ARG at beginning of all lines in defun."
  (interactive "sLine-prefix: ")
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match prefix-arg nil nil)))))

(defun my-quote-region (key-ch &optional start-ch end-ch)
  "Surround region with START-CH and END-CH, iff region is marked, otherwise self-insert KEY-CH."
  (if (region-active-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (insert (or start-ch key-ch))
        (goto-char (point-max))
        (insert (or end-ch start-ch key-ch)))
    (insert key-ch)))

(defun my-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun my-replace-to-num-for-each-region (begin end place start stop step) ;###nokey
  "Copy the active region and replace a placeholder string with a number."
  (interactive "r\nsPlaceholder: \nn Start: \nn Stop: \nn Step: ")
  (let ((template (buffer-substring begin end))
        (count (1+ (/ (- stop start) step))))
    (delete-region begin end)
    (dotimes (time count)
      (let ((value (number-to-string (+ start (* time step))))
            (expansion template))
        (while (string-match place expansion)
          (setq expansion (replace-match value nil t expansion)))
        (insert expansion)))))

(defun my-strip-control-m ()            ;###nokey
  "Remove ^M-."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun my-swap-line-down () ; my-swap-line-up would be same as transpose-lines
  "Transpose lines and move cursor to beginning of new 2nd line."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))

(defun my-switch-to-scratch ()          ;###nokey
  (interactive) (my-bury-then-switch-to-buffer "*scratch*"))

(defun my-switch-to-compilation ()      ;###nokey
  (interactive) (my-bury-then-switch-to-buffer "*compilation*"))

(defun my-toggle-hl-line-mode ()
  "Toggle hl-line mode."
  (if (and (boundp 'hl-line-mode) hl-line-mode) (hl-line-mode -1)
    (hl-line-mode)))

(defun my-toggle-linum-mode ()
  "Toggle linum mode."
  (if (and (boundp 'linum-mode) linum-mode) (linum-mode -1)
    (linum-mode) (setq linum-format "%d")))

(defun my-toggle-selective-display (column) ;###nokey, defalias only
  "Set selective display fold for everything greater than current column, or toggle off if active."
  (interactive "P")
  (set-selective-display                ; selective-display-ellipses defaults to t
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun my-touch-buffer ()
  "Touch, to be saved, and force recompile."
  (interactive)
  (set-buffer-modified-p t))

(defun my-tag-beg-internal ()
  (let ((p (save-excursion (backward-word 1) (point))))
    p))

(defun my-tags-complete-tag (string predicate what)
  (save-excursion
    (if (eq what t) ; If we need to ask for the tag table, allow that
        (all-completions string (tags-completion-table) predicate)
      (try-completion string (tags-completion-table) predicate))))

(defun my-toggle-window-split ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun my-toggle-case (&optional forward-chars)
  "Toggle the case of the char at point and optionally forward char."
  (interactive "P")
  (let* ((this-point (point))
         (next-point (1+ this-point))
         (char (buffer-substring this-point next-point)))
    (if (equal (upcase char) char)
        (downcase-region this-point next-point)
      (upcase-region this-point next-point)))
  (when forward-chars (forward-char forward-chars)))

(defun my-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun my-toggle-narrow ()
  "Narrow to region, iff region is marked, otherwise widen."
  (interactive)
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end))
    (widen)))

(defun my-toggle-narrow-indirect ()
  "Narrow to indirect region, iff region is marked, otherwise widen."
  (interactive)
  (if (region-active-p)
      (my-narrow-to-region-indirect (region-beginning) (region-end))
    (widen)))

(defun my-backward-up-list ()
  "Make backward-up-list also work from inside a string."
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (backward-char))
    (backward-up-list)))

(defun my-up-list ()
  "Make up-list also work from inside a string."
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (forward-char))
    (up-list)))

(defun my-uniq-lines (start end)        ;###nokey
  "Removes duplicate lines from the selected region."
  (interactive "*r")
  (goto-char start)
  (beginning-of-line)
  (let ((last ""))
    (while (< (point) end)
      (let* ((bol (point))
             (eol (progn (end-of-line) (point)))
             (text (buffer-substring bol eol)))
        (forward-char)
        (if (string= last text)
            (delete-region bol (point))
          (setq last text))))))

(defun my-what-face (pos)
  "Print the face found at the current point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my-window-half-height ()
  (max 1 (/ (+ 1 (window-height (selected-window))) 2)))

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury *scratch* buffer instead of killing it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When C-c C-c called interactively with no active region, copy a single line instead."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;; ###########################################################################
;;; --- Programming modes
;;; ###########################################################################

;; ============================================================================
;; --- Base mode
;;     TODO: https://github.com/lunaryorn/flycheck

(require 'compile)

(defun my-base-programming-mode-hook (&optional tw cc fc itm)
  (if (fboundp 'autopair-mode) (autopair-mode) (electric-pair-mode t)) ; OFF w/ (autopair-mode -1)
  (add-hook 'find-file-hooks 'goto-address-prog-mode) ; Make URLs in comments/strings clickable
  (setq indent-tabs-mode itm)                         ; default nil means spaces, not tabs
  (setq tab-width (or tw 2))
  (when cc (setq comment-column cc))    ; globally defaulted; use C-x ; to set comment column (comment-set-column)
  (when fc (setq fill-column fc))       ; globally defaulted
  (turn-on-font-lock)
  (which-function-mode t)
  (whitespace-mode 1))

(defun my-compilation-hook ()
  (window-configuration-to-register ?c) ; save window configuration for later restore after compile using alias rcc
  (setq compilation-scroll-output 'first-error)
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (toggle-read-only 1)
  (let* ((c-buf-name "*compilation*")
         (c-buffer (get-buffer c-buf-name))
         (c-window (and c-buffer (get-buffer-window c-buffer))))
    (if c-window
        (select-window c-window)
      (setq c-window (split-window-vertically)))
    (switch-to-buffer c-buf-name)))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(defun my-view-mode-hook () ; add Vi style movement in view-mode / read-only buffer
  (auto-revert-mode)        ; automatically revert file if changed outside
  (define-key view-mode-map       "h" 'backward-char)
  (define-key view-mode-map       "l" 'forward-char)
  (define-key view-mode-map       "j" 'next-line)
  (define-key view-mode-map       "k" 'previous-line)
  (define-key view-mode-map       "q" 'toggle-read-only)
  (define-key view-mode-map       "i" 'View-quit)
  (define-key view-mode-map       "I" (my-defun-interactive (beginning-of-line) (toggle-read-only)))
  (define-key view-mode-map       "A" (my-defun-interactive (end-of-line) (toggle-read-only)))
  (define-key view-mode-map       "%" 'my-match-paren)
  (define-key view-mode-map       "~" (my-defun-interactive (toggle-read-only) (my-toggle-case 1) (toggle-read-only)))
  (define-key view-mode-map       "n" 'isearch-repeat-forward)
  (define-key view-mode-map       "N" 'isearch-repeat-backward)
  (when (fboundp                      'jump-char-forward) ; Note: other than w/ Vi, keys below not bound to current line
    (define-key view-mode-map     "f" 'jump-char-forward) ; t T yet missing...
    (define-key view-mode-map     "F" 'jump-char-backward)
    (define-key view-mode-map     "," 'jump-char-repeat-forward) ; swap Vi settings: ',' no forward
    (define-key view-mode-map     ";" 'jump-char-repeat-backward))
  (define-key view-mode-map       "/" ; current search string now in isearch-string
    (my-defun-interactive
     (isearch-forward)
     (define-key view-mode-map    "n" 'isearch-repeat-forward)
     (define-key view-mode-map    "N" 'isearch-repeat-backward)))
  (define-key view-mode-map       "?" ; current search string now in isearch-string
    (my-defun-interactive
     (isearch-backward)
     (define-key view-mode-map    "N" 'isearch-repeat-forward)
     (define-key view-mode-map    "n" 'isearch-repeat-backward))))
(add-hook 'view-mode-hook 'my-view-mode-hook)

(defun my-set-cursor-according-to-mode ()
  "Change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (blink-cursor-mode 1) (setq cursor-type '(bar . 4)))
   (overwrite-mode
    (blink-cursor-mode 0) (setq cursor-type 'hollow))
   (t
    (blink-cursor-mode 0) (setq cursor-type 'box))))
(add-hook 'post-command-hook 'my-set-cursor-according-to-mode)

;; ============================================================================
;; --- Text mode

(defun my-text-mode-hook ()
  (add-hook 'find-file-hooks 'goto-address-mode)
  (modify-syntax-entry ?- "w")          ; now '-' is not considered a word-delimiter
  (setq-default word-wrap t)            ; break long line
  (setq fill-column 79)
  (auto-fill-mode nil)                  ; Don't fill and wrap automatically beyond (current-fill-column) / fill-column
  ;;(turn-on-auto-fill)
  (whitespace-mode -1))
(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-after-setting-font-hook ()
  (w32-send-sys-command #xf030))
(add-hook 'after-setting-font-hook 'my-after-setting-font-hook)

;; ============================================================================
;; --- C / C++ / Makefile

(defun my-c-common-mode-hook ()
  (when abbrev-file-name
    (setq abbrev-mode t)
    (define-abbrev-table 'c-mode-abbrev-table
      '(("#is"   "#include <>"   "C-b") ; also snippet inc-1
        ("#ih"   "#include \"\"" "C-b") ; also snippet inc
        ("#ifn"  "#ifndef")
        ("#e"    "#endif")
        ("#ec"   "#endif /* */"  "C-3 C-b")
        ("#ifd"  "#ifdef")
        ("#id"   "#if defined")
        ("pdb"   "HALT();" nil 1))))
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  (defun my-occur-h1 () (interactive) (occur "^/// +--- +"))
  (defun my-occur-h2 () (interactive) (occur "^// +--- +"))
  (local-set-key (kbd "C-. o 1")       'my-occur-h1)
  (local-set-key (kbd "C-. o 2")       'my-occur-h2)

  (when (fboundp 'hide-lines)           ; poor man's hide-comnt.el - only hide lines starting w/ //
    (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +//"))
    (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
    (local-set-key (kbd "C-c C-#")     'show-all-invisible))

  ;; mappings here go to c-mode-base-map
  ;; (local-set-key [return]              'newline-and-indent) ; c-context-line-break
  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key [M-j]                 'c-indent-new-comment-line)
  (local-set-key (kbd "C-ß")           'c-backslash-region) ; default C-c C-\ wont work w/ DE keyboard
  (local-set-key "\C-xf"               'ff-find-other-file) ; load .h for .c and vice versa; alias ff-find-related-file
  (local-set-key "*"                   (my-defun-interactive (my-quote-region "*" "/*" "*/")))
  (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
  (local-set-key [f9]                  'compile)
  (local-set-key [C-f9]                'compile)   ; f9 currently (24-Jun-12) not recognized
  (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
  (local-set-key [C-S-f9]              'recompile)
  (local-set-key "\C-xm"               'recompile) ; no prompt required here
  (local-set-key [S-f9]                (my-defun-interactive (save-some-buffers t) (recompile)))
  (local-set-key [M-f9]                'kill-compilation) ; also C-c C-k

  (when t ; overide/extend movement keys
    (local-set-key (kbd "M-C-S-f")     'c-beginning-of-defun) ; also C-M-a
    (local-set-key (kbd "M-C-f")       'c-end-of-defun) ; also C-M-e
    (local-set-key (kbd "M-C-S-.")     'c-beginning-of-statement) ; also M-a
    (local-set-key (kbd "M-C-.")       'c-end-of-statement) ; also M-e, overrides regexp isearch
    (local-set-key (kbd "M-C-S-c")     'c-backward-conditional)
    (local-set-key (kbd "M-C-c")       'c-forward-conditional)
    (local-set-key (kbd "M-C-S-#")     'c-up-conditional)
    ;;(local-set-key (kbd "M-C-#")       'c-down-conditional)
    (local-set-key (kbd "M-C-S-#")     'c-down-conditional-with-else))
  (when t ; overide/extend mark keys
    (local-set-key (kbd "s-f")         'c-mark-function))

  (when (require 'hideif)
    (setq hide-ifdef-initially t)       ; start with ifdefs hidden
    (when my-use-shortcut-alias
      (defalias 'ifm 'hide-ifdef-mode)  ; toggle ifdef mode; will also hide 'if 0'...
      (defalias 'ifh 'hide-ifdefs)
      (defalias 'ifs 'show-ifdefs)
      (defalias 'ifd 'hide-ifdef-define)
      (defalias 'ifu 'hide-ifdef-undef))
    (eval-after-load "hideif"
      '(progn (setq hide-ifdef-env '((_MSC_VER  . t)
                                     (_WIN32    . t)))))) ; TODO: more defaults for win/vc/...

  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.c;*.h\" --word ") ; --start
  (my-base-programming-mode-hook)
  (setq c-basic-offset 2)
  (setq c-tab-always-indent t)
  (setq comment-start "//" comment-end "" ; use C++ style comments for C and C++
        comment-multi-line t)
  ;; (setq c-backslash-column 72) (setq c-backslash-max-column) ; see c-backslash-region
  (setq-default c-electric-flag t))     ; make electric actions (reindentation,...) happen for electric keys or keywords
(add-hook 'c-mode-common-hook  'my-c-common-mode-hook) ; c-mode-common-hook for C and C++

(defun my-c-mode-hook ()
  (setq ido-file-extensions-order '(".c" ".h"  "" ".md"))
  (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; '_' make no word boundary
  (modify-syntax-entry ?# "w" c-mode-syntax-table) ; '#' make no word boundary; also required for #... abbrevs
  ;; not set, still using find.exe & grep.exe
  ;; (setq grep-find-command                          ; for  M-x grep-find; which is an alias find-grep
  ;;       "grep -rnH --exclude=.git --include=\*.{c,h} --include=-e 'pattern' ~/src/trunk/etc/cpp-src/*")
  (if (fboundp 'my-init-gtags-mode)     ; use `btags C .' in C directory, which will make gtags do the right thing
      (my-init-gtags-mode)
    (my-init-ctags-file-or-list "c")))
(add-hook 'c-mode-hook  'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (setq ido-file-extensions-order '(".cpp" ".hpp" ".cxx" ".hxx" ".c" ".h"  "" ".md"))
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table) ; '_' make no word boundary
  (modify-syntax-entry ?# "w" c++-mode-syntax-table) ; '#' make no word boundary; also required for #... abbrevs
  (my-init-ctags-file-or-list "cpp"))   ; TODO: ~/cpp.tags currently too large
(add-hook 'c++-mode-hook  'my-c++-mode-hook)

(defun my-makefile-mode-hook ()
  (setq ido-file-extensions-order '("" ".mak" ".c" ".h" ".cpp" ".hpp" ".cxx" ".hxx"))
  (modify-syntax-entry ?\\ "_")         ; make \ a word separator in makefiles
  (modify-syntax-entry ?$ "_")          ; make $ a word separator in makefiles
  (local-unset-key "\C-ci") ; my-indent-buffer wracks the makefile, so don't bind here
  (local-set-key (kbd "C-ß")           'makefile-backslash-region) ; default C-c C-\ wont work w/ DE keyboard
  (local-set-key [f9]                  'compile)
  (local-set-key [C-f9]                'compile)   ; f9 currently (24-Jun-12) not recognized
  (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
  (local-set-key [C-S-f9]              'recompile)
  (local-set-key "\C-xm"               'recompile) ; no prompt required here
  (setq comment-multi-line t
        comment-column 40               ; use C-x ; to set comment column (comment-set-column)
        fill-column 79)
  (setq indent-line-function 'tab-to-tab-stop)
  (setq indent-tabs-mode t)             ; we need tabs for makefile
  (setq show-trailing-whitespace t))    ; make this visible even w/o whitespace-mode
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

;; ============================================================================
;; --- ELisp

;; keys which might be useful also in other buffers than elisp
(global-set-key [S-f9] 'my-eval-region-or-sexp) ; C-x C-e doesn't work w/ active region

(defun my-elisp-mode-hook ()
  (when abbrev-file-name (setq abbrev-mode t))
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  (setq ido-file-extensions-order '(".emacs" ".el"))
  (defun my-occur-h1 () (interactive) (occur "^;;; +--- +"))
  (defun my-occur-h2 () (interactive) (occur "^;; +--- +"))
  (local-set-key (kbd "C-. o 1")       'my-occur-h1)
  (local-set-key (kbd "C-. o 2")       'my-occur-h2)

  (when (fboundp 'hide-lines)
    (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +;;"))
    (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
    (local-set-key (kbd "C-c C-#")     'show-all-invisible))

  (make-local-variable 'hippie-expand-try-functions-list)
  ;;try-expand-list ; not so far on top; elisp/TeX
  ;;try-complete-lisp-symbol-partially
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)

  (local-set-key [return]              'newline-and-indent)
  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
  (local-set-key [f9]                  'eval-buffer) ; eval-defun ; eval-region
  (local-set-key [C-f9]                'eval-buffer) ; f9 currently (24-Jun-12) not recognized
  (local-set-key [M-f9]                (my-defun-interactive (byte-compile-file (buffer-file-name)))) ; compile
  (local-set-key [C-M-f9]              (my-defun-interactive (byte-compile-file (buffer-file-name) t))) ; compile & load
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here

  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask *.el --word ") ; --start
  (my-base-programming-mode-hook)

  (my-init-ctags-file-or-list "elisp"))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

;; ============================================================================
;; --- Python

;; py-execute-buffer does not correctly handle the comment w/ the encoding
;;  'set-buffer-process-coding-system' could help, but how to know the correct one? This is file dependent...
;; Or: enter a line 1 with a shebang, like: #! /bin/env python.
;;  Otherwise python-mode inserts one plus an empty line, with encoding comment in line 3 and hence not parsed by Python

(add-to-list 'load-path (concat user-emacs-directory "src/python"))
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(defun my-python-mode-hook ()
  (when abbrev-file-name (setq abbrev-mode t))
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  (setq ido-file-extensions-order '(".py"))
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (font-lock-add-keywords
               'python-mode
               '(("\\<\\(pdb.set_trace\\)" 1 font-lock-warning-face prepend))
               'set)))

  (local-set-key [return]              'newline-and-indent) ; py-newline-and-indent
  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j")           (my-defun-interactive (join-line 1))) ; overriden in python mode
  (local-set-key (kbd "C-S-j")         (my-defun-interactive (join-line))) ; overriden in python mode
  (local-set-key [tab]                 'my-hippie-smart-tab) ; py-indent-region behaves strange...
  (local-set-key [backtab]             'py-shift-region-left) ; other modes: my-hippie-unexpand
  (local-set-key [(control shift tab)] 'py-shift-region-right) ; other modes: tab-to-tab-stop; still M-i
  (local-set-key [f9]                  'py-execute-buffer)
  (local-set-key [C-f9]                'py-execute-buffer) ; f9 currently (24-Jun-12) not recognized
  (local-set-key [S-f9]                'py-execute-region)
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here

  (when (require 'pydoc-info nil 'noerror) ; https://bitbucket.org/jonwaltman/pydoc-info/src/default/pydoc-info.el
    (info-lookup-add-help
     :mode 'python-mode
     :parse-rule 'pydoc-info-python-symbol-at-point
     :doc-spec
     '(("(python)Index" pydoc-info-lookup-transform-entry)
       ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry))))

  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask *.py --word ") ; --start
  (my-base-programming-mode-hook 4)

  (my-init-ctags-file-or-list "python") ; TODO: laaaarge
  ;; M-x pylint
  ;; (local-set-key [C-tab]  'py-complete) ; (setq py-complete-function 'py-completion-at-point)
  ;; py-comment-region
  ;; http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
  (setq pdb-path "py.bat DBG")          ; M-x pdb ==> py.bat DBG ==> python -m pdb myfile.py
  ;; py-which-shell, py-toggle-shells py-default-interpreter
  ;; (setq py-shell-name "py.bat RUN") ; py-shell-toggle-1 ; py-shell-toggle-2
  ;; when setting py-shell-name, probably not required: python-python-command + python-python-command-args
  ;; py-python-command-args
  ;; py-fontify-shell-buffer-p t
  ;; (setq py-cleanup-temporary nil)     ; dont delete temp files (exec region only?)
  (setq py-mark-decorators t)
  (setq py-start-run-py-shell nil)       ; don't start python shell w/ python mode
  (setq py-start-run-ipython-shell nil)) ; same for ipython shell
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; ============================================================================
;; --- Javascript mode - the standard moded available since Emacs 23.2

(defun my-js-mode-hook ()
  (when abbrev-file-name (setq abbrev-mode t))
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  ;; (set (make-local-variable 'compile-command)
  ;;      (format "go build %s" (file-name-nondirectory buffer-file-name)))

  (defun my-occur-h1 () (interactive) (occur "^/// +--- +"))
  (defun my-occur-h2 () (interactive) (occur "^// +--- +"))
  (local-set-key (kbd "C-. o 1")       'my-occur-h1)
  (local-set-key (kbd "C-. o 2")       'my-occur-h2)

  (when (fboundp 'hide-lines)           ; poor man's hide-comnt.el - only hide lines starting w/ //
    (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +//"))
    (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
    (local-set-key (kbd "C-c C-#")     'show-all-invisible))

  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key [M-j]                 'c-indent-new-comment-line)
  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key "*"                   (my-defun-interactive (my-quote-region "*" "/*" "*/")))
  (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
  (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
  (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here
  (local-set-key "\C-xm"               'recompile) ; no prompt required here
  (local-set-key [M-f9]                'kill-compilation) ; also C-c C-k

  (when t ; overide/extend movement keys
    (local-set-key (kbd "M-C-S-f")     'c-beginning-of-defun) ; also C-M-a
    (local-set-key (kbd "M-C-f")       'c-end-of-defun) ; also C-M-e
    (local-set-key (kbd "M-C-S-.")     'c-beginning-of-statement) ; also M-a
    (local-set-key (kbd "M-C-.")       'c-end-of-statement) ; also M-e, overrides regexp isearch
    (local-set-key (kbd "M-C-S-c")     'c-backward-conditional)
    (local-set-key (kbd "M-C-c")       'c-forward-conditional)
    (local-set-key (kbd "M-C-S-#")     'c-up-conditional)
    ;;(local-set-key (kbd "M-C-#")       'c-down-conditional)
    (local-set-key (kbd "M-C-S-#")     'c-down-conditional-with-else))
  (when t ; overide/extend mark keys
    (local-set-key (kbd "s-f")         'c-mark-function))

  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.js\" --word ") ; --start
  (my-base-programming-mode-hook 4)
  (setq c-basic-offset 4)
  (setq c-tab-always-indent t)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; '_' make no word boundary
  (setq comment-start "//" comment-end ""
        comment-multi-line t)
  (setq-default c-electric-flag t)      ; make electric actions (reindentation,...) happen for electric keys or keywords
  (my-init-ctags-file-or-list "javascript"))
(add-hook 'js-mode-hook 'my-js-mode-hook)

;; ============================================================================
;; --- Golang
;;     Go mode from Go distribution
;;     yasnippets from https://github.com/dominikh/yasnippet-go
;;       manually installed below ~/.emacs.d/snippets/go-mode

(require 'go-mode-load)
(defun my-go-mode-hook ()
  (when abbrev-file-name (setq abbrev-mode t))
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  (set (make-local-variable 'compile-command)
       (format "go build %s" (file-name-nondirectory buffer-file-name)))

  (defun my-occur-h1 () (interactive) (occur "^/// +--- +"))
  (defun my-occur-h2 () (interactive) (occur "^// +--- +"))
  (local-set-key (kbd "C-. o 1")       'my-occur-h1)
  (local-set-key (kbd "C-. o 2")       'my-occur-h2)

  (when (fboundp 'hide-lines)           ; poor man's hide-comnt.el - only hide lines starting w/ //
    (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +//"))
    (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
    (local-set-key (kbd "C-c C-#")     'show-all-invisible))

  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key "*"                   (my-defun-interactive (my-quote-region "*" "/*" "*/")))
  (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
  (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
  (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here
  (local-set-key "\C-xm"               'recompile) ; no prompt required here
  (local-set-key [M-f9]                'kill-compilation) ; also C-c C-k

  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.c;*.h\" --word ") ; --start
  (my-base-programming-mode-hook 4)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; '_' make no word boundary
  (setq comment-start "//" comment-end ""
        comment-multi-line t)
  (setq tags-table-list nil)
  ;; write go file so that it looks like everyone's - but display as I like it; but not:
  ;; can't add tab-setting for gofmt and re-formatting after the save changes cursor pos
  ;; (add-hook 'before-save-hook (lambda () (gofmt-before-save) (my-indent-region-or-buffer)))
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; ============================================================================
;; --- Rebol mode
;;     Rebol mode from Github

(add-to-list 'auto-mode-alist '("\\.\\(r\\|r3\\|red\\|reds\\)\\'" . rebol-mode))
(require 'rebol)
(defun my-rebol-mode-hook ()
  (set (make-local-variable 'time-stamp-start  "Date:?\\s-*"))
  (set (make-local-variable 'time-stamp-end    "\\s-*$"))
  (set (make-local-variable 'time-stamp-format "%02d-%3b-%:y %02H:%02M:%02S"))

  (when abbrev-file-name (setq abbrev-mode t))
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  ;; (set (make-local-variable 'compile-command)
  ;;      (format "go build %s" (file-name-nondirectory buffer-file-name)))

  (defun my-occur-h1 () (interactive) (occur "^;=== +"))
  (defun my-occur-h2 () (interactive) (occur "^;--- +"))
  (local-set-key (kbd "C-. o 1")       'my-occur-h1)
  (local-set-key (kbd "C-. o 2")       'my-occur-h2)

  (when (fboundp 'hide-lines)           ; poor man's hide-comnt.el - only hide lines starting w/ //
    (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +;"))
    (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
    (local-set-key (kbd "C-c C-#")     'show-all-invisible))

  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key "*"                   (my-defun-interactive (my-quote-region "*" "/*" "*/")))
  (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
  (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
  (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here
  (local-set-key "\C-xm"               'recompile) ; no prompt required here
  (local-set-key [M-f9]                'kill-compilation) ; also C-c C-k

  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.r;*.red;*.redl\" --word ")
  (my-base-programming-mode-hook 4)
  ;; (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; '_' make no word boundary
  (setq comment-start ";" comment-end ""
        comment-multi-line t)
  (setq tags-table-list nil))
(add-hook 'rebol-mode-hook 'my-rebol-mode-hook)

;; ============================================================================
;; --- SQL

(defun my-sql-mode-hook ()
  (when abbrev-file-name (setq abbrev-mode t))

  (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here

  (my-base-programming-mode-hook 4 nil 79))
(add-hook 'sql-mode-hook 'my-sql-mode-hook)

;; ============================================================================
;; --- Win32

(when (and (not (fboundp 'ntcmd-mode)) (require 'ntcmd nil t))
  (add-to-list 'auto-mode-alist '("\\.bat\\'" . ntcmd-mode))
  (autoload 'ntcmd-mode "ntcmd" "ntcmd-mode" t) ; TODO: also working here? (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (defun my-ntcmd-mode-hook ()          ; editing batch files

    (defun my-send-region-to-shell ()
      "Execute shell (or command on region)"
      (interactive)
      (if (region-active-p)
          (shell-command (buffer-substring-no-properties (region-beginning) (region-end)))
        (shell)))

    (defun my-clear-shell-buffer ()
      (interactive)
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))

    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
    (local-set-key [C-f9]             'compile) ; f9 currently (24-Jun-12) not recognized
    (local-set-key [C-S-f9]           'recompile) ; no prompt required here
    (local-set-key "%"                (my-defun-interactive (my-quote-region "%")))
    (local-set-key (kbd "C-z")        'bury-buffer)
    (make-variable-buffer-local 'comint-completion-addsuffix)
    ;;(set-buffer-process-coding-system 'raw-text-dos 'raw-text-dos)
    (setq comint-completion-addsuffix '("\\" . " "))
    (setq comint-process-echoes t)
    (setq comment-start "::" comment-end "")
    (setq tab-width ntcmd-indent-level))
  (add-hook 'ntcmd-mode-hook 'my-ntcmd-mode-hook))

(defun my-shell-setup ()
  (ansi-color-for-comint-mode-on)
  (setq my-key-defining-minor-mode nil)) ; need C-c C-p / C-c C-n / C-up / C-down here, instead of my keys
(setq shell-mode-hook 'my-shell-setup)

;;; ###########################################################################
;;; --- Keys
;;;     On Thinkpad, disable the C-M-left/right/up/down keys w/ F12 -> Options -> Hot Key Manager,
;;;      e.g. set to C-M-S-left/right/up/down keys
;;;     To ignore key, bind to function (ignore) - dooh... - or (undefined)
;;;     Note: To find current binding through all minor-mode maps: (key-binding key) ; lookup-key et al require a map
;;;     Note: Eval for active keymaps: (mapcar (lambda(x)(car(rassq x minor-mode-map-alist)))(current-minor-mode-maps))
;;;     Note: prefix arg defaults to 4, and it *multiplies* on each repeat
;;; ###########################################################################

;; ============================================================================
;; --- CUA mode keys

(define-key cua--rectangle-keymap    (kbd "C-.") 'cua-rotate-rectangle) ; cycle rectangle corners
                                        ; default (RETURN, \r) doesn't work

;; ============================================================================
;; --- Minibuffer keys
;;     Note: lisp expression minibuffer keymap read-expression-map inherits from minibuffer-local-map

(define-key minibuffer-local-map     [tab]     'my-hippie-smart-tab) ; Default minibuffer keymap incl isearch-mode-map
(define-key minibuffer-local-map     [backtab] 'my-hippie-unexpand)

;; insert from point into minibuf, e.g. in occur, query-replace;
;;  will also be available in read-expression-map for the `eval-expression` minibuffer
;;  C-v already works here; C-g C-z / redo will go back to previous match

(define-key minibuffer-local-map     "\C-cb"          'my-minibuffer-insert-buffer-name)
(define-key minibuffer-local-map     "\C-cf"          'minibuffer-insert-file-name-at-point)
(when (fboundp                                        'my-minibuffer-insert-symbol-at-point)
  (define-key minibuffer-local-map   "\C-cs"          'my-minibuffer-insert-symbol-at-point))
(when (fboundp                                        'my-minibuffer-insert-word-at-point)
  (define-key minibuffer-local-map   "\C-cw"          'my-minibuffer-insert-word-at-point)
  (define-key minibuffer-local-map   "\C-w"           'my-minibuffer-insert-word-at-point)) ; support isearch-like C-w

;; history completion in minibuffer
(define-key minibuffer-local-map     (kbd "M-p")      'previous-complete-history-element)
(define-key minibuffer-local-map     (kbd "M-n")      'next-complete-history-element)
(define-key minibuffer-local-map     (kbd "<up>")     'previous-complete-history-element)
(define-key minibuffer-local-map     (kbd "<down>")   'next-complete-history-element)

;; ============================================================================
;; --- search mode minibuffer keys

(define-key isearch-mode-map         "\C-c"           (make-sparse-keymap))
(define-key isearch-mode-map         "\C-cb"          'my-isearch-insert-buffer-name)
(define-key isearch-mode-map         "\C-cf"          'my-isearch-insert-file-name-at-point) ; only inserts, if filename at point
(when (fboundp                                        'my-isearch-insert-symbol-at-point)
  (define-key isearch-mode-map       "\C-cs"          'my-isearch-insert-symbol-at-point))
(when (fboundp                                        'my-isearch-insert-word-at-point)
  (define-key isearch-mode-map       "\C-cw"          'my-isearch-insert-word-at-point))

(define-key isearch-mode-map         "\C-k"           (make-sparse-keymap))
(define-key isearch-mode-map         "\C-kl"          'my-isearch-kill-line) ; kill line w/ match and continue search
(define-key isearch-mode-map         "\C-km"          'my-isearch-kill-match) ; kill search match and continue search
(define-key isearch-mode-map         "\C-kz"          'my-isearch-zap-up-to)  ; kill from mark to *before* match

(define-key isearch-mode-map         [(control return)] 'my-isearch-exit-before-match) ; RETURN quits search AFTER match
(define-key isearch-mode-map         (kbd  "C-#")     'my-isearch-comment-line) ; comment line w/ match and continue search
(define-key isearch-mode-map         (kbd  "C-o")     'my-isearch-occur) ; run occur for current search term; already M-S-o
(define-key isearch-mode-map         (kbd  "C-v")     'isearch-yank-pop) ; make C-v also work in search mode, already M-y
;;                                         "M-o"       facemenu-set-face
(define-key isearch-mode-map         (kbd  "M-z")     'my-isearch-zap-up-to) ; see "\C-kz"

;; Important search-mode keys (help on that with ‘isearch-mode-help’ (‘C-h m’ during a search))
;;
;; (Isearch) RET        Exit, leaving point at location found.
;; (Isearch) C-j        Match end of line.
;; (Isearch) C-g        Abort the search, putting back the cursor to its initial position.
;; (Isearch) C-q        Quote next char.
;; (Isearch) C-r        Revert search direction; also works w/ isearch-forward-word.
;; (Isearch) C-s        Repeat the search as many times as you want throughout the buffer.
;; (Isearch) C-w        Select (rest of) word at point as the search string; Repeat to add more words to search string.
;; (Isearch) C-M-y      Appends the character after point to the search string.
;; (Isearch) C-M-w      Delete character from end of search string.
;; (Isearch) C-y        Select text up to the end of the line as the search string.
;; (Isearch) M-y / C-v  Yank (paste) text last copied to the kill-buffer (clipboard) to end of the search string.
;; (Isearch) M-n, M-p, up, down   Re-use a previous search string. Repeat to choose older strings.
;; (Isearch) M-TAB      Complete the current search string against all previous search strings. (isearch-complete)
;; (Isearch) M-r        Toggle between regular-expression searching and literal-string searching.
;; (Isearch) M-e        Pause to edit the search string. Resume search with C-j or C-s or C-r.
;; (Isearch) M-s SPC    (24.3) Toggles lax space matching for ordinary and regexp Isearch (replace-lax-whitespace).
;; (Isearch) M-s _      Toggles symbol search mode.
;; (Isearch) M-c / M-s c  Toggles search case-sensitivity.
;; (Isearch) M-s w      Toggle word search mode (isearch-toggle-word).
;; (Isearch) M-s h r    isearch-highlight-regexp; exit isearch and highlight regexp from current search string
;; (Global) M-s h u     unhighlight-regexp; Unhighlight regular expression (see also lazy-highlight-cleanup)
;; (Global) C-c *       isearch-forward-word
;; (Global) M-s _       Starts a symbol (identifier) incremental search (isearch-forward-symbol).
;; (Global/Isearch) M-% Start interactive query replace using the current string. (query-replace)
;; (Global/Isearch) M-s o  Call occur with current search string
;; (Global) C-u M-%     query-replace for word (respecting word boundaries)
;; (Global) C-M-%       Start reg-exp query replace using the current search string (query-replace-regexp)
;; (smex)               C-h f, while Smex is active, runs describe-function on the currently selected command.
;; (smex)               M-. jumps to the definition of the selected command.
;; (smex)               C-h w shows the key bindings for the selected command. (Via where-is.)

;; ============================================================================
;; --- Special keys
;;     <pause> also available

(if smex-initialized-p
    (progn (my-define-key [apps]     'smex) ; C-s/left / C-r/right scroll; execute-extended-command still available as M-x
           (my-define-key [M-apps]   'smex-major-mode-commands))
  (my-define-key          [apps]     'execute-extended-command)) ; M-x alternative
(my-define-key            [S-apps]   'eval-expression)           ; M-: alternative
(my-define-key            [C-apps]   'my-ido-goto-symbol-definition)
(my-define-key            [C-S-apps] 'imenu)

;; ============================================================================
;; --- C-, ? -> 2-key commands, mapped to super-modifier; mark region & multi-cursor edit

(when t
  (define-key function-key-map       (kbd "C-,")      'event-apply-super-modifier)

  (my-define-key                     (kbd "s-b")      'mark-whole-buffer) ; also C-c a
  (my-define-key                     (kbd "s-f")      'mark-defun)        ; overriden below in c-mode, also C-M-h
  (my-define-key                     (kbd "s-g")      'mark-page)         ; also C-x C-p
  (my-define-key                     (kbd "s-p")      'mark-paragraph)    ; also M-h
  (my-define-key                     (kbd "s-w")      'mark-word) ; mark word starting at point; also M-@
  (my-define-key                     (kbd "s-x")      'mark-sexp) ; also C-M-SPC
  (my-define-key                     (kbd "s-.")      'mark-end-of-sentence)

  (when (fboundp 'er/expand-region)
    ;; mark functions from er/expand; see https://github.com/magnars/expand-region.el
    (my-define-key                   (kbd "s-#")      'er/mark-comment)
    (my-define-key                   (kbd "s-c")      'er/mark-method-call)
    (my-define-key                   (kbd "s-s")      'er/mark-symbol)
    (my-define-key                   (kbd "s-w")      'er/mark-word)        ; mark word around point
    ;; TODO: er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs
    (my-define-key                   (kbd "s-+")      'er/expand-region)    ; +/-/0 - expand/contract/reset
    (my-define-key                   (kbd "s--")      'er/contract-region)) ; +/-/0 - expand/contract/reset

  ;; C-, XXX - multi cursor keys; defalias for multiple-cursors-mode below
  (when (fboundp 'mc/edit-lines)
    (my-define-key                   (kbd "s-<return>")   'set-rectangular-region-anchor) ; Add cursors to multiple lines
    (my-define-key                   (kbd "s-C-<return>") 'mc/edit-lines)                 ; Add cursors to each line of region
    (my-define-key                   (kbd "s-<down>") 'mc/mark-next-like-this)            ; useful w/ er/expand-region
    (my-define-key                   (kbd "s-<up>")   'mc/mark-previous-like-this)
    (my-define-key                   (kbd "s-,")      'mc/mark-all-like-this) ; Marks all parts of the buffer that matches the current region
    (my-define-key                   (kbd "s-;")      'mc/mark-all-like-this-dwim) ; Tries to be smart about marking everything you want. Can be pressed multiple times
    ))

;; --- C-< ? -> 2-key commands, mapped to hyper-modifier
;;     H-C-? can be entered by pressing C-< and ? together, so that H-C-? can be typed easier than H-?;
;;     but other than C-? or M-? it won't repeat
;; (define-key function-key-map      (kbd "C-<")      'event-apply-hyper-modifier) ; F11 and umlaut would work too

;; ============================================================================
;; --- C-. ? -> do things around point

(when t
  (defvar my-ctrl-point-map (make-keymap)
    "Keymap for local bindings and functions, prefixed by (C-.)")

  (define-key global-map             (kbd "C-.")      my-ctrl-point-map) ; C-. prefix

  ;; C-SPC                           toogle mark and start region
  ;;   C-u C-SPC, C-- C-SPC          goto last mark and pop it
  ;;   C-SPC C-SPC                   set the mark but disable the region (it's a toggle, so use twice to set and continue)
  (define-key my-ctrl-point-map      (kbd "SPC")      'exchange-point-and-mark) ; standard C-x C-x overriden
  ;; C-S-SPC                         toggle global mark; w/ global mark, copy/yank will be inserted at global mark
  (define-key my-ctrl-point-map      (kbd "S-SPC")    'pop-global-mark) ; goto global mark and pop it; also C-x C-SPC
  ;; pop-to-mark-command             M-x ... RET Jump to mark, and pop a new position for mark off the ring.
  ;; push-mark-command               M-x ... RET Set mark at where point is.
  ;; push-mark-no-activate           M-x ... RET Pushes `point' to `mark-ring' and does not activate the region

  (define-key my-ctrl-point-map      (kbd "=")        'what-cursor-position)    ; also C-x =
  (define-key my-ctrl-point-map      (kbd "~")        'my-toggle-case)          ; C-c . -> repeat
  (define-key my-ctrl-point-map      (kbd "$")        'my-what-face)
  (define-key my-ctrl-point-map      (kbd "?")        'describe-char)           ; also C-u C-x =, more details
  (define-key my-ctrl-point-map      (kbd "<")        'my-toggle-selective-display)
  (define-key my-ctrl-point-map      (kbd "!")        'set-goal-column)
  (define-key my-ctrl-point-map      (kbd "<f1>")     'info-lookup-symbol)
  ;; thing-at-point-url-at-point; (thing-at-point 'filename)

  (when (fboundp 'evil-numbers/inc-at-pt)
    (define-key my-ctrl-point-map    "+"              'evil-numbers/inc-at-pt)
    (define-key my-ctrl-point-map    "-"              'evil-numbers/dec-at-pt))

  ;; C-. a adds word at point to abbrev
  (when abbrev-file-name
    (define-key my-ctrl-point-map    (kbd "a")        'add-global-abbrev)) ; also C-x a g

  ;; C-. b sets bookmark; C-. C-b asks for bookmark and jumps there; C-. C-M-b lists bookmarks
  (when bookmark-default-file
    (define-key my-ctrl-point-map    (kbd "b")        'bookmark-set) ; also C-x r m
    (define-key my-ctrl-point-map    (kbd "C-b")      'bookmark-jump) ; also C-x r b
    (define-key my-ctrl-point-map    (kbd "C-S-b")    'bookmark-delete)
    (define-key my-ctrl-point-map    (kbd "C-M-b")    'bookmark-bmenu-list)) ; also C-x r l

  (define-key my-ctrl-point-map      (kbd "d")        'my-ido-goto-symbol-definition)
  (define-key my-ctrl-point-map      (kbd "e")        'my-insert-next-line-char)
  (define-key my-ctrl-point-map      (kbd "g")        'my-google-at-point)
  (define-key my-ctrl-point-map      (kbd "n")        'my-toggle-narrow)
  (define-key my-ctrl-point-map      (kbd "C-n")      'my-toggle-narrow-indirect) ; TODO: does this need a kill-buffer?
  (define-key my-ctrl-point-map      (kbd "u")        'browse-url-at-point)
  (define-key my-ctrl-point-map      (kbd "y")        'my-insert-previous-line-char)

  ;; C-. 1/2/3 stores point in register 1/2/3, C-. C-1/2/3 jumps to point in register 1/2/3
  (define-key my-ctrl-point-map      (kbd "1")        (my-defun-interactive (point-to-register 1))) ; C-x r SPC
  (define-key my-ctrl-point-map      (kbd "C-1")      (my-defun-interactive (jump-to-register 1))) ; C-x r j
  (define-key my-ctrl-point-map      (kbd "2")        (my-defun-interactive (point-to-register 2)))
  (define-key my-ctrl-point-map      (kbd "C-2")      (my-defun-interactive (jump-to-register 2)))
  (define-key my-ctrl-point-map      (kbd "3")        (my-defun-interactive (point-to-register 3)))
  (define-key my-ctrl-point-map      (kbd "C-3")      (my-defun-interactive (jump-to-register 3)))

  ;; C-. . pushes point to stack, C-. C-. pops point from stack and moves to it
  (define-key my-ctrl-point-map      (kbd ".")   'my-push-point)
  (define-key my-ctrl-point-map      (kbd "C-.") 'my-pop-point)

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map    "#" map)                              ; # CPP if/else/endif movement
    (define-key map                  "b"   'backward-ifdef)
    (define-key map                  "f"   'forward-ifdef)
    (define-key map                  "p"   'previous-ifdef)
    (define-key map                  "n"   'next-ifdef)
    (define-key map                  "u"   'up-ifdef)
    (define-key map                  "d"   'down-ifdef)
    (define-key map                  "-"   'hide-ifdef-block)
    (define-key map                  "+"   'show-ifdef-block))

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map "i" map)                              ; i insert
    (my-define-key-interactive map "b"                                  ; i b buffer file name w/o extension
                               (let* ((fn  (file-name-nondirectory buffer-file-name))
                                      (fns (file-name-sans-extension fn)))
                                 (if (= 0 (length fns)) ;take care of e.g. ".emacs"
                                     (insert fn)
                                   (insert fns))))
    (my-define-key-interactive map "d"                                  ; i d date
                               (insert (format-time-string "%02d-%b-%02y")))
    (my-define-key-interactive map "f"                                  ; i f buffer file name w/ extension
                               (insert (file-name-nondirectory buffer-file-name)))
    (my-define-key-interactive map "g"                                  ; i g header include guard
                               (let ((ident (upcase (concat ; same as yasnippet C/C++ header guard
                                                     (file-name-nondirectory
                                                      (file-name-sans-extension buffer-file-name))
                                                     "_"
                                                     (file-name-extension buffer-file-name)))))
                                 (insert ident)))
    (my-define-key-interactive map "h"                                  ; i h hostname
                               (insert (or (getenv "HOSTNAME") (getenv "COMPUTERNAME") "unknown")))
    (my-define-key-interactive map "t"                                  ; i t date/time
                               (insert (format-time-string "%02d-%b-%02y %H:%M")))
    (my-define-key-interactive map "u"                                  ; i u user name
                               (insert (or user-full-name (getenv "USERNAME") (getenv "USER")))))

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map "f" map)                              ; f find file; TODO: ffap-alist ; find-file-in-project
    (define-key map "d" 'find-function-at-point)                        ; f d - open file for elisp function at point
    (define-key map "f" 'find-file-at-point)                            ; f f - find file at point (ffap is an alias)
    (define-key map "F" 'ffap-other-window)                             ; f F
    (define-key map "d" 'diff-buffer-with-file)                         ; f d - compare buffer w/ associated file
    (define-key map "l" 'find-file-literally)                           ; f l - open file in unibyte
    (define-key map "r" 'ido-find-file-read-only)
    (define-key map "R" 'find-file-read-only-other-window)
    (define-key map "v" 'view-file)
    (define-key map "V" 'view-file-other-window))

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map "s" map)                              ; s search
    (when (fboundp 'thing-at-point)

      (defmacro my-define-isearch-string (key symbol)
        `(my-define-key-interactive map ,key                            ; continue w/ C-c n / C-c C-n
                                    (let ((thing (thing-at-point ,symbol)))
                                      (setq isearch-string thing) (search-forward thing))))
      (define-key map           "c" 'lazy-highlight-cleanup)
      (my-define-isearch-string "s" 'symbol)                            ; s s - search symbol
      (my-define-isearch-string "w" 'word)))                            ; s w - search word

  (let ((map (make-sparse-keymap)))                                     ; q query
    (define-key my-ctrl-point-map "q" map))

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map "o" map)                              ; o occur, current buffer
    (when (fboundp 'thing-at-point)
      (defmacro my-define-occur (key thing)
        `(my-define-key-interactive map ,key
                                    (occur (thing-at-point ,thing))))
      (my-define-occur "d" 'defun)                                      ; o d - occur for current defun
      (my-define-occur "s" 'symbol)                                     ; o d - occur for current symbol
      (my-define-occur "w" 'word)                                       ; o d - occur for current word
      (my-define-occur "x" 'sexp)))                                     ; o d - occur for current sexp

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map "m" map)                              ; m multi-occur, all open buffers
    (when (fboundp 'thing-at-point)
      (defmacro my-define-multi-occur (key thing)
        `(my-define-key-interactive map ,key
                                    (multi-occur-in-matching-buffers "." (thing-at-point ,thing))))
      (my-define-multi-occur "d" 'defun)                                ; m d - multi occur for current defun
      (my-define-multi-occur "s" 'symbol)                               ; m d - multi occur for current symbol
      (my-define-multi-occur "w" 'word)                                 ; m d - multi occur for current word
      (my-define-multi-occur "x" 'sexp)))                               ; m d - multi occur for current sexp

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map "t" map)                              ; t tags
    ;; TODO: !!!!!!
    ;; M-. / C-u M-. / M-- M-.        find-tag / next tag match / back to previous match
    ;;                                `find-tags' search only for definitions of tags that match your substring or regexp.
    ;; M-,                            tags-loop-continue last M-x tags-search or M-x tags-query-replace command
    ;; M-*                            pop-tag-mark - jump back to where M-. was last invoked
    ;; M-x tags-query-replace         Perform a query-replace-regexp on each file in the selected tags table
    ;; M-x tags-search                Search through all files listed in tags table for match for REGEXP
    ;;                                `tags-search' and `tags-query-replace' find every occurrence of regexp current buffer.
    ))

;; ============================================================================
;; --- TAB and C-ö ? - indentation and completion keys
;;     Note: Emacs has two tab’s (kbd TAB)/\t and (kbd <tab>)\[tab], where prior is bound by Emacs
;;       to indent-for-tab-command and latter has prio.
;;       For win32, both UI and console, TAB key will be resolved to <tab>
;;     Note: use C-q TAB to insert a literal tab, no matter what key binding is set
;;     This explicitly creates a separate binding for <tab>, so it doesn't conflict with C-i:
;;      (global-set-key [tab] (or (key-binding [tab]) (key-binding "\C-i")))

(my-define-key [tab]                 'my-hippie-smart-tab) ; replacement for dabbrev-expand
;; yas-snippet will override tab and call current binding my-hippie-smart-tab only if no snippet expansion found.
;; my-hippie-smart-tab calls hippie-expand, indent-region, indent-for-tab-command (the default TAB command).
(my-define-key [backtab]             'my-hippie-unexpand)
;;              C-tab                yas-expand

(when t
  (my-define-key    (kbd "C-ö C-i")  'indent-for-tab-command) ; Indent current line or region, or insert tab; default for C-i
  (my-define-key    (kbd "C-ö a")    'back-to-indentation)    ; similar to C-a
  (my-define-key    (kbd "C-ö C-c")  'indent-to-column) ; Indent from point with tabs and spaces until column is reached.
  (my-define-key    (kbd "C-ö C-e")  'edit-tab-stops)
  (my-define-key    (kbd "C-ö p")    'indent-relative) ; Indent point relative to previous nonblank line or do a tab-to-tab-stop
  (my-define-key    (kbd "C-ö C-p")  'indent-relative-maybe) ; Indent point relative to previous nonblank line
  (my-define-key    (kbd "C-ö r")    'my-indent-region-or-buffer) ; instead of indent-region
  (my-define-key    (kbd "C-ö t")    'tab-to-tab-stop) ; also M-i; insert spaces or tabs to next column in tab-stop-list
  (my-define-key    (kbd "C-ö C-t")  'move-to-tab-stop) ; Move point to next tab-stop, only inserting if required
  (my-define-key    (kbd "C-ö y")    'indent-rigidly)   ; also C-x TAB, C-x C-i; indent region, unindent w/ C-u negative
  (my-define-key    (kbd "C-ö C-y")  'indent-code-rigidly)) ; does not affect lines starting inside comments or strings

;; ============================================================================
;; --- C-d ? - delete keys, delete w/o kill-ring

(when t ;; TODO: c-mode keys: c-electric-delete-forward, c-electric-backspace ...
  (my-define-key    (kbd "C-d C-d")  'delete-char) ; default for C-d
  (my-define-key    (kbd "C-d SPC")  'just-one-space) ; also M-SPC
  (my-define-key    (kbd "C-d a")    'my-delete-to-beginning-of-line)
  (my-define-key    (kbd "C-d b")    'delete-blank-lines) ; also C-x C-o; delete blank lines below current line
  (my-define-key    (kbd "C-d c")    'delete-char)
  (my-define-key    (kbd "C-d C-c")  (my-defun-interactive (backward-delete-char-untabify 1))) ; also works w/ autopair-backspace
  (my-define-key    (kbd "C-d e")    'my-delete-to-end-of-line)                                ; delete to EOL
  (my-define-key    (kbd "C-d j")    'my-join-lines-or-region) ; join this line and next or join all lines in region
  (my-define-key    (kbd "C-d C-j")  'delete-indentation) ; join this line to previous and fix up whitespace at join
  (my-define-key    (kbd "C-d m")    'delete-matching-lines) ; delete lines matching REGEXP; aka flush-lines
  (my-define-key    (kbd "C-d r")    'delete-region)
  (my-define-key    (kbd "C-d s")    'delete-horizontal-space) ; join two words by deleting all whitespace around point
  (my-define-key    (kbd "C-d C-s")  (my-defun-interactive     ; kill whitespace after point
                                      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))
  (my-define-key    (kbd "C-d w")    'my-delete-word)
  (my-define-key    (kbd "C-d C-w")  'my-backward-delete-word))

;; ============================================================================
;; --- C-k ? - kill keys, delete to kill-ring

(when t
  (my-define-key   (kbd "C-k +")     'append-next-kill) ; default is C-M-w, which is changed here
  (my-define-key   (kbd "C-k C-k")   'kill-whole-line)
  (my-define-key   (kbd "C-k a")     (my-defun-interactive (kill-line 0))) ; kill to BOL
  (my-define-key   (kbd "C-k c")     'ffap-copy-string-as-kill)
  (my-define-key   (kbd "C-k e")     'kill-line)       ; kill to EOL; default for C-k
  (my-define-key   (kbd "C-k l")     'kill-whole-line) ; kills whole line including its newline, regardless of position
  (my-define-key   (kbd "C-k n")     (my-defun-interactive (save-excursion (move-end-of-line nil) (kill-line 2))))
                                        ; kills next line w/o changing cursor
  (my-define-key   (kbd "C-k p")     (my-defun-interactive (save-excursion (move-beginning-of-line nil) (kill-line -1))))
                                        ; kills previous line w/o changing cursor
  (my-define-key   (kbd "C-k r")     'kill-region)   ; advice to kill line, if no region is active
  (my-define-key   (kbd "C-k w")     'kill-word) ; also M-d
  (my-define-key   (kbd "C-k C-w")   'backward-kill-word)
  (my-define-key   (kbd "C-k x")     'kill-sexp) ; kill sexp forward, kill "foo" in any mode, kill tag in nXML; also C-M-k
  (my-define-key   (kbd "C-k C-x")   (my-defun-interactive (kill-sexp -1))) ; kill sexp backward
  (my-define-key   (kbd "C-k y")     'my-kill-word-and-space)
  (my-define-key   (kbd "C-k C-z")   'zap-to-char) ; zap including char; default M-z, changed in my init.el;
                                        ; C-- prefix zaps backward; C-u n M-z : zap to n-th occurence
  (my-define-key   (kbd "C-k z")     'zap-up-to-char) ; zap excluding char
  (my-define-key   (kbd "C-k .")     'kill-sentence)) ; also zap-to-char '.'
(my-define-key     (kbd "C-y")       'my-kill-word-and-space)

;; ============================================================================
;; --- C-t ? - transpose/swap/move keys

(when t
  (my-define-key   (kbd "C-t l")     'transpose-lines) ; transpose w/ line above, also C-x C-t
  (my-define-key   (kbd "C-t C-l")   'my-swap-line-down) ; transpose w/ line below
  (my-define-key   (kbd "C-t p")     'transpose-paragraphs)
  (my-define-key   (kbd "C-t r")     'my-move-region-or-line-up)
  (my-define-key   (kbd "C-t C-r")   'my-move-region-or-line-down)
  (my-define-key   (kbd "C-t C-t")   'transpose-chars) ; default for C-t
  (my-define-key   (kbd "C-t x")     'transpose-sexps)
  (my-define-key   (kbd "C-t w")     'transpose-words)  ; also M-t
  (my-define-key   (kbd "C-t .")     'transpose-sentences))

;; ============================================================================
;; --- Meta / Control / Standard key combinations

(my-define-key [remap downcase-word] 'my-downcase-region-or-word) ; M-u, will suppress M-- M-u
(my-define-key [remap upcase-word]   'my-upcase-region-or-word)   ; M-l, will suppress M-- M-l

(my-define-key (kbd "C-%")           'my-match-paren)
(my-define-key (kbd "C-#")           'comment-dwim)     ; also M-; C-u M-; to kill comment (comment-kill)
(my-define-key (kbd "C-M-#")         'comment-region)   ; C-u comment-region to uncomment
(my-define-key (kbd "C-M-'")         'uncomment-region) ; C-M-S-#

(my-define-key (kbd "C-j")           (my-defun-interactive (join-line 1))) ; join this and next line
(my-define-key (kbd "C-S-j")         (my-defun-interactive (join-line))) ; join previous and this line
(my-define-key (kbd "C-M-j")         'my-join-lines-or-region)
(my-define-key [M-j]                 'indent-new-comment-line) ; Break line at point and indent, continuing comment

(my-define-key (kbd "C-o")           (lambda (&optional p) (interactive "P") ; like Vim o
                                       (if p (my-insert-next-line 1) (my-open-next-line 1))))
(my-define-key (kbd "C-S-o")         (lambda (&optional p) (interactive "P") ; like Vim O
                                       (if p (my-insert-previous-line 1) (my-open-previous-line 1))))
;;                   C-M-o           split-line; split current line at point and indent new line to point's column
(my-define-key (kbd "C-x C-r")       'my-rename-current-buffer-file)
;;                   C-x z           repeat most recently executed command like vi-., (repeat)
;;                   C-x +           balance windows
(my-define-key [remap list-buffers]  (my-defun-interactive (ibuffer t))) ; "\C-x\C-b"
;;                   \C-x\C-c        save-buffers-kill-emacs (exit, quit)
;;                   \C-x\C-d        list-directory
;;                   \C-x\C-e        eval-last-sexp
;;                   \C-x\C-f        set-fill-column to current column; or C-u col C-x C-f to specify column
(my-define-key      "\C-x\C-k"       'my-delete-current-buffer-file)
;;                   \C-x\C-m        compile; see programming mode hooks
;;                   \C-x\C-n        set-goal-column; for vertical editing (counting starts at 0); turn of w/ C-u C-x C-n
(my-define-key      "\C-x\C-x"       'kill-region) ; standard kill region or line also for CUA
(when window-system
  (global-unset-key "\C-x\C-z"))     ; suspend-frame aka iconify-or-deiconify-frame

(my-define-key      "\C-c\C-a"       'copy-from-above-command) ; copy the rest of previous line forward
(my-define-key      "\C-c\C-b"       'browse-url-of-file) ; open current file im browser
(my-define-key      "\C-c\C-c"       'kill-ring-save) ; standard copy region or line also for CUA; supports C-k +
(my-define-key      "\C-c\C-d"       'my-duplicate-current-line)
(my-define-key      "\C-c\C-j"       'webjump)            ; webjump-sites
;;                   \C-c\C-m        TODO: helpful, allows C-c C-m confirm/edit mininbuffer prompt C-m (= RET)
(my-define-key      "\C-c\C-o"       'overwrite-mode)
(my-define-key      "\C-c\C-p"       'my-copy-from-above-to-char) ; copy initial part of previous line
(my-define-key      "\C-c\C-r"       'toggle-read-only)

(my-define-key      "\C-ca"          'mark-whole-buffer) ; C-a already used; also C-x h
(when (fboundp                       'jump-char-forward) ; Note: other than w/ Vi, keys below not bound to current line
  (my-define-key    (kbd "C-c f")    'jump-char-forward) ; binds , and ; like Vim
  (my-define-key    (kbd "C-c C-f")  'jump-char-backward)) ; binds , and ; like Vim
(my-define-key      (kbd "C-c g")    'goto-line) ; C-g already used; also M-g M-g; C-u C-c g goes to line in LAST buffer
(my-define-key      (kbd "C-c C-g")  'move-to-column) ; also M-g TAB
(my-define-key      (kbd "C-c C-S-g") 'goto-char) ; Set point to position, a number or marker; (24.3) also M-g c
(my-define-key      (kbd "C-c j")    'ace-jump-mode) ; C-c j: jump-word, C-u C-c j jump-char, C-u C-u C-c j jump-line
(my-define-key      (kbd "C-c C-j")  'ace-jump-mode-pop-mark)
(my-define-key      (kbd "C-c l")    'goto-last-change) ; C-u C-c l will set mark before move

(my-define-key      (kbd "C-c ~")    'my-toggle-case)
(my-define-key      (kbd "C-c .")    'repeat)                  ; also C-x z
(my-define-key      (kbd "C-c C-.")  'repeat-complex-command)  ; repeat last minibuffer command; C-x M-: alternative
(my-define-key      (kbd "C-c *")    'my-isearch-yank-symbol)
;;                        M-s w       isearch-forward-word     ; search whole word forward
(my-define-key      (kbd "C-c n")    'isearch-repeat-forward)  ; search forward for isearch-string
(my-define-key      (kbd "C-c C-n")  'isearch-repeat-backward) ; search backward for isearch-string

;; ============================================================================
;; --- Buffer movement current and other window

(my-define-key [remap move-beginning-of-line] 'my-beginning-of-line-dynamic) ; [(home)], "\C-a"

(my-define-key [C-up]                (my-defun-interactive (scroll-up-line)))   ; overrides backward-paragraph
(my-define-key [C-down]              (my-defun-interactive (scroll-down-line))) ; overrides forward-paragraph
(my-define-key [C-M-up]              'backward-paragraph) ; overrides backward-up-list, still C-M-u
(my-define-key [C-M-down]            'forward-paragraph)  ; overrides down-list still C-M-d

(my-define-key (kbd "<prior>")       (my-defun-interactive (scroll-down-command (my-window-half-height))))
(my-define-key (kbd "<next>")        (my-defun-interactive (scroll-up-command (my-window-half-height))))

(my-define-key (kbd "<C-home>")      (my-defun-interactive (goto-char(point-min))))
(my-define-key (kbd "<C-end>")       (my-defun-interactive (goto-char(point-max))))

;; C-l                               cycle cursor vertical center / top / bottom
;; C-left / C-right                  right-word, left-word

(when t
  (my-define-key (kbd "M-C-S-b") 'beginning-of-buffer) ; also C-home
  (my-define-key (kbd "M-C-b")   'end-of-buffer) ; also C-end

  (my-define-key (kbd "M-C-S-f") 'beginning-of-defun) ; overriden below in C-mode; also C-M-a
  (my-define-key (kbd "M-C-f")   'end-of-defun) ; overriden below in C-mode; also C-M-e

  (my-define-key (kbd "M-C-u")   'my-up-list) ; forward (backward w/ neg arg) out of one level of parens; also C-M-u
  (my-define-key (kbd "M-C-S-u") 'my-backward-up-list)  ; backward (forward w/ neg arg) down one level of parens; also C-M-d
  (my-define-key (kbd "M-C-d")   'down-list)  ; forward (backward w/ neg arg) down one level of parens; also C-M-d
  (my-define-key (kbd "M-C-S-l") 'backward-list) ; backward across one balanced group of parens; also C-M-p
  (my-define-key (kbd "M-C-l")   'forward-list) ; forward across one balanced group of parens; also C-M-n

  (my-define-key (kbd "M-C-S-.") 'backward-sentence) ; overriden below in C-mode; also M-a
  (my-define-key (kbd "M-C-.")   'forward-sentence) ; overriden below in C-mode; also M-e

  ;; (my-define-key (kbd "M-C-S-x") 'backward-sexp) ; also C-M-left
  ;; (my-define-key (kbd "M-C-x")   'forward-sexp) ; also C-M-right

  ;; (my-define-key (kbd "M-C-t") 'move-to-tab-stop) ; Move point to next tab-stop, only inserting if required

  (my-define-key (kbd "M-C-S-p") 'backward-paragraph) ; also C-M-up, see above
  (my-define-key (kbd "M-C-p")   'forward-paragraph)) ; also C-M-down, see above

;; Buffer movement other window, not symmetric w/ movement for current buffer, but more reasonable

;; M-home, M-end                     buffer begin / end other window of other buffer
(my-define-key [M-up]                (my-defun-interactive (scroll-other-window 1)))
(my-define-key [M-down]              (my-defun-interactive (scroll-other-window-down 1)))

(my-define-key (kbd "M-<prior>")     (my-defun-interactive (scroll-other-window (my-window-half-height))))
(my-define-key (kbd "M-<next>")      (my-defun-interactive (scroll-other-window-down (my-window-half-height))))

(when t                                 ; M-o ? - do things in other window (overrides unused face setting)
  (defvar my-alt-o-map (make-keymap)
    "Keymap for local bindings and functions, prefixed by (M-o)")

  (define-key global-map             (kbd "M-o")      my-alt-o-map) ; M-o prefix
  (define-key my-alt-o-map           (kbd "f")        'ffap-other-window)
  (define-key my-alt-o-map           (kbd "l")        'my-center-other-window) ; vertically cycle cursor in other window
  (define-key my-alt-o-map           (kbd "s")        'my-isearch-other-window)
  (define-key my-alt-o-map           (kbd "C-x d")    (my-defun-interactive (dired-other-window default-directory))))

(my-define-key (kbd "<M-left>")      'winner-undo) ; default same as C-left, so override
(my-define-key (kbd "<M-right>")     'winner-redo) ; default same as C-right, so override

;; ============================================================================
;; --- Functions keys
;;     Note: C-M-Fx does not work for some F-keys

(my-define-key [f2]                  (my-defun-interactive (save-buffer)))
(my-define-key [S-f2]                'ido-write-file) ; file name is selected interactively by typing a substring; also C-x C-w
(my-define-key [C-S-f2]              'ido-insert-file)
(my-define-key [M-f2]                'save-some-buffers)
(my-define-key [C-f2]                (my-defun-interactive (save-buffer) (kill-buffer (buffer-name)) ))

(my-define-key [f3]                  'ido-find-file)
(my-define-key [M-f3]                'ido-find-file-other-window) ; Switch and show file in another window
(my-define-key [M-S-f3]              'ido-find-alternate-file) ; Switch to alternate file and show it in another window.
;;             f3                    my-ido-choose-from-recentf
(my-define-key [C-S-f3]              'recentf-open-files)
(my-define-key [S-f3]                'revert-buffer)

(my-define-key [f4]                  (my-safe-interactive (next-error))) ; C-u F4: reparse error buffer and go to 1st error
(my-define-key [S-f4]                (my-safe-interactive (previous-error)))
(my-define-key [C-f4]                'iconify-or-deiconify-frame)
(my-define-key [C-M-f4]              'delete-other-frames) ; delete all frames except the selected one
(my-define-key [M-f4]                'delete-frame)

(my-define-key [f5]                  'other-window)
(my-define-key [M-f5]                'switch-window)
(my-define-key [S-f5]                'my-other-window-backward)
(my-define-key [C-f5]                'delete-window) ; also C-x 0
(my-define-key [C-S-f5]              'my-kill-buffer-and-window) ; kill-buffer-and-window
(my-define-key [C-M-f5]              'my-close-and-kill-next-window) ; C-x 1 : delete-other-windows
(my-define-key [C-M-S-f5]            'my-kill-other-buffer-and-window)

(my-define-key [f6]                  'next-buffer)
(my-define-key [S-f6]                'previous-buffer)
(my-define-key [C-f6]                'my-kill-current-buffer) ; kill w/o asking
(my-define-key [M-f6]                'my-next-buffer-other-window)
(my-define-key [M-S-f6]              'my-previous-buffer-other-window)
(my-define-key [M-C-f6]              'my-kill-other-buffer) ; kill w/o asking
(my-define-key [M-C-S-f6]            'my-kill-other-buffers-and-windows)
;; TODO: support 2-column mode: 2C-two-columns, 2C-split, 2C-associate-buffer, 2C-dissociate

(my-define-key [f7]                  'ido-switch-buffer)
(my-define-key [C-f7]                'ido-kill-buffer) ; kill w/ asking
(my-define-key [C-M-f7]              'my-delete-current-buffer-file)
(my-define-key [M-f7]                (my-defun-interactive (ido-switch-buffer-other-window) (other-window -1)))
(my-define-key [S-f7]                'my-toggle-window-split)
(my-define-key [C-S-f7]              'my-toggle-window-dedicated)

(my-define-key [f8]                  'bury-buffer)
(my-define-key [S-f8]                'unbury-buffer)
(my-define-key [C-f8]                'rotate-frame-clockwise)
(my-define-key [C-S-f8]              'rotate-frame-anticlockwise)
(my-define-key [M-f8]                'flip-frame) ; Flip vertically
(my-define-key [M-S-f8]              'flop-frame) ; Flop horizontally2

(global-unset-key [f10])             ; menu-bar-open
(my-define-key [S-f10]               (my-defun-interactive (find-file user-init-file)))
(my-define-key [C-f10]               (my-defun-interactive (load-file user-init-file)))

(when (fboundp 'outline-cycle)
  (my-define-key [f11]               (my-defun-interactive (when outline-minor-mode outline-cycle))))

;; ============================================================================
;; --- Defalias - shortcuts

(when my-use-shortcut-alias
  (defalias 'alc 'my-align-comment)
  (defalias 'aleq 'my-align-to-equals)
  (defalias 'alrx 'align-regexp)        ; align-entire ; align-string
  (defalias 'doe (my-defun-interactive (add-hook 'after-init-hook '(lambda () (setq debug-on-error t)))))
  (defalias 'dox (my-defun-interactive (setq eval-expression-debug-on-error (not eval-expression-debug-on-error))))
  (defalias 'dtw 'delete-trailing-whitespace)
  (defalias 'google 'my-google-at-point)
  (defalias 'goto-column 'move-to-column) ; (24.3) also M-g TAB
  (defalias 'hcm 'highlight-changes-mode)
  (defalias 'hlm 'my-toggle-hl-line-mode)
  (defalias 'hlrx 'highlight-regexp)
  (defalias 'hlrxu 'unhighlight-regexp)
  (defalias 'hlrxi 'isearch-highlight-regexp)
  (defalias 'hlrxc 'lazy-highlight-cleanup)
  (defalias 'ibr 'isearch-backward-regexp)
  (defalias 'ifr 'isearch-forward-regexp)
  (defalias 'ifs 'isearch-forward-symbol)
  (defalias 'ifw 'isearch-forward-word)
  (defalias 'lim 'my-toggle-linum-mode)
  (defalias 'llm 'longlines-mode) ; allow long lines
  (defalias 'omm 'outline-minor-mode)
  (defalias 'qr  'query-replace) ;  => M-x qr (also C-%)
  (defalias 'qrr 'query-replace-regexp) ;  => M-x qrr (also C-M-%)
  (defalias 'sir 'string-insert-rectangle) ; insert rectangle with symbol, entered in minibuffer
  (defalias 'ssd 'my-toggle-selective-display)
  ;; store/restore frame/window config to register f/w; also save-window-excursion
  (defalias 'sfc (my-defun-interactive (frame-configuration-to-register ?f))) ; C-x r w f
  (defalias 'rfc (my-defun-interactive (jump-to-register ?f))) ; C-x r j f
  (defalias 'swc (my-defun-interactive (window-configuration-to-register ?w))) ; C-x r w w
  (defalias 'rwc (my-defun-interactive (jump-to-register ?w))) ; C-x r j w
  (defalias 'rcc (my-defun-interactive (jump-to-register ?c))) ; stored in my-compilation-hook
  (defalias 'touch 'my-touch-buffer)
  (defalias 'tqr 'tags-query-replace) ; => M-x tqr
  (defalias 'vtn (my-defun-interactive (visit-tags-table-buffer t))) ; visit next table in `tags-table-list'
  (defalias 'wsm 'whitespace-mode)
  "defalias aliases loaded")
;;(my-define-key (kbd "<C-right>") 'enlarge-window-horizontally) ; C-x }
;;(my-define-key (kbd "<C-left>") 'shrink-window-horizontally) ; C-x {

;; ============================================================================
;; --- Defalias - consistently prefixed commands

(when my-use-consistent-alias
  (defalias '-buffer-bury 'bury-buffer)
  (defalias '-buffer-kill-current 'my-kill-current-buffer)
  (defalias '-buffer-kill-prompt 'ido-kill-buffer)
  (defalias '-buffer-mark 'mark-whole-buffer)
  (defalias '-buffer-next 'next-buffer)
  (defalias '-buffer-other-all-kill 'my-kill-other-buffers-and-windows)
  (defalias '-buffer-other-kill 'my-kill-other-buffer)
  (defalias '-buffer-other-next 'my-next-buffer-other-window)
  (defalias '-buffer-other-prev 'my-previous-buffer-other-window)
  (defalias '-buffer-prev 'previous-buffer)
  (defalias '-buffer-revert 'revert-buffer)
  (defalias '-buffer-swap 'my-swap-buffers)
  (defalias '-buffer-unbury 'unbury-buffer)
  (defalias '-column-goto 'move-to-column)
  (defalias '-column-goto-tab-stop 'move-to-tab-stop)
  (defalias '-column-indent 'indent-to-column)
  (defalias '-column-set-goal 'set-goal-column)
  (defalias '-function-prefix 'my-prefix-defun)
  (defalias '-line-center 'center-line)
  (defalias '-line-copy-from-col 'copy-from-above-command)
  (defalias '-line-copy-to-char 'my-copy-from-above-to-char)
  (defalias '-lines-sort 'sort-lines)
  (defalias '-mode-hl-line 'my-toggle-hl-line-mode)
  (defalias '-mode-linum 'my-toggle-linum-mode)
  (defalias '-mode-outline 'outline-minor-mode)
  (defalias '-mode-multi-cursor 'multiple-cursors-mode)
  (defalias '-mode-overwrite 'overwrite-mode)
  (defalias '-mode-subword 'subword-mode) ; for cc-mode; C-right / C-left are NOT bound to forward-word / backward-word
  (defalias '-mode-whitespace 'whitespace-mode)
  (defalias '-occur-comment (my-defun-interactive (occur (concat "^\\s-*" comment-start "+\\s-*---+"))))
  (defalias '-occur-todo (my-defun-interactive (occur "TODO:")))
  (defalias '-os-explorer 'my-open-explorer)
  (defalias '-os-dir-delete 'delete-directory)
  (defalias '-os-dir-list 'list-directory)
  (defalias '-os-file-copy 'copy-file)
  (defalias '-os-file-delete 'delete-file)
  (defalias '-os-file-find 'ido-find-file)
  (defalias '-os-file-find-at-point 'find-file-at-point)
  (defalias '-os-file-insert 'insert-file) ;ido-insert-file?
  (defalias '-os-file-rename 'rename-file)
  (defalias '-os-shell-interrupt 'comint-interrupt-subjob) ; interrupt-shell-subjob
  (defalias '-os-shell-open 'my-open-shell)
  (defalias '-paragraph-center 'center-paragraph)
  (defalias '-paragraph-fill 'fill-paragraph)
  (defalias '-paragraph-kill-backward 'backward-kill-paragraph)
  (defalias '-paragraph-kill-forward 'kill-paragraph)
  (defalias '-paragraph-mark 'mark-paragraph)
  (defalias '-paragraph-transpose 'transpose-paragraphs)
  (defalias '-region-center 'center-region)
  (defalias '-region-downcase 'downcase-region)
  (defalias '-region-fill 'fill-region)
  (defalias '-region-prefix 'my-prefix-region)
  (defalias '-region-upcase 'uppercase-region)
  (defalias '-sentence-kill-backward 'backward-kill-sentence)
  (defalias '-sentence-kill-forward 'kill-sentence)
  (defalias '-sentence-move-backward 'backward-sentence)
  (defalias '-sentence-move-forward 'forward-sentence)
  (defalias '-sentence-transpose 'transpose-sentences)
  (defalias '-sexp-kill-backward 'backward-kill-sexp)
  (defalias '-sexp-kill-forward 'kill-sexp)
  (defalias '-sexp-transpose 'transpose-sexps)
  (defalias '-window-compare-nows 'compare-windows) ; ignore whitespace
  (defalias '-window-compare-ws (my-defun-interactive (compare-windows t))) ; respect whitespace
  "defalias menu loaded")

;;; ###########################################################################
;;; --- Final startup settings
;;; ###########################################################################

;; ============================================================================
;; --- Load user and system dependent init files

(let ((user-settings-file (concat user-emacs-directory "init_user_" user-login-name ".el")))
  (when (file-exists-p user-settings-file)
    (load user-settings-file)
    (message "**Finished loading user init file %s." user-settings-file)))

(let ((system-settings-file (concat user-emacs-directory "init_system_" system-name ".el")))
  (when (file-exists-p system-settings-file)
    (load system-settings-file)
    (message "**Finished loading system init file %s." system-settings-file)))

(setq default-directory "~")
(cd default-directory) ; go home

(message "***Finished loading %s in %ds." ; (emacs-init-time) not yet ready
         dotfile-name (time-to-seconds (time-since my-emacs-load-start-time))) ; (sit-for 2)
(setq debug-on-error nil)

;;; ###########################################################################
;;; --- Final display settings
;;; ###########################################################################

(defun my-init-graphic-or-term-frame (first-frame frame)
  (with-selected-frame frame
    (if (display-graphic-p)     ; Note: a display allows several frames and different fonts at once, ...
        (my-init-graphic-frame first-frame frame) ; ... whereas a terminal does not
      (my-init-term-frame first-frame frame)))) ; TODO: also call in term-setup-hook and/or after-make-frame-functions?

;; run for each new frame (emacsclient; graphics or terminal)
(add-hook 'after-make-frame-functions
          (lambda (frame) (my-init-graphic-or-term-frame nil frame)))

;; run once for emacs start (emacs, runemacs)
(require 'server)
(unless (server-running-p)
  (my-init-graphic-or-term-frame t (selected-frame)))

;;; ###########################################################################
;;; --- scratchpad
;;; ###########################################################################

;; --- relevant keys, predefined and mine from here
;;     http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley   - open w/ C-. u or C-c RET
;;     http://defindit.com/readme_files/emacs_bindings.html

;; C-u 10 C-{key}               perform key 10 times
;; C-1 C-0 {key}                the same - just less keystrokes

;; C-M-i   -> M-TAB
;; Esc-TAB -> M-TAB
;; C-x C-m C-m -> C-x C-m (confirm default minibuffer content) C-m (where C-m is RET)

;; C-g                          cancel; (keyboard-quit)
;; ESC ESC ESC                  escape current interactive command, minibuffer edit, ... (keyboard-escape-quit)

;; C-z                          undo
;; C-g C-z                      redo

;; C-x C-+ / C-x C-- / C-x C-0  increase / decrease /reset buffer text size; text-scale-increase/text-scale-decrease

;; M-m                          (back-to-indentation) / similar to original C-a
;; M-SPACE                      just-one-space
;; C-- M-SPACE                  just-one-space incl newlines
;; M-d                          kill word
;; M-y                          cua-paste-pop (CUA yank-pop replacement)
;; M-z                          zap-to-char; C-- M-z : zap backwards; C-u n M-z : zap to n-th occurence

;; M-q                          cc-mode: c-fill-paragraph (like fill-paragraph for C and C++ comments)
;; M-q                          else: fill-paragraph
;; C-c C-q                      c-mode: c-indent-defun, indent current function

;; C-x 1                        unsplit window
;; C-x 2                        split window below
;; C-x 3                        split window right
;; C-x +                        balance windows
;; (unbound)                    split-window-horizontally
;; (unbound)                    split-window-vertically
;; C-x <ret> f unix <ret>       conv. DOS to UNIX
;; C-h e                        view-echo-area-messages

;; kmacro-start-macro           C-x (
;; kmacro-end-macro             C-x )
;; kmacro-end-and-call-macro    C-x e; any more e repeats again

;; C-RET                        start CUA rectangle mode
;;   C-q                        cancels the rectangle, next C-RET will not activate same rectangle
;;   C-RET                      cancels the rectangle, next C-RET will activate same rectangle
;;   [C-space]                  activates the region bounded by the rectangle, so make region from rectangle
;;   self-inserting char        when on bottom of 1-char region, insert char in all lines
;;   TAB, C-i                   for 1-char region, indent rectangle; for region > 1 char, indent after rectangle
;;   C-. (cua-rotate-rectangle) cycle through corners; allows to insert/append chars
;;   M-up/M-dn/M-l/M-r          move selected region (not content)
;;   [M-a]                      aligns all words at the left edge of the rectangle
;;   [M-b]                      fills the rectangle with blanks (tabs and spaces)
;;   [M-c]                      closes the rectangle by removing all blanks at the left edge
;;   [M-f]                      fills the rectangle with a single character (prompt)
;;   [M-i]                      increases first number found on each line by numeric prefix arg (default 1)
;;   [M-k]                      kills the rectangle as normal multi-line text (for paste)
;;   [M-l] / [M-u]              downcases / upocases the rectangle
;;   [M-m]                      copies the rectangle as normal multi-line text (for paste)
;;   [M-n]                      fills each line of rectangle with increasing numbers using a prompted format string
;;   [M-o]                      opens rectangle by moving its text to the right and filling the rectangle with blanks
;;   [M-p]                      toggles virtual straight rectangle edges
;;   [M-P]                      inserts tabs and spaces (padding) to make real straight edges
;;   [M-q]                      performs text filling on the rectangle
;;   [M-r]                      replaces REGEXP (prompt) by STRING (prompt) in rectangle
;;   [M-R]                      reverse the lines in the rectangle
;;   [M-s]                      fills each line of the rectangle with the same STRING (prompt)
;;   [M-t]                      performs text fill of the rectangle with TEXT (prompt)
;;   [M-|]                      runs shell command on rectangle
;;   [M-']                      restricts rectangle to lines with CHAR (prompt) at left column
;;   [M-/]                      restricts rectangle to lines matching REGEXP (prompt)
;;   [C-?]                      Shows a brief list of the above commands
;;   [M-C-up] / [M-C-down]      scrolls lines INSIDE the rectangle up and down; recover lost top/bottom lines w/ [C-z]

;; TODO: support directory local variables or use (dir-locals-set-class-variables)
;;  see http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html

;; TODO: support http://marmalade-repo.org/
;; (require 'package)
;; (add-to-list 'package-archives
;;     '("marmalade" .
;;       "http://marmalade-repo.org/packages/"))
;; (package-initialize)

;; TODO: support mail editing for Outlook,
;;  see http://www.emacswiki.org/emacs/MsOutlook and https://github.com/dholm/outlookedit

;; (set-buffer-file-coding-system 'undecided-dos) ; unix to dos; successor for default-buffer-file-coding-system
;; (set-buffer-file-coding-system 'undecided-unix) ; dos to unix

;; (defun utf-8-revert () "Reload file assuming utf-8 encoding." (interactive)(revert-buffer-with-coding-system 'utf-8))
;; (defun utf-8-dos () "Use `utf-8-dos' encoding." (interactive)(set-buffer-file-coding-system 'utf-8-dos t))
;; (defun utf-8-unix () "Use `utf-8-unix' encoding." (interactive)(set-buffer-file-coding-system 'utf-8-unix t))

;; To make ‘M-:’ pretty-print:
;;       (my-define-key [remap eval-expression] 'pp-eval-expression)
;; To make ‘C-x C-e’ pretty-print:
;;       (my-define-key [remap eval-last-sexp] 'pp-eval-last-sexp)
;;  This means that the result of a sexp evaluation is pretty-printed in a separate buffer, `*Pp Eval Output*’.

;; (defun my-delete-backward-to-ws ()
;;   (interactive)
;;   (delete-region (point) (save-excursion (skip-syntax-backward "^ ") (point))))

;; C-x k == C-x # when editing emacsclient is waiting
;; (add-hook 'server-switch-hook
;;   (lambda ()
;;     (my-define-key (kbd "C-x k") '(lambda ()
;;                                     (interactive)
;;                                     (if server-buffer-clients
;;                                         (server-edit)
;;                                       (ido-kill-buffer))))))

;; ; load given package if directory exists
;; (defmacro load-if-dir (dir-name &rest body)
;;   (let ((path-to-load (make-symbol "path-to-load")))
;;     `(let ((,path-to-load ,dir-name))
;;        (when (file-directory-p ,path-to-load)
;;       (add-to-list 'load-path ,path-to-load)
;;       ,@body))))

;; ; load given file if exists
;; (defmacro load-if-file (file-name &rest body)
;;   (let ((file-to-load (make-symbol "file-to-load")))
;;     `(let ((,file-to-load ,file-name))
;;        (when (file-readable-p ,file-to-load)
;;       (load ,file-to-load)
;;       ,@body))))

;; (defmacro if-avail (module-name &rest body)
;;   (let ((module-to-load (make-symbol "module-to-load")))
;;     `(let ((,module-to-load (concat emacs-pkg-home ,module-name ".el")))
;;        (if (file-readable-p ,module-to-load)
;;         (progn
;;           ,@body)
;;       (message (concat "Can not find file: " ,module-to-load))))))


;; ; load given file if exists, and add dir to path
;; (defmacro load-if-dir-and-file (dir-name file-name &rest body)
;;   `(progn
;;      (load-if-dir ,dir-name)
;;      (load-if-file (concat ,dir-name ,file-name))
;;      ,@body))

;; (set-frame-parameter (selected-frame) 'alpha '(90 90)) ; transparent
;; (set-frame-parameter (selected-frame) 'alpha '(100 100)) ; opaque

;; (defun adjust-opacity (frame incr)
;;   (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
;;          (newalpha (+ incr oldalpha)))
;;     (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
;;       (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
;; (global-set-key (kbd "M-C-8") '(lambda () (interactive) (adjust-opacity nil -5)))
;; (global-set-key (kbd "M-C-9") '(lambda () (interactive) (adjust-opacity nil 5)))
;; (global-set-key (kbd "M-C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; (defun my-insert-file-at-point ()
;;   (interactive)
;;   (let ((bounds (bounds-of-thing-at-point 'filename))
;;      (filename (thing-at-point 'filename)))
;;     (delete-region (car bounds) (cdr bounds))
;;     (insert-file filename)))

;; Here is the key piece of code, which opens filename using the appropriate application:
;; (shell-command (concat "rundll32 shell32,ShellExec_RunDLL " (shell-quote-argument filename)))
;; maybe simpler - if working:
;; (shell-command (concat "start " (shell-quote-argument filename)))

;; (defvar my-func-history nil)
;; (defun my-func (str)
;;   (interactive (list (read-from-minibuffer "Input string: " (car my-func-history) nil nil 'my-func-history)))
;;   (insert str))

;; goal-column fill-region fill-paragraph fill-column

;; (dolist (key '("\C-a" "\C-b" "\C-c" "\C-d" "\C-e" "\C-f" "\C-g"
;;                "\C-h" "\C-k" "\C-l" "\C-n" "\C-o" "\C-p" "\C-q"
;;                "\C-t" "\C-u" "\C-v" "\C-x" "\C-z" "\e"))
;;   (global-unset-key key))

;; http://tromey.com/elpa/
;; http://dotfiles.org/.emacs
;; http://www.math.uh.edu/~bgb/emacs_keys.html
;; flyspell - spell checker

;; (defun my-bury-successful-compilation-finish-hook (buffer string)
;;   "Bury a compilation buffer if succeeded without warnings."
;;   (if (and (string-match "compilation" (buffer-name buffer))
;;            (string-match "^finished" string)
;;            (not (with-current-buffer buffer
;;                   (search-forward "warning" nil t))))
;;       (run-with-timer 1 nil
;;                       (lambda (buf)
;;                         (bury-buffer buf)
;;                         (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                       buffer)))
;; (add-hook 'compilation-finish-functions 'my-bury-successful-compilation-finish-hook)

;; (defun my-compilation-exit-autoclose (status code msg)
;;   "Bury the compilation window if there was no error at all."
;;   (when (and (eq status 'exit) (zerop code)) ; If M-x compile exists with a 0 then...
;;     (bury-buffer) ; ...bury the *compilation* buffer, so that C-x b doesn't go there and...
;;     (delete-window (get-buffer-window (get-buffer "*compilation*")))) ; ...delete the *compilation* window
;;   (cons msg code)) ; Always return the anticipated result of compilation-exit-message-function
;; (setq compilation-exit-message-function 'my-compilation-exit-autoclose)

;; (tooltip-show "foo\n")

;; I use outline-minor-mode with the following home baked configuration:
;; ;; Python stuff for outline mode.
;; (defvar py-outline-regexp "^\\([ \t]*\\)\\(def\\|class\\|if\\|elif\\|else\\|while\\|for\\|try\\|except\\|with\\)"
;;   "This variable defines what constitutes a 'headline' to outline mode.")
;; (defun py-outline-level ()
;;   "Report outline level for Python outlining."
;;   (save-excursion
;;     (end-of-line)
;;     (let ((indentation (progn
;;                          (re-search-backward py-outline-regexp)
;;                          (match-string-no-properties 1))))
;;       (if (and (> (length indentation) 0)
;;                (string= "\t" (substring indentation 0 1)))
;;           (length indentation)
;;         (/ (length indentation) py-indent-offset)))))
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (outline-minor-mode 1)
;;              (setq
;;               outline-regexp py-outline-regexp
;;               outline-level 'py-outline-level)))
;; or: http://bytes.com/topic/python/answers/36925-how-use-fold-hideshow-emacs-python-mode
;; or: https://bitbucket.org/ArneBab/.emacs.d/src/cc10d3514575/libs/python-magic.el

;; ;; Rigidly indent. C-c 4 indents in, C-c - 4 outdents the same. Keeps
;; ;; working on the same region if repeated (highlight disappears)
;; (fset 'ind4 [?\C-u ?4 ?\C-x tab])
;; (my-define-key (kbd "C-c 4") 'ind4)
;; (fset 'ind2 [?\C-u ?2 ?\C-x tab])
;; (my-define-key (kbd "C-c 2") 'ind2)
;; (fset 'outd4 [?\C-u ?- ?4 ?\C-x tab])
;; (my-define-key (kbd "C-c - 4") 'outd4)

;; mode setting - e.g. if python is not supported -:
;; (mapc (lambda (mode-hook) (add-hook mode-hook 'turn-on-watchwords))
;;         '(c-mode-common-hook tcl-mode-hook emacs-lisp-mode-hook
;;           ruby-mode-hook java-mode-hook haskell-mode-hook
;;           ess-mode-hook python-mode-hook sh-mode-hook))

;; do something in all open buffers
;; | (defun indent-all-buffers ()
;; |   "Reindent all currently open buffers, as long as they are not read-only. Read-only buffers are simply skipped."
;; |   (interactive)
;; |   (dolist (elt (mapcar (function buffer-name) (buffer-list)) nil)
;; |     (save-current-buffer
;; |       (set-buffer elt)
;; |       (if (not buffer-read-only)
;; |           (indent-region (point-min) (point-max) nil))))

;; ;; Turn Control-z into another keymap.  The previous binding becomes
;; ;; `Control-z Control-z'.  This frees up some easy-to-reach
;; ;; keybindings for my own use.  Note that the previous binding is
;; ;; normally `suspend-frame', which is also bound to `C-x C-z', so we
;; ;; don't *have* to preserve it.
;; (defvar ctrl-z-map (make-sparse-keymap))
;; (let ((orig-ctrl-z-binding (lookup-key (current-global-map) [(control ?z)])))
;;   (global-set-key [(control ?z)] ctrl-z-map)
;;   (global-set-key [(control ?z) (control ?z)] orig-ctrl-z-binding))
;; (global-set-key [(control ?z) (control ?g)] 'keyboard-quit)
