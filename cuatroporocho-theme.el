;; ============================================================================
;; File:            init.el
;; Last changed by: Frank Ruben   on 23-09-2013 21:29:29
;; Purpose:         Emacs initialisation
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
;; (debug-on-entry) / (cancel-debug-on-entry) / see alias 'doe below
;; (set eval-expression-debug-on-error t) ; If non-nil set `debug-on-error' to t in `eval-expression'
;; if all that doesn't work: M-x edebug-defun in function, then call the function

(setq dotfile-name (or load-file-name (buffer-file-name)))
;; if above doesn't work, use: (setq dotfile-name (buffer-file-name))

(unless noninteractive (message "***Starting to load %s." dotfile-name))
(setq my-emacs-load-start-time (current-time))

(add-to-list 'load-path (concat user-emacs-directory "src")) ; some el's are directly copied to src root
                                        ; for all others, I'll define their path before loading them

(eval-after-load 'info '(progn (push "~/.emacs.d/info" Info-default-directory-list)))
;; add new info files to ~/emacs-xxx/info/dir, e.g.:
;; Languages
;; * C-library: (libc).            GNU C library functions and macros.
;; * Python: (python).             The Python Documentation.

(setq my-use-shortcut-alias t)          ; not sure whether this is useless w/ smex
(setq my-use-consistent-alias t)        ; not useless even w/ smex

;;; ###########################################################################
;;; --- Loading packages
;;; ###########################################################################

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-user-dir "~/.emacs.d/elpa/")
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))

  (package-initialize)
  ;; (package-refresh-contents)
  (mapc #'(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
        '(ace-jump-mode
          autopair
          dired+
          evil-numbers
          expand-region
          ggtags
          goto-last-change
          hide-lines
          jump-char
          magit
          multiple-cursors
          outline-magic
          ;;;;; TODO --- no package: python-mode ; TODO: in win-init: no standard package, so needs when ...
          ;; pydoc-info ; no package
          rainbow-mode
          rainbow-delimiters
          smex
          ;; transpose-frame ; no package
          winner
          workgroups
          yasnippet)))
  (delete-other-windows)

;; ============================================================================
;; --- Keydef macros

(defmacro my-define-key-interactive (map key &rest body)
  "Define BODY with KEY as interactive lambda for keymap MAP."
  `(define-key ,map ,key (my-defun-interactive ,@body)))

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
(setq cua-keep-region-after-copy t      ; selection remains after C-c, standard win32 behaviour
      cua-paste-pop-rotate-temporarily t)
(transient-mark-mode t)                 ; highlight region between point and mark

;;; ###########################################################################
;;; --- Init frame and server mode
;;;     Note: Defined here, but to be called at end of init file
;;; ###########################################################################

(defun my-init-graphic-frame (first-frame frame)
  (if first-frame
      (progn ; started w/ "runemacs"

        ;; --- start the server for emacsclient use (clear the server file w/ server-force-delete)
        (unless noninteractive (message "***Server started."))
        (server-start)

        ;; --- desktop load/save, load recent file, and cursor position handling
        (desktop-save-mode)

        (when (recentf-mode 1)
          (setq recentf-max-saved-items 100)
          (defun my-ido-choose-from-recentf () ; C-f3; see below
            "Use ido to select a recently opened file from the `recentf-list' shown in minibuffer."
            (interactive)
            (find-file (ido-completing-read "Open file: " recentf-list nil t nil nil (thing-at-point 'filename)))))

        (require 'saveplace) ; Save cursor position to file save-place-file
        (setq-default save-place t)
        (setq save-place-file (concat user-emacs-directory ".emacs-places")) ; per default in ~/

        (when (require 'workgroups nil t) ; save windows/buffer configuration (note: there is a newer workgroups2)
          (let ((workgroups-file (concat user-emacs-directory ".emacs-workgroups")))
            (setq wg-prefix-key (kbd "C-c w")
                  wg-no-confirm t
                  my-workgroups-file workgroups-file
                  wg-morph-on nil)

            (define-key wg-map (kbd "i") (my-defun-interactive (find-file my-workgroups-file)))
            (define-key wg-map (kbd "l") 'my-wg-load-default) ; <prefix> C-l is wg-load, which loads the base config
            (define-key wg-map (kbd "s") 'my-wg-save-default) ; <prefix> C-s is wg-save, which saves the base config
                                        ; <prefix> c - wg-create-workgroup, create a new empty workgroup
                                        ; <prefix> C - wg-clone-workgroup, clone the current workgroup
                                        ; <prefix> k - wg-kill-workgroup, delete workgroup from the list of workgroups
                                        ; <prefix> r - wg-revert-workgroup, revert working config to base config
                                        ; <prefix> u - wg-update-workgroup, update current base config from current config
                                        ; <prefix> U - update all and hence save workgroups for later restoration
                                        ; <prefix> v - wg-switch-to-workgroup
                                        ; <prefix> x - wg-swap-workgroups, swap current and previous wg in the workgroups list
            (workgroups-mode 1)

            (defun my-wg-load-default ()
              "Run `wg-load' on the default workgroups save file."
              (interactive)
              (when (file-exists-p my-workgroups-file)
                (wg-load my-workgroups-file))) ; w/ default wg-switch-on-load = t, this will also load the 1st workgroup

            (defun my-wg-save-default ()
              "Run `wg-save' on the default workgroups save file."
              (interactive)
              (condition-case nil
                  (wg-save my-workgroups-file)
                (error nil)))

            (add-hook 'kill-emacs-hook 'my-wg-save-default)
            (my-wg-load-default) ))

        ;; --- init display & frame: font and color scheme
        (my-init-font)
        (my-init-theme t)

        ;; --- final display settings (call these at the end)
        (when (eq system-type 'windows-nt) (w32-send-sys-command #xf030))
        (add-hook 'window-setup-hook (lambda ()
                                       (menu-bar-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1) ; no chrome
                                       (set-fringe-mode 4) ; needs some, display bugs w/ 0
                                       (balance-windows)))

        (unless noninteractive (message "***%s." "1st graphic frame")))

    ;; else, not in first frame:
    ;; started w/ "emacsclientw" (same frame) or "emacsclientw --tty" (new frame) for win32
    ;; Note: init.el is only evaluated once when Emacs starts, not when emacsclient connects.
    (unless noninteractive(message "***%s." "nth graphic frame"))))

(defun my-init-term-frame (first-frame frame)
  (if first-frame
      (progn ; started w/ "emacs -nw" for win32 or console emacs for Unix; don't start server for standalone terminal
        (my-init-theme nil)             ; no font setting, terminal emacs uses terminal font
        (menu-bar-mode -1)
        (balance-windows)
        (unless noninteractive (message "***%s." "1st term frame")))
    ;; else: currently (w32 only) not expected
    (save-buffers-kill-emacs)))

(defun my-init-font ()                           ; test w/ M-x describe-font
  (let ((font-list (list "Ubuntu Mono-12"        ; quite nice - and narrow (same height as those below)
                         "Consolas-12"           ; Win7+ system font, nicer - but more wide
                         "Source Code Pro-11"))) ; nice too, but also more wide w/ same size as "Consolas-12"
    (while font-list
      (condition-case nil
          (progn (set-frame-font (car font-list))
                 (setq font-list ()))
        (error (setq font-list (cdr font-list)))))))

(defun my-init-theme (for-graphic)
  (let* ((themes-dir (concat user-emacs-directory "src/themes/"))
         (cuatroporocho-dir (concat themes-dir "cuatroporocho-theme/"))
         (solarized-dir (concat themes-dir "solarized/")))
    (add-to-list 'load-path themes-dir)
    (add-to-list 'custom-theme-load-path themes-dir)
    (if for-graphic
        (if nil                       ; for now, I like my cuatroporocho theme more...
            (progn (add-to-list 'load-path solarized-dir)
                   (add-to-list 'custom-theme-load-path solarized-dir)
                   (load-theme 'solarized-light t)) ; (load-theme 'solarized-dark t)
          (progn (add-to-list 'load-path cuatroporocho-dir)
                 (add-to-list 'custom-theme-load-path cuatroporocho-dir)
                 (load-theme 'cuatroporocho t) (load "cuatroporocho-theme"))) ; (load...) required, see zenburn
      (progn (add-to-list 'load-path cuatroporocho-dir)
             (add-to-list 'custom-theme-load-path cuatroporocho-dir)
             (load-theme 'cuatroporocho t) (load "cuatroporocho-theme"))))) ; (my-init-theme t)
;; (load-theme 'wombat t) ; (load-theme 'tomorrow-night t) ; (load-theme 'twilight t)
;; (load-theme 'zenburn t) (load-theme 'inkpot t) ; (load-theme 'monokai t) ; (load-theme 'blackboard t)
;; (load-theme 'leuven t) ; (load-theme 'mesa t) ; (load-theme 'dichromacyd t)

;;; ###########################################################################
;;; --- Vanilla packages and their settings
;;; ###########################################################################

;; ============================================================================
;; --- time stamp and user info

(when (require 'time-stamp)             ; (setq time-stamp-line-limit 5)
  (setq time-stamp-active t)            ; C-x e this to test: (time-stamp-string)
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-start  "[lL]ast changed by[:]?\\s-*")  ; (re-search-forward time-stamp-start)
  (setq time-stamp-end    "\\s-*$")
  (setq time-stamp-format "%U\ton %02d-%02m-%:y %02H:%02M:%02S")
  (setq user-full-name "Frank Ruben"
        user-mail-address (concat "frank" "ruben" "27" "@" "gmail" "." "com")))

;; ============================================================================
;; --- modeline and other display settings

(blink-cursor-mode 0)                   ; don't blink cursor
(display-time)                          ; display time in the modeline
(global-visual-line-mode -1)            ; no word-wrap, no editing on visual lines
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-interval 60)
(setq font-lock-maximum-decoration t
      jit-lock-defer-time 0.1)          ; defer fontification to not slow down continuous scrolling
(when window-system
  (mouse-avoidance-mode 'animate)       ; drive out the mouse when it's too near to the cursor
  (setq use-file-dialog nil             ; disable use of file dialog
        use-dialog-box nil)             ; disable file selection dialog
  (setq frame-title-format
        '( "%b - " invocation-name "@" system-name "; uptime " (:eval (emacs-uptime)) )))
(setq scroll-conservatively 101         ; don't recenter after scroll; pefered over (setq scroll-step 1)
      scroll-preserve-screen-position 1 ; try to keep screen position when PgDn/PgUp
      scroll-margin 2)           ; Recenter window whenever point gets within this many lines of top or bottom of window
(setq visible-bell t)            ; disable beep

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
  (setq vc-make-backup-files t)                                      ; Make backups of files, even if version controlled
  (setq auto-save-file-name-transforms `((".*" ,backup-dir) t)))     ; not used for auto-save-visited-file-name t

(setq backup-by-copying t ; (backup-buffer)
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 10
      version-control t)
(add-hook 'before-save-hook (lambda() (setq buffer-backed-up nil)))       ; backup EVERY save

(auto-save-mode)
(setq auto-save-visited-file-name t     ; auto-save buffer visited file, not under other name
      auto-save-interval 20             ; keystrokes between auto saves
      auto-save-timeout 60)             ; idle seconds between auto saves

;; ============================================================================
;; --- abbrevs; see C-. a keys below

(let ((abbrev-file (concat user-emacs-directory "emacs.abbrevs.el")))
  (unless (file-exists-p abbrev-file)   ; command-line will fail, if this is nil
    (shell-command (concat "touch " abbrev-file)))
  (setq abbrev-file-name abbrev-file
        save-abbrevs 'silently)
  (quietly-read-abbrev-file)      ; abbrev-prefix-mark to combine abbrevs w/in works
  (define-abbrev-table 'global-abbrev-table
    '(("xxx"    "" (lambda() (insert comment-start "TODO:" comment-end) (indent-according-to-mode)))
      ("fb"     "" (lambda() (insert (or user-full-name (getenv "USERNAME") (getenv "USER")))))
      ("cutf8"  "# -*- coding: utf-8 -*-" nil 1)
      ("ciso1"  "# -*- coding: iso-8859-1 -*-" nil 1))))

;; ============================================================================
;; --- bookmarks; see C-. b keys below

(let ((bookmark-file (concat user-emacs-directory "emacs.bookmarks.el")))
  (if (file-exists-p bookmark-file)
      (setq bookmark-default-file bookmark-file
            bookmark-save-flag t) ; autosave each change
    (setq bookmark-default-file nil)))

;; ============================================================================
;; --- tags / gtags / GNU global (ctags, cscope replacement); see C-. t keys below
;;     http://www.gnu.org/software/global/globaldoc.html
;;     GNU global works nicely, so already configured for C-mode, not yet for others
;;     Golang has its own features for that

(when (require 'gtags nil t)
  (setq gtags-suggested-key-mapping nil
        gtags-disable-pushy-mouse-mapping nil
        gtags-path-style 'relative)

  (defun my-cycle-next-gtag ()
    "Find next matching tag, for GTAGS, after doing gtags-find-tag, gtags-find-rtag, or gtags-find-symbol."
    (interactive)
    (let ((latest-gtags-buffer
           (car (delq nil (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                  (buffer-list)) ))))
      (cond (latest-gtags-buffer
             (switch-to-buffer latest-gtags-buffer)
             (forward-line)
             (gtags-select-it nil)))))

  (defun my-init-gtags-mode ()  ; not all keys from gtags supported
    (gtags-mode 1)
    (global-set-key [remap find-tag]           'gtags-find-tag)     ; M-. ; find definition of tag
                                                                    ; C-u M-. / M-- M-. ; next / previous tag match
    (global-set-key [remap tags-loop-continue] 'my-cycle-next-gtag) ; M-, ; continue tags-search or tags-query-replace
    (global-set-key [remap pop-tag-mark]       'gtags-pop-stack)    ; M-* ; jump back to where M-. was last invoked
    (global-set-key (kbd "C-:")                'gtags-find-rtag)    ; find all references of tag
    (when (boundp 'my-alt-o-map)
      (define-key my-alt-o-map (kbd "M-.")     'gtags-find-tag-other-window))
    (global-set-key (kbd "C-M-:")              'gtags-find-tag-from-here) ;  find usage/definition from definition/usage
    ;; (define-key gtags-mode-map (concat gtags-prefix-key "P") 'gtags-find-file) ; open file as indexed by gtags
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

(let ((ff-exe (if (eq system-type 'windows-nt)
                  (concat (getenv "LOCAL") "\\ff\\FirefoxPortable.exe")
                (executable-find "firefox"))))
  (when (file-executable-p ff-exe)
    (setq browse-url-firefox-program ff-exe
          browse-url-browser-function 'browse-url-firefox
          browse-url-new-window-flag t
          browse-url-firefox-new-window-is-tab t)))

;; ============================================================================
;; --- outline; keep keys in sync w/ org mode

(when (require 'outline)                                                   ; standard emacs
  (defun my-outline-minor-mode-hook ()                                     ; activate w/ alias omm
    (local-set-key (kbd "C-<")                 outline-mode-prefix-map)    ; outline-minor-mode-prefix doesn't work...;
    ;; C-o - hide-other                                                    ; negative-argument, also bound to M--
    ;; C-s - show-subtree
    ;; C-a - show-all
    (local-set-key (kbd "C-M-<")               'outline-up-heading)        ; also used in org mode
    (when (require 'outline-magic nil t)                                   ; extra package w/ some helpful functions
      (local-set-key (kbd "<C-tab>")           (my-inherit-prefix-map (current-global-map) (kbd "<C-tab>")))
      (local-set-key (kbd "<C-tab> <C-tab>")   'outline-cycle)             ; org mode: org-cycle
      (local-set-key (kbd "C-t")               (my-inherit-prefix-map (current-global-map) (kbd "C-t")))
      (local-set-key (kbd "C-t s")             'outline-move-subtree-down) ; org mode: org-shiftmetadown
      (local-set-key (kbd "C-t C-s")           'outline-move-subtree-up)   ; org mode: org-shiftmetaup
      (local-set-key (kbd "M-S-<up>")          'outline-move-subtree-up)   ; org mode: org-shiftmetaup
      (local-set-key (kbd "M-S-<down>")        'outline-move-subtree-down) ; org mode: org-shiftmetadown

      (define-key outline-mode-prefix-map "<"  'outline-promote)  ; org mode: org-shiftmetaleft
      (define-key outline-mode-prefix-map ">"  'outline-demote))) ; org mode: org-shiftmetaright
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
            (if (and nil (boundp 'company-mode) company-mode) ; and nil ...: company not working here, use C-tab C-tab
                (company-complete-common)
              (hippie-expand nil))
          (indent-for-tab-command)))))

  (require 'dabbrev)
  (setq dabbrev-case-replace nil      ; preserve case when expanding
        dabbrev-case-fold-search nil) ; case is significant

  (defadvice hippie-expand (around hippie-expand-case-fold)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))
  (ad-activate 'hippie-expand)

  (setq hippie-expand-try-functions-list '(try-expand-all-abbrevs ; search thru defined abbrevs
                                           try-expand-dabbrev ; like dabbrev-expand, dynamic abbrevs
                                           try-expand-dabbrev-visible ; dabbrev for visible parts of all windows
                                           try-expand-dabbrev-all-buffers ; dabbrev for all buffers
                                           try-complete-file-name-partially ; more might be added in programming modes
                                           try-complete-file-name))) ; yas-hippie-try-expand added below w/ yasnippet

;; ============================================================================
;; --- tramp mode
;;     The tramp version dl'ed into user-emacs-directory is 2.7.2, since the one delivered w/ 24.3.1 was buggy,
;;     so I removed that one. (bug was the "no proper ls found").
;;     The bug is not solved anyway... (test w/ "C-x f C-f /frank@192.168.1.101:install.txt")
;;     For the manual tramp, cygwin shell has been used to manually run ./configure,
;;     which requires adding cygwin64/bin and emacs/bin to the cygwin path (which means that cygwin can run the windows emacs)

(defun my-load-tramp ()
  (let ((tramp-rootdir (concat user-emacs-directory "src/tramp/lisp/")))
    (when (file-directory-p tramp-rootdir)
      (add-to-list 'load-path tramp-rootdir)
      (require 'tramp)
      ))) ; (my-load-tramp) ; M-x tramp-version

;; ============================================================================
;; --- org mode
;;     The org mode version dl'ed into user-emacs-directory is 8.x, whereas emacs 24.3.1 has org mode version 7.9.3f,
;;     the config below is for 8.x and fails for 7.x; version mismatch and compiling did lead to errors,
;;     so I removed the emacs org mode version.
;;
;;     http://doc.norang.ca/org-mode.html http://www.suenkler.info/emacs-orgmode.html http://nflath.com/2010/03/org-mode-2/
;;     http://members.optusnet.com.au/~charles57/GTD/gtd_workflow.html
;;     http://www.gitwiki.org/Tests/org-mode.org http://nullman.net/tutorial/emacs-files/.emacs.d/kyle-init/orgrc.el.html
;;     http://lumiere.ens.fr/~guerry/u/emacs.el http://thomasf.github.io/solarized-css/test/org-hacks.html
;;
;;     quick-start emacs just for org mode capture: /usr/bin/emacsclient -n -e '(make-capture-frame)' :
;;       http://www.windley.com/archives/2010/12/capture_mode_and_emacs.shtml https://github.com/mrvdb/emacs-config/blob/master/mrb.org

(defun my-load-org ()
  (let ((org-rootdir (concat user-emacs-directory "src/org/lisp/")))
    (when (file-directory-p org-rootdir)
      (add-to-list 'load-path org-rootdir)
      (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
      (require 'org)

      (defun my-org-mode-hook ()
        (setq fill-column 120)
        (setq org-tags-column -100)

        (setq org-todo-keywords
              '((sequence "TODO(t)" "DOING(p/!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "DELEGATED(l@)") ; for me
                (sequence "TASK(k)"                           "|" "DONE(d!)" "CANCELED(c@)") ; for others
                ))
        (setq org-todo-keyword-faces
              '(("TODO"      . org-todo)
                ("DOING"     . isearch)
                ("WAIT"      . isearch-secondary)
                ("DONE"      . org-done)
                ("DELEGATED" . font-lock-comment-delimiter-face)
                ("CANCELED"  . font-lock-comment-face)
                ("TASK"      . org-todo)
                ))
        (setq org-tag-alist '((:startgroup . nil)
                              ("@work" . ?w) ("@home" . ?h) ("@errand" . ?e)
                              (:endgroup . nil)
                              ("mail" . ?m) ("phone" . ?p) ("meeting" . ?g)
                              ("laptop" . ?l) ("mypc" . ?y)
                              ("bug" . ?b) ("orga" . ?o) ("test" . ?t)
                              )
        (setq org-priority-faces
              '((?A . diff-removed)
                (?B . diff-changed)
                (?C . diff-added)))
        (setq org-agenda-deadline-faces
              '((1.0001 . org-warning)              ; due yesterday or before
                (0.0    . org-upcoming-deadline)))  ; due today or later

        ;; org-agenda-custom-commands -> shortcuts to jump to agenda items via e.g. state
        ;; org-todo-state-tags-triggers -> map between state change and tags
        ;; (setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))
        (org-set-regexps-and-options)   ; re-compute internal vars e.g. after changing org-todo-keywords - IMPORTANT

        (setq org-use-speed-commands t) ; single keys for point at begin of a headline or code block, ? for help
        (global-set-key (kbd "C-. .")        'org-mark-ring-push) ; seems to work the same as my-push-point, so...
        (global-set-key (kbd "C-. C-.")      'org-mark-ring-goto) ; ... use that instead, if we run org-mode
        (global-set-key [C-S-f10]            (my-defun-interactive (find-file org-default-notes-file)))

        (global-set-key (kbd "C-c c")        (make-sparse-keymap)) ; C-c C-c is already bound to kill-ring-save
        (global-set-key (kbd "C-c c a")      'org-agenda) ; org-occur-link-in-agenda-files
        (global-set-key (kbd "C-c c c")      'org-capture)
        (global-set-key (kbd "C-c c f")      'org-cycle-agenda-files)
        (global-set-key (kbd "C-c c l")      'org-store-link) ; also edit link, if cursor is on a link

        (local-set-key  (kbd "C-c C-c")      nil) ; I still like my kill-ring-save, even in org mode
        (local-set-key  (kbd "C-c c")        (my-inherit-prefix-map (current-global-map) (kbd "C-c c")))
        (local-set-key  (kbd "C-c c C-c")    'org-ctrl-c-ctrl-c) ; set tags in headline, or update information at point
        (local-set-key  (kbd "C-c c -")      'org-ctrl-c-minus)  ; insert table separator or modify bullet status
                                                                 ; or turn plain line or region into list items
        (local-set-key  (kbd "C-c c *")      'org-ctrl-c-star)   ; compute table, or change heading status of lines
        (local-set-key  (kbd "C-c c #")      'org-toggle-comment)
        (local-set-key  (kbd "C-c c C-l")    'org-insert-link)
        (local-set-key  (kbd "C-c c b")      'org-iswitchb)
        (local-set-key  (kbd "C-c c r")      'org-refile) ; refile entry or region to different location; C-c C-w already bound
        (local-set-key  (kbd "C-c c t")      'org-todo) ; cycle thru todo keywords; C-u C-u C-c C-t cycles thru keyword sets
        (local-set-key  (kbd "C-c c n")      'org-narrow-to-subtree) ; narrow buffer to current subtree / widen it again

        (local-set-key [tab]                 nil) ; stick w/ my default for tab and set C-tab for org-cycle below
        (local-set-key (kbd "<C-tab>")       (my-inherit-prefix-map (current-global-map) (kbd "<C-tab>")))
        (local-set-key (kbd "<C-tab> <C-tab>") 'org-cycle)
        ;; S-TAB                             org-shifttab -> org-global-cycle plus table movement
        ;; C-u num S-TAB                     show to level num
        ;; C-u     S-TAB                     show start visibility

        ;; RET                               org-return
        ;; M-RET                             org-meta-return - insert heading or insert table row
        ;; M-S-RET                           org-insert-todo-heading - insert heading w/ todo entry
        ;; M-<TAB>                           in-buffer completion - key-combo futily w/ win; TODO: test and bind to other
        ;; <C-S-return>                      org-insert-todo-heading-respect-content
        (local-set-key       [C-M-return]    'org-return-indent) ; default is "C-j", which is already bound
        (local-set-key       [C-M-S-return]  'org-insert-heading-respect-content) ; default is "C-RET", which is already bound
        (local-set-key       (kbd "C-v")     'org-yank)
        (local-set-key       (kbd "C-t")     (my-inherit-prefix-map (current-global-map) (kbd "C-t")))
        (local-set-key       (kbd "C-t e")   'org-transpose-element)
        (local-set-key       (kbd "C-k")     (my-inherit-prefix-map (current-global-map) (kbd "C-k")))
        (local-set-key       (kbd "C-k C-o") 'org-kill-line)

        (local-set-key       "*"             (my-defun-interactive (my-quote-region "*"))) ; bold
        (local-set-key       "/"             (my-defun-interactive (my-quote-region "/"))) ; italic
        (local-set-key       "_"             (my-defun-interactive (my-quote-region "_"))) ; underlined
        (local-set-key       "="             (my-defun-interactive (my-quote-region "="))) ; code
        (local-set-key       "~"             (my-defun-interactive (my-quote-region "~"))) ; verbatim
        (local-set-key       "+"             (my-defun-interactive (my-quote-region "+"))) ; strike-through

        (when t                         ; movement keys - http://orgmode.org/guide/Motion.html#Motion
          (local-set-key (kbd "C-M-<")       'outline-up-heading)
          (local-set-key (kbd "C-M-S-l")     'org-backward-heading-same-level)
          (local-set-key (kbd "C-M-l")       'org-forward-heading-same-level)
          (local-set-key (kbd "C-M-S-h")     'outline-previous-visible-heading)
          (local-set-key (kbd "C-M-h")       'outline-next-visible-heading)
          (local-set-key (kbd "C-M-:")       'org-backward-sentence)
          (local-set-key (kbd "C-M-.")       'org-forward-sentence)
          (local-set-key (kbd "C-M-S-e")     'org-backward-element) ; org-up-element / org-down-element
          (local-set-key (kbd "C-M-e")       'org-forward-element))

        (when t                  ; structure editing - http://orgmode.org/guide/Structure-editing.html#Structure-editing
          ;;                      the S-cursor and C-S-keys do hide CUA selection - so set region via mark in org mode
          ;; S-up / down          org-shiftup /down - decrease/increase prio or timestamp; also M-n / M-p
          ;; S-left / S-right     org-shiftleft /right - context dependent cycle at point, e.g. timestamp, todo set
          ;; C-S-down / C-S-up    org-shiftcontroldown /up - change timestamps synchronously down in CLOCK log lines
          ;; C-S-left / C-S-right org-shiftcontrolleft /right - cycle thru todo set
          ;; M-S-up / down        org-shiftmetaup /down - move subtree up/down (swap with previous/next subtree of same level)
          ;;                      hides scroll other window - also mapped to C-t s / C-t C-s below
          ;; M-S-left / M-S-right org-shiftmetaleft /right - promote/demote the current subtree by one level
          (local-set-key (kbd "C-t")         (my-inherit-prefix-map (current-global-map) (kbd "C-t"))) ;TODO: is global parent or outline?
          (local-set-key (kbd "C-t s")       'org-shiftmetadown) ; Move subtree up/down (swap with previous/next subtree of same level)
          (local-set-key (kbd "C-t C-s")     'org-shiftmetaup)   ;  also M-S-up / down
          (local-set-key (kbd "C-t o")       'org-metadown)      ; Move subtree up/down or move table row up/down
          (local-set-key (kbd "C-t C-o")     'org-metaup)        ;  also M-up / down (which hides scroll other window)

          (local-set-key (kbd "C-< <")       'org-shiftmetaleft)  ; Promote/demote current heading by one level.
          (local-set-key (kbd "C-< C-<")     'org-shiftmetaright)) ;  default M-left / right, which is hidden by winner mode
        )
      (add-hook 'org-mode-hook 'my-org-mode-hook)

      (setq org-directory "~/org")
      (setq org-default-notes-file (concat org-directory "/todo.org"))                ; open w/ C-S-f10
      (setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org"))) ; for org-todo-list
      ;;(setq org-archive-location "%s_archive::") ; put the archive in a separate file
      ;;(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
      ;; filemask for archive, might work w/ file-expand-wildcards: "^[^.#].*\\.\\(org$\\|org_archive$\\)"
      ;;(run-at-time "00:59" 3600 'org-save-all-org-buffers)
      ;;setq org-refile-targets
      (setq org-replace-disputed-keys t) ; disable the org mode bindings for shift+arrow keys
      (setq org-startup-folded nil)
      (setq org-startup-indented t)
      (setq org-use-fast-todo-selection t)
      (setq org-reverse-note-order t)
      (setq org-src-fontify-natively t) ; fontify code blocks in org files

      (setq org-capture-templates
            '(
              ;; --- Journal
              ("j" "journal" entry (file+datetree (concat org-directory "/journal.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("J" "journal (long)" entry (file+datetree (concat org-directory "/journal.org"))
               "* %?%i %^g\n  LINK: %a\n%U\n" :clock-in t :clock-resume t)
              ;; --- Links - can't use plain entries here, since I want to tag the links
              ;;             %x: get link from X-clipboard, doesn't work for win, whereas %c does; TODO: check for linux
              ("l" "links")
              ("lc" "programming communities" entry (file+headline (concat org-directory "/links.org") "Communities")
               "*** LIVE %?%i: %c %^g\n")
              ("lr" "programming references" entry (file+headline (concat org-directory "/links.org") "Reference, API")
               "*** LIVE %?%i: %c %^g\n")
              ("lt" "programming tutorials" entry (file+headline (concat org-directory "/links.org") "Tutorials")
               "*** LIVE %?%i: %c %^g\n")
              ("ls" "social" entry (file+headline (concat org-directory "/links.org") "Social")
               "*** LIVE %?%i: %c %^g\n")
              ("lf" "finance" entry (file+headline (concat org-directory "/links.org") "Finance")
               "*** LIVE %?%i: %c %^g\n")
              ("lp" "politics" entry (file+headline (concat org-directory "/links.org") "Politics")
               "*** LIVE %?%i: %c %^g\n")
              ;; --- Notes
              ("n" "note" entry (file (concat org-directory "/notes.org"))
               "* %? \n%U\n%a\n" :clock-in t :clock-resume t)
              ("N" "note (long)" entry (file (concat org-directory "/notes.org"))
               "* %?%i %^g\n%U\n%a\n" :clock-in t :clock-resume t)
              ;; --- Todos and tasks
              ("t" "todo" entry (file+headline (concat org-directory "/todo.org") "My tasks")
               "** TODO %?\n%U\n" :clock-in t :clock-resume t)
              ("T" "todo (w/ region)" entry (file+headline (concat org-directory "/todo.org") "My tasks")
               "** TODO %?%i\n  LINK: %a\n%U\n" :clock-in t :clock-resume t)
              ("k" "task" entry (file+headline (concat org-directory "/todo.org") "Others' tasks")
               "** TASK %^{assignee}p %?\n%U\n" :clock-in t :clock-resume t)
              ))
      ))))

(unless (and nil (featurep 'org) (fboundp 'my-org-mode-hook))
  (condition-case nil
      (my-load-org)
    (error t)))                         ; C-x e: (my-load-org) (my-org-mode-hook) ; also for reload

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
      (let ((yas-snippet-dirs (list (concat user-emacs-directory "snippets/") ; mine + https://github.com/dominikh/yasnippet-go
                                    (concat yas-rootdir "snippets/"))))
        (mapc 'yas-load-directory yas-snippet-dirs))
      ;; (yas-global-mode 1) ; don't enable globally, add (yas-minor-mode-on) to mode hook
      (setq hippie-expand-try-functions-list
            (cons 'yas-hippie-try-expand hippie-expand-try-functions-list)) ; make yas available via hippie-expand
      (define-key yas-minor-mode-map [(tab)]        nil)
      (define-key yas-minor-mode-map (kbd "TAB")    nil)
      (define-key yas-minor-mode-map (kbd "<C-tab>") (my-inherit-prefix-map (current-global-map) (kbd "<C-tab>")))
      (define-key yas-minor-mode-map "<C-tab> C-s"   'yas-expand)
      ;; probably not necessary, see binding for yas-keymap
      ;; (define-key yas-minor-mode-map "<C-tab> s"   'yas-next-field-or-maybe-expand)
      ;; (define-key yas-minor-mode-map "<C-tab> S"   'yas-prev-field)
      ;; not working
      ;; (make-variable-buffer-local 'yas-trigger-key)
      ;; (setq yas-trigger-key "<C-tab> C-s") ; TODO: currently not working, even if bound e.g. to F12; hippie + tab works
      ;; (setq yas-next-field-key "<C-tab> s")
      ;; (setq yas-prev-field-key "<C-tab> S")
      (setq yas-wrap-around-region t)

      ;; -- autoinsert

      (require 'autoinsert)
      ;; (auto-insert-mode); don't enable globally, add "(add-hook 'find-file-hook 'auto-insert)" per mode
      (setq auto-insert-directory autoinsert-dir)
      (setq auto-insert-query nil) ; don't be prompted before insertion

      ;; -- define autoinsert w/ yasnippets
      ;;    C-x e: (yas--get-template-by-uuid 'emacs-lisp-mode "car") ; to test, enter: car + TAB
      ;;    C-x e: (yas--get-template-by-uuid 'python-mode "autoinsert.py")

      (defun my-yas-expand-by-uuid (mode uuid)
        "Expand snippet template in MODE by its UUID."
        (yas-expand-snippet (yas--template-content (yas--get-template-by-uuid mode uuid))))

      (defun my-other-define-yas-auto-insert (condition mode template &optional after)
        "Set `auto-insert-alist' to expand SNIPPET-KEY at file creation."
        (add-to-list 'auto-insert-alist `(,condition . (lambda () (my-yas-expand-by-uuid ',mode ,template))) after))

      ;;;;;; TODO: zzzzzzzzzzzzzzzzzz : wont work here? move to their modes???
      ;; test auto inserts w/ 'M-x auto-insert' in respective buffer; defun my-load-yas
      ;; (my-other-define-yas-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)\\'" 'cc-mode "autoinsert.c")
      ;; (my-other-define-yas-auto-insert "\\.\\([Cc]\\|cc\\|cpp\\)\\'" 'cc-mode "autoinsert.c")
      ;; (my-other-define-yas-auto-insert "\\.el\\'" 'emacs-lisp-mode "autoinsert.el")
      ;; (my-other-define-yas-auto-insert "\\.go\\'" 'go-mode "autoinsert.go")
      ;; (my-other-define-yas-auto-insert "\\.js\\'" 'js-mode "autoinsert.js")
      ;; (my-other-define-yas-auto-insert "\\.py\\'" 'python-mode "autoinsert.py")
      ;; (my-other-define-yas-auto-insert "\\.\\(r\\|r3\\|red\\|reds\\)\\'" 'rebol-mode "autoinsert.r")

      (unless noninteractive (message "***Autoinsert and yasnippets [%s] support loaded." yas--version)))))

(unless (and (fboundp 'yas-global-mode) (fboundp 'auto-insert-mode))
  (condition-case nil
      (my-load-yas)
    (error t)))  ; C-x e: (my-load-yas) ; also for reload

;; ============================================================================
;; --- thingatpt - including function definitions

(when (require 'thingatpt)              ; standard emacs

  (defun my-change-number-at-point (change)
    (let ((number (number-at-point))
          (point (point)))
      (when number
        (progn
          (forward-word)
          (search-backward (number-to-string number))
          (replace-match (number-to-string (funcall change number)))
          (goto-char point)))))

  (defun my-increment-number-at-point () ; C-. +  see below
    "Increment number at point like Vim."
    (interactive)
    (my-change-number-at-point '1+))

  (defun my-decrement-number-at-point () ; C-. -  see below
    "Decrement number at point like Vim."
    (interactive)
    (my-change-number-at-point '1-))

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
;;                 switch on per major mode or use alias `wsm' as defined below

(when (require 'whitespace)             ; standard emacs

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

  (add-hook 'before-save-hook 'whitespace-cleanup t)) ; force whitespace cleanup; set APPEND flag, to run time-stamp 1st

;; ============================================================================
;; --- other vanilla packages w/o autoloads

(when (require 'ido)
  (ido-mode t)                            ; ido is an iswitch improvement
  (set-default 'imenu-auto-rescan t)
  (ido-everywhere t)                      ; ido e.g. also in dired mode, e.g. for C to copy files
  (setq ido-enable-flex-matching t        ; enable fuzzy matching
        ido-ignore-extensions t           ; make ido use completion-ignored-extensions
        ido-use-virtual-buffers t         ; ido also supports files from already closed buffers; uses recentf
        ido-use-filename-at-point 'guess) ; guess the context when using filename at point (ffap)
  ;;                                        disable ffap w/ ido-use-url-at-point
  ;;    ido-file-extensions-order           set per programming mode
  ;; TAB                Attempt to complete the input like the normal completing read functionality
  ;; C-a                Toggles showing ignored files (see ido-ignore-files)
  ;; C-b                Reverts to the old switch-buffer completion engine
  ;; C-c                Toggles if searching of buffer and file names should ignore case. (see ido-case-fold)
  ;; C-f                Reverts to the old find-file completion engine
  ;; C-d                open directory w/ dired
  ;; C-j                ido-select-text, select the buffer currently named by the prompt, which might be a directory as well
  ;; C-p                Toggles prefix matching; when on input will only match the beginning of a filename instead of any part of it
  ;; C-s / C-r          Moves to the next and previous match, respectively
  ;; C-t                Toggles matching by Emacs regular expression
  ;; Backspace          Deletes characters as usual or goes up one directory if it makes sense to do so
  ;; C-SPC              Restricts the completion list to anything that matches your current input
  ;; //                 ignore the preceding path, and go back to the top-most directory
  ;; ~/                 Jumps to the home directory
  ;; M-d                Searches for the input in all sub-directories to the directory you’re in
  ;; C-k                Kills the currently focused buffer or deletes the file depending on the mode
  ;; M-m                Creates a new sub-directory to the directory you’re in
  )

(require 'misc)                         ; C-k z zap-up-to-char; see below

(when (require 'webjump nil t)          ; C-c C-w; see below
  ;; see https://github.com/renard/webjump-plus-plus for version w/ query arg for current symbol, otherwise just C-c s
  (setq webjump-sites
        (append '(("stackoverflow" . "http://www.stackoverflow.com")
                  ("godoc" . "http://godoc.org/"))
                webjump-sites)))

;; ============================================================================
;; --- other optional packages

(when (require 'ace-jump-mode nil t))   ; C-c j, C-i j; see below

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
  ;; same for wrap-region, which I also tested and which didn't fix the `' issue.
  (electric-pair-mode -1))

(when (require 'hide-lines nil t))      ; C-c # hide-matching-lines, hide-non-matching-lines; see below

(when (require 'goto-last-change nil t)) ; C-c l; see below

(when (require 'rainbow-mode nil t))    ; M-x rainbow-mode per buffer; helpful e.g. in colortheme definition

(when (require 'smex nil t)
  (setq smex-save-file (concat user-emacs-directory ".smex.save")) ; Must be set before initializing smex
  (smex-initialize)
  (setq smex-prompt-string "M-smex "))

(when (require 'switch-window nil t))   ; overrides C-x o; also M-f5; see below

(when (require 'transpose-frame nil t)) ; rotate-frame-clockwise, flip-frame, ..., f8 keys; see below

(when (require 'winner nil 'noerror)    ; winner-undo, winner-redo - restore window configuration
  (setq winner-dont-bind-my-keys t)     ; don't use default bindings, use M-left / M-right; see below
  (winner-mode t))                      ; turn on the global minor mode

(let ((er-rootdir (concat user-emacs-directory "src/expand-region/")))
  (when (file-directory-p er-rootdir)
    (add-to-list 'load-path er-rootdir)
    (require 'expand-region)))          ; C-, keys; see below

(let ((mc-rootdir (concat user-emacs-directory "src/multiple-cursors/")))
  (when (file-directory-p mc-rootdir)
    (add-to-list 'load-path mc-rootdir)
    (require 'multiple-cursors)))       ; C-, keys; see below

;;; ###########################################################################
;;; --- OS, files, ...
;;; ###########################################################################

;; ============================================================================
;; --- find, grep, dired

(require 'grep)

(defun my-dired-mode-hook ()
  (fset 'dired-find-file 'dired-find-alternate-file) ; visit this file or directory instead of dired buffer
  (put 'dired-find-alternate-file 'disabled nil) ; allow dired to reuse its buffer

  (setq truncate-lines t       ; no continuation lines
        dired-dwim-target t    ; make copy automatically select the directory in other dired buffer in split window mode
        dired-recursive-copies 'ask
        dired-recursive-deletes 'ask)

  (defun my-dired-shell-command ()
    "Launch marked files or current file in default applications instead of emacs using `cmd /c start.'"
    (interactive)
    (dired-do-shell-command
     (case system-type
       ;;(gnu/linux "..."")
       (windows-nt "cmd /c start "))
     nil
     (dired-get-marked-files t current-prefix-arg)))

  (defun my-diredext-exec-git-command-in-shell (command &optional arg file-list) ; ###nokey
    "Run a shell command `git COMMAND`' on the marked files or current file."
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list ; feedback whether this file or marked files are used:
        (dired-read-shell-command "git command on %s: " current-prefix-arg files)
        current-prefix-arg
        files)))
    (unless (string-match "[*?][ \t]*\\'" command)
      (setq command (concat command " *")))
    (setq command (concat "git " command))
    (dired-do-shell-command command arg file-list)
    (message command))

  (defun my-dired-do-multi-occur (regexp)
    "Run `multi-occur' with REGEXP on all marked files."
    (interactive (list (read-regexp "Regexp: "))) ; will open *ALL* marked files
    (multi-occur (mapcar 'find-file-noselect (dired-get-marked-files)) regexp))

  (define-key dired-mode-map (kbd "C-M-o")
    (my-defun-interactive (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\""))))
  (define-key dired-mode-map (kbd "M-RET") 'my-dired-shell-command) ; ! is bound to dired-do-shell-command
  (define-key dired-mode-map (kbd "/") (lambda () (interactive) (find-alternate-file ".."))) ; open parent dir in current buffer
  (define-key dired-mode-map (kbd "C-/") 'dired-up-directory) ; this is replaced by the binding above, it is usually bound to ^
  (define-key dired-mode-map "o" 'my-dired-do-multi-occur)
  (define-key dired-mode-map (kbd "M-o") 'dired-find-file-other-window)

  ;; grep-find-ignored-directories grep-find-ignored-files
  ;; https://github.com/moljac024/emacs/blob/master/config/dired.el
  ;; start async from dired: Select the file you want to run, press & ENTER. same as dired-do-async-shell-command?

  (when (eq system-type 'windows-nt)
    (make-local-variable 'coding-system-for-read)
    (setq coding-system-for-read 'cp1251)))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

;; lots of fiddling w/ grep/find/gnuwin32 which brought up nothing but frustration;
;; instead now using ack.pl and my pyvimgrep.py below by setting it via grep-apply-setting in programming modes.
;; Working on win32:
;;   M-x lgrep                  prompted grep for current dir; using grep-template; works w/ ack; see below
;;   M-x rgrep                  prompted recursive grep; using grep-find-template; on win32 works w/ ack; see below
;;   M-x find-lisp-find-dired   Use find lisp library to find files for dired; bound below to C-x C-d
;;   M-x find-lisp-find-dired-subdirectories  Use find lisp library to find directories for dired; bound below to C-x C-D
;; Not working on win32, issues w/ GNU find:
;;   M-x find-dired             Run `find' and go into Dired mode for output - use find-lisp-find-dired instead
;;   M-x find-name-dired        Run `find' for file name matching os pattern and go into Dired mode for output
;;   M-x find-grep-dired        Run `find' for file containing regexp and go into Dired mode for output

;; -- dired mode keys
;; a / o                        open file in dired buffer / in other window
;; e | f | RET                  open file in new buffer
;; g                            refresh
;; i                            include dir listing in current buffer
;; k                            kill marked lines
;; C-u k                        remove dir listing from current buffer
;; m / u / t                    mark / unmark file / toggle marks
;; s                            to toggle sorting by name/date
;; x                            dired-do-flagged-delete - delete files with delete flag
;; A                            search regexp for marked files
;; Q                            query-replace for marked files, so supports replace for multiple buffers
;; =                            compare marked and current file
;; * c OLD NEW                  dired-change-marks - Replace all marks that use OLD with marks that use NEW
;; * .                          dired-mark-extension
;; % d                          dired-flag-file-deletion - flag for deletion
;; % m                          dired-mark - mark files; e.g. * c D * -> change delete flag to mark flag
;; % D                          dired-do-delete - delete marked files (not files w/ delete flag, which would be x)
;; % R / % C                    rename / copy - e.g. % R ^.*$ <RET> x-\& <RET> renames each selected file by prepending ‘x-’ to its name
;; % H / % S                    make hard-link / soft-link
;; % u / % l                    dired-upcase / dired-downcase
;; C-c C-r                      dired-toggle-read-only - will also call wdired-change-to-wdired-mode
;; C-c C-c                      wdired-finish-edit - Actually rename files based on your editing in the Dired buffer

;;; ###########################################################################
;;; --- Convenience settings
;;; ###########################################################################

(fset 'yes-or-no-p 'y-or-n-p)                         ; use y and n instead of 'yes' and 'no
(global-auto-revert-mode)                             ; reverts buffer when file changes on disk
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-defun  'disabled nil)                 ; C-x n d - elisp, cc-mode, go-mode
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)                 ; C-x n n - also see my-toggle-narrow
(put 'scroll-left 'disabled t)
(put 'scroll-right 'disabled t)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq apropos-do-all t)                               ; make apropos command search more
(setq backward-delete-char-untabify-method 'untabify) ; backward-delete-char-untabify turns tab to spaces, then deletes one space
(setq column-number-mode 1)                           ; show the column number in each modeline
(setq comment-fill-column nil)                        ; Use `fill-column' as column for `comment-indent'
(setq completion-auto-help 1)                         ; only show auto-completions buffer on 2nd tab-press if no match is found
(setq completion-ignored-extensions (append (list ".exe" ".i" ".s") completion-ignored-extensions))
(setq confirm-nonexistent-file-or-buffer t)           ; don't let switch-to-buffer create new buffers w/o asking
(setq cursor-in-non-selected-windows nil)             ; turn off cursors in non-selected windows
(setq delete-by-moving-to-trash t)                    ; use the system's trash can
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)                    ; Don't insert instructions in the *scratch* buffer
(setq lazy-highlight-initial-delay 0.5)
(setq line-number-mode 1)                             ; show the line number in each modeline
;; (setq next-line-add-newlines t)
(setq pop-up-windows nil)                             ; don't keep splitting windows for things
(setq query-replace-highlight t)                      ; Highlight during query
(setq read-buffer-completion-ignore-case t)           ; Ignore case when completing a buffer name
(setq read-file-name-completion-ignore-case t)        ; Ignore case when looking for a file
(setq search-highlight t)                             ; Highlight incremental search
(setq size-indication-mode t)                         ; show file size
(setq tab-always-indent t                             ; for indent-for-tab-command: hitting TAB always just indents the current line
      completion-cycle-threshold 5                    ; cycle for less than 5 candidates
      completion-ignore-case t)                       ; don't consider case significance for completion
(setq truncate-partial-width-windows nil)             ; same truncate column, if single or multiple windows
(setq view-read-only t)                               ; activate view-mode (and its hook below) for read-only buffer
(setq w32-get-true-file-attributes nil)               ; supposed to stop emacs from pausing all the time...
(setq-default comment-column 40)                      ; Column to indent right-margin comments to
(setq-default fill-column 120)                        ; text width per line
(setq-default indent-tabs-mode nil)                   ; spaces, not tabs
(setq-default require-final-newline t)                ; make sure file ends with newline
(setq-default sentence-end-double-space nil)          ; no two spaces in german
(setq-default sort-fold-case t)                       ; sort-lines ignoring case
(setq-default truncate-lines t)                       ; give each line of text just one screen line
(setq-default x-stretch-cursor t)                     ; when on a tab, make the cursor the tab length
;; paragraph-start and paragraph-separate
;; sentence-end - function or variable
(savehist-mode t)                                     ; keep minibuffer history between session; in savehist-file
(setq savehist-file (concat user-emacs-directory ".emacs.savehist"))
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(show-paren-mode t)                                   ; Show matching parens

(setq kill-emacs-query-functions                      ; Add Emacs close confirmation
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

(defun my-backup-region-or-buffer ()    ;###nokey - if providing a key for that, an extra key for write-region might also help
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

(defun my-buffer-narrowed-p (&optional buffer-or-name)
  "Return t if buffer narrowing is in effect for current buffer or optionally named buffer BUFFER-OR-NAME."
  (let ((buffer (if buffer-or-name
                      (or (get-buffer buffer-or-name)
                          (error  "Optional arg BUFFER-OR-NAME does not find buffer: `%S'"))
                    (current-buffer))))
    (with-current-buffer buffer
      (or (> (point-min) 1)
          (/= (buffer-size) (1- (point-max)))
          (/= (- (point-max) (point-min)) (buffer-size))))))

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

(when (eq system-type 'windows-nt)
  (defun my-cmd-exec ()                   ;###nokey
    "Run the current BAT file in a compilation buffer."
    (interactive)
    (save-buffer)
    (let ((compilation-buffer-name-function
           (function
            (lambda (ign)
              (concat "*" (buffer-file-name) "*")))))
      (compile (concat (w32-shell-name) " -c " (buffer-file-name))))))

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

(defun my-delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

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

(defun my-dired-jump (&optional other-window file-name) ; C-x j; see below
  "Jump to dired buffer corresponding to current buffer. Taken from dired-x.el; See there for more info."
  (interactive
   (list nil (and current-prefix-arg
                  (read-file-name "Jump to dired file: "))))
  (let* ((file (or file-name buffer-file-name))
         (dir (if file (file-name-directory file) default-directory)))
    (if (and (eq major-mode 'dired-mode) (null file-name))
        (progn
          (setq dir (dired-current-directory))
          (dired-up-directory other-window)
          (unless (dired-goto-file dir)
            ;; refresh and try again
            (dired-insert-subdir (file-name-directory dir))
            (dired-goto-file dir)))
      (if other-window
          (dired-other-window dir)
        (dired dir))
      (if file
          (or (dired-goto-file file)
              ;; refresh and try again
              (progn
                (dired-insert-subdir (file-name-directory file))
                (dired-goto-file file))
              ;; Toggle omitting, if it is on, and try again.
              (when dired-omit-mode
                (dired-omit-mode)
                (dired-goto-file file)))))))

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

(defun my-duplicate-region (begin end)
  "Insert a copy of the current region after the region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert (buffer-substring begin end))))

(defun my-duplicate-region-or-line (begin end)
  "Insert a copy of the current region after the region, iff region is marked, otherwise duplicate current line."
  (interactive "r")
  (if (region-active-p)
      (my-duplicate-region begin end)
    (my-duplicate-current-line)))

(defun my-eval-and-replace ()
  "Replaces the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-eval-region-or-sexp (begin end)
  "Eval region, iff region is marked, otherwise eval last sexp."
  (interactive "r")
  (if (region-active-p)
      (eval-region begin end)
    (eval-last-sexp nil)))

(defun my-copy-file-name-to-clipboard ()
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

(defun my-hex-strip-lead-int (str)
  (if (and (> (length str) 2) (string= (substring str 0 2) "0x"))
      (substring str 2) str))

(defun my-int-to-hex-int (int)
  (format "0x%X" int))

(defun my-hex-to-dec-int (str)
  (cond
   ((and (stringp str)
         (string-match "\\([0-9a-f]+\\)" (setf str (my-hex-strip-lead-int str))))
    (let ((out 0))
      (mapc
       (lambda (ch)
         (setf out (+ (* out 16)
                      (if (and (>= ch 48) (<= ch 57)) (- ch 48) (- ch 87)))))
       (coerce (match-string 1 str) 'list))
      out))
   ((stringp str) (string-to-number str))
   (t str)))                            ; (my-hex-to-dec-int "ABCD") (my-hex-to-dec-int "0xABCD")

(defun my-recompile-el ()               ;###nokey
  "Compile current buffer, if it's a lisp buffer and compiled file isn't uptodate."
  (interactive)
  (when (and buffer-file-name
             (string-match "/.*\\.el$"  buffer-file-name)
             (file-newer-than-file-p buffer-file-name
                                     (concat buffer-file-name "c"))
             (y-or-n-p (format "byte-compile %s? "
                               (file-name-nondirectory (buffer-file-name)))))
    (byte-compile-file buffer-file-name))) ; (add-hook 'after-save-hook 'my-recompile-el)

(defun my-replace-region-or-thing-int (arg replace-fn)
  (let ((curr (if (region-active-p)
                  (progn (when (> (point) (mark))
                           (exchange-point-and-mark))
                         (delete-and-extract-region (point) (mark)))
                (save-excursion
                  (let (start end)
                    (unless (looking-at "\\b" ) (forward-same-syntax -1)) (setq start (point))
                    (forward-same-syntax) (setq end (point))
                    (delete-and-extract-region start end))))))
    (insert (funcall replace-fn curr))))

(defun my-hex-to-dec-region-or-thing (arg)
  "Convert region, iff region is marked, otherwise current number at point to hexadecimal."
  (interactive "*p")
  (my-replace-region-or-thing-int arg (lambda (curr) (format "%d" (my-hex-to-dec-int curr))))) ; 0xABCD

(defun my-int-to-hex-region-or-thing (arg)
  "Convert region, iff region is marked, otherwise current hexadecimal number at point to decimal."
  (interactive "*p")
  (my-replace-region-or-thing-int arg (lambda (curr) (my-int-to-hex-int (string-to-number curr 10))))) ; 62123

(defun my-ido-goto-symbol-definition (&optional symbol-list)
  "Refresh imenu and jump to a given symbol's definition in the buffer using ido."
  (interactive)
  (unless (featurep 'imenu) (require 'imenu nil t))
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
               (my-ido-goto-symbol-definition (imenu--make-index-alist)) ; create index for definitions in current buffer
               (setq selected-symbol
                     (ido-completing-read "Symbol: " symbol-names nil nil nil nil (thing-at-point 'symbol)))
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
          (setq position (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun my-inherit-prefix-map (base-map key)
  (let ((base-prefix-map (lookup-key (current-global-map) key))
        (map (make-sparse-keymap)))
    (setq map (cons 'keymap base-prefix-map))
    map))

(defun my-indent-region-or-buffer (&optional noclean)
  "Indent region, iff region is marked, otherwise buffer; if not `noclean', untabify and delete trailing whitespace."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (unless noclean (untabify (region-beginning) (region-end)))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (unless noclean (whitespace-cleanup-region (region-beginning) (region-end))))))

(defun my-insert-file-at-point ()
  "Open file named at point and replace filename with its content."
  (interactive)                         ; will only work for filenames w/ '/', no '\' or '\\'
  (let ((bounds (bounds-of-thing-at-point 'filename))
        (filename (thing-at-point 'filename)))
    (delete-region (car bounds) (cdr bounds))
    (insert-file filename)))

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

(defun my-isearch-kill-whole-line ()
  "Kill matching line including its terminating newline and continue searching."
  (interactive)
  (save-excursion
    (beginning-of-line) (kill-whole-line))
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
          (push-mark (point) t nil)     ; push-mark-no-activate
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t)
          (isearch-search-and-update))
      (ding))))

(defun my-join-lines-or-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (join-line 1))))

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

(defun my-mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

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

(defun my-normalize-path (path)
  "Return path as absolute, canonized and with home directory replaced by '~'."
  (let* ((path (expand-file-name path))
         (home (expand-file-name (getenv "HOME"))))
    (replace-regexp-in-string home "~" path))) ; (my-normalize-path "c:/usr/home/.emacs.d/init.el")

(defvar my-point-stack nil
  "Stack to store point locations.")

(defun my-exchange-point-and-mark-no-activate () ; ###nokey
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil)) ; (define-key (current-global-map) [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

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

(defvar my-occur-h1-re nil
  "Language/buffer dependend regexp for my-occur-h1.")
(defvar my-occur-h2-re nil
  "Language/buffer dependend regexp for my-occur-h2.")
(defun my-occur-h1 () (interactive) (when my-occur-h1-re (occur my-occur-h1-re)))
(defun my-occur-h2 () (interactive) (when my-occur-h2-re (occur my-occur-h2-re)))

(when (eq system-type 'windows-nt)
  ;; TODO: check http://www.emacswiki.org/emacs/MsShellExecute and http://www.emacswiki.org/emacs/w32-browser.el
  (defun my-open-explorer ()
    "Launch the windows explorer in the current directory and selects current file."
    (interactive) ; see also w32-browser.el for more w32 shell stuff
    (w32-shell-execute
     "open" "explorer" (concat "/e,/select,"
                               (convert-standard-filename buffer-file-name))))

  (defun my-open-file (filename)
    "Open FILENAME using the appropriate application."
    ;; this doesn't work: (concat "start " (shell-quote-argument filename))
    ;; this might work: "cmd /c start "
    ;; this might work, not tested: (w32-shell-execute "open" (convert-standard-filename file))
    (shell-command (concat "rundll32 shell32,ShellExec_RunDLL " (shell-quote-argument filename))))

  (defun my-open-shell ()
    "Launch the windows cmd in the current directory and selects current file."
    (interactive)
    (w32-shell-execute
     "open" "cmd.exe" (concat "/k for %i in ("
                              (convert-standard-filename buffer-file-name)
                              ") do cd /d %~di%~pi"))))

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

(defun my-rename-current-buffer-file () ;###nokey
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

(defun my-reselect-last-region ()
  (interactive)
  (let ((start (mark t))
        (end (point)))
    (goto-char start)
    (call-interactively' set-mark-command)
    (goto-char end)))

(defun my-strip-control-m ()            ;###nokey
  "Remove ^M-."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun my-swap-line-down () ; my-swap-line-up would be same as transpose-lines; or check library line-move
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
    (widen) (kill-buffer)))

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

(defadvice query-replace                ; TODO: doesn't work
  (around replace-wrap
          (FROM-STRING TO-STRING &optional DELIMITED START END))
  "Execute a query-replace, wrapping to the top of the buffer after you reach the bottom."
  (save-excursion
    (let ((start (point)))
      ad-do-it
      (beginning-of-buffer)
      (ad-set-args 4 (list (point-min) start))
      ad-do-it)))

;;; ###########################################################################
;;; --- Programming modes
;;; ###########################################################################

;; ============================================================================
;; --- Base mode

(require 'compile)

(add-to-list 'load-path (concat user-emacs-directory "src/company"))
(when (require 'company nil 'noerror)
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-minimum-prefix-length 0)               ; autocomplete already after '.'
  (setq company-idle-delay 0.3)                        ; longer delay before autocompletion popup
  (setq company-echo-delay 0)                          ; removes annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  )

(require 'flycheck nil 'noerror)

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
  (window-configuration-to-register ?C) ; save window configuration for later restore after compile using alias rcc
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
  (when (and (eq system-type 'windows-nt) (display-graphic-p))
    (w32-send-sys-command #xf030)))
(add-hook 'after-setting-font-hook 'my-after-setting-font-hook)

;; ============================================================================
;; --- C / C++ / Makefile

(defun my-c-common-mode-hook ()
  (setq abbrev-mode t)
  (define-abbrev-table 'c-mode-abbrev-table
    '(("#is"   "#include <>"   "C-b") ; also snippet inc-1
      ("#ih"   "#include \"\"" "C-b") ; also snippet inc
      ("#ifn"  "#ifndef")
      ("#e"    "#endif")
      ("#ec"   "#endif /* */"  "C-3 C-b")
      ("#ifd"  "#ifdef")
      ("#id"   "#if defined")
      ("pdb"   "HALT();" nil 1)))
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  (setq-local my-occur-h1-re "^/// +=== +")
  (setq-local my-occur-h2-re "^// +--- +")

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
    (local-set-key (kbd "C-M-S-f")     'c-beginning-of-defun) ; also C-M-a
    (local-set-key (kbd "C-M-f")       'c-end-of-defun) ; also C-M-e
    (local-set-key (kbd "C-M-:")       'c-beginning-of-statement) ; also M-a
    (local-set-key (kbd "C-M-.")       'c-end-of-statement) ; also M-e, overrides regexp isearch
    (local-set-key (kbd "C-M-S-c")     'c-backward-conditional)
    (local-set-key (kbd "C-M-c")       'c-forward-conditional)
    (local-set-key (kbd "C-M-S-#")     'c-up-conditional)
    ;;(local-set-key (kbd "C-M-#")       'c-down-conditional)
    (local-set-key (kbd "C-M-S-#")     'c-down-conditional-with-else))
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

  (my-base-programming-mode-hook)
  (when (fboundp 'company-mode) (company-mode -1)) ;disable auto-complete
  (when (fboundp 'flycheck-mode) (flycheck-mode -1)) ;disable syntax-ckeck

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
  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --down 0 --filemask \"*.c;*.h\" --word ") ; grep
  (grep-apply-setting 'grep-find-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.c;*.h\" --word ") ; find-grep / grep-find
  (grep-apply-setting 'grep-template "ack --with-filename --nogroup --nocolor --no-recurse --type=cc <C> <R> <F>") ; lgrep
  (grep-apply-setting 'grep-find-template "ack --with-filename --nogroup --nocolor --type=cc <C> <R> <D>") ; rgrep
  (if (fboundp 'my-init-gtags-mode)     ; use `btags C .' in C directory, which will make gtags do the right thing
      (my-init-gtags-mode)
    (my-init-ctags-file-or-list "c")))
(add-hook 'c-mode-hook  'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (setq ido-file-extensions-order '(".cpp" ".hpp" ".cxx" ".hxx" ".c" ".h"  "" ".md"))
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table) ; '_' make no word boundary
  (modify-syntax-entry ?# "w" c++-mode-syntax-table) ; '#' make no word boundary; also required for #... abbrevs
  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --down 0 --filemask \"*.cpp;*.hpp\" --word ") ; grep
  (grep-apply-setting 'grep-find-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.cpp;*.hpp;*.cxx;*.hxx\" --word ") ; find-grep / grep-find
  (grep-apply-setting 'grep-template "ack --with-filename --nogroup --nocolor --no-recurse --type=cpp <C> <R> <F>") ; lgrep
  (grep-apply-setting 'grep-find-template "ack --with-filename --nogroup --nocolor --type=cpp <C> <R> <D>") ; rgrep
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

(defun my-elisp-mode-hook ()
  (setq abbrev-mode t)
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  (setq ido-file-extensions-order '(".emacs" ".el"))

  (setq-local my-occur-h1-re "^;;; +--- +")
  (setq-local my-occur-h2-re "^;; +--- +")

  (when (fboundp 'hide-lines)
    (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +;;"))
    (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
    (local-set-key (kbd "C-c C-#")     'show-all-invisible))

  (when (and nil (fboundp 'company-mode)) ;currently do not enable auto-complete for elisp
    (set (make-local-variable 'company-backends) '(company-elisp))
    (company-mode t)
    (local-set-key (kbd "<C-tab>")         (my-inherit-prefix-map (current-global-map) (kbd "<C-tab>")))
    (local-set-key (kbd "<C-tab> <C-tab>") 'company-complete-common))

  (make-local-variable 'hippie-expand-try-functions-list)
  ;;try-expand-list ; not so far on top; elisp/TeX
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)

  (local-set-key [return]              'newline-and-indent)
  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
  (local-set-key [f9]                  'eval-buffer) ; default C-M-x is eval-defun ; eval-region
  (local-set-key [C-f9]                'eval-buffer) ; f9 currently (24-Jun-12) not recognized
  (local-set-key [M-f9]                (my-defun-interactive (byte-compile-file (buffer-file-name)))) ; compile
  (local-set-key [C-M-f9]              (my-defun-interactive (byte-compile-file (buffer-file-name) t))) ; compile & load
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here

  ;; (grep-apply-setting 'grep-command "ack --with-filename --nogroup --nocolor --type=elisp -i ") ; doesn't work w/o manual adding of '.'
  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --down 0 --filemask *.el --word ") ; grep
  (grep-apply-setting 'grep-find-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask *.el --word ") ; find-grep / grep-find
  (grep-apply-setting 'grep-template "ack --with-filename --nogroup --nocolor --no-recurse --type=elisp <C> <R> <F>") ; lgrep
  (grep-apply-setting 'grep-find-template "ack --with-filename --nogroup --nocolor --type=elisp <C> <R> <D>") ; rgrep
  (my-base-programming-mode-hook)
  (when (fboundp 'company-mode) (company-mode -1)) ;disable auto-complete
  (when (fboundp 'flycheck-mode) (flycheck-mode -1)) ;disable syntax-ckeck

  (when (display-graphic-p)
    (font-lock-add-keywords nil `(("\\<lambda\\>"
                                   (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                                             ,(make-char 'greek-iso8859-7 107))
                                             nil))))))

  (my-init-ctags-file-or-list "elisp"))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

;; ;; ============================================================================
;; ;; --- Python zzzzzzzzzzzzzzzzzz - get Py mode running

;; ;; py-execute-buffer does not correctly handle the comment w/ the encoding
;; ;;  'set-buffer-process-coding-system' could help, but how to know the correct one? This is file dependent...
;; ;; Or: enter a line 1 with a shebang, like: #! /bin/env python.
;; ;;  Otherwise python-mode inserts one plus an empty line, with encoding comment in line 3 and hence not parsed by Python

;; (add-to-list 'load-path (concat user-emacs-directory "src/python"))
;; (require 'python-mode)
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (defun my-python-mode-hook ()
;;   (setq abbrev-mode t)
;;   (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
;;   (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

;;   (setq ido-file-extensions-order '(".py"))
;;   (add-hook 'font-lock-mode-hook
;;             (lambda ()
;;               (font-lock-add-keywords
;;                'python-mode
;;                '(("\\<\\(pdb.set_trace\\)" 1 font-lock-warning-face prepend))
;;                'set)))

;;   (local-set-key [return]              'newline-and-indent) ; py-newline-and-indent
;;   (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
;;   (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
;;   (local-set-key (kbd "C-j")           (my-defun-interactive (join-line 1))) ; overriden in python mode
;;   (local-set-key (kbd "C-S-j")         (my-defun-interactive (join-line))) ; overriden in python mode
;;   (local-set-key [tab]                 'my-hippie-smart-tab) ; py-indent-region behaves strange...
;;   (local-set-key [backtab]             'py-shift-region-left) ; other modes: my-hippie-unexpand
;;   (local-set-key [(control shift tab)] 'py-shift-region-right) ; other modes: tab-to-tab-stop; still M-i
;;   (local-set-key [f9]                  'py-execute-buffer)
;;   (local-set-key [C-f9]                'py-execute-buffer) ; f9 currently (24-Jun-12) not recognized
;;   (local-set-key [S-f9]                'py-execute-region)
;;   (local-set-key [C-S-f9]              'recompile) ; no prompt required here

;;   (when (require 'pydoc-info nil 'noerror) ; https://bitbucket.org/jonwaltman/pydoc-info/src/default/pydoc-info.el
;;     (info-lookup-add-help
;;      :mode 'python-mode
;;      :parse-rule 'pydoc-info-python-symbol-at-point
;;      :doc-spec
;;      '(("(python)Index" pydoc-info-lookup-transform-entry)
;;        ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry))))

;;   (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --down 0 --filemask *.py --word ") ; --start
;;   (grep-apply-setting 'grep-find-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask *.py --word ") ; find-grep / grep-find
;;   (grep-apply-setting 'grep-template "ack --with-filename --nogroup --nocolor --no-recurse --type=py <C> <R> <F>") ; lgrep
;;   (grep-apply-setting 'grep-find-template "ack --with-filename --nogroup --nocolor --type=py <C> <R> <D>") ; rgrep
;;   (my-base-programming-mode-hook 4)
;;   (when (fboundp 'company-mode) (company-mode -1)) ;disable auto-complete
;;   (when (fboundp 'flycheck-mode) (flycheck-mode -1)) ;disable syntax-ckeck

;;   (my-init-ctags-file-or-list "python") ; TODO: laaaarge
;;   ;; M-x pylint
;;   ;; (local-set-key (kbd "<C-tab>")          (my-inherit-prefix-map (current-global-map) (kbd "<C-tab>")))
;;   ;; (local-set-key (kbd "<C-tab> <C-tab>")  'py-complete) ; (setq py-complete-function 'py-completion-at-point)
;;   ;; py-comment-region
;;   ;; http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
;;   (setq pdb-path "py.bat DBG")          ; M-x pdb ==> py.bat DBG ==> python -m pdb myfile.py
;;   ;; py-which-shell, py-toggle-shells py-default-interpreter
;;   ;; (setq py-shell-name "py.bat RUN") ; py-shell-toggle-1 ; py-shell-toggle-2
;;   ;; when setting py-shell-name, probably not required: python-python-command + python-python-command-args
;;   ;; py-python-command-args
;;   ;; py-fontify-shell-buffer-p t
;;   ;; (setq py-cleanup-temporary nil)     ; dont delete temp files (exec region only?)
;;   (setq py-mark-decorators t)
;;   (setq py-start-run-py-shell nil)       ; don't start python shell w/ python mode
;;   (setq py-start-run-ipython-shell nil)) ; same for ipython shell
;; (add-hook 'python-mode-hook 'my-python-mode-hook)

;; ============================================================================
;; --- Javascript mode - the standard mode available since Emacs 23.2

(defun my-js-mode-hook ()
  (setq abbrev-mode t)
  (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
  (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

  (setq-local my-occur-h1-re "^/// +=== +")
  (setq-local my-occur-h2-re "^// +--- +")

  (when (fboundp 'hide-lines)           ; poor man's hide-comnt.el - only hide lines starting w/ //
    (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +//"))
    (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
    (local-set-key (kbd "C-c C-#")     'show-all-invisible))

  (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
  (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
  (local-set-key [M-j]                 'c-indent-new-comment-line)
  (local-set-key "*"                   (my-defun-interactive (my-quote-region "*" "/*" "*/")))
  (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
  (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
  (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here
  (local-set-key "\C-xm"               'recompile) ; no prompt required here
  (local-set-key [M-f9]                'kill-compilation) ; also C-c C-k

  (when t ; overide/extend movement keys
    (local-set-key (kbd "C-M-S-f")     'c-beginning-of-defun) ; also C-M-a
    (local-set-key (kbd "C-M-f")       'c-end-of-defun) ; also C-M-e
    (local-set-key (kbd "C-M-:")       'c-beginning-of-statement) ; also M-a
    (local-set-key (kbd "C-M-.")       'c-end-of-statement)) ; also M-e, overrides regexp isearch
  (when t ; overide/extend mark keys
    (local-set-key (kbd "s-f")         'c-mark-function))

  (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --down 0 --filemask \"*.js\" --word ") ; grep
  (grep-apply-setting 'grep-find-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.js\" --word ") ; find-grep / grep-find
  (grep-apply-setting 'grep-template "ack --with-filename --nogroup --nocolor --no-recurse --type=js <C> <R> <F>") ; lgrep
  (grep-apply-setting 'grep-find-template "ack --with-filename --nogroup --nocolor --type=js <C> <R> <D>") ; rgrep
  (my-base-programming-mode-hook 4)
  (when (fboundp 'company-mode) (company-mode -1)) ;disable auto-complete
  (when (fboundp 'flycheck-mode) (flycheck-mode -1)) ;disable syntax-ckeck

  (when (display-graphic-p)
    (font-lock-add-keywords nil `(("\\<function\\>"
                                   (0 (progn (compose-region (match-beginning 0) (match-end 0) "ƒ")
                                             nil))))))

  (setq c-basic-offset 4)
  (setq c-tab-always-indent t)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; '_' make no word boundary
  (setq comment-start "//" comment-end ""
        comment-multi-line t)
  (setq-default c-electric-flag t)      ; make electric actions (reindentation,...) happen for electric keys or keywords
  (my-init-ctags-file-or-list "javascript"))
(add-hook 'js-mode-hook 'my-js-mode-hook)

;; ============================================================================
;; --- HTML mode; http://web-mode.org/ might be helpful here

(defun my-html-mode-hook ()

  (set (make-local-variable 'sgml-basic-offset) 4) ; default indentation is 4, but...
  (sgml-guess-indent)                              ; let emacs guess

  (defun my-sgml-insert-gt ()
    "Inserts a `>' character and calls `my-sgml-close-tag-if-necessary', leaving point where it is."
    (interactive)
    (insert ">")
    (save-excursion (my-sgml-close-tag-if-necessary)))

  (defun my-sgml-close-tag-if-necessary ()
    "Calls `sgml-close-tag' if tag immediately before point is an opening tag not followed by a matching closing tag."
    (when (looking-back "<\\s-*\\([^<> \t\r\n]+\\)[^<>]*>")
      (let ((tag (match-string 1)))
        (unless (and (not (sgml-unclosed-tag-p tag))
                     (looking-at (concat "\\s-*<\\s-*/\\s-*" tag "\\s-*>")))
          (sgml-close-tag)))))

  ;; JS keys
  (when t ; overide/extend movement keys
    (local-set-key (kbd "C-M-S-f")     'c-beginning-of-defun) ; also C-M-a
    (local-set-key (kbd "C-M-f")       'c-end-of-defun) ; also C-M-e
    (local-set-key (kbd "C-M-:")       'c-beginning-of-statement) ; also M-a
    (local-set-key (kbd "C-M-.")       'c-end-of-statement)) ; also M-e, overrides regexp isearch
  (when t ; overide/extend mark keys
    (local-set-key (kbd "s-f")         'c-mark-function))

  ;; HTML keys - more can be done w/ snippets
  (define-key html-mode-map      ">"        'my-sgml-insert-gt) ; or use sgml-tag or use sgml-close-tag
  ;; C-c C-a sgml-attributes; interactively insert attribute values for the current tag
  ;; C-c C-d sgml-delete-tag; delete the tag at or after point, and delete the matching tag too
  ;; C-c C-e sgml-close-tag
  ;; C-c C-n sgml-name-char; insert a symbolic character name for character entered
  ;; C-c C-t sgml-tag; interactively specify a tag and its attributes
  (define-key html-mode-map (kbd "C-M-t")   'sgml-skip-tag-forward) ; default is C-c C-f
  (define-key html-mode-map (kbd "C-M-S-t") 'sgml-skip-tag-backward) ; default is C-c C-b
  ;;;(define-key html-mode-map (kbd "ZZZ") 'sgml-tags-invisible) ; toggle visibility of tags in buffer -> preview
  )
(add-hook 'html-mode-hook 'my-html-mode-hook) ; (my-html-mode-hook)

;; ;; ============================================================================
;; ;; --- Golang - zzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
;; ;;     Go mode from Go distribution
;; ;;     yasnippets from https://github.com/dominikh/yasnippet-go
;; ;;       manually installed below ~/.emacs.d/snippets/go-mode
;; ;;     See also: http://honnef.co/posts/2013/03/writing_go_in_emacs/

;; (require 'go-mode)                      ;go-mode-load

;; (defun my-go-mode-hook ()
;;   (setq abbrev-mode t)
;;   (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
;;   (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

;;   (when (require 'go-errcheck nil 'noerror)) ;orig doesn't work yet for windows - changed the .el file manually
;;                                         ;this works: "errcheck github.com/FrankRuben/frupid"
;;   (when (and nil                        ;currently disabled - somehow spoils me unit tests by generating its own
;;              (fboundp 'flycheck-mode)   ;requires flycheck - https://github.com/lunaryorn/flycheck
;;              (require 'go-flycheck nil 'noerror))
;;     (flycheck-mode))                        ;use flycheck & go-flycheck, if available

;;   (when (and (fboundp 'company-mode)        ;enable auto-complete for Go
;;              (require 'company-go nil 'noerror)) ;https://github.com/nsf/gocode/blob/master/emacs-company/company-go.el
;;     (set (make-local-variable 'company-backends) '(company-go)) ;  company-abbrev company-dabbrev-code company-dabbrev
;;     (company-mode t)
;;     (local-set-key (kbd "<C-tab>")         (my-inherit-prefix-map (current-global-map) (kbd "<C-tab>")))
;;     (local-set-key (kbd "<C-tab> <C-tab>") 'company-complete-common)) ; see also above in my-hippie-smart-tab

;;   (set (make-local-variable 'compile-command)
;;        (format "go build %s" (file-name-nondirectory buffer-file-name)))

;;   (setq-local my-occur-h1-re "^/// +=== +")
;;   (setq-local my-occur-h2-re "^// +--- +")

;;   (when (fboundp 'hide-lines)           ; poor man's hide-comnt.el - only hide lines starting w/ //
;;     (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +//"))
;;     (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
;;     (local-set-key (kbd "C-c C-#")     'show-all-invisible))

;;   (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
;;   (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
;;   (local-set-key "*"                   (my-defun-interactive (my-quote-region "*" "/*" "*/")))
;;   (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
;;   (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
;;   (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
;;   (local-set-key [C-S-f9]              'recompile) ; no prompt required here
;;   (local-set-key "\C-xm"               'recompile) ; no prompt required here
;;   (local-set-key [M-f9]                'kill-compilation) ; also C-c C-k
;;   (local-set-key [C-M-f9]              'go-errcheck)
;;   (local-set-key [C-f1]                'godef-describe)   ; requires code.google.com/p/rog-go/exp/cmd/godef
;;   (local-set-key [C-S-f1]              'godoc)
;;   (local-set-key [M-.]                 'godef-jump) ; M-.; M-* (pop-tag-mark) jumps back to previous position
;;   (when (boundp 'my-alt-o-map)
;;     (define-key my-alt-o-map (kbd "M-.") 'godef-jump-other-window))

;;   (when t ; overide/extend movement keys
;;     (local-set-key (kbd "C-M-S-f")     (my-defun-interactive (go-beginning-of-defun))) ; also C-M-a
;;     (local-set-key (kbd "C-M-f")       (my-defun-interactive (go-end-of-defun))))      ; also C-M-e

;;   ;; go-import-add, go-remove-unused-imports, go-goto-imports
;;   ;; go-play-buffer' `go-play-region' `go-download-play'
;;   ;; (progn (gofmt) (my-indent-region-or-buffer))
;;   ;; open godoc in browser - see webjump - godoc

;;   (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --down 0 --filemask \"*.go\" --word ") ; grep
;;   (grep-apply-setting 'grep-find-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.go\" --word ") ; find-grep / grep-find
;;   (grep-apply-setting 'grep-template "ack --with-filename --nogroup --nocolor --no-recurse --type=go <C> <R> <F>") ; lgrep
;;   (grep-apply-setting 'grep-find-template "ack --with-filename --nogroup --nocolor --type=go <C> <R> <D>") ; rgrep
;;   (my-base-programming-mode-hook 4)

;;   (setq comment-multi-line t)
;;   (setq tags-table-list nil)
;;   ;; write go file so that it looks like everyone's - but display as I like it; but not:
;;   ;; can't add tab-setting for gofmt and re-formatting after the save changes cursor pos
;;   ;; (add-hook 'before-save-hook (lambda () (gofmt-before-save) (my-indent-region-or-buffer)))
;;   )
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;; ;; ============================================================================
;; ;; --- Rebol mode
;; ;;     Rebol mode from Github

;; (add-to-list 'auto-mode-alist '("\\.\\(r\\|r3\\|red\\|reds\\)\\'" . rebol-mode))
;; (require 'rebol)
;; (defun my-rebol-mode-hook ()
;;   (set (make-local-variable 'time-stamp-start  "Date:?\\s-*"))
;;   (set (make-local-variable 'time-stamp-end    "\\s-*$"))
;;   (set (make-local-variable 'time-stamp-format "%02d-%3b-%:y %02H:%02M:%02S"))

;;   (setq abbrev-mode t)
;;   (when (fboundp 'auto-insert) (add-hook 'find-file-hook 'auto-insert))
;;   (when (fboundp 'yas-minor-mode-on) (yas-minor-mode-on))

;;   ;; (set (make-local-variable 'compile-command)
;;   ;;      (format "go build %s" (file-name-nondirectory buffer-file-name)))

;;   (setq-local my-occur-h1-re "^;;; +--- +")
;;   (setq-local my-occur-h2-re "^;; +--- +")

;;   (when (fboundp 'hide-lines)           ; poor man's hide-comnt.el - only hide lines starting w/ //
;;     (defun my-hide-comment-lines () (interactive) (hide-matching-lines "^ +;"))
;;     (local-set-key (kbd "C-c #")       'my-hide-comment-lines)
;;     (local-set-key (kbd "C-c C-#")     'show-all-invisible))

;;   (local-set-key [S-return]            (my-defun-interactive (end-of-line) (newline-and-indent)))
;;   (local-set-key [C-M-return]          'reindent-then-newline-and-indent)
;;   (local-set-key "*"                   (my-defun-interactive (my-quote-region "*" "/*" "*/")))
;;   (local-set-key "'"                   (my-defun-interactive (my-quote-region "'" "`" "'")))
;;   (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
;;   (local-set-key "\C-x\C-m"            'compile)   ; allows C-x C-m C-m (RET) to accept compile prompt
;;   (local-set-key [C-S-f9]              'recompile) ; no prompt required here
;;   (local-set-key "\C-xm"               'recompile) ; no prompt required here
;;   (local-set-key [M-f9]                'kill-compilation) ; also C-c C-k

;;   (grep-apply-setting 'grep-command "py.bat run %home%\\bin\\pyvimgrep.py --down 0 --filemask \"*.r;*.red;*.redl\" --word ") ;grep
;;   (grep-apply-setting 'grep-find-command "py.bat run %home%\\bin\\pyvimgrep.py --filemask \"*.r;*.red;*.redl\" --word ") ; find-grep / grep-find
;;   ;; no ack type for rebol/Red yet; the following is untested
;;   (grep-apply-setting 'grep-template "ack --with-filename --nogroup --nocolor --no-recurse --type-set r=.r --type=rebol <C> <R> <F>") ; lgrep
;;   (grep-apply-setting 'grep-find-template "ack --with-filename --nogroup --nocolor --type-set r=.r --type=rebol <C> <R> <D>") ; rgrep
;;   (my-base-programming-mode-hook 4)
;;   (when (fboundp 'company-mode) (company-mode -1)) ;disable auto-complete
;;   (when (fboundp 'flycheck-mode) (flycheck-mode -1)) ;disable syntax-ckeck

;;   ;; (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; '_' make no word boundary
;;   (setq comment-start ";" comment-end ""
;;         comment-multi-line t)
;;   (setq tags-table-list nil))
;; (add-hook 'rebol-mode-hook 'my-rebol-mode-hook)

;; ============================================================================
;; --- SQL

(defun my-sql-mode-hook ()
  (setq abbrev-mode t)
  (local-set-key [C-f9]                'compile) ; f9 currently (24-Jun-12) not recognized
  (local-set-key [C-S-f9]              'recompile) ; no prompt required here

  (my-base-programming-mode-hook 4 nil 79)
  (when (fboundp 'company-mode) (company-mode -1)) ;disable auto-complete
  (when (fboundp 'flycheck-mode) (flycheck-mode -1))) ;disable syntax-ckeck
(add-hook 'sql-mode-hook 'my-sql-mode-hook)

;; ============================================================================
;; --- EShell

(defun my-eshell-mode-hook ()
  (local-set-key (kbd "C-u") 'eshell-kill-input) ;
  (local-set-key (kbd "C-M-o")          ; start Explorer for current dir
                 (my-defun-interactive (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\""))))
  )
(add-hook 'eshell-mode-hook 'my-eshell-mode-hook)

;; ============================================================================
;; --- Win32

(when (and (eq system-type 'windows-nt)
           (not (fboundp 'ntcmd-mode))
           (require 'ntcmd nil t))
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

(define-key cua--rectangle-keymap    (kbd "C-SPC") 'cua-rotate-rectangle) ; cycle rectangle corners
                                        ; default (RETURN, \r) doesn't work

;; M-up/M-down/M-left/M-right   move the entire rectangle overlay (but not the contents)
;; [C-return]           cancels the rectangle
;; [C-space]            activates the region bounded by the rectangle

;; If you type a normal (self-inserting) character when the rectangle is
;; active, the character is inserted on the "current side" of every line
;; of the rectangle.  The "current side" is the side on which the cursor
;; is currently located. If the rectangle is only 1 column wide,
;; insertion will be performed to the left when the cursor is at the
;; bottom of the rectangle.

;; cua-mode's rectangle support also includes all the normal rectangle functions with easy access:
;; C-M-up/C-M-down      scrolls the lines INSIDE the rectangle up and down; lines scrolled outside
;;                      the top or bottom of the rectangle are lost, but can be recovered using [C-z].
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle by the amount given by the numeric prefix
;;                      argument (default 1). It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles virtual straight rectangle edges
;; [M-P] inserts tabs and spaces (padding) to make real straight edges
;; [M-q] performs text filling on the rectangle
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

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
(define-key isearch-mode-map         "\C-kl"          'my-isearch-kill-whole-line) ; kill line w/ match and continue search
(define-key isearch-mode-map         "\C-km"          'my-isearch-kill-match) ; kill search match and continue search
(define-key isearch-mode-map         "\C-kz"          'my-isearch-zap-up-to)  ; kill from mark to *before* match

(define-key isearch-mode-map         [(control return)] 'my-isearch-exit-before-match) ; RETURN quits search AFTER match
(if (display-graphic-p)                 ; comment line w/ match and continue search
    (define-key isearch-mode-map     (kbd  "C-#")     'my-isearch-comment-line)
  (define-key isearch-mode-map        "\C-\\"    'my-isearch-comment-line))
(define-key isearch-mode-map         (kbd  "C-o")     'my-isearch-occur) ; run occur for current search term; already M-S-o
(define-key isearch-mode-map         (kbd  "C-v")     'isearch-yank-pop) ; make C-v also work in search mode, already M-y
;;                                         "M-o"       facemenu-set-face
(define-key isearch-mode-map         (kbd  "M-z")     'my-isearch-zap-up-to) ; see "\C-kz"
(define-key isearch-mode-map         (kbd  "<C-tab>") 'isearch-complete-edit) ; default "\M-\t" doesn't work for Windows

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
;; (Isearch) M-TAB      Complete the current search string against all previous search strings. (isearch-complete-edit)
;; (Isearch) M-r        Toggle between regular-expression searching and literal-string searching.
;; (Isearch) M-e        Pause to edit the search string. Resume search with C-j or C-s or C-r.
;; (Isearch) M-s SPC    (24.3) Toggles lax space matching for ordinary and regexp Isearch (replace-lax-whitespace).
;; (Isearch) M-s _      (24.3) Toggles symbol search mode.
;; (Isearch) M-c / M-s c  (24.3) Toggles search case-sensitivity.
;; (Isearch) M-s w      Toggle word search mode (isearch-toggle-word).
;; (Isearch) M-s h r    isearch-highlight-regexp; exit isearch and highlight regexp from current search string
;; (Global) M-s h u     unhighlight-regexp; Unhighlight regular expression (see also lazy-highlight-cleanup)
;; (Global) M-s w       isearch-forward-word
;; (Global) M-s _       Starts a symbol (identifier) incremental search (isearch-forward-symbol).
;; (Global/Isearch) M-% Start interactive query replace using the current string. (query-replace)
;; (Global/Isearch) M-s o  Call occur with current search string
;; (Global) C-u M-%     query-replace for word (respecting word boundaries)
;; (Global) C-M-%       Start reg-exp query replace using the current search string (query-replace-regexp)

;; (smex) C-h f         while Smex is active, runs describe-function on the currently selected command.
;; (smex) M-.           jumps to the definition of the selected command.
;; (smex) C-h w         shows the key bindings for the selected command. (Via where-is.)

;; ============================================================================
;; --- Special keys
;;     <pause> also available

(if smex-initialized-p
    (progn (global-set-key [apps]     'smex) ; C-s/left / C-r/right scroll; execute-extended-command still available as M-x
           (global-set-key [M-apps]   'smex-major-mode-commands))
  (global-set-key          [apps]     'execute-extended-command)) ; M-x alternative
(global-set-key            [S-apps]   'eval-expression)           ; M-: alternative; C-u M-: inserts results into buffer
(global-set-key            [C-apps]   'my-ido-goto-symbol-definition)
(global-set-key            [C-S-apps] 'imenu)

;; ============================================================================
;; --- C-, ? -> 2-key commands, mapped to super-modifier; mark region & multi-cursor edit

(when t
  (define-key function-key-map        (kbd "C-,")      'event-apply-super-modifier)

  (global-set-key                     (kbd "s-b")      'mark-whole-buffer) ; also C-c a
  (global-set-key                     (kbd "s-f")      'mark-defun)        ; overriden below in c-mode, also C-M-h
  (global-set-key                     (kbd "s-g")      'mark-page)         ; also C-x C-p
  (global-set-key                     (kbd "s-l")      'my-mark-line)
  (global-set-key                     (kbd "s-p")      'mark-paragraph)    ; also M-h
  (global-set-key                     (kbd "s-w")      'mark-word) ; mark word starting at point; also M-@
  (global-set-key                     (kbd "s-x")      'mark-sexp) ; also C-M-SPC
  (global-set-key                     (kbd "s-.")      'mark-end-of-sentence)

  (when (fboundp 'er/expand-region)
    ;; mark functions from er/expand; see https://github.com/magnars/expand-region.el
    (global-set-key                   (kbd "s-#")      'er/mark-comment)
    (global-set-key                   (kbd "s-c")      'er/mark-method-call)
    (global-set-key                   (kbd "s-s")      'er/mark-symbol)
    (global-set-key                   (kbd "s-w")      'er/mark-word)        ; mark word around point
    ;; TODO: er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs
    (global-set-key                   (kbd "s-+")      'er/expand-region)    ; +/-/0 - expand/contract/reset
    (global-set-key                   (kbd "s--")      'er/contract-region)) ; +/-/0 - expand/contract/reset

  ;; C-, XXX - multi cursor keys; defalias for multiple-cursors-mode below
  (when (fboundp 'mc/edit-lines)
    (global-set-key                   (kbd "s-<return>")   'set-rectangular-region-anchor) ; Add cursors to multiple lines
    (global-set-key                   (kbd "s-C-<return>") 'mc/edit-lines)                 ; Add cursors to each line of region
    (global-set-key                   (kbd "s-<down>") 'mc/mark-next-like-this)            ; useful w/ er/expand-region
    (global-set-key                   (kbd "s-<up>")   'mc/mark-previous-like-this)
    (global-set-key                   (kbd "s-,")      'mc/mark-all-like-this) ; Marks all parts of the buffer that matches the current region
    (global-set-key                   (kbd "s-;")      'mc/mark-all-like-this-dwim) ; Tries to be smart about marking everything you want. Can be pressed multiple times
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

  (global-set-key                    (kbd "C-.")      my-ctrl-point-map) ; C-. prefix

  ;; C-SPC                           toggle mark and start region
  ;;   C-u C-SPC, C-- C-SPC          (cua-set-mark 1) - goto last mark and pop it
  ;;   C-SPC C-SPC                   set the mark but disable the region (it's a toggle, so use twice to set and continue)
  ;; C-S-SPC                         toggle global mark; w/ global mark, copy/yank will be inserted at global mark
  (define-key my-ctrl-point-map      (kbd "SPC")      'exchange-point-and-mark) ; standard C-x C-x overriden
  (define-key my-ctrl-point-map      (kbd "C-SPC")    'my-reselect-last-region)
  (define-key my-ctrl-point-map      (kbd "S-SPC")    'pop-global-mark) ; goto global mark and pop it; also C-x C-SPC
  ;; pop-to-mark-command             M-x ... RET Jump to mark, and pop a new position for mark off the ring.
  ;; push-mark-command               M-x ... RET Set mark at where point is.

  (define-key my-ctrl-point-map      (kbd "=")        'what-cursor-position)    ; also C-x =
  (define-key my-ctrl-point-map      (kbd "~")        'my-toggle-case)          ; C-c . -> repeat
  (define-key my-ctrl-point-map      (kbd "$")        'my-what-face)
  (define-key my-ctrl-point-map      (kbd "?")        'describe-char)           ; also C-u C-x =, more details
  (define-key my-ctrl-point-map      (kbd "<")        'my-toggle-selective-display)
  (define-key my-ctrl-point-map      (kbd "!")        'set-goal-column)
  (define-key my-ctrl-point-map      (kbd "<f1>")     'info-lookup-symbol)
  ;; thing-at-point-url-at-point; (thing-at-point 'filename)

  (define-key my-ctrl-point-map      "+"              'my-increment-number-at-point)
  (define-key my-ctrl-point-map      "-"              'my-decrement-number-at-point)

  ;; C-. a adds word at point to abbrev
  (define-key my-ctrl-point-map    (kbd "a")          'add-global-abbrev) ; also C-x a g

  ;; C-. b sets bookmark; C-. C-b asks for bookmark and jumps there; C-. C-M-b lists bookmarks
  (when bookmark-default-file
    (define-key my-ctrl-point-map    (kbd "b")        'bookmark-set) ; also C-x r m
    (define-key my-ctrl-point-map    (kbd "C-b")      'bookmark-jump) ; also C-x r b
    (define-key my-ctrl-point-map    (kbd "C-S-b")    'bookmark-delete)
    (define-key my-ctrl-point-map    (kbd "C-M-b")    'bookmark-bmenu-list)) ; also C-x r l
  ;; (set-register ?I '(file . "~/.emacs.d/init.el")) -> C-x r j I ; case sensitive

  ;; C-. r sets registers (global by default); missing: number-to-register, increment-register
  (define-key my-ctrl-point-map      (kbd "r")        'copy-to-register)    ; also C-x r s r; copy region into register r
  (define-key my-ctrl-point-map      (kbd "R")        'copy-rectangle-to-register) ; also C-x r r r; copy region-rectangle into register r
  (define-key my-ctrl-point-map      (kbd "C-r")      'insert-register)     ; also C-x r i r; insert text from register r
  (define-key my-ctrl-point-map      (kbd "M-r")      'append-to-register)  ; append region to text in register r
  (define-key my-ctrl-point-map      (kbd "M-S-r")    'prepend-to-register) ; prepend region to text in register r
  ;; TODO: bind C-x r j - jump-to-register

  (define-key my-ctrl-point-map      (kbd "d")        'my-duplicate-region-or-line) ; won't work w/ C-c or C-x key for region
  (define-key my-ctrl-point-map      (kbd "e")        'my-insert-next-line-char)
  (define-key my-ctrl-point-map      (kbd "g")        'my-google-at-point)
  (define-key my-ctrl-point-map      (kbd "h")        'my-int-to-hex-region-or-thing)
  (define-key my-ctrl-point-map      (kbd "C-h")      'my-hex-to-dec-region-or-thing)
  (define-key my-ctrl-point-map      (kbd "j")        'ace-jump-char-mode) ; can be used w/ mark set for copy/delete/...
  (define-key my-ctrl-point-map      (kbd "n")        'my-toggle-narrow)
  (define-key my-ctrl-point-map      (kbd "C-n")      'my-toggle-narrow-indirect) ; will also kill-buffer after widen
  ;;                                 (kbd "r")        TODO: all region commands in C-. r ?; see -region aliases below
  (define-key my-ctrl-point-map      (kbd "u")        'browse-url-at-point)
  (define-key my-ctrl-point-map      (kbd "y")        'my-insert-previous-line-char)

  ;; C-. 1/2/3 stores point in register 1/2/3, C-. C-1/2/3 jumps to point in register 1/2/3
  (define-key my-ctrl-point-map      (kbd "1")        (my-defun-interactive (point-to-register 1))) ; C-x r SPC
  (define-key my-ctrl-point-map      (kbd "C-1")      (my-defun-interactive (jump-to-register 1))) ; C-x r j
  (define-key my-ctrl-point-map      (kbd "2")        (my-defun-interactive (point-to-register 2)))
  (define-key my-ctrl-point-map      (kbd "C-2")      (my-defun-interactive (jump-to-register 2)))
  (define-key my-ctrl-point-map      (kbd "3")        (my-defun-interactive (point-to-register 3)))
  (define-key my-ctrl-point-map      (kbd "C-3")      (my-defun-interactive (jump-to-register 3)))

  ;; C-. . pushes point to stack, C-. C-. pops point from stack and moves to it; might be overriden w/ org mode functions
  (define-key my-ctrl-point-map      (kbd ".")        'my-push-point)
  (define-key my-ctrl-point-map      (kbd "C-.")      'my-pop-point)

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map    "#" map) ; # CPP if/else/endif movement
    (define-key map                  "b" 'backward-ifdef)
    (define-key map                  "f" 'forward-ifdef)
    (define-key map                  "p" 'previous-ifdef)
    (define-key map                  "n" 'next-ifdef)
    (define-key map                  "u" 'up-ifdef)
    (define-key map                  "d" 'down-ifdef)
    (define-key map                  "-" 'hide-ifdef-block)
    (define-key map                  "+" 'show-ifdef-block))

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map    "i" map)                             ; C-. i ? -> insert
    (define-key map                  (kbd "C-b")      'ido-insert-buffer) ; Insert contents of buffer after point
    (define-key map                  (kbd "C-f")      'ido-insert-file)   ; Insert contents of file after point
    (define-key map                  (kbd "C-x")      'my-copy-file-name-to-clipboard)
    (my-define-key-interactive map   "b"                                  ; i b buffer file name w/o extension
                               (let* ((fn  (file-name-nondirectory buffer-file-name))
                                      (fns (file-name-sans-extension fn)))
                                 (if (= 0 (length fns)) ;take care of e.g. ".emacs"
                                     (insert fn)
                                   (insert fns))))
    (my-define-key-interactive map   "d"                                  ; C-. i d date
                               (insert (format-time-string "%02d-%b-%02y")))
    (my-define-key-interactive map   "f"                                  ; C-. i f buffer file name w/ extension
                               (insert (file-name-nondirectory buffer-file-name)))
    (my-define-key-interactive map   "p"                                  ; C-. i p full buffer path name
                               (insert (my-normalize-path buffer-file-name)))
    (my-define-key-interactive map   "g"                                  ; C-. i g header include guard
                               (let ((ident (upcase (concat ; same as yasnippet C/C++ header guard
                                                     (file-name-nondirectory
                                                      (file-name-sans-extension buffer-file-name))
                                                     "_"
                                                     (file-name-extension buffer-file-name)))))
                                 (insert ident)))
    (my-define-key-interactive map   "h"                                  ; C-. i h hostname
                               (insert (or (getenv "HOSTNAME") (getenv "COMPUTERNAME") system-name "unknown")))
    (my-define-key-interactive map   "n" 'my-insert-file-at-point)
    (my-define-key-interactive map   "t"                                  ; C-. i t date/time
                               (insert (format-time-string "%02d-%b-%02y %H:%M")))
    (my-define-key-interactive map   "u"                                  ; C-. i u user name
                               (insert (or user-full-name (getenv "USERNAME") (getenv "USER")))))

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map    "f" map)                           ; C-. f ? -> find, find file
    (define-key map                  "c" 'diff-buffer-with-file)        ; f c - compare buffer w/ associated file
    (define-key map                  "D" 'find-function-at-point)       ; f d - open file for elisp function at point
    (define-key map                  "d" 'find-function)                ; f D - jump to elisp function def in same buffer
    (define-key map                  "f" 'find-file-at-point)           ; f f - find file at point (ffap is an alias)
    (define-key map                  "F" 'ffap-other-window)            ; f F
    (define-key map                  "i" 'my-ido-goto-symbol-definition); f i - not really at-point, needs user input
    (define-key map                  "l" 'find-file-literally)          ; f l - open file in unibyte
    (define-key map                  "r" 'ido-find-file-read-only)
    (define-key map                  "R" 'find-file-read-only-other-window)
    (define-key map                  "v" 'view-file)
    (define-key map                  "V" 'view-file-other-window)) ; TODO: ffap-alist ; find-file-in-project

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map "s" map)                              ; C-. s ? -> search
    (when (fboundp 'thing-at-point)

      (defmacro my-define-isearch-string (key symbol)
        `(my-define-key-interactive map ,key                            ; continue w/ C-c n / C-c C-n
                                    (let ((thing (thing-at-point ,symbol)))
                                      (setq isearch-string thing) (search-forward thing))))
      (define-key map                "c" 'lazy-highlight-cleanup)
      (my-define-isearch-string      "s" 'symbol)                       ; s s - search symbol
      (my-define-isearch-string      "w" 'word)))                       ; s w - search word

  (let ((map (make-sparse-keymap)))                                     ; C-. q ? -> query
    (define-key my-ctrl-point-map    "q" map))

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map    "o" map)                           ; C-. o ? -> occur, current buffer
    (define-key map                  "1" 'my-occur-h1)
    (define-key map                  "2" 'my-occur-h2)
    (when (fboundp 'thing-at-point)
      (defmacro my-define-occur (key thing)
        `(my-define-key-interactive map ,key
                                    (occur (thing-at-point ,thing))))
      (my-define-occur               "d" 'defun)                        ; o d - occur for current defun
      (my-define-occur               "s" 'symbol)                       ; o d - occur for current symbol
      (my-define-occur               "w" 'word)                         ; o d - occur for current word
      (my-define-occur               "x" 'sexp)))                       ; o d - occur for current sexp

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map    "m" map)                           ; C-. m ? -> multi-occur, all open buffers
    (when (fboundp 'thing-at-point)
      (defmacro my-define-multi-occur (key thing)
        `(my-define-key-interactive map ,key
                                    (multi-occur-in-matching-buffers "." (thing-at-point ,thing))))
      (my-define-multi-occur         "d" 'defun)                        ; m d - multi occur for current defun
      (my-define-multi-occur         "s" 'symbol)                       ; m d - multi occur for current symbol
      (my-define-multi-occur         "w" 'word)                         ; m d - multi occur for current word
      (my-define-multi-occur         "x" 'sexp)))                       ; m d - multi occur for current sexp

  (let ((map (make-sparse-keymap)))
    (define-key my-ctrl-point-map    "t" map)                           ; C-. t ? -> tags
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
;; --- TAB and C-tab - indentation and completion keys
;;     Note: Emacs has two tab’s (kbd TAB)/\t and (kbd <tab>)\[tab], where prior is bound by Emacs
;;       to indent-for-tab-command and latter has prio.
;;       For win32, both UI and console, TAB key will be resolved to <tab>
;;     Note: use C-q TAB to insert a literal tab, no matter what key binding is set
;;     This explicitly creates a separate binding for <tab>, so it doesn't conflict with C-i:
;;      (global-set-key [tab] (or (key-binding [tab]) (key-binding "\C-i")))

(global-set-key [tab]                 'my-hippie-smart-tab) ; replacement for dabbrev-expand
;; yas-snippet will override tab and call current binding my-hippie-smart-tab only if no snippet expansion found.
;; my-hippie-smart-tab calls hippie-expand, indent-region, indent-for-tab-command (the default TAB command).
(global-set-key [backtab]             'my-hippie-unexpand)

(when t
  ;; C-tab C-tab will be bound to company-complete-common or py-complete below
  ;; C-tab C-s / C-tab s will be bound to yas-trigger-key and others below
  (global-set-key (kbd "<C-tab>")      (make-sparse-keymap))
  (global-set-key (kbd "<C-tab> C-i")  'indent-for-tab-command) ; Indent current line or region, or insert tab; default for C-i
  (global-set-key (kbd "<C-tab> a")    'back-to-indentation)    ; similar to C-a
  (global-set-key (kbd "<C-tab> C-c")  'indent-to-column) ; Indent from point with tabs and spaces until column is reached.
  (global-set-key (kbd "<C-tab> C-e")  'edit-tab-stops)
  (global-set-key (kbd "<C-tab> p")    'indent-relative) ; Indent point relative to previous nonblank line or do a tab-to-tab-stop
  (global-set-key (kbd "<C-tab> C-p")  'indent-relative-maybe) ; Indent point relative to previous nonblank line
  (global-set-key (kbd "<C-tab> r")    'my-indent-region-or-buffer) ; instead of indent-region
  (global-set-key (kbd "<C-tab> t")    'tab-to-tab-stop) ; also M-i; insert spaces or tabs to next column in tab-stop-list
  (global-set-key (kbd "<C-tab> C-t")  'move-to-tab-stop) ; Move point to next tab-stop, only inserting if required
  (global-set-key (kbd "<C-tab> y")    'indent-rigidly)   ; also C-x TAB, C-x C-i; indent region, unindent w/ C-u negative
                                        ; keeps working on the same region if repeated (highlight disappears)
  (global-set-key (kbd "<C-tab> C-y")  'indent-code-rigidly)) ; does not affect lines starting inside comments or strings

;; ============================================================================
;; --- C-d ? - delete keys, delete w/o kill-ring

(when t ;; TODO: c-mode keys: c-electric-delete-forward, c-electric-backspace ...
  (global-set-key (kbd "C-d")          (make-sparse-keymap))
  (global-set-key (kbd "C-d C-d")      'delete-char) ; default for C-d - (lookup-key (current-global-map) (kbd "C-d"))
  (global-set-key (kbd "C-d SPC")      'just-one-space) ; also M-SPC
  (global-set-key (kbd "C-d a")        'my-delete-to-beginning-of-line)
  (global-set-key (kbd "C-d b")        'delete-blank-lines) ; also C-x C-o; delete blank lines below current line
  (global-set-key (kbd "C-d c")        'delete-char)
  (global-set-key (kbd "C-d C-c")      (my-defun-interactive (backward-delete-char-untabify 1))) ; also works w/ autopair-backspace
  (global-set-key (kbd "C-d e")        'my-delete-to-end-of-line)                                ; delete to EOL
  (global-set-key (kbd "C-d j")        'my-join-lines-or-region) ; join this line and next or join all lines in region
  (global-set-key (kbd "C-d C-j")      'delete-indentation) ; join this line to previous and fix up whitespace at join
  (global-set-key (kbd "C-d l")        'my-delete-current-line)
  (global-set-key (kbd "C-d m")        'delete-matching-lines) ; delete lines matching REGEXP; aka flush-lines
  (global-set-key (kbd "C-d r")        'delete-region)
  (global-set-key (kbd "C-d s")        'delete-horizontal-space) ; join two words by deleting all whitespace around point
  (global-set-key (kbd "C-d C-s")      (my-defun-interactive     ; kill whitespace after point
                                       (delete-region (point) (progn (skip-chars-forward " \t") (point)))))
  (global-set-key (kbd "C-d w")        'my-delete-word)
  (global-set-key (kbd "C-d C-w")      'my-backward-delete-word))

;; ============================================================================
;; --- C-k ? - kill keys, delete to kill-ring

(when t
  (global-set-key   (kbd "C-k")       (make-sparse-keymap))
  (global-set-key   (kbd "C-k +")     'append-next-kill) ; default is C-M-w, which is changed here
  (global-set-key   (kbd "C-k C-k")   'kill-whole-line)
  (global-set-key   (kbd "C-k a")     (my-defun-interactive (kill-line 0))) ; kill to BOL
  (global-set-key   (kbd "C-k c")     'ffap-copy-string-as-kill)
  (global-set-key   (kbd "C-k e")     'kill-line)       ; kill to EOL; default for C-k
  (global-set-key   (kbd "C-k l")     'kill-whole-line) ; kills whole line including its newline, regardless of position
  (global-set-key   (kbd "C-k n")     (my-defun-interactive (save-excursion (move-end-of-line nil) (kill-line 2))))
                                        ; kills next line w/o changing cursor
  (global-set-key   (kbd "C-k p")     (my-defun-interactive (save-excursion (move-beginning-of-line nil) (kill-line -1))))
                                        ; kills previous line w/o changing cursor
  (global-set-key   (kbd "C-k r")     'kill-region)   ; also C-w; advice to kill line, if no region is active
  (global-set-key   (kbd "C-k w")     'my-kill-word-and-space)
  (global-set-key   (kbd "C-k W")     'kill-word) ; also M-d
  (global-set-key   (kbd "C-k C-w")   'backward-kill-word)
  (global-set-key   (kbd "C-k x")     'kill-sexp) ; kill sexp forward, kill "foo" in any mode, kill tag in nXML; also C-M-k
  (global-set-key   (kbd "C-k C-x")   (my-defun-interactive (kill-sexp -1))) ; kill sexp backward
  (global-set-key   (kbd "C-k C-z")   'zap-to-char) ; zap including char; default M-z, changed in my init.el;
                                        ; C-- prefix zaps backward; C-u n M-z : zap to n-th occurence
  (global-set-key   (kbd "C-k z")     'zap-up-to-char) ; zap excluding char
  (global-set-key   (kbd "C-k .")     'kill-sentence)) ; also zap-to-char '.'

;; ============================================================================
;; --- C-t ? - transpose/swap/move keys

(when t
  (global-set-key   (kbd "C-t")       (make-sparse-keymap))
  (global-set-key   (kbd "C-t l")     'transpose-lines) ; transpose w/ line above, also C-x C-t
  (global-set-key   (kbd "C-t C-l")   'my-swap-line-down) ; transpose w/ line below
  (global-set-key   (kbd "C-t p")     'transpose-paragraphs)
  (global-set-key   (kbd "C-t r")     'my-move-region-or-line-up)
  (global-set-key   (kbd "C-t C-r")   'my-move-region-or-line-down)
  (global-set-key   (kbd "C-t C-t")   'transpose-chars) ; default for C-t
  (global-set-key   (kbd "C-t x")     'transpose-sexps)
  (global-set-key   (kbd "C-t w")     'transpose-words)  ; also M-t
  (global-set-key   (kbd "C-t .")     'transpose-sentences))

;; ============================================================================
;; --- Meta / Control / Standard key combinations

(global-set-key [remap downcase-word]  'my-downcase-region-or-word) ; M-u, will suppress M-- M-u
(global-set-key [remap upcase-word]    'my-upcase-region-or-word)   ; M-l, will suppress M-- M-l

(global-set-key     (kbd "C-%")        'my-match-paren)
(if (display-graphic-p)
    (global-set-key (kbd "C-#")        'comment-dwim)     ; also M-; C-u M-; to kill comment (comment-kill)
  (global-set-key       "\C-\\"        'comment-dwim))    ; overrides default binding to toggle-input-method
(global-set-key     (kbd "C-M-#")      'comment-region)   ; C-u comment-region to uncomment
(global-set-key     (kbd "C-M-'")      'uncomment-region) ; C-M-S-#
(global-unset-key   (kbd "C-M-t"))                        ; transpose-sexps, which is bound in this init.el to C-t-x
(global-unset-key   (kbd "C-M-x"))                        ; eval-defun; I prefer C-M-SPC + S-f9, so make this available

(global-set-key     (kbd "C-j")        (my-defun-interactive (join-line 1))) ; join this and next line
(global-set-key     (kbd "C-S-j")      (my-defun-interactive (join-line)))   ; join previous and this line
(global-set-key     (kbd "C-M-j")      'my-join-lines-or-region)
(global-set-key     [M-j]              'indent-new-comment-line) ; Break line at point and indent, continuing comment

(global-set-key     (kbd "C-o")        (lambda (&optional p) (interactive "P") ; like Vim o
                                         (if p (my-insert-next-line 1) (my-open-next-line 1))))
(global-set-key     (kbd "C-S-o")      (lambda (&optional p) (interactive "P") ; like Vim O
                                         (if p (my-insert-previous-line 1) (my-open-previous-line 1))))
;;                   C-M-o             split-line; split current line at point and indent new line to point's column
;;                   C-x z             repeat most recently executed command like vi-., (repeat)
;;                   C-x +             balance windows
(global-set-key [remap list-buffers]   (my-defun-interactive (ibuffer t))) ; ibuffer is nice, so bind to \C-x\C-b
;;                    C-x C-c          save-buffers-kill-emacs (exit, quit)
;;                    C-x d *.el       open dired w/ only *.el files
(global-set-key     "\C-x\C-d"         'find-lisp-find-dired)
(global-set-key     "\C-x\C-D"         'find-lisp-find-dired-subdirectories) ; overrides ido-list-directory, list-directory
;;                   \C-x\C-e          eval-last-sexp
;;                   \C-x\C-f          set-fill-column to current column; or C-u col C-x C-f to specify column
(global-set-key     "\C-x\C-k"         'my-delete-current-buffer-file)
(global-set-key (kbd "C-x j")          'my-dired-jump)
;;                   \C-x\C-m          compile; see programming mode hooks
;;                   \C-x\C-n          set-goal-column; for vertical editing (counting starts at 0); turn of w/ C-u C-x C-n
(global-set-key (kbd "C-x C-r")        'my-eval-and-replace)
(global-set-key     "\C-x\C-x"         'kill-region) ; standard kill region or line also for CUA
(when window-system
  (global-unset-key "\C-x\C-z"))       ; suspend-frame aka iconify-or-deiconify-frame

(global-set-key      "\C-c\C-a"        'copy-from-above-command) ; copy the rest of previous line forward
(global-set-key      "\C-c\C-b"        'browse-url-of-file) ; open current file im browser; see also browse-url-of-buffer
(global-set-key      "\C-c\C-c"        'kill-ring-save) ; standard copy region or line also for CUA; supports C-k +
(global-set-key      "\C-c\C-d"        'my-duplicate-current-line)
;;                    \C-c\C-m         TODO: helpful, allows C-c C-m confirm/edit mininbuffer prompt C-m (= RET)
(global-set-key      "\C-c\C-o"        'overwrite-mode)
(global-set-key      "\C-c\C-p"        'my-copy-from-above-to-char) ; copy initial part of previous line
(if (fboundp 'read-only-mode)
    (global-set-key  "\C-c\C-r"        'read-only-mode) ; default binding is C-x C-q, this fits better to C-c C-o
  (global-set-key    "\C-c\C-r"        'toggle-read-only)) ; < emacs 24.3

(global-set-key      (kbd "C-c a")     'mark-whole-buffer) ; C-a already used; also C-x h
(global-set-key      (kbd "C-c g")     'goto-line) ; C-g already used; also M-g M-g; C-u C-c g goes to line in LAST buffer
(global-set-key      (kbd "C-c C-g")   'move-to-column) ; also M-g TAB
(global-set-key      (kbd "C-c C-S-g") 'goto-char) ; Set point to position, a number or marker; (24.3) also M-g c
(global-set-key      (kbd "C-c j")     'ace-jump-mode) ; C-c j: jump-word; C-u C-c j jump-char - also C-. j, see above
(global-set-key      (kbd "C-c S-j")   'ace-jump-line-mode) ; same as C-u C-u C-c j
(global-set-key      (kbd "C-c C-j")   'ace-jump-mode-pop-mark) ; C-. j - ace-jump-char-mode, see above
(global-set-key      (kbd "C-c l")     'goto-last-change) ; C-u C-c l will set mark before move

(global-set-key      (kbd "C-c +")     'my-toggle-case)
(global-set-key      (kbd "C-c .")     'repeat)                  ; also C-x z
(global-set-key      (kbd "C-c C-.")   'repeat-complex-command)  ; repeat last minibuffer command; C-x M-: alternative
(global-set-key      (kbd "C-c *")     'my-isearch-yank-symbol)  ; continue w/ C-c n / C-c C-n
;;                         M-s w        isearch-forward-word     ; search whole word or sequence of words forward
;;                         M-s _        isearch-forward-symbol   ; do incremental search forward for a symbol
(global-set-key      (kbd "C-c n")     'isearch-repeat-forward)  ; search forward for isearch-string
(global-set-key      (kbd "C-c C-n")   'isearch-repeat-backward) ; search backward for isearch-string
(global-set-key      (kbd "C-c C-w")   'webjump)                 ; webjump-sites
(global-set-key      (kbd "C-c v")     'cua-paste-pop)           ; also M-y; needs a preceeding C-v

(global-set-key      (kbd "C-w")       'my-kill-word-and-space)  ; use C-x C-x for kill-region

;;(global-set-key    (kbd "C-y")       '...) ; TODO: that's a free key
;; (dolist (key '("\C-a" "\C-b" "\C-c" "\C-d" "\C-e" "\C-f" "\C-g"
;;                "\C-h" "\C-k" "\C-l" "\C-n" "\C-o" "\C-p" "\C-q"
;;                "\C-t" "\C-u" "\C-v" "\C-x" "\C-z" "\e"))
;;   (global-unset-key key))

;; ============================================================================
;; --- Buffer movement current and other window

(global-set-key [remap move-beginning-of-line] 'my-beginning-of-line-dynamic) ; [(home)], "\C-a"

(global-set-key [C-up]                (my-defun-interactive (scroll-up-line)))   ; overrides backward-paragraph
(global-set-key [C-down]              (my-defun-interactive (scroll-down-line))) ; overrides forward-paragraph
(global-set-key [C-M-up]              'backward-paragraph) ; overrides backward-up-list, still C-M-u
(global-set-key [C-M-down]            'forward-paragraph)  ; overrides down-list still C-M-d

(global-set-key (kbd "<prior>")       (my-defun-interactive (scroll-down-command (my-window-half-height))))
(global-set-key (kbd "<next>")        (my-defun-interactive (scroll-up-command (my-window-half-height))))

(global-set-key (kbd "<C-home>")      (my-defun-interactive (goto-char(point-min)))) ; C-Fn-left on Asus
(global-set-key (kbd "<C-end>")       (my-defun-interactive (goto-char(point-max)))) ; C-Fn-right on Asus

;; C-l                                cycle cursor vertical center / top / bottom
;; C-left / C-right                   right-word, left-word
(global-set-key (kbd "C-<")           (my-defun-interactive ; dont override CUA-select + right-word by messing w/ C-S-...
                                       (if (looking-at "\\s-") (forward-whitespace 1) (forward-same-syntax))))
;; forward-whitespace, forward-symbol from thingatp, -1 for backward

(when t
  ;; C-M-left                         backward-sexp
  ;; C-M-right                        forward-sexp
  (global-set-key (kbd "C-M-S-b")     'beginning-of-buffer) ; also C-home
  (global-set-key (kbd "C-M-b")       'end-of-buffer) ; also C-end
  (global-set-key (kbd "C-M-S-f")     'beginning-of-defun) ; overriden below in C-mode; also C-M-a
  (global-set-key (kbd "C-M-f")       'end-of-defun) ; overriden below in C-mode; also C-M-e
  (global-set-key (kbd "C-M-u")       'my-up-list) ; forward (backward w/ neg arg) out of one level of parens; also C-M-u
  (global-set-key (kbd "C-M-S-u")     'my-backward-up-list)  ; backward (forward w/ neg arg) down one level of parens; also C-M-d
  (global-set-key (kbd "C-M-d")       'down-list)  ; forward (backward w/ neg arg) down one level of parens; also C-M-d
  (global-set-key (kbd "C-M-S-l")     'backward-list) ; backward across one balanced group of parens; also C-M-p
  (global-set-key (kbd "C-M-l")       'forward-list) ; forward across one balanced group of parens; also C-M-n
  (global-set-key (kbd "C-M-:")       'backward-sentence) ; overriden in C-mode and org-mode; also M-a
  (global-set-key (kbd "C-M-.")       'forward-sentence) ; overriden in C-mode and org-mode; also M-e
  (global-set-key (kbd "C-M-S-p")     'backward-paragraph) ; also C-M-up, see above
  (global-set-key (kbd "C-M-p")       'forward-paragraph)) ; also C-M-down, see above

;; Buffer movement other window, not symmetric w/ movement for current buffer, but more reasonable

;; M-home, M-end                      buffer begin / end other window of other buffer
(global-set-key [M-up]                (my-defun-interactive (scroll-other-window 1)))
(global-set-key [M-down]              (my-defun-interactive (scroll-other-window-down 1)))

(global-set-key (kbd "M-<prior>")     (my-defun-interactive (scroll-other-window (my-window-half-height))))
(global-set-key (kbd "M-<next>")      (my-defun-interactive (scroll-other-window-down (my-window-half-height))))

(when t                                 ; M-o ? - do things in other window (overrides unused face setting)
  (defvar my-alt-o-map (make-keymap)
    "Keymap for local bindings and functions, prefixed by (M-o)")

  (global-set-key                    (kbd "M-o")      my-alt-o-map) ; M-o prefix
  (define-key my-alt-o-map           (kbd "f")        'ffap-other-window)
  (define-key my-alt-o-map           (kbd "l")        'my-center-other-window) ; vertically cycle cursor in other window
  (define-key my-alt-o-map           (kbd "s")        'my-isearch-other-window)
  (define-key my-alt-o-map           (kbd "C-x d")    (my-defun-interactive (dired-other-window default-directory))))

(global-set-key (kbd "<M-left>")      'winner-undo) ; default same as C-left, so override
(global-set-key (kbd "<M-right>")     'winner-redo) ; default same as C-right, so override

;; ============================================================================
;; --- Functions keys
;;     Note: C-M-Fx does not work for some F-keys

(global-set-key [f2]                  (my-defun-interactive (save-buffer)))
(global-set-key [S-f2]                'ido-write-file) ; file name is selected interactively by typing a substring; also C-x C-w
(global-set-key [C-S-f2]              'ido-insert-file)
(global-set-key [M-f2]                'save-some-buffers)
(global-set-key [C-f2]                (my-defun-interactive (save-buffer) (kill-buffer (buffer-name)) ))
(global-set-key [C-M-f2]              'my-rename-current-buffer-file)

(global-set-key [f3]                  'ido-find-file)
(global-set-key [M-f3]                'ido-find-file-other-window) ; Switch and show file in another window
(global-set-key [M-S-f3]              'ido-find-alternate-file) ; Switch to alternate file and show it in another window.
(when (fboundp 'my-ido-choose-from-recentf)
  (global-set-key [C-f3]              'my-ido-choose-from-recentf))
(global-set-key [C-S-f3]              'recentf-open-files)
(global-set-key [S-f3]                'revert-buffer)

(global-set-key [f4]                  (my-safe-interactive (next-error))) ; C-u F4: reparse error buffer and go to 1st error
(global-set-key [S-f4]                (my-safe-interactive (previous-error)))
(global-set-key [C-f4]                'iconify-or-deiconify-frame)
(global-set-key [C-M-f4]              'delete-other-frames) ; delete all frames except the selected one
(global-set-key [M-f4]                'delete-frame)

(global-set-key [f5]                  'other-window)
(global-set-key [M-f5]                'switch-window)
(global-set-key [S-f5]                'my-other-window-backward)
(global-set-key [C-f5]                'my-kill-buffer-and-window) ; kill-buffer-and-window
(global-set-key [C-S-f5]              'delete-window) ; also C-x 0
(global-set-key [C-M-f5]              'my-close-and-kill-next-window) ; C-x 1 : delete-other-windows
(global-set-key [C-M-S-f5]            'my-kill-other-buffer-and-window)

(global-set-key [f6]                  'next-buffer)
(global-set-key [S-f6]                'previous-buffer)
(global-set-key [C-f6]                'my-kill-current-buffer) ; kill w/o asking
(global-set-key [M-f6]                'my-next-buffer-other-window)
(global-set-key [M-S-f6]              'my-previous-buffer-other-window)
(global-set-key [C-M-f6]              'my-kill-other-buffer) ; kill w/o asking
(global-set-key [C-M-S-f6]            'my-kill-other-buffers-and-windows)
;; TODO: support 2-column mode: 2C-two-columns, 2C-split, 2C-associate-buffer, 2C-dissociate

(global-set-key [f7]                  'ido-switch-buffer) ; simpler than ibuffer
(global-set-key [C-f7]                'ido-kill-buffer) ; kill w/ asking
(global-set-key [C-M-f7]              'my-delete-current-buffer-file)
(global-set-key [M-f7]                (my-defun-interactive (ido-switch-buffer-other-window) (other-window -1)))
(global-set-key [S-f7]                'my-toggle-window-dedicated)
(global-set-key [C-S-f7]              'my-toggle-window-split)

(global-set-key [f8]                  'bury-buffer)
(global-set-key [S-f8]                'unbury-buffer) ; unbury the last buried buffer
(global-set-key [C-f8]                'rotate-frame-clockwise)
(global-set-key [C-S-f8]              'rotate-frame-anticlockwise)
(global-set-key [M-f8]                'flip-frame) ; Flip vertically
(global-set-key [M-S-f8]              'flop-frame) ; Flop horizontally2

(global-set-key [S-f9]                'my-eval-region-or-sexp) ; C-x C-e doesn't work w/ active region

(global-unset-key [f10])              ; menu-bar-open
(global-set-key [S-f10]               (my-defun-interactive (find-file user-init-file)))
(global-set-key [C-f10]               (my-defun-interactive (load-file user-init-file)))

(when (eq system-type 'gnu/linux)                               ; both work on Ubuntu
  (global-set-key [C-f11]             'toggle-frame-maximized)  ; default is M-f10
  (global-set-key [f11]               'toggle-frame-fullscreen) ; or devhelp-word-at-point
  )

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
  ;; store/restore frame/window config to register f/w; also save-window-excursion; register chars a case sensitive
  (defalias 'sfc (my-defun-interactive (frame-configuration-to-register ?F))) ; C-x r w F
  (defalias 'rfc (my-defun-interactive (jump-to-register ?F))) ; C-x r j F
  (defalias 'swc (my-defun-interactive (window-configuration-to-register ?W))) ; C-x r w W
  (defalias 'rwc (my-defun-interactive (jump-to-register ?W))) ; C-x r j W
  (defalias 'rcc (my-defun-interactive (jump-to-register ?C))) ; stored in my-compilation-hook
  (defalias 'touch 'my-touch-buffer)
  (defalias 'tqr 'tags-query-replace) ; => M-x tqr
  (defalias 'vtn (my-defun-interactive (visit-tags-table-buffer t))) ; visit next table in `tags-table-list'
  (defalias 'wsm 'whitespace-mode)
  "defalias aliases loaded")

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
  (defalias '-os-file-open 'my-open-file)
  (defalias '-os-buffer-rename 'my-rename-current-buffer-file)
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
  (defalias '-region-duplicate 'my-duplicate-region)
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
  (defalias '-compare-buffers 'ediff-buffers) ; interactive compare w/ ediff
  (defalias '-compare-window-nows 'compare-windows) ; ignore whitespace
  (defalias '-compare-window-ws (my-defun-interactive (compare-windows t))) ; respect whitespace
  "defalias menu loaded")

;;; ###########################################################################
;;; --- Final startup settings
;;; ###########################################################################

;; ============================================================================
;; --- Load user and system dependent init files

(let ((user-settings-file (concat user-emacs-directory "init_user_" user-login-name ".el")))
  (when (file-exists-p user-settings-file)
    (load user-settings-file)
    (unless noninteractive (message "**Finished loading user init file %s." user-settings-file))))

(let ((system-settings-file (concat user-emacs-directory "init_system_" system-name ".el")))
  (when (file-exists-p system-settings-file)
    (load system-settings-file)
    (unless noninteractive (message "**Finished loading system init file %s." system-settings-file))))

(setq default-directory "~")
(cd default-directory) ; go home

(unless noninteractive
  (message "***Finished loading %s in %ds." ; (emacs-init-time) not yet ready
           dotfile-name (time-to-seconds (time-since my-emacs-load-start-time)))) ; (sit-for 2)
(setq debug-on-error nil)

;;; ###########################################################################
;;; --- Final display settings
;;; ###########################################################################

(defun my-init-graphic-or-term-frame (for-graphic first-frame frame)
  (with-selected-frame frame
    (if for-graphic                     ; Note: a display allows several frames and different fonts at once, ...
        (my-init-graphic-frame first-frame frame) ; ... whereas a terminal does not
      (my-init-term-frame first-frame frame))))

;; run for each new frame (emacsclient; graphics or terminal)
;; TODO: also call in term-setup-hook and/or after-make-frame-functions?
(add-hook 'after-make-frame-functions
          (lambda (frame) (my-init-graphic-or-term-frame (display-graphic-p) nil frame)))

;; run once for emacs start (standalone emacs, runemacs)
(if (display-graphic-p)
    (progn
      (require 'server)
      (unless (server-running-p)        ; if we're in non-terminal mode, server will be started now
        (my-init-graphic-or-term-frame t t (selected-frame))))
  (when menu-bar-mode                   ; if we're in terminal-mode, menu-bar will be switched off now
    (my-init-graphic-or-term-frame nil t (selected-frame))))

;;; ###########################################################################
;;; --- scratchpad
;;; ###########################################################################

;; --- relevant keys, predefined and mine from here

;;     http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley   - open w/ C-. u or C-c RET
;;     http://defindit.com/readme_files/emacs_bindings.html
;;     http://www.math.uh.edu/~bgb/emacs_keys.html

;; C-u 10 C-{key}               perform key 10 times
;; C-1 C-0 {key}                the same - just less keystrokes

;; C-M-i   -> M-TAB
;; Esc-TAB -> M-TAB
;; C-x C-m C-m -> C-x C-m (confirm default minibuffer content) C-m (where C-m is RET)

;; C-g                          cancel; (keyboard-quit)
;; ESC ESC ESC                  escape current interactive command, minibuffer edit, ... (keyboard-escape-quit)

;; C-z                          undo; Undo some previous changes - don't undo previous undos
;; C-g C-z                      redo - only 1 C-g for multiple C-z's, so e.g. to redo twice: C-g C-z C-z,;
;;                              C-g counts as a non-undo key to signal the end of your undo sequence - as would e.g. C.f
;;                              see http://www.dr-qubit.org/undo-tree/undo-tree.el for nice explanation

;; C-x C-+ / C-x C-- / C-x C-0  increase / decrease /reset buffer text size; text-scale-increase/text-scale-decrease

;; M-m                          (back-to-indentation) / similar to original C-a
;; M-SPACE                      just-one-space
;; C-- M-SPACE                  just-one-space incl newlines
;; M-d                          kill word
;; M-y                          cua-paste-pop (CUA yank-pop replacement) - replace just killed text w/ previous kill
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

;; C-x (                        kmacro-start-macro
;; C-x )                        kmacro-end-macro
;; C-x e (only e furtheron)     kmacro-end-and-call-macro, Call last keyboard macro, ending it first if currently being defined.
;;                              call-last-kbd-macro
;;                              name-last-kbd-macro
;; CRTL-u CTRL-x q              enter recursive editing when executing a macro (C-M-c to continue)
;;  -> C-x ( { type fixed prefix } CRTL-u CTRL-x q { type variable text } C-M-c { type fixed suffix } C-x )

;; C-x r s {letter}             copy-to-register
;; C-x r r {letter}             copy-rectangle-to-register
;; C-x r i {letter}             insert-register

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
;;   [C-M-up] / [C-M-down]      scrolls lines INSIDE the rectangle up and down; recover lost top/bottom lines w/ [C-z]

;; --- snippets and other stuff

;; TODO: support directory local variables or use (dir-locals-set-class-variables)
;;  see http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html

;; TODO: support mail editing for Outlook,
;;  see http://www.emacswiki.org/emacs/MsOutlook and https://github.com/dholm/outlookedit

;; (set-buffer-file-coding-system 'undecided-dos) ; unix to dos; successor for default-buffer-file-coding-system
;; (set-buffer-file-coding-system 'undecided-unix) ; dos to unix

;; (defun utf-8-revert () "Reload file assuming utf-8 encoding." (interactive)(revert-buffer-with-coding-system 'utf-8))
;; (defun utf-8-dos () "Use `utf-8-dos' encoding." (interactive)(set-buffer-file-coding-system 'utf-8-dos t))
;; (defun utf-8-unix () "Use `utf-8-unix' encoding." (interactive)(set-buffer-file-coding-system 'utf-8-unix t))

;; C-x k == C-x # when editing emacsclient is waiting
;; (add-hook 'server-switch-hook
;;   (lambda ()
;;     (global-set-key (kbd "C-x k") '(lambda ()
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
;; (global-set-key (kbd "C-M-8") '(lambda () (interactive) (adjust-opacity nil -5)))
;; (global-set-key (kbd "C-M-9") '(lambda () (interactive) (adjust-opacity nil 5)))
;; (global-set-key (kbd "C-M-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; goal-column fill-region fill-paragraph fill-column

;; http://tromey.com/elpa/
;; http://dotfiles.org/.emacs
;; flyspell - spell checker

;; (tooltip-show "foo\n")

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

;; do something in all open buffers
;; | (defun indent-all-buffers ()
;; |   "Reindent all currently open buffers, as long as they are not read-only. Read-only buffers are simply skipped."
;; |   (interactive)
;; |   (dolist (elt (mapcar (function buffer-name) (buffer-list)) nil)
;; |     (save-current-buffer
;; |       (set-buffer elt)
;; |       (if (not buffer-read-only)
;; |           (indent-region (point-min) (point-max) nil))))

;; (defmacro Windows (&rest body)
;;   (list 'if (string-match "windows" (prin1-to-string system-type))
;;         (cons 'progn body)))
