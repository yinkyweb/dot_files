;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORIGINAL .emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORIGINAL DONE .emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; OMAJINAI
(global-font-lock-mode t)
(show-paren-mode 1)
(setq-default tab-width 2)
;(setq-default tab-width 999)
(setq load-path (cons (expand-file-name "~/elisp") load-path))
(setq load-path (cons (expand-file-name "~/elisp/common") load-path))
(setq load-path (cons (expand-file-name "~/elisp/ruby") load-path))
(setq load-path (cons (expand-file-name "~/elisp/php") load-path))
(setq load-path (cons (expand-file-name "~/elisp/js") load-path))
(setq load-path (cons (expand-file-name "~/elisp/scheme") load-path))
(setq load-path (cons (expand-file-name "~/elisp/irc") load-path))
(setq load-path (cons (expand-file-name "~/elisp/apel") load-path))
(setq load-path (cons (expand-file-name "~/elisp/yasnippet") load-path))

; Japanese
; (require 'un-define)
(set-language-environment 'Japanese)
(set-terminal-coding-system 'utf-8-unix)
(setq file-name-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'mule-utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

; enable alt key
(load-library "term/bobcat")

; global-set-key
;(global-set-key "\C-i" 'self-insert-command)
;(global-set-key "\C-i"  'scroll-down)
(global-set-key "\C-m" 'newline)
(global-set-key "\C-j" 'newline-and-indent)

; enable M-? as help
(global-set-key "\M-?" 'help)

; enable C-h as BS
(define-key global-map "\C-h" 'backward-delete-char)

; not to use backup file
(setq make-backup-files nil)
(setq auto-save-default nil)

; tab configuration of c-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
              (c-set-style "k&r")
              (setq c-basic-offset 2)
;              (setq indent-tabs-mode t)
              (setq indent-tabs-mode nil)
              (setq tab-width 2)
))

; differentiate duplicate file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; indent region with C-x C-i
(global-set-key "\C-x\C-i" 'indent-region)

; flash region
(transient-mark-mode t)

; scroll
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)
(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))
(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))
(global-set-key "\C-q" 'scroll-n-lines-behind)
(global-set-key "\C-z" 'scroll-n-lines-ahead)
(global-set-key "\C-x\C-q" 'quoted-insert) ; move quoted-insert to other key bind
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

; ac-mode
(load "ac-mode")
(add-hook 'text-mode-hook 'ac-mode-on)
(setq ac-mode-exception '(dired-mode hex-mode))
(add-hook 'find-file-hooks 'ac-mode-without-exception)
(setq ac-mode-goto-end-of-word t)

; php-mode
(require 'php-mode)

; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))

; javascript.el
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
(setq javascript-indent-level 4)

; gauche
(setq scheme-program-name "gosh -i")
(add-hook 'scheme-mode-hook
          '(lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 2)
))


; elscreen
(load "elscreen" "ElScreen" t)
(elscreen-set-prefix-key "\C-]")
;(elscreen-set-prefix-key "\C-o")

; elscreen-tab
(require 'elscreen-tab)

; psvn
(require 'psvn)

; w3m
;; (require 'w3m-load)

; irc
(autoload 'irchat "irchat" "Internet Relay Chat" t)
(setq irchat-server "irc.tokyo.wide.ad.jp")

;; Show tab, zenkaku-space, white spaces at end of line
;; http://www.bookshelf.jp/soft/meadow_26.html#SEC317
;(defface my-face-tab         '((t (:background "Yellow"))) nil :group 'my-faces)
(defface my-face-zenkaku-spc '((t (:background "LightBlue"))) nil :group 'my-faces)
(defface my-face-spc-at-eol  '((t (:foreground "Red" :underline t))) nil :group 'my-faces)
(defvar my-face-tab         'my-face-tab)
(defvar my-face-zenkaku-spc 'my-face-zenkaku-spc)
(defvar my-face-spc-at-eol  'my-face-spc-at-eol)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-tab append)
     ("　" 0 my-face-zenkaku-spc append)
     ("[ \t]+$" 0 my-face-spc-at-eol append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
;; settings for text file
(add-hook 'text-mode-hook
          '(lambda ()
             (progn
               (font-lock-mode t)
               (font-lock-fontify-buffer))))

; keymap
;; (global-set-key "\C-D" 'delete-rectangle)
;; (global-set-key "\C-u" 'comment-region)
;; (global-set-key "\C-U" 'uncomment-region)
; (global-set-key [f7] 'delete-rectangle)
; (global-set-key [?\C-^] 'delete-rectangle)

;; gca
(require 'gca)
(define-key scheme-mode-map "\C-c\C-u" 'gca-insert-use) 
(let ((m (make-sparse-keymap)))
  (define-key m "h" 'gca-show-info)
  (define-key m "i" 'auto-info-mode)
  (define-key scheme-mode-map "\C-c\C-d" m))
(define-key scheme-mode-map [(control c) (control ,)] 'gca-info-next)
(define-key scheme-mode-map [(control .)] 'gca-completion-current-word)
(define-key scheme-mode-map [(control c) (control .)] 'gca-insert-template)
(define-key c-mode-map [(control c) (control .)] 'gca-insert-template)
(define-key scheme-mode-map "\C-ct" 'gca-make-test)
(define-key scheme-mode-map "\C-ch" 'gca-show-history)
(setq scheme-program-name "gosh ~/elisp/scheme/tgosh.scm")

;;
(require 'tramp)
(setq tramp-default-method "ssh")

; for PEAR coding standard
;; (defun php-mode-hook ()
;;   (setq tab-width 4
;;         c-basic-offset 4
;;         c-hanging-comment-ender-p nil
;; 		indent-tabs-mode
;; 		(not
;; 		 (and (string-match "/\\(PEAR\\|pear\\)/" (buffer-file-name))
;; 			  (string-match "\.php$" (buffer-file-name))))))

;; gtags mode (http://d.hatena.ne.jp/higepon/20060107/1136628498)
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))
(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))

; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/yasnippet/snippets")

; kahua
(require 'kahua)
(setq auto-mode-alist
      (append '(("\\.kahua$" . kahua-mode)) auto-mode-alist))

; anything
(require 'anything-config)
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-recentf
                             anything-c-source-file-name-history
                             anything-c-source-locate))
(define-key anything-map "\C-p" 'anything-previous-line)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-v" 'anything-next-source)
(define-key anything-map "\M-v" 'anything-previous-source)
(global-set-key "\C-c\C-c" 'anything)
