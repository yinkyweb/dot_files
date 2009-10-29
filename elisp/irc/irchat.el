;;; see file irchat-original-copyright.el for old change log and copyright info

;; irchat-jp version by kick@wide.ad.jp (1994/03/17 - 2005/03/02)
;; Copyright (C) 1994-1999,2004-2005 KIKUCHI Takahiro

(defconst irchat-version "irchat-jp26d(2005/03/02)")
(defconst irchat-url "http://www.ircnet.jp/irchat.html")

(require 'irchat-inlines)
(require 'irchat-globals)
(require 'irchat-vars)
(require 'irchat-iso-2022-jp)
(require 'irchat-misc)
(require 'irchat-filter)
(require 'irchat-handle)
(require 'irchat-commands)
(require 'irchat-ctcp)
(require 'irchat-dcc)

(defconst irchat-emacs-version
  (concat (if (featurep 'xemacs)
	      "XEmacs "
	    "Emacs ")
	  emacs-version
	  (if (featurep 'mule)
	      (if (boundp 'mule-version)
		  (concat " Mule " mule-version)
		" Mule")
	    "")
	  (if (boundp 'nemacs-version)
	      (concat " Nemacs " nemacs-version) "")
	  (if (boundp 'system-configuration)
	      (concat " (" system-configuration ")") "")))

(fset 'irchat-decode irchat-decode-function)
(fset 'irchat-encode irchat-encode-function)
(fset 'irchat-decode-message irchat-decode-message-function)
(fset 'irchat-encode-message irchat-encode-message-function)

(if irchat-want-traditional
    (defvar irchat-command-window-on-top nil
      "*If non-nil, the Command window will be put at the top of the screen.
Else it is put at the bottom.")

  (defvar irchat-command-window-on-top t
    "*If non-nil, the Command window will be put at the top of the screen.
Else it is put at the bottom."))
  
(defsubst irchat-channel-freeze-local (chan value)
  (put (intern chan) 'freeze value))

(defsubst irchat-channel-beep-local (chan value)
  (put (intern chan) 'beep value))

(defsubst irchat-channel-suppress-local (chan value)
  (put (intern chan) 'suppress value))

(defvar irchat-blink-parens t
  "*Should we blink matching parenthesis in irchat command buffer?")

(defvar irchat-one-buffer-mode nil
  "*When non-nil, irchat will put up only a dialogue-buffer (on the
screen). Useful for those (perverts) who use 24 line terminals.")

(defvar irchat-channel-buffer-mode nil
  "*When non-nil, irchat will display a channel buffer.")

(defvar irchat-how-to-show-links-reply nil
  "*how to show LINKS reply.")

(defvar irchat-auto-whois-nick ""
  "*nick which automatically whois.")

(defvar irchat-nickname-already-in-use nil
  "*Need ask another nickname.")

(defvar irchat-nickname-erroneus nil
  "*Need ask another nickname.")

(defvar irchat-invited-channel)
(setq irchat-invited-channel nil)

(defvar irchat-Command-mode-hook nil
  "*A hook for IRCHAT Command mode.")

(defvar irchat-Dialogue-mode-hook nil
  "*A hook for IRCHAT Dialogue mode.")

(defvar irchat-Others-mode-hook nil
  "*A hook for IRCHAT Others mode.")

(defvar irchat-Channel-mode-hook nil
  "*A hook for IRCHAT Current channel mode.")

(defvar irchat-Exit-hook nil
  "*A hook executed when signing off IRC.")

(defvar irchat-ignore-nickname nil
  "*A list of nicknames, as symbols, to ignore.  Messages from these people
won't be displayed.")

(defvar irchat-kill-realname nil
  "*A list of real names of people to ignore. Messages from them
won't be displayed.")

(defvar irchat-buggy-emacs-pos-visible-in-window-p nil
  "*You should set non-nil if your emacs has buggy pos-visible-in-window-p.")

(defvar irchat-freeze nil
  "If non-nil the Dialogue window will not be scrolled automatically to bring
new entries into view.")

(defvar irchat-debug-buffer nil)
(defvar irchat-server-buffer nil)

(defvar irchat-buffer-format " IRC:%s")
(defvar irchat-Command-buffer "*IRC*")
(defvar irchat-Dialogue-buffer (format irchat-buffer-format " Dialogue"))
(defvar irchat-Others-buffer (format irchat-buffer-format " Others"))
;;(defvar irchat-Private-buffer (format irchat-buffer-format " Private"))
(defvar irchat-Private-buffer)
(defvar irchat-Channel-buffer nil)
(defvar irchat-KILLS-buffer  (format irchat-buffer-format " KILLS"))
(defvar irchat-IGNORED-buffer (format irchat-buffer-format " IGNORED"))
(defvar irchat-WALLOPS-buffer (format irchat-buffer-format " WALLOPS"))

(defvar irchat-server-process nil)

(defvar irchat-command-map nil)
(defvar irchat-Command-mode-map nil)
(defvar irchat-Dialogue-mode-map nil)
(defvar irchat-Others-mode-map nil)
(defvar irchat-Channel-mode-map nil)
(defvar irchat-CTCP-command-map nil)
(defvar irchat-DCC-command-map nil)

(put 'irchat-Command-mode 'mode-class 'special)
(put 'irchat-Dialogue-mode 'mode-class 'special)
(put 'irchat-Others-mode 'mode-class 'special)
(put 'irchat-Channel-mode 'mode-class 'special)

(if irchat-command-map
    nil
  (define-prefix-command 'irchat-command-map)
  (setq irchat-command-map (make-keymap))
  (fset 'irchat-command-prefix irchat-command-map)
  (define-key irchat-command-map "0" 'irchat-Command-jump-channel0)
  (define-key irchat-command-map "1" 'irchat-Command-jump-channel1)
  (define-key irchat-command-map "2" 'irchat-Command-jump-channel2)
  (define-key irchat-command-map "3" 'irchat-Command-jump-channel3)
  (define-key irchat-command-map "4" 'irchat-Command-jump-channel4)
  (define-key irchat-command-map "5" 'irchat-Command-jump-channel5)
  (define-key irchat-command-map "6" 'irchat-Command-jump-channel6)
  (define-key irchat-command-map "7" 'irchat-Command-jump-channel7)
  (define-key irchat-command-map "8" 'irchat-Command-jump-channel8)
  (define-key irchat-command-map "9" 'irchat-Command-jump-channel9)
  (define-key irchat-command-map "\C-a" 'irchat-Command-alternative-channel)
  (define-key irchat-command-map "A" 'irchat-Command-admin)
  (define-key irchat-command-map "a" 'irchat-Command-away)
  (define-key irchat-command-map "\C-b" 'irchat-Command-browse-url)
  (define-key irchat-command-map "B" 'irchat-Command-beep-on-message)
  (define-key irchat-command-map "b" 'irchat-Command-scroll-down)
  (define-key irchat-command-map "\C-c" 'irchat-CTCP-command-prefix)
  (define-key irchat-command-map "c" 'irchat-Command-inline)
  (define-key irchat-command-map "\C-d" 'irchat-DCC-command-prefix)
  (define-key irchat-command-map "D" 'irchat-Command-debug)
  (define-key irchat-command-map "\C-f" 'irchat-Command-scroll-freeze)
  (define-key irchat-command-map "F" 'irchat-Command-finger-direct)
  (define-key irchat-command-map "f" 'irchat-Command-finger)
  (define-key irchat-command-map "\C-i" 'irchat-Command-ignore)
  (define-key irchat-command-map "i" 'irchat-Command-invite)
  (define-key irchat-command-map "\C-j" 'irchat-Command-next-channel)
  (define-key irchat-command-map "j" 'irchat-Command-join)
  (define-key irchat-command-map "\C-k" 'irchat-Command-kick)
  (define-key irchat-command-map "\C-l" 'irchat-Command-redisplay)
  (define-key irchat-command-map "L" 'irchat-Command-load-vars)
  (define-key irchat-command-map "l" 'irchat-Command-list)
  (define-key irchat-command-map "m" 'irchat-Command-send-minibuffer)
  (define-key irchat-command-map "\C-m" 'irchat-Command-modec)
  (define-key irchat-command-map "\C-n" 'irchat-Command-names)
  (define-key irchat-command-map "n" 'irchat-Command-nickname)
  (define-key irchat-command-map "\C-o" 'irchat-Command-toggle-display-mode)
  (define-key irchat-command-map "o" 'irchat-Command-ison)
  (define-key irchat-command-map "\C-p" 'irchat-Command-part)
  (define-key irchat-command-map "P" 'irchat-Channel-ctcp-ping)
  (define-key irchat-command-map "p" 'irchat-Command-send-private)
  (define-key irchat-command-map "q" 'irchat-Command-quit)
  (define-key irchat-command-map "\C-r" 'irchat-Command-search-backward)
  (define-key irchat-command-map "R" 'irchat-Command-caesar-line)
  (define-key irchat-command-map "r" 'irchat-Command-reconfigure-windows)
  (define-key irchat-command-map "\C-s" 'irchat-Command-search-forward)
  (define-key irchat-command-map "S" 'irchat-Command-suppress-others)
  (define-key irchat-command-map "s" 'irchat-Command-servers)
  (define-key irchat-command-map "\C-t" 'irchat-Command-trace)
  (define-key irchat-command-map "T" 'irchat-Channel-ctcp-time)
  (define-key irchat-command-map "t" 'irchat-Command-topic)
  (define-key irchat-command-map "\C-u" 'irchat-Command-userhost)
  (define-key irchat-command-map "U" 'irchat-Channel-ctcp-userinfo)
  (define-key irchat-command-map "u" 'irchat-Command-users)
  (define-key irchat-command-map "V" 'irchat-Channel-ctcp-version)
  (define-key irchat-command-map "v" 'irchat-Command-version)
  (define-key irchat-command-map "\C-v" 'irchat-Command-search-url)
  (define-key irchat-command-map "w" 'irchat-Command-who)
  (define-key irchat-command-map "W" 'irchat-Command-wait)
  (define-key irchat-command-map "\C-y" 'irchat-Command-send-yank)
  (define-key irchat-command-map "Y" 'irchat-Command-debug-user)
  (define-key irchat-command-map "\C-?" 'irchat-Command-scroll-down)
  (define-key irchat-command-map " " 'irchat-Command-scroll-up)
  (define-key irchat-command-map "!" 'irchat-Command-send-exec)
  (define-key irchat-command-map "$" 'irchat-Command-eod-buffer)
  (define-key irchat-command-map ">" 'irchat-Command-next-channel)
  (define-key irchat-command-map "<" 'irchat-Command-previous-channel)
  (define-key irchat-command-map "/" 'irchat-Command-generic))

(if irchat-CTCP-command-map
    nil
  (define-prefix-command 'irchat-CTCP-command-map)
  (setq irchat-CTCP-command-map (make-keymap))
  (fset 'irchat-CTCP-command-prefix irchat-CTCP-command-map)
  (define-key irchat-CTCP-command-map "0" 'irchat-Command-jump-channel10)
  (define-key irchat-CTCP-command-map "1" 'irchat-Command-jump-channel11)
  (define-key irchat-CTCP-command-map "2" 'irchat-Command-jump-channel12)
  (define-key irchat-CTCP-command-map "3" 'irchat-Command-jump-channel13)
  (define-key irchat-CTCP-command-map "4" 'irchat-Command-jump-channel14)
  (define-key irchat-CTCP-command-map "5" 'irchat-Command-jump-channel15)
  (define-key irchat-CTCP-command-map "6" 'irchat-Command-jump-channel16)
  (define-key irchat-CTCP-command-map "7" 'irchat-Command-jump-channel17)
  (define-key irchat-CTCP-command-map "8" 'irchat-Command-jump-channel18) 
  (define-key irchat-CTCP-command-map "9" 'irchat-Command-jump-channel19)
  (define-key irchat-CTCP-command-map "a" 'irchat-Command-ctcp-action)
  (define-key irchat-CTCP-command-map "c" 'irchat-Command-ctcp-clientinfo)
  (define-key irchat-CTCP-command-map "f" 'irchat-Command-ctcp-finger)
  (define-key irchat-CTCP-command-map "g" 'irchat-Command-ctcp-generic)
  (define-key irchat-CTCP-command-map "p" 'irchat-Command-ctcp-ping)
  (define-key irchat-CTCP-command-map "t" 'irchat-Command-ctcp-time)
  (define-key irchat-CTCP-command-map "u" 'irchat-Command-ctcp-userinfo)
  (define-key irchat-CTCP-command-map "v" 'irchat-Command-ctcp-version)
  (define-key irchat-CTCP-command-map "\C-c"
    'irchat-Command-ctcp-clientinfo-generic)
  (define-key irchat-CTCP-command-map "\C-u" 
    'irchat-Command-ctcp-userinfo-from-commandbuffer)
  (define-key irchat-CTCP-command-map "U" 
    'irchat-Command-ctcp-userinfo-from-minibuffer)
  )

(if irchat-DCC-command-map
    nil
  (define-prefix-command 'irchat-DCC-command-map)
  (setq irchat-DCC-command-map (make-keymap))
  (fset 'irchat-DCC-command-prefix irchat-DCC-command-map)
  (define-key irchat-DCC-command-map "c" 'irchat-Command-dcc-chat)
  (define-key irchat-DCC-command-map "g" 'irchat-Command-dcc-get)
  (define-key irchat-DCC-command-map "k" 'irchat-Command-dcc-kill)
  (define-key irchat-DCC-command-map "l" 'irchat-Command-dcc-list)
  (define-key irchat-DCC-command-map "s" 'irchat-Command-dcc-send)
  )

(if irchat-Others-mode-map
    nil
  (setq irchat-Others-mode-map (make-keymap))
  (suppress-keymap irchat-Others-mode-map t)
  (let ((i 0))
    (while (< i 128)
      (define-key irchat-Others-mode-map (format "%c" i)
	'irchat-Command-reconfigure-windows)
      (setq i (1+ i)))))

(if irchat-Channel-mode-map
    nil
  (setq irchat-Channel-mode-map (make-keymap))
  (suppress-keymap irchat-Channel-mode-map)
  (define-key irchat-Channel-mode-map "0" 'irchat-Command-jump-channel0)
  (define-key irchat-Channel-mode-map "1" 'irchat-Command-jump-channel1)
  (define-key irchat-Channel-mode-map "2" 'irchat-Command-jump-channel2)
  (define-key irchat-Channel-mode-map "3" 'irchat-Command-jump-channel3)
  (define-key irchat-Channel-mode-map "4" 'irchat-Command-jump-channel4)
  (define-key irchat-Channel-mode-map "5" 'irchat-Command-jump-channel5)
  (define-key irchat-Channel-mode-map "6" 'irchat-Command-jump-channel6)
  (define-key irchat-Channel-mode-map "7" 'irchat-Command-jump-channel7)
  (define-key irchat-Channel-mode-map "8" 'irchat-Command-jump-channel8)
  (define-key irchat-Channel-mode-map "9" 'irchat-Command-jump-channel9)
  (define-key irchat-Channel-mode-map "a" 'irchat-Command-away)
  (define-key irchat-Channel-mode-map "A" 'irchat-Command-alternative-channel)
  (define-key irchat-Channel-mode-map "B" 'irchat-Command-beep-on-message)
  (define-key irchat-Channel-mode-map "b" 'irchat-Current-scroll-down)
  (define-key irchat-Channel-mode-map "F" 'irchat-Command-finger-direct)
  (define-key irchat-Channel-mode-map "f" 'irchat-Channel-freeze)
  (define-key irchat-Channel-mode-map "i" 'irchat-Command-invite)
  (define-key irchat-Channel-mode-map "J" 'irchat-Command-next-channel)
  (define-key irchat-Channel-mode-map "j" 'irchat-Command-join)
  (define-key irchat-Channel-mode-map "L" 'irchat-Command-load-vars)
  (define-key irchat-Channel-mode-map "l" 'irchat-Command-list)
  (define-key irchat-Channel-mode-map "m" 'irchat-Command-modec)
  (define-key irchat-Channel-mode-map "n" 'irchat-Command-names)
  (define-key irchat-Channel-mode-map "o" 'other-window)
  (define-key irchat-Channel-mode-map "p" 'irchat-Command-part)
  (define-key irchat-Channel-mode-map "r" 'irchat-Command-reconfigure-windows)
  (define-key irchat-Channel-mode-map "S" 'irchat-Command-suppress-others)
  (define-key irchat-Channel-mode-map "t" 'irchat-Command-topic)
  (define-key irchat-Channel-mode-map "u" 'irchat-Command-users)
  (define-key irchat-Channel-mode-map "v" 'irchat-Command-version)
  (define-key irchat-Channel-mode-map "w" 'irchat-Command-who)
  (define-key irchat-Channel-mode-map "!" 'irchat-Command-send-exec)
  (define-key irchat-Channel-mode-map "$" 'end-of-buffer)
  (define-key irchat-Channel-mode-map "/" 'irchat-Command-generic)
  (define-key irchat-Channel-mode-map ">" 'irchat-Command-next-channel)
  (define-key irchat-Channel-mode-map "<" 'irchat-Command-previous-channel)
  (define-key irchat-Channel-mode-map " " 'irchat-Current-scroll-up)
  (define-key irchat-Channel-mode-map "\C-?" 'irchat-Current-scroll-down)
  (define-key irchat-Channel-mode-map "\C-n" 'forward-line)
  (define-key irchat-Channel-mode-map "\C-m" 'irchat-Command-enter-message)
  (define-key irchat-Channel-mode-map "\C-c" 'irchat-command-prefix))

(if irchat-Dialogue-mode-map
    nil
  (setq irchat-Dialogue-mode-map (make-keymap))
  (suppress-keymap irchat-Dialogue-mode-map)
  (define-key irchat-Dialogue-mode-map "0" 'irchat-Command-jump-channel0)
  (define-key irchat-Dialogue-mode-map "1" 'irchat-Command-jump-channel1)
  (define-key irchat-Dialogue-mode-map "2" 'irchat-Command-jump-channel2)
  (define-key irchat-Dialogue-mode-map "3" 'irchat-Command-jump-channel3)
  (define-key irchat-Dialogue-mode-map "4" 'irchat-Command-jump-channel4)
  (define-key irchat-Dialogue-mode-map "5" 'irchat-Command-jump-channel5)
  (define-key irchat-Dialogue-mode-map "6" 'irchat-Command-jump-channel6)
  (define-key irchat-Dialogue-mode-map "7" 'irchat-Command-jump-channel7)
  (define-key irchat-Dialogue-mode-map "8" 'irchat-Command-jump-channel8)
  (define-key irchat-Dialogue-mode-map "9" 'irchat-Command-jump-channel9)
  (define-key irchat-Dialogue-mode-map "A" 'irchat-Command-alternative-channel)
  (define-key irchat-Dialogue-mode-map "a" 'irchat-Command-away)
  (define-key irchat-Dialogue-mode-map "b" 'irchat-Current-scroll-down)
  (define-key irchat-Dialogue-mode-map "F" 'irchat-Command-finger-direct)
  (define-key irchat-Dialogue-mode-map "f" 'irchat-Dialogue-freeze)
  (define-key irchat-Dialogue-mode-map "i" 'irchat-Command-invite)
  (define-key irchat-Dialogue-mode-map "J" 'irchat-Command-next-channel)
  (define-key irchat-Dialogue-mode-map "j" 'irchat-Command-join)
  (define-key irchat-Dialogue-mode-map "L" 'irchat-Command-load-vars)
  (define-key irchat-Dialogue-mode-map "l" 'irchat-Command-list)
  (define-key irchat-Dialogue-mode-map "m" 'irchat-Command-modec)
  (define-key irchat-Dialogue-mode-map "n" 'irchat-Command-names)
  (define-key irchat-Dialogue-mode-map "o" 'other-window)
  (define-key irchat-Dialogue-mode-map "p" 'irchat-Command-part)
  (define-key irchat-Dialogue-mode-map "r" 'irchat-Command-reconfigure-windows)
  (define-key irchat-Dialogue-mode-map "t" 'irchat-Command-topic)
  (define-key irchat-Dialogue-mode-map "u" 'irchat-Command-users)
  (define-key irchat-Dialogue-mode-map "v" 'irchat-Command-version)
  (define-key irchat-Dialogue-mode-map "w" 'irchat-Command-who)
  (define-key irchat-Dialogue-mode-map "!" 'irchat-Command-send-exec)
  (define-key irchat-Dialogue-mode-map "$" 'end-of-buffer)
  (define-key irchat-Dialogue-mode-map "/" 'irchat-Command-generic)
  (define-key irchat-Dialogue-mode-map ">" 'irchat-Command-next-channel)
  (define-key irchat-Dialogue-mode-map "<" 'irchat-Command-previous-channel)
  (define-key irchat-Dialogue-mode-map " " 'irchat-Current-scroll-up)
  (define-key irchat-Dialogue-mode-map "\C-?" 'irchat-Current-scroll-down)
  (define-key irchat-Dialogue-mode-map "\C-n" 'forward-line)
  (define-key irchat-Dialogue-mode-map "\C-m" 'irchat-Command-enter-message)
  (define-key irchat-Dialogue-mode-map "\C-c" 'irchat-command-prefix))

(if irchat-Command-mode-map
    nil
  (setq irchat-Command-mode-map (make-sparse-keymap))
;;(define-key irchat-Command-mode-map "/" 'irchat-Command-irc-compatible)
  (define-key irchat-Command-mode-map "\C-[\C-i" 'lisp-complete-symbol)
  (define-key irchat-Command-mode-map "\C-i" 'irchat-Command-complete)
  (define-key irchat-Command-mode-map "\C-m" 'irchat-Command-send-line)
  (define-key irchat-Command-mode-map "\C-c" 'irchat-command-prefix))

;;;
;;; irchat
;;;
(defun irchat (&optional confirm)
  "Connect to the IRC server and start chatting.
If optional argument CONFIRM is non-nil, ask which IRC server to connect.
If already connected, just pop up the windows."
  (interactive "P")
  (irchat-update-time)
  (if (fboundp 'add-hook)
      (add-hook 'kill-emacs-hook 'irchat-quit))
  (if (file-exists-p (expand-file-name irchat-variables-file))
      (load (expand-file-name irchat-variables-file)))
  (if (irchat-server-opened)
      (irchat-configure-windows)
    (unwind-protect
	(progn
	  (setq irchat-fatal-error-message nil)
	  (irchat-Command-setup-buffer)
	  (irchat-start-server confirm))
      (if (not (irchat-server-opened))
	  (irchat-Command-quit 'error)
	;; IRC server is successfully open. 
	(irchat-init)
	(irchat-init2)))))

(defun irchat-init ()
  (if (null irchat-old-window-configuration)
      (setq irchat-old-window-configuration
	    (current-window-configuration)))
  (let ((obuf (current-buffer)))
    (set-buffer irchat-Command-buffer)
    (setq mode-line-process (format " {%s}" irchat-server))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (sit-for 0)))
  (make-variable-buffer-local 'irchat-freeze-local)
  (set-default 'irchat-freeze-local irchat-default-freeze-local)
  (make-variable-buffer-local 'irchat-freeze-indicator-local)
  (if irchat-default-freeze-local
      (set-default 'irchat-freeze-indicator-local "F")
    (set-default 'irchat-freeze-indicator-local "-"))
  (make-variable-buffer-local 'irchat-beep-local)
  (set-default 'irchat-beep-local irchat-default-beep-local)
  (make-variable-buffer-local 'irchat-beep-indicator-local)
  (if irchat-default-beep-local
      (set-default 'irchat-beep-indicator-local "B")
    (set-default 'irchat-beep-indicator-local "-"))
  (make-variable-buffer-local 'irchat-suppress-local)
  (set-default 'irchat-suppress-local irchat-default-suppress-local)
  (make-variable-buffer-local 'irchat-suppress-indicator-local)
  (if irchat-default-suppress-local
      (set-default 'irchat-suppress-indicator-local "S")
    (set-default 'irchat-suppress-indicator-local " "))
  (make-variable-buffer-local 'irchat-previous-pattern)
  (irchat-Dialogue-setup-buffer)
  (irchat-Others-setup-buffer)
  (irchat-Private-setup-buffer)
  (irchat-KILLS-setup-buffer)
  (irchat-IGNORED-setup-buffer)
  (irchat-WALLOPS-setup-buffer)
  (setq irchat-no-configure-windows t)
  (setq irchat-Channel-buffer irchat-Private-buffer))

(defun irchat-init2 ()
  (let ((chans irchat-current-channels))
    (setq irchat-current-channels nil)
    (setq irchat-chanbuf-list nil)
    (if chans
	(while (car chans)
	  (irchat-Command-join (car chans))
	  (setq chans (cdr chans)))
      (if irchat-startup-channel
	  (irchat-Command-join (irchat-chan-real irchat-startup-channel))
	(if irchat-startup-channel-list
	    (let ((rest irchat-startup-channel-list))
	      (while rest
		(irchat-Command-join (irchat-chan-real (car rest)))
		(setq rest (cdr rest))))))))
  (run-hooks 'irchat-Startup-hook)
  (let (ok)
    (while (and (not ok) (irchat-server-opened))
      (accept-process-output irchat-server-process)
      (if (or irchat-nickname-already-in-use irchat-nickname-erroneus)
	  (progn  	
	    (setq irchat-trying-nickname
		  (irchat-read-nick (format
				     (if irchat-nickname-already-in-use
					 irchat-prompt-already-in-use-nick
				       irchat-prompt-erroneus-nick)
				     irchat-trying-nickname)))
	    (if (irchat-server-opened)
		(irchat-send "NICK %s" irchat-trying-nickname)
	      (setq irchat-nickname irchat-trying-nickname)
	      (setq irchat-nickname-already-in-use nil
		    irchat-nickname-erroneus nil)
	      (irchat 'always))))
      (setq irchat-nickname-already-in-use nil
	    irchat-nickname-erroneus nil)
      (if (not irchat-no-configure-windows)
	  (setq ok t))))
  (irchat-Channel-select 0)
  (irchat-Channel-change)
  (irchat-Command-describe-briefly))

(defun irchat-Command-mode ()
  "Major mode for IRCHAT.  Normal edit function are available.
Typing Return or Linefeed enters the current line in the dialogue.
The following special commands are available:
For a list of the generic commands type \\[irchat-Command-generic] ? RET.
\\{irchat-Command-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq	mode-line-modified "--- "
	major-mode 'irchat-Command-mode
	mode-name "IRCHAT Commands"
	irchat-privmsg-partner nil
	irchat-away-indicator "-"
	irchat-freeze-indicator "-"
	mode-line-format irchat-mode-line-format-Command)
  (use-local-map irchat-Command-mode-map)
  (if irchat-blink-parens
      nil
    (make-variable-buffer-local 'blink-matching-paren)
    (set-default 'blink-matching-paren t)
    (setq blink-matching-paren nil))
  (run-hooks 'irchat-Command-mode-hook))

(defun irchat-Dialogue-mode ()
  "Major mode for displaying the IRC dialogue.
All normal editing commands are turned off.
Instead, these commands are available:
\\{irchat-Dialogue-mode-map}"
  (kill-all-local-variables)
  (setq mode-line-modified "--- "
	major-mode 'irchat-Dialogue-mode
	mode-name "IRCHAT Dialogue"
	mode-line-format irchat-mode-line-format-Dialogue)
  (use-local-map irchat-Dialogue-mode-map)
  (set-buffer irchat-Dialogue-buffer)
  (setq buffer-read-only t)
  (run-hooks 'irchat-Dialogue-mode-hook))

(defun irchat-Others-mode ()
  "Major mode for displaying the IRC others message except current channel.
All normal editing commands are turned off.
Instead, these commands are available:
\\{irchat-Others-mode-map}"
  (kill-all-local-variables)
  (setq mode-line-modified "--- "
	major-mode 'irchat-Others-mode
	mode-name "IRCHAT Others"
	mode-line-format irchat-mode-line-format-Others)
  (use-local-map irchat-Others-mode-map)
  (set-buffer irchat-Others-buffer)
  (setq buffer-read-only t)
  (run-hooks 'irchat-Others-mode-hook))

(defun irchat-Channel-mode ()
  "Major mode for displaying the IRC current channel buffer.
All normal editing commands are turned off.
Instead, these commands are available:
\\{irchat-Channel-mode-map}"
  (kill-all-local-variables)
  (setq mode-line-modified "--- "
	major-mode 'irchat-Channel-mode
	mode-name "IRCHAT Current channel"
	mode-line-format irchat-mode-line-format-Channel)
  (use-local-map irchat-Channel-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'irchat-Channel-mode-hook))

(defun irchat-configure-windows ()
  "Configure Command mode and Dialogue mode windows.
One is for entering commands and text, the other displays the IRC dialogue."
  (setq irchat-no-configure-windows nil)
  (let ((obuf (current-buffer)))
    (if (or (one-window-p t)
	    (null (get-buffer-window irchat-Command-buffer))
	    (null (get-buffer-window irchat-Dialogue-buffer))
	    (and irchat-channel-buffer-mode
		 (or (null (get-buffer-window irchat-Channel-buffer))
		     (null (get-buffer-window irchat-Others-buffer))))
	    (and (null irchat-channel-buffer-mode)
		 (or (get-buffer-window irchat-Channel-buffer)
		     (get-buffer-window irchat-Others-buffer))))
	(progn
	  (if irchat-command-window-on-top
	      (progn
		(if (not irchat-use-full-window)
		    (set-window-configuration irchat-old-window-configuration))
		(switch-to-buffer irchat-Command-buffer)
		(if irchat-use-full-window
		    (delete-other-windows))
		(if irchat-one-buffer-mode
		    (switch-to-buffer irchat-Dialogue-buffer)
		  (split-window-vertically (max window-min-height 
						irchat-command-window-height))
		  (other-window 1)
		  (if irchat-channel-buffer-mode
		      (progn
			(split-window-vertically
			 (max window-min-height 
			      (/ (* (window-height)
				    irchat-channel-window-height-percent)
				 100)))
			(switch-to-buffer irchat-Channel-buffer)
			(other-window 1)
			(switch-to-buffer irchat-Others-buffer)
			(goto-char (point-max))
			(recenter (- (window-height) 1))
			(select-window
			 (get-buffer-window irchat-Command-buffer)))
		    (switch-to-buffer irchat-Dialogue-buffer)
		    (if (not irchat-freeze)
			(progn
			  (goto-char (point-max))
			  (recenter (- (window-height) 1))))
		    (select-window
		     (get-buffer-window irchat-Command-buffer)))))
	    ;; mta@tut.fi wants it like this
	    (switch-to-buffer irchat-Dialogue-buffer)
	    (if (not irchat-freeze)
		(progn
		  (goto-char (point-max))
		  (recenter (- (window-height) 1))))
	    (if irchat-use-full-window
		(delete-other-windows))
	    (if irchat-one-buffer-mode
		nil
	      (split-window-vertically
	       (- (window-height) (max window-min-height 
				       irchat-command-window-height)))
	      (if irchat-channel-buffer-mode
		  (progn
		    (split-window-vertically
		     (max window-min-height 
			  (/ (* (window-height)
				(- 100 irchat-channel-window-height-percent))
			     100)))
		    (switch-to-buffer irchat-Others-buffer)
		    (goto-char (point-max))
		    (recenter (- (window-height) 1))
		    (other-window 1)
		    (switch-to-buffer irchat-Channel-buffer)))
	      (other-window 1)
	      (switch-to-buffer irchat-Command-buffer)
	      (if (not irchat-channel-buffer-mode)
		  (progn
		    (select-window
		     (get-buffer-window irchat-Dialogue-buffer))
		    (if (not irchat-freeze)
			(progn
			  (goto-char (point-max))
			  (recenter (- (window-height) 1))))))
	      (select-window
	       (get-buffer-window irchat-Command-buffer))))))
    (set-buffer obuf)))

(defun irchat-Command-setup-buffer ()
  "Initialize Command mode buffer."
  (or (get-buffer irchat-Command-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-Command-buffer))
	(irchat-Command-mode))))

(defun irchat-Dialogue-setup-buffer ()
  "Initialize Dialogue mode buffer."
  (or (get-buffer irchat-Dialogue-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-Dialogue-buffer))
	(irchat-Dialogue-mode))))

(defun irchat-Others-setup-buffer ()
  "Initialize Others mode buffer."
  (or (get-buffer irchat-Others-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-Others-buffer))
	(irchat-Others-mode))))

(defun irchat-Private-setup-buffer ()
  "Initialize private conversation buffer."
  (or (get-buffer irchat-Private-buffer-name)
      (save-excursion
	(set-buffer (get-buffer-create irchat-Private-buffer-name))
	(insert "This is my private buffer.\n")
	(irchat-Channel-mode)))
  (setq irchat-Private-buffer (get-buffer irchat-Private-buffer-name)))

(defun irchat-KILLS-setup-buffer ()
  "Initialize KILLS buffer."
  (or (get-buffer irchat-KILLS-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-KILLS-buffer)))))

(defun irchat-IGNORED-setup-buffer ()
  "Initialize IGNORED buffer."
  (or (get-buffer irchat-IGNORED-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-IGNORED-buffer)))))

(defun irchat-WALLOPS-setup-buffer ()
  "Initialize WALLOPS buffer."
  (or (get-buffer irchat-WALLOPS-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-WALLOPS-buffer)))))

(defun irchat-clear-system ()
  "Clear all IRCHAT variables and buffers."
  (interactive)
  (if (and irchat-Command-buffer (get-buffer irchat-Command-buffer))
      (bury-buffer irchat-Command-buffer))
  (if (and irchat-Dialogue-buffer (get-buffer irchat-Dialogue-buffer))
      (bury-buffer irchat-Dialogue-buffer))
  (if (and irchat-KILLS-buffer (get-buffer irchat-KILLS-buffer))
      (bury-buffer irchat-KILLS-buffer))
  (if (and irchat-IGNORED-buffer (get-buffer irchat-IGNORED-buffer))
      (bury-buffer irchat-IGNORED-buffer))
  (if (and irchat-WALLOPS-buffer (get-buffer irchat-WALLOPS-buffer))
      (bury-buffer irchat-WALLOPS-buffer))
  (if (and irchat-debug-buffer (get-buffer irchat-debug-buffer))
      (bury-buffer irchat-debug-buffer))
  (setq irchat-debug-buffer nil))

(defun irchat-start-server (&optional confirm)
  "Open network stream to remote irc server.
If optional argument CONFIRM is non-nil, ask the host that the server
is running on."
  (if (irchat-server-opened)
      ;; Stream is already opened.
      nil
    ;; Open IRC server.
    (if (or
	 (and confirm
	      (not (eq confirm 'always)))
	 (null irchat-server))
	(setq irchat-server
	      (irchat-read-server "IRC server: " irchat-server)))
    (if (and confirm
	     (not (eq confirm 'always))
	     irchat-ask-for-nickname)
	(setq irchat-nickname
	      (irchat-read-nick "Enter your nickname: " irchat-nickname)))
    ;; If no server name is given, local host is assumed.
    (if (string-equal irchat-server "")
	(setq irchat-server (system-name)))
    (if (setq errstr (irchat-open-server irchat-server irchat-service))
	(error "ERROR: %s -- Cannot connect IRC server \"%s\""
	       errstr irchat-server))))

(defun irchat-open-server (host &optional service)
  "Open chat server on HOST.
If HOST is nil, use value of environment variable \"IRCSERVER\".
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "IRCSERVER"))) tmp errstr)
    (if (and host (assoc host irchat-server-alist)
	     (cdr (assoc host irchat-server-alist)))
	(setq host (cdr (assoc host irchat-server-alist))))
    (if (or (string-match "^\\(\\[[^]]+\\]\\):\\([0-9]+\\)\\(.*\\)" host)
	    (string-match "^\\([^\\[][^:]+\\):\\([0-9]+\\)\\(.*\\)" host))
	(progn
	  (setq tmp (matching-substring host 3))
	  (setq service (string-to-int (matching-substring host 2)))
	  (setq host (matching-substring host 1))
	  (if (string-match "^:\\(.*\\)" tmp)
	      (if (string= (matching-substring tmp 1) "")
		  (setq irchat-ask-for-password t)
		(setq irchat-ask-for-password nil)
		(setq irchat-password (matching-substring tmp 1))))))
    (setq irchat-my-server host) ;; temporary
    (message "Connecting to IRC server %s..." host)
    (if (null host)
	"IRC server is not specified."
      (if (setq errstr (irchat-open-server-internal host service))
	  errstr
	(setq irchat-after-registration nil)
	(irchat-send "PING :%s" host)
	(if (setq errstr (irchat-wait-for-response))
	    (progn
	      ;; We have to close connection here, since the function
	      ;;  `irchat-server-opened' may return incorrect status.
	      (irchat-close-server-internal)
	      errstr)
	  (setq irchat-after-registration t)
	  (set-process-sentinel irchat-server-process 'irchat-sentinel)
	  (set-process-filter irchat-server-process 'irchat-filter)
	  (if (or irchat-ask-for-password 
		  irchat-reconnect-with-password)
	      (let (password)
		(setq password (irchat-read-passwd "Server Password: "))
		(if (not (string= password ""))
		    (irchat-send "PASS %s" password)))
	    (if irchat-password
		(irchat-send "PASS %s" irchat-password)))
	  (setq irchat-reconnect-with-password nil)
	  (irchat-send "USER %s * * :%s" 
		       (if (and (stringp irchat-user)
				(not (string= irchat-user "")))
			   irchat-user "Nobody")
		       (if (and (stringp irchat-name)
				(not (string= irchat-name "")))
			   (irchat-encode-var irchat-name)
			 "Nanashi no Gombei"))
	  (setq irchat-trying-nickname irchat-nickname)
	  (irchat-send "NICK %s" irchat-nickname)
	  (setq irchat-after-registration t)
	  nil)))))

(defun irchat-close-server ()
  "Close chat server."
  (unwind-protect
      (progn
	;; Un-set default sentinel function before closing connection.
	(and irchat-server-process
	     (eq 'irchat-sentinel
		 (process-sentinel irchat-server-process))
	     (set-process-sentinel irchat-server-process nil))
	;; We cannot send QUIT command unless the process is running.
	(if (irchat-server-opened)
	    (irchat-send "QUIT")))
    (irchat-close-server-internal)))

(defun irchat-server-opened ()
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and irchat-server-process
       (memq (condition-case tmp
		 (process-status irchat-server-process)
	       (error nil))
	     '(open run))))

(defun irchat-open-server-internal (host service)
  "Open connection to chat server on HOST by SERVICE (default is irc)."
  (irchat-define-service-coding-system service)
  (save-excursion
    ;; Initialize communication buffer.
    (setq irchat-server-buffer (get-buffer-create " *IRC*"))
    (set-buffer irchat-server-buffer)
    (kill-all-local-variables)
    (erase-buffer)
    (setq irchat-server-name host)
    (let (errstr)
      (if (string-match "^[^\\[]" host)
	  (condition-case tmp
	      (setq irchat-server-process
		    (open-network-stream "IRC" irchat-server-buffer
					 host service))
	    (error
	     (setq irchat-server-process nil)
	     (setq errstr (irchat-expand-error tmp))))
	(if (not (string-match
		  "^\\[\\(.+\\)\\]$" host))
	    (setq errstr (concat "Unmatch [ and ]: " host))
	  (condition-case tmp
	      (setq irchat-server-process
		    (start-process "IRC" (current-buffer)
				   irchat-dcc-program "tcp" "connect"
				   (matching-substring host 1) 
				   (format "%s" service)))
	    (error
	     (setq irchat-server-process nil)
	     (setq errstr (concat "Cannot execute: "
				  (irchat-expand-error tmp)))))
	  (irchat-set-process-coding-system irchat-server-process)
	  (set-process-sentinel irchat-server-process 'irchat-sentinel)))
      errstr)))

(defun irchat-expand-error (list)
  (let (str)
    (setq list (cdr list))
    (while list
      (if (stringp (car list))
          (if str
              (setq str (concat str ": " (car list)))
            (setq str (car list))))
      (setq list (cdr list)))
    (or str "error")))

(defun irchat-close-server-internal ()
  "Close connection to chat server."
  (if (and irchat-server-process (string-match "^[^\\[]" irchat-server-name))
      (delete-process irchat-server-process))
  (if irchat-server-buffer
      (kill-buffer irchat-server-buffer))
  (setq irchat-server-buffer nil
	irchat-server-process nil))

(defun irchat-wait-for-response ()
  (save-excursion
    (let ((status t) (wait t) strerr
	  (regexp "^:[^ ]+ [4P][5O][1N][ G]"))
      (set-buffer irchat-server-buffer)
	(while wait
	  (if (setq errstr (irchat-accept-response))
	      (setq wait nil)
	    (goto-char (point-min))
	    (cond ((looking-at "ERROR")
		   (setq status nil)
		   (setq wait nil))
		  ((looking-at ".")
		   (setq wait nil)))))
      ;; Save status message.
      (end-of-line)
      (setq strerr (buffer-substring (point-min) (point)))
      (if status
	  (progn
	    (setq wait t)
	    (while wait
	      (goto-char (point-max))
	      (forward-line -1)		;(beginning-of-line)
	      (if (looking-at regexp)
		  (setq wait nil)
		(message "IRCHAT: Reading...")
		(irchat-accept-response)
		(message "")))
	    ;; Successfully received server response.
	    (delete-region (point-min) (point-max))
	    nil)
	strerr))))

(defun irchat-accept-response ()
  "Read response of server. Only used at startup time"
  (if (irchat-server-opened)
      (condition-case tmp
	  (accept-process-output irchat-server-process)
	(error
	 (if (string-equal "select error: Invalid argument" (nth 1 tmp))
	     ;; Ignore select error.
	     nil)
	   (irchat-expand-error tmp)))
    (if (not irchat-reconnect-automagic)
	"IRC server connection closed."
      (if irchat-grow-tail
	  (irchat 'always)
	(irchat))
      nil)))

(defun irchat-scroll-if-visible (window)
  (if window (set-window-point window (point-max))))

(provide 'irchat)
