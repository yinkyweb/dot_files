;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; user modifiable variables
;;;
(defvar irchat-version-plus nil
  "*If you define this, it will be used for CTCP version reply.")

(defvar irchat-command-window-height 4
  "*How large should Command window be on startup.")

(defvar irchat-channel-window-height-percent 63
  "*How large percent should Current channel window be on startup.")

(defvar irchat-want-traditional nil
  "*Do we want Irchat to look like IrcII.")

(defvar irchat-variables-file "~/.irchat_vars.el"
  "*Where to look for variables. Helps to remove clutter from your .emacs.
This feature is most likely to dissappear in near future. The preferred 
way is to put irchat variables on .emacs or file loaded from there.")

(defvar irchat-server-alist
  '(("irc.nara.wide.ad.jp" . "irc.nara.wide.ad.jp:6661")
    ("irc.tokyo.wide.ad.jp" . "irc.tokyo.wide.ad.jp:6661")
    ("irc.media.kyoto-u.ac.jp" . "irc.media.kyoto-u.ac.jp:6661"))
  "*IRC server assoc-list which is used for input IRC server.")

(defvar irchat-server (or (getenv "IRCSERVER") nil)
  "*Name of the host running the IRC server. 
Value initialized from the IRCSERVER environment variable if one is set")

(defvar irchat-service 
  (let ((ircport-env (getenv "IRCPORT")))
    (if ircport-env
	(if (> (string-to-int ircport-env) 0)
	    (string-to-int ircport-env)
	  ircport-env)
      6667))
  "*IRC service name or (port) number.")

(defvar irchat-dcc-auto-get-file nil
  "*If non-nil, get a file automatically when DCC request received.")

(defvar irchat-dcc-program "dcc"
  "*Name of dcc-program.")

(defvar irchat-dcc-directory "~/tmp"
  "*Directory where irchat-dcc puts its files.")

(defvar irchat-name (or (getenv "IRCNAME") (getenv "NAME") (user-full-name))
  "*Your realname.
Initialized from the IRCNAME or NAME environment variable, or your GCOS field.")

(defvar irchat-user (user-real-login-name)
  "*Your username.")

(defvar irchat-password (or (getenv "IRCPASSWORD") nil)
  "*Your password when connecting to server.")

(defvar irchat-nickname (or (getenv "IRCNICK") (user-real-login-name))
  "*The nickname you want to use in IRC.
Default is the environment variable IRCNICK, or your login name.")

(defvar irchat-startup-channel nil
  "obsolete.")

(defvar irchat-startup-channel-list nil
  "*The channel list to join automatically at startup.
If nil, do not join any channel.")

(defvar irchat-default-channel-binding nil
  "*The channel list to bind the channel number when joining.")

(defvar irchat-channel-conversion-map nil
  "*The map of channel conversion.")

(defvar irchat-channel-conversion-default-mask "*.jp"
  "*The default mask of channel conversion.")

(defvar irchat-browser-function 'irchat-exec-netscape
  "*The function to exec a browser. irchat-exec-netscape is a sample.
You can use browse-url if you have it.")

(defvar irchat-browse-url-command-format "netscape -remote 'openURL(%s)'"
  "*The command when browsing URLs.")

(defvar irchat-print-userhost-when nil
  "*If 'always, print userhost. If 'new, print userhost for a new user.")

(defvar irchat-signoff-msg "irchat exiting..."
  "*Default signoff message to use when you quit IRC.")

(defvar irchat-channel-signoff-msg "bye..."
  "*Default signoff message to use when you part channel.")

(defvar irchat-ignore-changes nil
  "*Ignore changes? Good in topic-wars/link troubles.")

(defvar irchat-print-time t
  "*If non-nil, print time prefix on each line in channel buffer*")

(defvar irchat-others-high-watermark 10000
  "*The value of high watermark in Others buffer*")

(defvar irchat-others-low-watermark 5000
  "*The value of high watermark in Others buffer*")

(defvar irchat-use-full-window t
  "*If non-nil, IRCHAT will use whole emacs window. Annoying for GNUS-
users, therefore added by nam.")

(defvar irchat-display-channel-always nil
  "*If non nil, display channel name still in channel buffer.")

(defvar irchat-default-suppress-local nil
  "*If non nil, channel buffer local suppress flag is on at starting.")

(defvar irchat-default-freeze-local nil
  "*If non nil, channel buffer local freeze flag is on at starting.")

(defvar irchat-default-beep-local nil
  "*If non nil, channel buffer local beep flag is on at starting.")

(defvar irchat-beep-command-list
  (list 'PRIVMSG 'INVITE 'KILL 'KICK 'DCC)
  "*Command list you want a beep when the command is received
on buffer where beep flag is on.")

(defvar irchat-message-try-to-connect-server nil
  "*If non nil, notify to mini buffer when trying to connect server.")

(defvar irchat-reconnect-automagic nil
  "*Automatic reconnection, default is disabled")

(defvar irchat-ask-for-nickname nil
  "*Ask for nickname if irchat was entered with \\[universal-argument].")

(defvar irchat-ask-for-password nil
  "*Ask for password when connecting to server.")

(defvar irchat-ask-for-channel-password nil
  "*Ask for channel password when joining channel with password.")

(defvar irchat-reconnect-with-password nil
  "*auto recconect to server with password after incorrect password.")

(defvar irchat-grow-tail "_"
  "*Add irchat-grow-tail to nick when reconnecting. Otherwise you might get
killed again if automagic reconnect is too fast.")

(defvar irchat-show-wallops t)

(defvar irchat-beep-on-bells nil
  "*If non-nil, and the IRC Dialogue buffer is not selected in a window,
an IRC message arriving containing a bell character, will cause you
to be notified.
If value is 'always, an arriving bell will always cause a beep (or flash).")

(defvar irchat-ctcp-userinfo "No user information given."
  "*Userinfo message given to anyone asking via CTCP.")

(defvar irchat-url-regexp "http:[-#%&+\,./0-9:=\?@A-Z_a-z~]+"
  "*The regular expression for URL.")

;;;
;;; irchat-decode/encode
;;;
(defvar irchat-decode-function 'irchat-decode-iso-2022-jp
  "*Function for decoding string. The function should returns
the decoded string on successful conversion or nil.")
(defvar irchat-encode-function 'irchat-encode-iso-2022-jp
  "*Function for encoding string. The function should returns
the encoded string on successful conversion or nil.")
(defvar irchat-decode-message-function 'irchat-decode-message-iso-2022-jp
  "*Function for decoding string. The function always should returns
the decoded string.")
(defvar irchat-encode-message-function 'irchat-encode-message-iso-2022-jp
  "*Function for decoding string. The function always should returns
the encoded string.")
  
;;;
;;; irchat-mode-line
;;;
(defvar irchat-mode-line-format-for-im
  (list '(mode-line-mule-info
	  ("-" mode-line-mule-info))
	'(skk-mode-invoked
	  (skk-mode skk-input-mode-string " ----"))
	'(egg:*mode-on*
	  ((minibuffer-window-selected (display-minibuffer-mode "m" " ") " ")
	   "["
	   (minibuffer-window-selected (display-minibuffer-mode
					mode-line-egg-mode-in-minibuffer
					mode-line-egg-mode)
				       mode-line-egg-mode)
	   "]"))
	'(canna:*initialized*
	  (minibuffer-window-selected (display-minibuffer-mode
				       mode-line-canna-mode-in-minibuffer
				       mode-line-canna-mode)
				      mode-line-canna-mode)))
  "*mode line format for input method.
You may use this format for irchat-mode-line-format-Command.")

(defvar irchat-mode-line-format-Command-simple
  (list "" 
	'irchat-away-indicator 'irchat-freeze-indicator
	" IRCHAT: Commands"
	" {" 'irchat-chanbuf-indicator "} "
	'irchat-current-nickname " on " 'irchat-current-servername
	" -%-")
  "*You may use this format for irchat-mode-line-format-Command.")

(defvar irchat-mode-line-format-Command
  (cons "" 
	(append irchat-mode-line-format-for-im
		irchat-mode-line-format-Command-simple))
  "*Mode line format for Command buffer.")

(defvar irchat-mode-line-format-Channel
  (list ""
	'irchat-beep-indicator-local 'irchat-freeze-indicator-local
	'irchat-suppress-indicator-local
	"IRCHAT: Current" 
	" {" 'irchat-chanbuf-indicator "} "
	'(-3 . "%p")
	" -%-")
  "*Mode line format for Channel buffer.")

(defvar irchat-mode-line-format-Dialogue
  (list ""
	'irchat-away-indicator 'irchat-freeze-indicator
	" {" 'irchat-chanbufs-indicator "} "
	'(-3 . "%p")
	" -%-")
  "*Mode line format for Dialogue buffer.")

(defvar irchat-mode-line-format-Others
  (list ""
	'irchat-away-indicator "-"
	" {" 'irchat-chanbufs-indicator "} "
	'(-3 . "%p")
	" -%-")
  "*Mode line format for Others buffer.")

;;;
;;; irchat-format
;;;
(defvar irchat-format-separator-channel ",")
(defvar irchat-format-separator-nick ",")
(defvar irchat-format-separator-mode " ")

(defvar irchat-format-join  "%1N has joined channel %2c")
(defvar irchat-format-joinx "%1N has joined channel %2c")
(defvar irchat-format-part  "%1N has left channel %2c (%3m)")
(defvar irchat-format-partx "%1N has left channel %2c (%3m)")
(defvar irchat-format-kick  "%1N has kicked %3n out from channel %2c (%4m)")
(defvar irchat-format-kickx "%1N has kicked %3n out from channel %2c (%4m)")
(defvar irchat-format-mode  "New mode for %2c set by %1n: %3o")
(defvar irchat-format-modex "New mode set by %1n: %3o")
(defvar irchat-format-topic  "New topic on %2c set by %1n: %3m")
(defvar irchat-format-topicx "New topic set by %1n: %3m")
(defvar irchat-format-quit  "%1N has left IRC (%2m)")
(defvar irchat-format-nick  "%1n is now known as %2n")
(defvar irchat-format-kill    "*** You were killed by %1n Path: %3m")
(defvar irchat-format-invite  "*** %1n invites you to channel %3c")
(defvar irchat-format-wallops "*** Wallops: %1n: %2m")
(defvar irchat-format-notice  "%3m")
(defvar irchat-format-error   "ERROR: %2m")

(defvar irchat-format-death   "*** You were Killed")
(defvar irchat-format-mode-me "*** Your new mode is set: %1o")
(defvar irchat-format-kick-me "*** You were kicked off channel %2c by %1n (%4m)")

(defvar irchat-format-action-to-channel  "*%2c:%1n* %3m")
(defvar irchat-format-action-to-channelx "*%1n* %3m")
(defvar irchat-format-action-to-me       "*%1n* %3m")

(defvar irchat-format-action-by-me0  "+%2n+ %3m")
(defvar irchat-format-action-by-me1  "+%2c:%1n+ %3m")
(defvar irchat-format-action-by-me1x "+%1n+ %3m")

(defvar irchat-format-message-to-channel-normal  "<%2c:%1n> %3m")
(defvar irchat-format-message-to-channel-normalx "<%1n> %3m")
(defvar irchat-format-message-to-channel-notice  "[%2c:%1n] %3m")
(defvar irchat-format-message-to-channel-noticex  "[%1n] %3m")
(defvar irchat-format-message-to-channel-nonmem  "(%2c:%1n) %3m")
(defvar irchat-format-message-to-channel-nonmemx "(%1n) %3m")

(defvar irchat-format-message-to-me0 "=%1n= %3m")
(defvar irchat-format-message-to-me1 "=%1n== %3m")
(defvar irchat-format-message-to-me2 "=%1n=== %3m")

(defvar irchat-format-message-by-me0  ">%2n< %3m")
(defvar irchat-format-message-by-me1  ">%2c:%1n< %3m")
(defvar irchat-format-message-by-me1x ">%1n< %3m")
(defvar irchat-format-message-by-me2  ")%2c:%1n( %3m")
(defvar irchat-format-message-by-me2x ")%1n( %3m")

(defvar irchat-format-001  "*** %3m")
(defvar irchat-format-002  "*** %3m")
(defvar irchat-format-003  "*** %3m")
(defvar irchat-format-004  "*** %3m %4m %5m %6m")
(defvar irchat-format-005b "*** %3m")
(defvar irchat-format-005m "*** %3m:%4m")
(defvar irchat-format-042  "*** %4m: %3m")

(defvar irchat-format-200  "*** Link %1m (%4m %7m) ==> %6m (%5m) [%8t] %9m/%am")
(defvar irchat-format-201  "*** %3m %1m (%4m) ==> %5m")
(defvar irchat-format-202  "*** %3m %1m (%4m) ==> %5m")
(defvar irchat-format-203  "*** %3m %1m (%4m) ==> %5m")
(defvar irchat-format-204  "*** %3m %1m (%4m) ==> %5m")
(defvar irchat-format-205  "*** %3m %1m (%4m) ==> %5m")
(defvar irchat-format-206  "*** Server %1m (%4m) ==> %7m {%5m,%6m} %8m [%9m]\n")
(defvar irchat-format-207  "*** Service %1m (%4m) ==> %5m %6m %7m")
(defvar irchat-format-208  "*** %3m %1m (%4m) ==> %5m")
(defvar irchat-format-209  "*** Class %4m = %5m entries")
(defvar irchat-format-210  "*** %3m %1m (%4m) ==> %5m")
(defvar irchat-format-211  "%3m")
(defvar irchat-format-211h "    Time             Send-Q   Send-Msg    Send-KB   Recv-Msg    Recv-KB")
(defvar irchat-format-211p "%9-18T %4-8m %5-10m %6-10m %7-10m %8-10m")

(defvar irchat-format-221  "Mode for you: %3o")
(defvar irchat-format-257  "*** %3m")
(defvar irchat-format-258  "*** %3m")
(defvar irchat-format-259  "*** %3m")
(defvar irchat-format-262  "*** End of TRACE to (%3m) %1m [%4m]")
(defvar irchat-format-265o "*** %3m")
(defvar irchat-format-265  "*** Local: %3m users (%4m max users)")
(defvar irchat-format-266o "*** %3m")
(defvar irchat-format-266  "*** Global: %3m users (%4m max users)")

(defvar irchat-format-301  "%3n is AWAY: %4m")
(defvar irchat-format-302  nil)
(defvar irchat-format-302s "%3n is <%6r@%7r> [%4m, %5m]")
(defvar irchat-format-303  "Following people(s) are on: %3m")
(defvar irchat-format-303n "No one you requested is on now.")
(defvar irchat-format-305  "*** %3m")
(defvar irchat-format-306  "*** %3m")
(defvar irchat-format-311  "%3n is <%4r@%5r> %6m")
(defvar irchat-format-312  "on via server %4m (%5m)")
(defvar irchat-format-313  "STATUS: %4m")
(defvar irchat-format-314  "%3n was <%4r@%5r> %6m")
(defvar irchat-format-315  nil)
(defvar irchat-format-317  "IDLE for %4t (%4a) [since %5T]")
(defvar irchat-format-318  nil)
(defvar irchat-format-319  "channels: %4{irchat-whoischannels}")
(defvar irchat-format-321  nil)
(defvar irchat-format-322  "Topic for %3c (%4m users): %5m")
(defvar irchat-format-322x "Topic (%4m users): %5m")
(defvar irchat-format-323  nil)
(defvar irchat-format-324  "Mode for %3c: %4o")
(defvar irchat-format-331  "No topic is set for %3c")
(defvar irchat-format-332  "Topic for %3c: %4m")
(defvar irchat-format-332x "Topic: %4m")
(defvar irchat-format-333  "by %4m (%5T)")
(defvar irchat-format-341  "Inviting user %3n to channel %4c")
(defvar irchat-format-346  "Invitations on %3c: %4m")
(defvar irchat-format-347  nil)
(defvar irchat-format-351  "%4m: version %3m (%6m) [sid %5m]")
(defvar irchat-format-351o "%4m: version %3m (%5m)")
(defvar irchat-format-352  "%8+3m %3+9c %7+9n <%4r@%5r> %am")
(defvar irchat-format-353  "%4c = %5r")
(defvar irchat-format-361  "You just KILLED %3n. %4m")
(defvar irchat-format-364  "%3+30m %6m")
(defvar irchat-format-364u "%5+2m %3+30m <== %4+30m")
(defvar irchat-format-365  "*** No match server. (%3m)")
(defvar irchat-format-366  nil)
(defvar irchat-format-367  "Banned on %3c: %4m")
(defvar irchat-format-368  "*** No ban list. (%3c)")
(defvar irchat-format-369  nil)
(defvar irchat-format-371  "*** %3m")
(defvar irchat-format-372  "*** %3m")
(defvar irchat-format-373  nil)
(defvar irchat-format-374  nil)
(defvar irchat-format-375  nil)
(defvar irchat-format-376  nil)
(defvar irchat-format-381  "OPER: %3m")
(defvar irchat-format-382  "*** %4m %3m")
(defvar irchat-format-391  "Time: %4m (%3m)")

(defvar irchat-format-407  "*** %3c %4m")
(defvar irchat-format-441  "*** user %3n is not on channel %4c")
(defvar irchat-format-476  "*** Bad Channel Mask: %3c")

(defvar irchat-format-stats-header "STATS %2m %1m")
(defvar irchat-format-dcc-report "%1m%2mCTCP %3m %4u%5mfrom %6n to %7c %8m")
(defvar irchat-format-dcc-reply "%2u@%1n: %3m")
(defvar irchat-format-irchat "%1m %2m")

(defvar irchat-prompt-message "to %1c: ")
(defvar irchat-prompt-join-password "Key for channel %1c: ")
(defvar irchat-prompt-mode-channel "Channel for MODE: ")
(defvar irchat-prompt-mode "Mode for channel %1c: ")
(defvar irchat-prompt-message-target "Private message to: ")
(defvar irchat-prompt-topic "Topic for channel %1c: ")

(defvar irchat-prompt-already-in-use-nick
  "IRC: Nickname \"%s\" already in use. Choose another one: ")
(defvar irchat-prompt-erroneus-nick
  "IRC: Erroneus nickname \"%s\". Choose another one: ")

(defvar irchat-Private-buffer-name " Private")

(provide 'irchat-vars)
