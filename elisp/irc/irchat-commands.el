;;; see file irchat-copyright.el for change log and copyright info

(defun irchat-Command-describe-briefly ()
  (message (substitute-command-keys "Type \\[describe-mode] for help")))

(defun irchat-Command-redisplay (&optional center)
  "Un-freezes and re-selects the Dialogue buffer in another window.
   With argument, recenter with that argument."
  (interactive "P")
  (if irchat-channel-buffer-mode
      (progn
	(if (null (get-buffer-window irchat-Channel-buffer))
	    (irchat-configure-windows))
	(set-buffer irchat-Channel-buffer)
	(goto-char (point-max))
	(set-window-point (get-buffer-window irchat-Channel-buffer)
			  (point-max)))
    (setq irchat-freeze nil)
    (setq irchat-freeze-indicator "-")
    (if (get-buffer-window irchat-Dialogue-buffer)
	(let ((owin (selected-window)) win)
	  (if (one-window-p)
	      (irchat-configure-windows)
	    (display-buffer irchat-Dialogue-buffer))
	  (if (setq win (get-buffer-window irchat-Dialogue-buffer))
	      (let ((obuf (current-buffer)))
		(set-buffer irchat-Dialogue-buffer)
		(goto-char (point-max))
		(select-window win)
		(recenter (- (window-height) 1))
		(select-window owin)
		(set-buffer obuf)))))))

(defun irchat-Command-debug-user ()
  "for debugging."
  (interactive)
  (let (chans str chan oper nick)
    (setq nick (irchat-read-nick "NICK: "))
    (setq chans (irchat-get-chans nick))
    (setq str (format "%s =" nick))
    (while chans
      (setq chan (car chans))
      (setq oper (cdr (assoc nick (get (intern chan) 'nicka))))
      (setq str (format "%s %s%s" str (or oper "")
			(irchat-chan-virtual chan)))
      (setq chans (cdr chans)))
    (irchat-insert (format "%s (shared channels)\n" str))
    (irchat-insert (format "%s = [%s] to %s last seen\n" nick
			   (irchat-past-time1
			    (irchat-get-lasttime nick))
			   (irchat-chan-virtual
			    (or (irchat-get-lastchan nick) "(not yet)"))))
    (irchat-insert (format "%s = <%s>\n" nick
			   (or (irchat-get-userhost nick) "not yet")))))

(defun irchat-Command-send-message (msg)
  "Send MESSAGE to current chat partner of current channel."
  (if irchat-current-target
      (irchat-send-privmsg irchat-current-target msg)
    (beep t)
    (message (substitute-command-keys 
	      "Type \\[irchat-Command-join] to join a channel/nick"))))

(defun irchat-Command-send-line ()
  "Send the current line to the current channel."
  (interactive)
  (irchat-Command-send-message (irchat-read-from-line)))

(defun irchat-Command-enter-message ()
  "Ask for a line as an entry in the IRC dialogue on the current channel."
  (interactive)
  (let ((msg "dummy"))
    (if irchat-current-target
	(while (not (string= msg ""))
	  (setq msg (irchat-read-message
		     (irchat-format irchat-prompt-message
				    irchat-current-target)))
	  (if (not (string= msg ""))
	      (irchat-Command-send-message msg)))
      nil)))

(defun irchat-Command-debug ()
  "Start debugging irchat."
  (interactive)
  (if irchat-debug-buffer
      (progn
	(setq irchat-debug-buffer nil)
	(other-window 1)
	(delete-window)
	(other-window -1))
    (if irchat-use-full-window
	(delete-other-windows))
    (irchat-configure-windows)
    (split-window-horizontally)
    (other-window 1)
    (setq irchat-debug-buffer (get-buffer-create "*IRC Debugging*"))
    (switch-to-buffer irchat-debug-buffer)
    (other-window -1)))

(defun irchat-Command-inline ()
  "Send current line as a message to the IRC server."
  (interactive)
  (irchat-send "%s" (irchat-read-from-line)))

(defun irchat-Command-join (chan &optional key)
  "Join a channel or private conversation.
If user nicname is given, join the same set of channels as 
the specified user. 
If Command-buffer is in chat-mode, start private conversation 
with specified user."
  (interactive (let (chan key (completion-ignore-case t))
		 (setq chan 
		       (if (not (numberp current-prefix-arg))
			   (irchat-read-chan
			    "Channel/Nick for JOIN: "
			    (append irchat-nick-alist
				    irchat-chan-alist)
			    (or irchat-invited-channel irchat-privmsg-partner))
			 current-prefix-arg))
		 (if (and current-prefix-arg
			  (not (numberp current-prefix-arg)))
		     (setq key
			   (if (eq current-prefix-arg '-)
			       (irchat-read-passwd
				(irchat-format irchat-prompt-join-password
					       chan))
			     (irchat-read-message
			      (irchat-format irchat-prompt-join-password
					     chan)))))
		 (list chan key)))
  (if (numberp chan)
      (irchat-Channel-jump chan)
    (setq irchat-invited-channel nil)
    (if (string= "" chan)
	(message "")
      (if (irchat-ischannel chan)
	  (irchat-Command-join-channel chan key)
	(irchat-Command-join-partner chan))))
  ;; refresh mode line
  (set-buffer-modified-p (buffer-modified-p)))

(defun irchat-Command-join-channel (chan key)
  (let ((found nil))
    (setq found (irchat-Channel-exist chan))
    (if found
	(progn
	  (if (not (string= chan found))
	      (irchat-Channel-replace found chan))
	  (irchat-Channel-join found))
      (if (null key)
	  (setq key (get (intern chan) 'key))
	(put (intern chan) 'key key))
      (if (null key)
	  (setq key ""))
      (irchat-chan-put chan)
      (irchat-send "JOIN %s %s" chan key))))

(defun irchat-Command-join-partner (partner)
  (setq irchat-partners (cons partner (irchat-delete partner irchat-partners)))
  (irchat-Channel-join partner))

(defun irchat-Command-part (chan &optional reason)
  "Part a channel or private conversation."
  (interactive
   (let (chan (completion-ignore-case t)
	      (reason (irchat-encode-var irchat-channel-signoff-msg)))
     (setq chan (irchat-read-chan
		 "Channel/Nick for PART: "
		 (append (list-to-assoclist irchat-partners)
			 (irchat-current-chan-alist))
		 irchat-current-target))
     (if current-prefix-arg
	 (setq reason (irchat-read-message "Reason: ")))
     (list chan reason)))
  (if (irchat-ischannel chan)
      (let ((found (irchat-Channel-exist chan)))
	(if found
	    (irchat-send "PART %s :%s" found reason)
	  (message "You are not in [%s]" (irchat-decode-chan chan))))
    (setq irchat-partners (irchat-delete chan irchat-partners))
    (irchat-Channel-part chan)))

(defun irchat-Command-ignore (nick)
  "Ignore messages from this user.  If already ignoring him/her, toggle."
  (interactive (let (nick (completion-ignore-case t))
		 (setq nick (irchat-read-nick "Nick for IGNORE: "))
		 (list nick)))
  (if (string= "" nick)
      (let ((mylist irchat-ignore-nickname) str)
	(setq str "*** Currently ignored people:")
	(while mylist
	  (setq str (format "%s %s" str (car mylist)))
	  (setq mylist (cdr mylist)))
	(irchat-insert (format "%s\n" str)))
    (if (memq (intern nick) irchat-ignore-nickname)
	(progn
	  (irchat-insert (format "*** Ignore OFF: %s\n" nick))
	  (setq irchat-ignore-nickname 
		(delq (intern nick) irchat-ignore-nickname)))
      (irchat-insert (format "*** Ignore ON: %s\n" nick))
      (setq irchat-ignore-nickname 
	    (cons (intern nick) irchat-ignore-nickname)))))

(defun irchat-Command-kick (chan nick reason)
  "Kick this user out."
  (interactive
   (let (nick chan (completion-ignore-case t) (reason "heh..."))
     (if (irchat-ischannel irchat-current-target)
	 (setq chan irchat-current-target)
       (setq chan (irchat-read-chan "Channel for KICK: "
				    (irchat-current-chan-alist))))
     (setq nick (irchat-read-nick "Nick for KICK: "))
     (if current-prefix-arg
	 (setq reason (irchat-read-message "Reason: ")))
     (list chan nick reason)))
  (irchat-send "KICK %s %s :%s" chan nick reason))

(defun irchat-Command-servers (regexp)
  "List the given regexp servers.
With Control-U as argument, show server conecting association."
  (interactive "sServer name: ")
  (setq irchat-how-to-show-links-reply current-prefix-arg)
  (irchat-send "LINKS %s" regexp))

(defun irchat-Command-list (&optional chan)
  "List the given channel and its topics.
If you enter only Control-U as argument, list the current channel.
With - as argument, list all channels."
  (interactive
   (if (or current-prefix-arg (not (irchat-ischannel irchat-current-target)))
       (if (eq current-prefix-arg '-)
	   (list '-)
	 (list
	  (let ((completion-ignore-case t))
	    (setq irchat-chan-target (irchat-read-chan "Channel for LIST: " 
						       irchat-chan-alist
						       irchat-chan-target)))))
     nil))
  (if (null chan)
      (if (irchat-ischannel irchat-current-target)
	  (setq chan irchat-current-target)
	(setq chan (irchat-Channel-lists)))
    (if (string= chan "")
	(setq chan (irchat-Channel-lists))))
  (if (eq chan '-)
      (irchat-send "LIST")
    (irchat-send "LIST %s" chan)))

(defun irchat-Channel-lists ()
  (if (null irchat-current-channels)
      ""
    (let ((chans (cdr irchat-current-channels))
	  (str (car irchat-current-channels)))
      (while chans
	(setq str (format "%s,%s" str (car chans)))
	(setq chans (cdr chans)))
      str)))
  
(defun irchat-Command-users ()
  "List the number of users and servers"
  (interactive)
  (irchat-send "LUSERS"))

(defun irchat-Command-version ()
  "Ask server version"
  (interactive)
  (irchat-send "VERSION"))

(defun irchat-Command-admin ()
  "Ask server admin"
  (interactive)
  (irchat-send "ADMIN"))

(defun irchat-Command-modec (&optional chan)
  "Send/Check the mode for you/channel."
  (interactive
   (if current-prefix-arg
       (if (eq current-prefix-arg '-)
	   (list '-)
	 (list
	  (let ((completion-ignore-case t))
	    (setq irchat-chan-target
		  (irchat-read-chan irchat-prompt-mode-channel
				    irchat-chan-alist
				    irchat-chan-target)))))
     nil))
  (if (and (null chan) (irchat-ischannel irchat-current-target))
      (setq chan irchat-current-target))
  (if (or (string= "" chan) (eq chan '-))
      (setq chan nil))
  (let (value)
    (if chan
	(setq value (irchat-read-message
		     (irchat-format irchat-prompt-mode chan)))
      (setq value (irchat-read-message "Mode for you: ")))
    (irchat-send "MODE %s %s" (or chan irchat-nickname) value)))

(defun irchat-Command-send-minibuffer (chan msg)
  "Send a message to another user/channel from minibuffer."
  (interactive (let (chan (completion-ignore-case t))
		 (setq chan
		       (irchat-read-chan
			irchat-prompt-message-target
			(append irchat-nick-alist irchat-chan-alist)
			irchat-privmsg-partner))
		 (list chan
		       (irchat-read-message
			(irchat-format irchat-prompt-message chan)))))
  (setq irchat-privmsg-partner chan)
  (irchat-send-privmsg chan msg))

(defun irchat-Command-send-private ()
  "Send a private message (current line) to another user."
  (interactive)
  (let ((completion-ignore-case t) msg start)
    (setq irchat-privmsg-partner
	  (irchat-read-chan irchat-prompt-message-target
			    (append irchat-nick-alist irchat-chan-alist)
			    irchat-privmsg-partner))
    (irchat-send-privmsg irchat-privmsg-partner (irchat-read-from-line))))

(defun irchat-Command-names (&optional chan)
  "List the nicknames of the current IRC users on given channel.
With an Control-U as argument, only the current channel is listed.
With - as argument, list all channels."
  (interactive
   (if (or current-prefix-arg (not (irchat-ischannel irchat-current-target)))
       (if (eq current-prefix-arg '-)
	   (list '-)
	 (list
	  (let ((completion-ignore-case t))
	    (setq irchat-chan-target (irchat-read-chan "Channel for NAMES: " 
						       irchat-chan-alist
						       irchat-chan-target)))))
     nil))
  (if (null chan)
      (if (irchat-ischannel irchat-current-target)
	  (setq chan irchat-current-target)
	(setq chan (irchat-Channel-lists)))
    (if (string= chan "")
	(setq chan (irchat-Channel-lists))))
  (if (eq chan '-)
      (irchat-send "NAMES")
    (irchat-send "NAMES %s" chan)))

(defun irchat-Command-nickname (nick)
  "Set your nickname."
  (interactive (list (irchat-read-nick "Enter your nickname: ")))
  (setq irchat-trying-nickname nick)
  (irchat-send "NICK %s" nick))

(defun irchat-Command-who (&optional chan)
  "Lists tue users that match the given expression.
If you enter only Control-U as argument, list the current channel.
With - as argument, list all users."
  (interactive 
   (if (or current-prefix-arg (not (irchat-ischannel irchat-current-target)))
       (if (eq current-prefix-arg '-)
	   (list '-)
	 (list
	  (let ((completion-ignore-case t))
	    (setq irchat-chan-target (irchat-read-chan "Channel for WHO: " 
						       irchat-chan-alist
						       irchat-chan-target)))))
     nil))
  (if (null chan)
      (if (irchat-match-my-nick irchat-current-target)
	  (setq chan (irchat-Channel-lists))
	(setq chan irchat-current-target))
    (if (string= chan "")
	(setq chan (irchat-Channel-lists))))
  (if (eq chan '-)
      (irchat-send "WHO")
    (irchat-send "WHO %s" chan)))

(defun irchat-Command-wait (nick &optional greeting)
  "Wait for NICK to enter IRC.  When this person appears, you will
be informed. If the optional argument GREETING is non-nil, it should 
be a string to send NICK upon entering."
  (interactive 
   (progn (setq nick (irchat-read-nick "Wait for: "))
	  (setq greeting
		(irchat-read-message
		 (format "Message to send %s upon entering: " nick)))
	  (if (string= greeting "")
	      (setq greeting nil))
	  (list nick greeting)))
  (put (intern nick) 'irchat-waited-for t)
  (if greeting 
      (put (intern nick) 'irchat-greeting greeting)))

(defun irchat-Command-trace (nick)
  "Get information about a specific user."
  (interactive (let (nick (completion-ignore-case t))
		 (setq nick (irchat-read-nick "Trace whom: "))
		 (list nick)))
  (irchat-send "TRACE %s" nick))

(defun irchat-Command-finger (nick)
  "Get information about a specific user."
  (interactive (let (nick (completion-ignore-case t))
		 (setq nick (irchat-read-nick "Finger whom: "))
		 (list nick)))
  (irchat-send "WHOIS %s" nick))

(defun irchat-Command-finger-direct (nick)
  "Get information about a specific user."
  (interactive (let (nick (completion-ignore-case t))
		 (setq nick (irchat-read-nick "Finger whom: "))
		 (list nick)))
  (irchat-send "WHOIS %s %s" nick nick))

(defun irchat-Command-ison (nick)
  "IsON user."
  (interactive "sIsON: ")
  (irchat-send "ISON %s" nick))

(defun irchat-Command-userhost (nick)
  "Ask for userhost."
  (interactive "sUserhost nick(s): ")
  (irchat-send "USERHOST %s" nick))

(defun irchat-Command-topic ()
  "Change topic/userinfo of channel/you."
  (interactive)
  (let (value)
    (if (irchat-ischannel irchat-current-target)
	(if current-prefix-arg
	    nil
	  (setq value
		(irchat-read-message
		 (irchat-format irchat-prompt-topic irchat-current-target))))
      (setq value
	    (irchat-read-message "Userinfo for you: "
				 (irchat-ctcp-userinfo-get))))
    (if (irchat-ischannel irchat-current-target)
	(if current-prefix-arg
	    (irchat-send "TOPIC %s" irchat-current-target)
	  (irchat-send "TOPIC %s :%s" irchat-current-target value))
      (irchat-ctcp-userinfo-put value))))

(defun irchat-Command-invite (&optional chan nick)
  "Invite user to channel."
  (interactive 
   (list
    (if current-prefix-arg
	(let ((completion-ignore-case t))
	  (irchat-read-chan "Channel for INVITE: "
			    (irchat-current-chan-alist)))
      nil)
    (let ((completion-ignore-case t)) 
      (irchat-read-nick "Nick for INVITE: "))))
  (if (null chan)
      (if (irchat-ischannel irchat-current-target)
	  (setq chan irchat-current-target)
	(setq chan irchat-nickname)))
  (irchat-send "INVITE %s %s" nick chan))

(defun irchat-Command-away (msg)
  "Mark/unmark yourself as being away."
  (interactive 
   (list (irchat-read-message "Away message: ")))
  (irchat-send "AWAY :%s" msg))

(defun irchat-Current-scroll-down ()
  "Scroll down current buffer"
  (interactive)
  (if (pos-visible-in-window-p (point-min))
      (message "Beginning of buffer")
    (scroll-down)))

(defun irchat-Command-scroll-down ()
  "Scroll Dialogue-buffer down from Command-buffer."
  (interactive)
  (if irchat-channel-buffer-mode
      (pop-to-buffer irchat-Channel-buffer)
    (pop-to-buffer irchat-Dialogue-buffer))
  (if (pos-visible-in-window-p (point-min))
      (message "Beginning of buffer")
    (scroll-down))
  (pop-to-buffer irchat-Command-buffer))

(defun irchat-Current-scroll-up ()
  "Scroll up current buffer."
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (progn
	(goto-char (point-max))
	(recenter 1))
    (scroll-up)))

(defun irchat-Command-scroll-up ()
  "Scroll Dialogue-buffer up from Command-buffer."
  (interactive)
  (let ((obuf (current-buffer)) owin win)
    (if irchat-channel-buffer-mode
	(set-buffer irchat-Channel-buffer)
      (set-buffer irchat-Dialogue-buffer))
    (if (setq win (get-buffer-window (current-buffer)))
	(progn
	  (setq owin (selected-window))
	  (select-window win)
	  (if (not (pos-visible-in-window-p (point-max)))
	      (scroll-up 1))
	  (if (pos-visible-in-window-p (point-max))
	      (progn
		(goto-char (point-max))
		(recenter 1))
	    (scroll-up))
	  (select-window owin)))
    (set-buffer obuf)))

(defun irchat-Command-scroll-freeze ()
  "Toggle the automatic scrolling of the Current/Dialogue window."
  (interactive)
  (if irchat-channel-buffer-mode
      (irchat-Channel-freeze)
    (irchat-Dialogue-freeze)))

(defun irchat-Dialogue-freeze ()
  "Toggle the automatic scrolling of the Dialogue window."
  (interactive)
  (setq irchat-freeze (not irchat-freeze))
  (if irchat-freeze
      (setq irchat-freeze-indicator "F")
    (setq irchat-freeze-indicator "-"))
  (set-buffer-modified-p (buffer-modified-p)))

(defun irchat-Channel-freeze (&optional value)
  "Toggle the automatic scrolling of the Channel window."
  (interactive)
  (save-excursion
    (set-buffer irchat-Channel-buffer)
    (cond ((eq value 'on)
	   (setq irchat-freeze-local t))
	  ((eq value 'off)
	   (setq irchat-freeze-local nil)))
    (setq irchat-freeze-local (not irchat-freeze-local))
    (if irchat-freeze-local
	(setq irchat-freeze-indicator-local "F")
      (setq irchat-freeze-indicator-local "-")))
  (set-buffer-modified-p (buffer-modified-p)))

(defun irchat-Command-beep-on-message (&optional value)
  "Toggle the automatic beep notice when the channel mesage is received."
  (interactive)
  (save-excursion
    (set-buffer irchat-Channel-buffer)
    (cond ((eq value 'on)
	   (setq irchat-beep-local t))
	  ((eq value 'off)
	   (setq irchat-beep-local nil)))
    (setq irchat-beep-local (not irchat-beep-local))
    (if irchat-beep-local
	(setq irchat-beep-indicator-local "B")
      (setq irchat-beep-indicator-local "-")))
  (set-buffer-modified-p (buffer-modified-p)))

(defun irchat-Command-suppress-others (&optional value)
  "Toggle to suppress this channel messages display to Others-buffer."
  (interactive)
  (save-excursion
    (set-buffer irchat-Channel-buffer)
    (cond ((eq value 'on)
	   (setq irchat-suppress-local t))
	  ((eq value 'off)
	   (setq irchat-suppress-local nil)))
    (setq irchat-suppress-local (not irchat-suppress-local))
    (if irchat-suppress-local
	(setq irchat-suppress-indicator-local "S")
      (setq irchat-suppress-indicator-local " ")))
  (set-buffer-modified-p (buffer-modified-p)))

(defun irchat-quit ()
  (irchat-Command-quit 'quit))

(defun irchat-Command-quit (&optional quit-msg)
  "Quit IRCHAT."
  (interactive "P")
  (if (or (not (irchat-server-opened))
	  quit-msg
	  (y-or-n-p "Quit IRCHAT? "))
      (let (quit-string)
	(message "")
	(if (and (get-buffer-process irchat-server-buffer)
		 (irchat-server-opened))
	    (progn
	      (if (and quit-msg (not (eq quit-msg 'quit)))
		  (setq quit-string (irchat-read-message "Signoff message: "))
		(setq quit-string (irchat-encode-var irchat-signoff-msg)))
	      (irchat-send "QUIT :%s" quit-string)))
	(if (not (and quit-msg (or (eq quit-msg 'quit) (eq quit-msg 'error))))
	    (irchat-handle-quit irchat-nickname quit-string))
	(irchat-clear-system)
	(if irchat-use-full-window
	    (delete-other-windows))
	(irchat-close-server)
	(if (not (and quit-msg (or (eq quit-msg 'quit) (eq quit-msg 'error))))
	    (if irchat-old-window-configuration
		(progn
		  (set-window-configuration irchat-old-window-configuration)
		  (setq irchat-old-window-configuration nil))))
	(run-hooks 'irchat-Exit-hook)
	(setq irchat-current-target nil)
	(setq irchat-current-channels nil))))

(defun irchat-Command-generic (msg)
  "Enter a generic IRC message, which is sent to the server.
 A ? lists the useful generic messages."
  (interactive (list (irchat-read-message "IRC Command: ")))
  (if (string= msg "?")
      (with-output-to-temp-buffer "*IRC Help*"
	(princ "The following generic IRC messages may be of interest to you:
TOPIC <channel> <new topic>	set the topic of your channel
INVITE <nickname> <channel>	invite another user to join your channel
LINKS <mask>			lists the currently reachable IRC servers
NAMES <channel>			lists users per channel
")
	(message (substitute-command-keys 
		  "Type \\[irchat-Command-reconfigure-windows] to continue")))
    (irchat-send "%s" msg)))

(defun irchat-Command-irc-compatible ()
  "If entered at column 0, allows you to enter a generic IRC message to
be sent to the server.  For a list of messages, see irchat-Command-generic."
  (interactive)
  (if (eq (current-column) 0)
      (call-interactively (function irchat-Command-generic))
    (self-insert-command 1)))

(defun irchat-Command-send-exec (command)
  "Execute command, and send it to the current channel."
  (interactive "sShell Command: ")
  (shell-command command t)
  (let ((opoint (point)))
    (while (< (point) (mark))
      (progn
	(irchat-Command-send-line)
	(set-buffer irchat-Command-buffer)))
    (push-mark opoint t)))

(defun irchat-Command-search-forward ()
  (interactive)
  (if current-prefix-arg
      (irchat-Command-search-backward)
    (re-search-forward irchat-regexp nil t)
    nil))

(defun irchat-Command-search-url ()
  (interactive)
  (let ((obuf (current-buffer)) url buf win)
    (if irchat-channel-buffer-mode
	(setq buf irchat-Channel-buffer)
      (setq buf irchat-Dialogue-buffer))
    (set-buffer buf)
    (if (if current-prefix-arg
	    (re-search-backward irchat-url-regexp nil t)
	  (re-search-forward irchat-url-regexp nil t))
	(progn
	  (setq url (buffer-substring (match-beginning 0) (match-end 0)))
	  (if current-prefix-arg
	      (goto-char (match-beginning 0))
	    (goto-char (match-end 0)))
	  (if (setq win (get-buffer-window buf))
	      (set-window-point win (point)))
	  (pop-to-buffer buf)
	  (message "found %s" url))
      (if current-prefix-arg
	  (message "there are no more URLs before this point.")
	(message "there are no more URLs after this point.")))
    (set-buffer obuf)))

(defun irchat-Command-browse-url ()
  (interactive)
  (let ((obuf (current-buffer)) opoint epoint url buf win ok)
    (if irchat-channel-buffer-mode
	(setq buf irchat-Channel-buffer)
      (setq buf irchat-Dialogue-buffer))
    (set-buffer buf)
    (setq opoint (point))
    (end-of-line)
    (setq epoint (point))
    (beginning-of-line)
    (setq ok nil)
    (while (and (re-search-forward irchat-url-regexp epoint t)
		(<= (match-beginning 0) opoint)
		(not (setq ok (<= opoint (match-end 0))))))
    (if (not ok)
	(progn
	  (goto-char opoint)
	  (setq ok (re-search-forward irchat-url-regexp epoint t))))
    (if (not ok)
	(progn
	  (beginning-of-line)
	  (setq ok (re-search-forward irchat-url-regexp epoint t))))
    (if (not ok)
	(message "there are no URLs in this line")
      (setq url (buffer-substring (match-beginning 0) (match-end 0)))
      (message "browsing %s" url)
      (if (and (boundp (setq fun (intern "irchat-browser-function")))
	       (eval fun))
	  (apply (eval fun) url (list current-prefix-arg))
	(message "variable \"%s\" must point to the function for browsing"
		 "irchat-browser-function")))
    (goto-char opoint)
    (set-buffer obuf)))

;;;
;;; send text in kill-buffer
;;;
(defun irchat-Command-send-yank (&optional howmany)
  "Yank kill-buffer, and send it to the current channel."
  (interactive)
  (let ((beg (point)) end)
    (insert (car kill-ring-yank-pointer))
    (setq end (point))
    (goto-char beg)
    (while (< (point) end)
      (progn
	(irchat-Command-send-line)
	(set-buffer irchat-Command-buffer)))))

;;;
;;; rot-13 encrypted data
;;;
(defun irchat-Command-caesar-line (&optional n)
  "*Caesar encrypt current line. Rotate optional N characters."
  (interactive)
  (beginning-of-line nil)
  (push-mark (point))
  (end-of-line)
  (irchat-caesar-region n))

;;;
;;;
;;;
(defun irchat-get-word-left ()
  "Return word left from point."
  (save-excursion
    (let (point-now)
      (setq point-now (point))
      (backward-word 1)
      (buffer-substring (point) point-now))))

(defun irchat-Command-complete ()
  "Complete word before point from userlist."
  (interactive)
  (insert
   (save-excursion
     (let ((completion-ignore-case t) point-now word result)
       (setq point-now (point)
	     word (irchat-get-word-left)
	     result (try-completion word irchat-nick-alist))
       (backward-word 1)
       (delete-region (point) point-now)
       (if (or (eq result t) (eq result nil))
           word
         result)))))

(defun irchat-Command-load-vars ()
  "Load configuration from irchat-variables-file."
  (interactive)
  (let ((file (expand-file-name irchat-variables-file)))
    (if (file-exists-p file)
	(progn
	  (load-file file)
	  (irchat-Command-reconfigure-windows)))))

(defun irchat-Command-reconfigure-windows ()
  (interactive)
  (let ((command-window (get-buffer-window irchat-Command-buffer))
	(dialogue-window (get-buffer-window irchat-Dialogue-buffer))
	(old-buffer (current-buffer)))
    (if (and command-window dialogue-window)
	(let ((c-height (window-height command-window))
	      (d-height (window-height dialogue-window)))
	  (delete-window command-window)
	  (pop-to-buffer irchat-Dialogue-buffer)
	  (enlarge-window (+ c-height d-height
			     (- (window-height dialogue-window)))))
      (pop-to-buffer irchat-Dialogue-buffer))
    (irchat-configure-windows)
    (if irchat-one-buffer-mode
	(pop-to-buffer irchat-Dialogue-buffer)
      (pop-to-buffer irchat-Command-buffer))))

;;;
;;; command to get end of the Dialogue buffer
;;;
(defun irchat-Command-eod-buffer ()
  (interactive)
  (let ((saved-buffer (current-buffer)))
    (set-buffer irchat-Dialogue-buffer)
    (goto-char (point-max))
    (set-buffer saved-buffer)))

(defun irchat-Command-toggle-display-mode ()
  (interactive)
  (setq irchat-channel-buffer-mode (not irchat-channel-buffer-mode))
  (irchat-configure-windows))

;;;
;;; handling channel buffers
;;;
(defun irchat-Command-next-channel ()
  "Select next channel or chat partner."
  (interactive)
  (if (numberp current-prefix-arg)
      (irchat-Channel-jump current-prefix-arg)
    (irchat-Channel-next)))

(defun irchat-Command-previous-channel ()
  "Select previous channel or chat partner."
  (interactive)
  (if (numberp current-prefix-arg)
      (irchat-Channel-jump current-prefix-arg)
    (irchat-Channel-previous)))

(defun irchat-Channel-jump (num)
  (if (or (= 0 num) (nth1 num irchat-chanbuf-list))
      (irchat-Channel-select num)
    (message "No binding at %d" num)))

(defun irchat-Command-alternative-channel ()
  (interactive)
  (if (or (= 0 irchat-chanbuf-alternative-number)
	  (nth1 irchat-chanbuf-alternative-number irchat-chanbuf-list))
      (irchat-Channel-select irchat-chanbuf-alternative-number)))

(defsubst irchat-channel-key (chan key)
  (put (intern chan) 'key key))

(defvar irchat-chanbuf-current-number 0)
(defvar irchat-chanbuf-alternative-number 0)

(defsubst irchat-Channel-buffer-get (chan)
  (get-buffer (format irchat-buffer-format (irchat-decode-chan chan))))

(defsubst irchat-Channel-buffer-new (chan)
  (get-buffer-create (format irchat-buffer-format (irchat-decode-chan chan))))

(defun irchat-Channel-buffer-replace (old new)
  (if (setq obuf (irchat-Channel-buffer-get old))
      nil))

(defun irchat-Channel-buffer-create (chan)
  (let (nbuf obuf tmp)
    (setq obuf (current-buffer))
    (setq nbuf (irchat-Channel-buffer-new chan))
    (set-buffer nbuf)
    (setq tmp irchat-Channel-buffer)
    (setq irchat-Channel-buffer nbuf)
    (setq irchat-Channel-buffer tmp)
    (insert (current-time-string))
    (insert " Created.\n")
;;    (irchat-Channel-freeze (get (intern chan) 'freeze))
;;    (irchat-Command-beep-on-message (get (intern chan) 'beep))
;;    (irchat-Command-suppress-others (get (intern chan) 'suppress))
    (irchat-Channel-mode)
    (set-buffer obuf)
    nbuf))

(defun irchat-Channel-select (num)
  (if (/= irchat-chanbuf-current-number num)
      (setq irchat-chanbuf-alternative-number irchat-chanbuf-current-number))
  (setq irchat-chanbuf-current-number num)
  (let (chan buf win)
    (if (/= num 0)
	(progn
	  (setq chan (nth1 num irchat-chanbuf-list))
	  (if (null (setq buf (irchat-Channel-buffer-get chan)))
	      (setq buf (irchat-Channel-buffer-create chan))))
      (setq buf irchat-Private-buffer))
    (if (= 0 num)
	(progn
	  (setq irchat-chanbuf-indicator "Private")
	  (setq irchat-current-target irchat-nickname))
      (if (irchat-ischannel chan)
	  (progn
	    (setq irchat-chanbuf-indicator
		  (format "Channel %s" (irchat-chan-noconv chan)))
	    (setq irchat-current-target chan))
	(setq irchat-chanbuf-indicator (format "With %s" chan))
	(setq irchat-current-target chan)))
    (setq irchat-current-nickname irchat-nickname)
    (setq irchat-current-servername irchat-my-server)
    (if (setq win (get-buffer-window irchat-Channel-buffer))
	(let (obuf)
	  (setq obuf (current-buffer))
	  (set-window-buffer win buf)
	  (set-buffer buf)
	  (if (not irchat-freeze-local)
	      (set-window-point win (point-max)))
	  (set-buffer obuf)))
    (setq irchat-Channel-buffer buf)
    (setq irchat-chanbuf-num num)
  (set-buffer-modified-p (buffer-modified-p))))

(defun irchat-Channel-exist (chan)
  (if (stringp chan)
      (let ((found nil))
	(mapcar 
	 '(lambda (elem)
	    (if (irchat-equal chan elem)
		(setq found elem)))
	 irchat-current-channels)
	found)
    nil))

(defun irchat-buffer-hidden ()
  (not (or (get-buffer-window irchat-Dialogue-buffer)
	   (get-buffer-window irchat-Others-buffer)
	   (get-buffer-window irchat-Private-buffer))))

(defun irchat-chanbuf-priv (dst)
  (or (irchat-chanbuf-get dst) 'private))

(defun irchat-chanbuf-get (chan)
  (cond
   ((stringp chan)
    (let ((rest irchat-chanbuf-list) found)
      (while rest
	(if (and (car rest) (irchat-equal chan (car rest)))
	    (setq found (car rest)))
	(setq rest (cdr rest)))
      (if found
	  (irchat-Channel-buffer-get found)
	nil)))
   ((eq chan 'private)
    irchat-Private-buffer)
   (t
    nil)))

(defun irchat-Channel-replace (old new)
  nil)

(defun irchat-Channel-next ()
  (let ((rest (nthcdr irchat-chanbuf-num irchat-chanbuf-list))
	(num (+ irchat-chanbuf-num 1))
	(found nil))
    (while (and rest (not found))
      (if (car rest)
	  (setq found t)
	(setq rest (cdr rest))
	(setq num (+ num 1))))
    (if found
	(irchat-Channel-select num)
      (irchat-Channel-select 0))))

(defun irchat-Channel-previous ()
  (let ((num (- irchat-chanbuf-num 1)))
    (if (< num 0)
	(setq num (length irchat-chanbuf-list)))
    (while (and (/= num 0)
		(null (nth1 num irchat-chanbuf-list)))
      (setq num (- num 1)))
    (irchat-Channel-select num)))

(defun irchat-Channel-join (chan)
  (let ((rest irchat-chanbuf-list)
	(num 1)
	(found nil))
    (while (and rest (not found))
      (if (and (car rest) (irchat-equal chan (car rest)))
	  (setq found t)
	(setq num (+ num 1))
	(setq rest (cdr rest))))
    (if (not found)
	(progn
	  (setq rest irchat-default-channel-binding)
	  (setq num 1)
	  (while (and rest (not found))
	    (if (and (car rest)
		     (irchat-equal (irchat-chan-real (car rest)) chan))
		(setq found t)
	      (setq num (+ num 1))
	      (setq rest (cdr rest))))
	  (if (or (not found)
		  (nth1 num irchat-chanbuf-list))
	      (progn
		(setq rest irchat-chanbuf-list)
		(setq num 1)
		(while (and rest (not found))
		  (if (and (null (car rest))
			   (null (nth1 num irchat-default-channel-binding)))
		      (setq found t)
		    (setq num (+ num 1))
		    (setq rest (cdr rest))))
		(if (not found)
		    (progn
		      (setq num (+ (length irchat-chanbuf-list) 1))
		      (while (nth1 num irchat-default-channel-binding)
			(setq num (+ num 1)))))))))
    (if (> num (length irchat-chanbuf-list))
	(setq irchat-chanbuf-list
	      (append irchat-chanbuf-list
		      (make-list (- num (length irchat-chanbuf-list)) nil))))
    (if (null (nth1 num irchat-chanbuf-list))
	(progn
	  (if (irchat-chan-get chan)
	      (setq chan (irchat-chan-get chan)))
	  (setcar (nth1cdr num irchat-chanbuf-list) chan)
	  (irchat-Channel-change)))
    (irchat-Channel-select num)))

(defun irchat-Channel-part (chan)
  (if chan
      (let ((rest irchat-chanbuf-list)
	    (num 1)
	    (found nil))
	(while (and rest (not found))
	  (if (and (car rest) (irchat-equal chan (car rest)))
	      (setq found t)
	    (setq num (+ num 1))
	    (setq rest (cdr rest))))
	(if (not found)
	    (message "Not found [%s]" (irchat-decode-chan chan))
	  (setcar (nth1cdr num irchat-chanbuf-list) nil)
	  (irchat-Channel-change)
	  (irchat-Channel-select 0)))
    (setq irchat-chanbuf-list nil)
    (irchat-Channel-change)
    (irchat-Channel-select 0)))

(defun irchat-add-my-channel (chan)
  (setq irchat-current-channels
	(cons chan irchat-current-channels)))

(defun irchat-del-my-channel (chan)
  (setq irchat-current-channels
	(irchat-delete chan irchat-current-channels)))

(defun irchat-Channel-change ()
  (let ((rest irchat-chanbuf-list) (string "") (n 1))
    (while rest
      (if (car rest)
	  (setq string (format "%s,%d%s%s" string n 
			       (if (irchat-ischannel (car rest)) "" ":")
			       (irchat-chan-virtual (car rest)))))
      (setq n (+ n 1))
      (setq rest (cdr rest)))
    (if (string= string "")
	(setq irchat-chanbufs-indicator "No channel")
      (setq irchat-chanbufs-indicator (substring string 1 (length string))))))

(defun irchat-chan-put (chan)
  (put (intern (irchat-upper chan)) 'join chan))

(defun irchat-chan-get (chan)
  (get (intern (irchat-upper chan)) 'join))

(defvar irchat-uniq-chan-format
  "^\\![0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z]\\(.*\\)")

(defun irchat-chan-noconv (chan)
  (if (string-match irchat-uniq-chan-format chan)
      (setq chan (concat "!" (matching-substring chan 1))))
  (if (irchat-chan-get chan)
      (setq chan (irchat-chan-get chan)))
  (irchat-decode-chan chan))

(defun irchat-chan-virtual (chan)
  (if chan
      (let ((rest irchat-channel-conversion-map) match)
	(if (string-match irchat-uniq-chan-format chan)
	    (setq chan (concat "!" (matching-substring chan 1))))
	(if (irchat-chan-get chan)
	    (setq chan (irchat-chan-get chan)))
	(setq chan (irchat-decode-chan chan))
	(while rest
	  (if (irchat-equal (car (car rest)) chan)
	      (setq match (cdr (car rest))))
	  (setq rest (cdr rest)))
	(if match
	    match
	  (if (and (string-match "^#\\(.*\\):\\([^:]*\\)$" chan)
		   (string= (matching-substring chan 2)
			    irchat-channel-conversion-default-mask))
	      (format "%%%s" (matching-substring chan 1))
	    chan)))
    nil))

(defun irchat-chan-real (chan)
  (if chan
      (if (string= chan (irchat-chan-virtual irchat-current-target))
	  irchat-current-target
	(progn
	  (mapcar '(lambda (elem)
		     (if (string= chan (cdr elem))
			 (setq chan (car elem))))
		  irchat-channel-conversion-map)
	  (if (string-match "^%\\(.*\\)$" chan)
	      (setq chan (format "#%s:%s" (matching-substring chan 1)
				 irchat-channel-conversion-default-mask)))
	  (setq chan (irchat-encode-chan chan))
	  (if (string-match "^!\\(.*\\)$" chan)
	      (let ((body (matching-substring chan 1)))
		(mapcar '(lambda (elem)
			   (if (irchat-chan-unique-equal body elem)
			       (setq chan elem)))
			irchat-current-channels)
		(if (irchat-chan-unique-equal body irchat-invited-channel)
		    (setq chan irchat-invited-channel))))
	  chan))
    nil))

(defconst irchat-chan-unique-format
  "^\\![0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z]\\(.*\\)")

(defun irchat-chan-unique-equal (body uniq)
  (if (and uniq
	   (string-match irchat-chan-unique-format uniq)
	   (irchat-equal body (matching-substring uniq 1)))
      uniq
    nil))
      
(defun irchat-Command-jump-channel0 ()
  (interactive)
  (irchat-Channel-jump 0))

(defun irchat-Command-jump-channel1 ()
  (interactive)
  (irchat-Channel-jump 1))

(defun irchat-Command-jump-channel2 ()
  (interactive)
  (irchat-Channel-jump 2))

(defun irchat-Command-jump-channel3 ()
  (interactive)
  (irchat-Channel-jump 3))

(defun irchat-Command-jump-channel4 ()
  (interactive)
  (irchat-Channel-jump 4))

(defun irchat-Command-jump-channel5 ()
  (interactive)
  (irchat-Channel-jump 5))

(defun irchat-Command-jump-channel6 ()
  (interactive)
  (irchat-Channel-jump 6))

(defun irchat-Command-jump-channel7 ()
  (interactive)
  (irchat-Channel-jump 7))

(defun irchat-Command-jump-channel8 ()
  (interactive)
  (irchat-Channel-jump 8))

(defun irchat-Command-jump-channel9 ()
  (interactive)
  (irchat-Channel-jump 9))

(defun irchat-Command-jump-channel10 ()
  (interactive)
  (irchat-Channel-jump 10))

(defun irchat-Command-jump-channel11 ()
  (interactive)
  (irchat-Channel-jump 11))

(defun irchat-Command-jump-channel12 ()
  (interactive)
  (irchat-Channel-jump 12))

(defun irchat-Command-jump-channel13 ()
  (interactive)
  (irchat-Channel-jump 13))

(defun irchat-Command-jump-channel14 ()
  (interactive)
  (irchat-Channel-jump 14))

(defun irchat-Command-jump-channel15 ()
  (interactive)
  (irchat-Channel-jump 15))

(defun irchat-Command-jump-channel16 ()
  (interactive)
  (irchat-Channel-jump 16))

(defun irchat-Command-jump-channel17 ()
  (interactive)
  (irchat-Channel-jump 17))

(defun irchat-Command-jump-channel18 ()
  (interactive)
  (irchat-Channel-jump 18))

(defun irchat-Command-jump-channel19 ()
  (interactive)
  (irchat-Channel-jump 19))

(defun irchat-Command-jump-channel (num)
  (interactive "nChannel Number: ")
  (irchat-Channel-jump num))

(provide 'irchat-commands)
