;;;
;;; CTCP (client-to-client protocol) queries
;;;
(defun irchat-Channel-ctcp-ping ()
  (interactive)
  (irchat-update-time)
  (irchat-ctcp-send-request irchat-current-target
			    (format "PING %s %s" irchat-time
				    irchat-current-target)))

(defun irchat-Channel-ctcp-version ()
  (interactive)
  (irchat-ctcp-send-request irchat-current-target "VERSION"))

(defun irchat-Channel-ctcp-userinfo ()
  (interactive)
  (irchat-ctcp-send-request irchat-current-target "USERINFO"))

(defun irchat-Channel-ctcp-time ()
  (interactive)
  (irchat-ctcp-send-request irchat-current-target "TIME"))

(defun irchat-Command-ctcp (command)
  (interactive)
  (irchat-ctcp-read-target command)
  (irchat-ctcp-send-request irchat-ctcp-target command))

(defun irchat-Command-ctcp-version ()
  "Ask about someones client version."
  (interactive)
  (irchat-Command-ctcp "VERSION"))

(defun irchat-Command-ctcp-time ()
  "Ask about someones time."
  (interactive)
  (irchat-Command-ctcp "TIME"))

(defun irchat-Command-ctcp-ping ()
  "Ask about someones ping."
  (interactive)
  (irchat-ctcp-read-target "PING")
  (irchat-update-time)
  (irchat-ctcp-send-request irchat-ctcp-target
			    (format "PING %s" irchat-time)))

(defun irchat-Command-ctcp-finger ()
  "Ask about someones finger."
  (interactive)
  (irchat-Command-ctcp "FINGER"))

(defun irchat-Command-ctcp-userinfo ()
  "Ask about someones userinfo."
  (interactive)
  (irchat-Command-ctcp "USERINFO"))

(defun irchat-Command-ctcp-clientinfo ()
  "Ask about someones available ctcp commands."
  (interactive)
  (irchat-Command-ctcp "CLIENTINFO"))

(defun irchat-Command-ctcp-action ()
  "Action."
  (interactive)
  (irchat-ctcp-read-target "ACTION")
  (let ((msg (irchat-read-message (format "CTCP ACTION argument: "))))
    (irchat-ctcp-send-request irchat-ctcp-target (format "ACTION %s" msg))
    (irchat-handle-message irchat-nickname irchat-ctcp-target msg 'MYACT)))

(defun irchat-Command-ctcp-clientinfo-generic ()
  "Ask about someones available ctcp commands."
  (interactive)
  (irchat-ctcp-read-target "clientinfo generic")
  (let ((completion-ignore-case t))
    (setq irchat-ctcp-lastcommand
	  (irchat-read-word "What CTCP command: " irchat-ctcp-lastcommand
			       irchat-ctcp-alist)))
  (irchat-ctcp-send-request irchat-ctcp-target
			    (format "CLIENTINFO%s%s"
				    (if (string= irchat-ctcp-lastcommand "")
					"" " ")
				    irchat-ctcp-lastcommand)))

(defun irchat-Command-ctcp-generic ()
  "Generic CTCP"
  (interactive)
  (irchat-ctcp-read-target "generic")
  (let ((completion-ignore-case t) arg)
    (setq irchat-ctcp-lastcommand
	  (irchat-read-word "What CTCP command: " irchat-ctcp-lastcommand
			       irchat-ctcp-alist))
    (if current-prefix-arg
	(setq arg (irchat-read-message (format "CTCP %s argument: "
					       (irchat-decode-message
						irchat-ctcp-lastcommand)))))
    (irchat-ctcp-send-request irchat-ctcp-target
			      (format "%s%s" irchat-ctcp-lastcommand
				      (if arg (format " %s" arg) "")))))

(defun irchat-Command-ctcp-userinfo-from-minibuffer ()
  "Set my userinfo from minibuffer."
  (interactive)
  (irchat-ctcp-userinfo-put (irchat-read-message "New userinfo: "
						 (irchat-ctcp-userinfo-get))))
  
(defun irchat-Command-ctcp-userinfo-from-commandbuffer ()
  "Set my userinfo from commandbuffer."
  (interactive)
  (irchat-ctcp-userinfo-put (irchat-read-from-line)))

(defun irchat-ctcp-read-target (type)
  (let ((completion-ignore-case t))
    (setq irchat-ctcp-target
	  (irchat-read-chan (format "CTCP %s query to: " type)
			    (append irchat-nick-alist irchat-chan-alist)
			    irchat-ctcp-target))))

(defvar irchat-ctcp-alist
  '(("ACTION") ("CLIENTINFO") ("DCC") ("ECHO") ("ERRMSG") ("PING") ("TIME") ("USERINFO") ("VERSION"))
  "*CTCP commands alist")

(defun irchat-ctcp-add-to-list (command)
  (setq irchat-ctcp-alist (append irchat-ctcp-alist (list (list command)))))

;;;
;;; irchat-ctcp
;;;
(defun irchat-ctcp-check (src dst msg mine)
  "It's CTCP request, act on it."
  (let (left ctcp right result)
    (if (string-match "^\\([^\001]*\\)\001\\([^\001]*\\)\001\\(.*\\)" msg)
	(progn
	  (setq left (matching-substring msg 1))
	  (setq ctcp (matching-substring msg 2))
	  (setq right (matching-substring msg 3))
	  (if mine
	      (setq result nil)
	    (setq result (irchat-ctcp-request src dst ctcp)))
	  (if result
	      (setq msg (concat left result right))
	    (setq msg (concat left right))))))
  msg)

(defun irchat-ctcp-common (src dst msg request)
  (let (cmd arg hook dir dcmd)
    (if request
	(setq dir "request")
      (setq dir "reply"))
    (string-match "^\\([^ ]*\\) *:?\\(.*\\)" msg)
    (setq cmd (matching-substring msg 1))
    (setq arg (matching-substring msg 2))
    (if (string= arg "")
	(setq arg nil))
    (if (string= cmd "")
	(progn
	  (setq cmd nil)
	  (irchat-ctcp-report src dst cmd arg "Empty" dir nil)
	  nil)
      (setq dcmd (downcase (irchat-decode-message cmd)))
      (setq hook (intern (concat "irchat-ctcp-" dir "-" dcmd "-hook")))
      (if (and (boundp hook)
	       (eval hook)
	       (eq (eval (list hook src dst cmd arg)) t))
	  ;; If we have a hook, and it returns T, do nothing more
	  nil
	;; else call the handler
	(if (fboundp (setq fun (intern
				(concat "irchat-ctcp-" dir "-" dcmd))))
	    (progn
	      (eval (list fun src dst cmd arg))
	      (if (and request (not (string= dcmd "action")))
		  (irchat-ctcp-report src dst cmd nil nil dir nil))
	      (if (not irchat-freeze)
		  (irchat-scroll-if-visible
		   (get-buffer-window (current-buffer))))
	      nil)
	  (progn
	    (if request
		(if (irchat-match-me dst)
		    ;; return unknown error if it's sended to only me.
		    (irchat-ctcp-send-errmsg src cmd
					     "Unrecognized command")))
	    (irchat-ctcp-report src dst cmd arg "Unknown" dir nil)
	    nil))))))

(defun irchat-ctcp-report (src dst cmd arg pre post insert)
  (let (msg)
    (setq msg (irchat-format irchat-format-dcc-report
			     (if pre pre "") (if pre " " "") post
			     (if cmd cmd "") (if cmd " " "") src dst
			     (if arg (format "[%s]" arg) "")))
    (if insert
	(irchat-insert (concat "*** " msg "\n") dst t)
      (message "%s" msg))))

(defun irchat-ctcp-insert-reply (src dst cmd arg &optional chan)
  (irchat-insert (irchat-format irchat-format-dcc-reply src cmd arg) chan))

(defun irchat-ctcp-send-reply (dst msg)
  (irchat-send "NOTICE %s :\001%s\001" dst msg))

(defun irchat-ctcp-send-errmsg (dst cmd msg)
  (irchat-ctcp-send-reply dst (format "ERRMSG %s: %s" cmd msg)))

(defun irchat-ctcp-send-request (dst msg)
  (irchat-send "PRIVMSG %s :\001%s\001" dst msg))

(defvar irchat-ctcp-list nil)

(defun irchat-ctcp-list-string ()
  (let ((str "") (rest irchat-ctcp-list))
    (while rest
      (setq str (concat str (car rest) " "))
      (setq rest (cdr rest)))
    str))

(defsubst irchat-ctcp-add-clientinfo (command info)
  (setq command (upcase command))
  (setq irchat-ctcp-list (append irchat-ctcp-list (list command)))
  (put (intern command) 'info info))

(irchat-ctcp-add-clientinfo "ACTION"
			    "contains action descriptions for atmosphere")
(irchat-ctcp-add-clientinfo "CLIENTINFO"
			    "gives available CTCP commands")
(irchat-ctcp-add-clientinfo "DCC"
			    "requests a Direct-Client-Connection")
(irchat-ctcp-add-clientinfo "ECHO"
			    "returns the arguments it receives")
(irchat-ctcp-add-clientinfo "ERRMSG"
			    "returns error messages")
(irchat-ctcp-add-clientinfo "PING"
			    "returns the arguments it receives")
(irchat-ctcp-add-clientinfo "TIME"
			    "tells you the time on the user's host")
(irchat-ctcp-add-clientinfo "USERINFO"
			    "returns user settable information")
(irchat-ctcp-add-clientinfo "VERSION"
			    "shows client type, version and environment")

(defun irchat-ctcp-userinfo-get ()
  (irchat-encode-var irchat-ctcp-userinfo))

(defun irchat-ctcp-userinfo-put (val)
  (setq irchat-ctcp-userinfo (irchat-decode-var val)))

;;;
;;; irchat-ctcp-request
;;;
(defun irchat-ctcp-request (src dst msg)
  (irchat-ctcp-common src dst msg t))

(defun irchat-ctcp-request-version (src dst cmd arg)
  (irchat-ctcp-send-reply src (format "VERSION %s%s on %s [%s]"
				      irchat-version
				      (if irchat-version-plus
					  (concat "+" (irchat-encode-var
						       irchat-version-plus))
					"")
				      irchat-emacs-version
				      irchat-url)))

(defun irchat-ctcp-request-userinfo (src dst cmd arg)
  (irchat-ctcp-send-reply src (format "USERINFO :%s"
				      (irchat-ctcp-userinfo-get))))

(defun irchat-ctcp-request-action (src dst cmd arg)
  (irchat-handle-message src dst arg 'ACTION))

(defun irchat-ctcp-request-dcc (src dst cmd arg)
  (irchat-dcc-request src dst arg))

(defun irchat-ctcp-request-time (src dst cmd arg)
  (irchat-ctcp-send-reply src (format "TIME %s" irchat-time-string)))

(defun irchat-ctcp-request-ping (src dst cmd arg)
  (irchat-ctcp-send-reply src (if arg (format "PING %s" arg) "PING")))

(defun irchat-ctcp-request-echo (src dst cmd arg)
  (irchat-ctcp-send-reply src (if arg (format "ECHO %s" arg) "ECHO")))

(defun irchat-ctcp-request-clientinfo (src dst cmd arg)
  (if (null arg)
      (irchat-ctcp-send-reply src (format "CLIENTINFO :%s :%s"
					  (irchat-ctcp-list-string)
	      "Use CLIENTINFO <COMMAND> to get more specific information"))
    (let (info)
      (setq info (get (intern (upcase (irchat-decode-message arg))) 'info))
      (if info
	  (irchat-ctcp-send-reply src (concat "CLIENTINFO " arg " " info))
	(irchat-ctcp-send-errmsg src "CLIENTINFO"
				 (concat arg " is not a valid command"))))))

;;;
;;; irchat-ctcp-reply
;;;
(defun irchat-ctcp-reply (src dst msg)
  (irchat-ctcp-common src dst msg nil))

(defun irchat-ctcp-reply-version (src dst cmd arg)  
  (if arg
      (irchat-ctcp-insert-reply src dst cmd arg)
    (message "Empty CTCP version notice from \"%s\"." src)))

(defun irchat-ctcp-reply-ping (src dst cmd arg)
  (if arg
      (let ((time arg) chan)
	(if (string-match "\\([^ ]+\\) \\(.*\\)" arg)
	    (progn
	      (setq time (matching-substring arg 1))
	      (setq chan (matching-substring arg 2))))
	(if (= 0 (string-to-int (or time "0")))
	    (irchat-ctcp-insert-reply src dst cmd (or time "nothing") chan)
	  (let ((diff (- irchat-time (string-to-int time))))
	    (while (< diff 0)
	      (setq diff (+ diff 86400)))
	    (irchat-ctcp-insert-reply src dst cmd 
				      (format "%s second%s" diff
					      (if (< diff 2) "" "s"))
				      chan))))
    (message "Empty CTCP ping notice from \"%s\"." src)))

(defun irchat-ctcp-reply-clientinfo (src dst cmd arg)
  (irchat-ctcp-insert-reply src dst cmd arg))

(defun irchat-ctcp-reply-userinfo (src dst cmd arg)  
  (irchat-ctcp-insert-reply src dst cmd arg))

(defun irchat-ctcp-reply-finger (src dst cmd arg)
  (irchat-ctcp-insert-reply src dst cmd arg))

(defun irchat-ctcp-reply-time (src dst cmd arg)  
  (irchat-ctcp-insert-reply src dst cmd arg))

(defun irchat-ctcp-reply-echo (src dst cmd arg)
  (irchat-ctcp-insert-reply src dst cmd arg))

(defun irchat-ctcp-reply-errmsg (src dst cmd arg)  
  (irchat-ctcp-insert-reply src dst cmd arg))

(defun irchat-ctcp-reply-comment (src dst cmd arg)  
  (irchat-ctcp-insert-reply src dst cmd arg))

(provide 'irchat-ctcp)
