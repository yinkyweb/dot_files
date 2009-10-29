;;; see file irchat-copyright.el for change log and copyright info

(defun irchat-greet-user (nick chan)
  (let ((n (intern nick)))
    (beep t)
    (message "IRCHAT: %s has entered! (%s)" nick
	     (if (string= chan "0") "on no channel yet"
	       (concat "on channel " (irchat-chan-virtual chan))))
    (if (get n 'irchat-greeting)
	(irchat-send "PRIVMSG %s :%s" nick (get n 'irchat-greeting)))
    (put n 'irchat-waited-for nil)
    (put n 'irchat-greeting nil)))

(defun irchat-update-userhost (nick userhost)
  (if (and nick userhost
	   (not (string= userhost (irchat-get-userhost  nick))))
      (progn
	(irchat-put-userhost nick userhost)
	(irchat-put-newuserhost nick t))))

(defun irchat-ischannel (chan)
  (string-match "^[+&#!]" chan))

(defun irchat-add-nick-to-alist (nick)
  (let ((dnick (downcase nick)) pair)
    (if (setq anick (get (intern dnick) 'anick))
	(if (not (string= nick anick))
	    (if (setq pair (assoc anick irchat-nick-alist))
		(progn
		  (rplaca pair nick)
		  (put (intern dnick) 'anick nick))))
      (setq irchat-nick-alist (cons (list nick) irchat-nick-alist))
      (put (intern dnick) 'anick nick))))

(defun irchat-del-nick-from-alist (nick)
  (let ((dnick (downcase nick)) pair)
    (if (setq anick (get (intern dnick) 'anick))
	(if (car irchat-nick-alist)
	    (if (and (car (car irchat-nick-alist))
		     (string= anick (car (car irchat-nick-alist))))
		(progn
		  (setq irchat-nick-alist (cdr irchat-nick-alist))
		  (put (intern dnick) 'anick nil))
	      (setq rest irchat-nick-alist)
	      (while (and rest (cdr rest))
		(if (string= anick (car (car (cdr rest))))
		    (progn
		      (rplacd rest (cdr (cdr rest)))
		      (put (intern dnick) 'anick nil)))
		(setq rest (cdr rest))))))))

(defun irchat-user-on-this-channel (nick chan)
  (if (and nick chan)
      (irchat-member chan (irchat-get-chans nick))
    nil))

(defun irchat-add-chan-to-alist (chan)
  (let ((ichan (irchat-chan-noconv chan))
	(vchan (irchat-chan-virtual chan)))
    (if (not (get (intern ichan) 'alist))
	(progn
	  (put (intern ichan) 'alist t)
	  (setq irchat-chan-alist (cons (list ichan) irchat-chan-alist))))
    (if (not (get (intern vchan) 'alist))
	(progn
	  (put (intern vchan) 'alist t)
	  (setq irchat-chan-alist (cons (list vchan) irchat-chan-alist))))))

(defun irchat-current-chan-alist ()
  (let ((rest irchat-current-channels) ichan vchan alist)
    (while rest
      (setq ichan (irchat-chan-noconv (car rest)))
      (setq vchan (irchat-chan-virtual (car rest)))
      (setq alist (cons (list ichan) alist))
      (if (not (string= ichan vchan))
	  (setq alist (cons (list vchan) alist)))
      (setq rest (cdr rest)))
    alist))

(defun irchat-add-to-channel (nicks chan)
  (let ((nicka (get (intern chan) 'nicka)))
    (while (string-match "^\\([@+]?\\)\\([^ ]+\\) ?\\(.*\\)" nicks)
      (let ((oper (matching-substring nicks 1))
	    (nick (matching-substring nicks 2))
	    chans)
	(setq nicks (matching-substring nicks 3))
	(setq nicka (cons (cons nick oper) nicka))
	(setq chans (irchat-get-chans nick))
	(if (not (irchat-member chan chans))
	    (irchat-put-chans nick (nconc chans (list chan))))
	(if (get (intern nick) 'irchat-waited-for)
	    (irchat-greet-user nick chan))
	(irchat-add-nick-to-alist nick)))
    (put (intern chan) 'nicka nicka)))

(defun irchat-del-from-channel (nick chan)
  (irchat-put-chans nick (irchat-delete chan (irchat-get-chans nick))))

(defun irchat-channel-operator (nick chan)
  (cdr (assoc nick (get (intern chan) 'nicka))))

(defun irchat-channel-operator-set (nick chan oper)
  (rplacd (assoc nick (get (intern chan) 'nicka)) oper))

(defun irchat-user-last-privmsg (nick)
  (irchat-get-lastchan nick))

(defun irchat-user-last-privmsg-time (nick)
  (let ((time (irchat-get-lasttime nick)))
    (if time (- irchat-time time) nil)))

(defun irchat-handle-dcc-chat (src dst msg)
  (irchat-handle-message (format "=%s" src) dst msg 'DCCMSG))

(defun irchat-handle-error (prefix msg &rest void)
  (if (not irchat-no-configure-windows)
      (irchat-insert-allchan (list 'ERROR prefix msg)))
  (setq irchat-fatal-error-message msg)
  (message "IRC ERROR: %s" msg))

(defun irchat-handle-nick (nick nick2 &rest void)
  (irchat-insert (list 'NICK nick (irchat-print-nick-userhost nick2))
		 (if (irchat-match-my-nick nick)
		     (cons 'private (irchat-get-chans nick))
		   (irchat-get-chans nick)))
  (irchat-update-userhost nick2 (irchat-get-userhost nick))
  (irchat-del-nick-from-alist nick)
  (irchat-add-nick-to-alist nick2)
  (irchat-put-chans nick2 (irchat-get-chans nick))
  (irchat-put-chans nick nil)
  (if (irchat-match-my-nick nick)
      (progn
	(setq irchat-nickname nick2)
	(if (string= nick irchat-current-target)
	    (setq irchat-current-target nick2))))
  (setq irchat-current-nickname irchat-nickname)
  (set-buffer-modified-p (buffer-modified-p)))

(defun irchat-handle-ping (prefix msg &rest void)
  (irchat-send-pong))

(defun irchat-handle-pong (prefix msg1 &optional msg2 &rest void)
  nil)

(defun irchat-handle-notice (src dst msg &rest void)
  (cond 
   ((null src)
    (if (string-match "ERROR" msg)
	(irchat-insert-allchan (list 'NOTICE src dst msg))
      (irchat-insert (list 'NOTICE src dst msg))))
   ((string-match "\001\\(.*\\)\001" msg)
    (irchat-ctcp-reply src dst (matching-substring msg 1)))
   (t
    (irchat-handle-message src dst msg 'NOTICE))))

(defun irchat-handle-privmsg (src dst msg &rest void)
  (if (or (null src)
	  (and src
	       (memq (intern src) irchat-ignore-nickname)
	       (irchat-msg-from-ignored src dst msg)))
      nil
    (if (and (string-match "\001\\(.*\\)\001" msg)
	     (or (not (irchat-match-my-nick src))
		 (irchat-match-me dst)))
	(setq msg (irchat-ctcp-check src dst msg nil))
      (setq msg (irchat-ctcp-check src dst msg t)))
    (if (not (string= msg ""))
	(irchat-handle-message src dst msg 'PRIVMSG))))

(defun irchat-handle-message (src dst msg cmd &rest void)
  (if (and (irchat-match-me dst)
	   (irchat-buffer-hidden))
      (message "IRCHAT: A private message has arrived from %s" src))
  (if (irchat-match-me dst)    
      (setq goal src)
    (setq goal dst))
  (if (and (or (eq cmd 'PRIVMSG) (eq cmd 'NOTICE))
	   (irchat-match-my-nick src)
	   (not (irchat-match-me dst)))
      (setq cmd 'MYMSG)) ; maybe replay
  (if (not (or (eq cmd 'MYMSG) (eq cmd 'MYACT)))
      (if (and irchat-beep-on-bells (string-match "\007" msg))
	  (beep t)))
  (irchat-insert (list cmd src dst msg)
		 (irchat-chanbuf-priv goal))
  (irchat-put-lastchan src dst)
  (irchat-put-lasttime src irchat-time))

(defun irchat-handle-wallops (nick msg &rest void)
  "Handle the WALLOPS message."
  (if irchat-show-wallops
      (irchat-insert (list 'WALLOPS nick msg)))
  (let ((buf (current-buffer)))
    (set-buffer irchat-WALLOPS-buffer)
    (goto-char (point-max))
    (insert (irchat-format "%1n: %2m"
			   (if nick (concat "from " nick) "") msg))
    (set-buffer buf)))

(defun irchat-handle-quit (nick reason &rest void)
  "Handle the QUIT message."
  (irchat-insert (list 'QUIT (irchat-print-nick-userhost nick) reason)
		 (irchat-get-chans nick))
  ;(irchat-del-nick-from-alist nick)
  (irchat-put-chans nick nil))

(defun irchat-handle-topic (nick chan topic &rest void)
  "Handle the TOPIC message."
  (if (not irchat-ignore-changes)
      (irchat-insert (list 'TOPIC nick chan topic) chan)))

(defun irchat-handle-mode (nick chan &rest args)
  "Handle the MODE message."
  (if (irchat-match-my-nick chan)
      (if (not irchat-ignore-changes)
	  (irchat-insert-special (list 'MODE-ME args)))
    (if (not irchat-ignore-changes)
	(irchat-insert (list 'MODE nick chan args) chan))))

(defun irchat-handle-kick (nick chan target reason &rest void)
  "Handle the KICK message."
  (if (irchat-match-my-nick target)
      (progn
	(irchat-insert (list 'KICK-ME nick chan target reason)
		       (list chan 'private))
	(irchat-del-my-channel chan)
	(put (intern chan) 'nicka nil)
	(if (null irchat-invited-channel)
	    (setq irchat-invited-channel chan))
	(irchat-Channel-part chan))
    (irchat-insert 
     (list 'KICK nick chan (irchat-print-nick-userhost target) reason) chan))
  (irchat-del-from-channel target chan))

(defun irchat-handle-invite (nick me chan &rest void)
  (if (not irchat-ignore-changes)
      (irchat-insert-special (list 'INVITE nick me chan)))
  (setq irchat-invited-channel chan))

(defun irchat-handle-kill (nick me path &rest void)
  (irchat-insert-special (list 'KILL nick me path))
  (irchat-insert-allchan (list "death" nick me path))
  (irchat-Channel-part nil))

(defun irchat-handle-join (nick chan &rest void)
  "Handle the JOIN message."
  (let ((oper "") (voice "") (oper ""))
    (if (string-match "^\\([^\007]+\\)\007\\(o*\\)\\(v*\\)$" chan)
	(setq voice (matching-substring chan 3)
	      oper (matching-substring chan 2)
	      chan (matching-substring chan 1)))
    (if (string= oper "o")
	(setq oper "@"))
    (if (string= voice "v")
	(setq oper (format "%s+" oper)))
    (if (irchat-match-my-nick nick)
	(progn
	  (irchat-add-my-channel chan)
	  (irchat-Channel-join chan)
	  (put (intern chan) 'init t)
	  (put (intern chan) 'nicka nil))
      (irchat-add-to-channel nick chan))
    (if (not irchat-ignore-changes)
	(irchat-insert (list 'JOIN 
			     (concat oper (irchat-print-nick-userhost nick))
			     chan)
		       (if (irchat-match-my-nick nick)
			   (list chan 'private) chan)))))

(defun irchat-handle-part (nick chan &optional reason &rest void)
  "Handle the PART message."
  (if (null reason) (setq reason ""))
  (if (irchat-match-my-nick nick)
      (progn
	(irchat-del-my-channel chan)
	(put (intern chan) 'nicka nil)))
  (if (not irchat-ignore-changes)
      (irchat-insert (list 'PART (irchat-print-nick-userhost nick)
			   chan reason)
		     (if (irchat-match-my-nick nick)
			 (list chan 'private) chan)))
  (if (irchat-match-my-nick nick)
      (progn
	(irchat-Channel-part chan)
	(if (null irchat-invited-channel)
	    (setq irchat-invited-channel chan))))
  (irchat-del-from-channel nick chan))

;;;
;;; 000 replies
;;;
(defun irchat-handle-000s (number prefix me &rest args)
  (irchat-handle-x00s number prefix me args))

(defun irchat-handle-001 (prefix me msg &rest void) ; RPL_WELCOME
  (if irchat-no-configure-windows
      (irchat-configure-windows))
  (irchat-insert-special (list 'irchat irchat-version irchat-url))
  (if (irchat-match-my-nick me)
      (irchat-insert-special (list 'NICK irchat-nickname me)))
  (irchat-insert-special (list "001" prefix me msg))
  (setq irchat-my-server prefix)
  (setq irchat-nickname me)
  (irchat-add-nick-to-alist irchat-nickname)
  (if (and (string-match " \\([^ !]+\\)!\\([^ @]+\\)@\\([^ ]+\\)$" msg)
	   (irchat-match-my-nick (matching-substring msg 1)))
      (irchat-got-user-host (matching-substring msg 2)
			    (matching-substring msg 3))
    (irchat-got-user-host nil nil)
    (setq irchat-my-server-version "")
    (setq irchat-my-server-mode-user "")
    (setq irchat-my-server-mode-channel "")
    (irchat-send "USERHOST %s" irchat-nickname)))

(defun irchat-got-user-host (user host)
  (if (and user host)
      (progn
	(setq irchat-my-user user)
	(setq irchat-my-host host)
	(setq irchat-my-user-host
	      (format "%s@%s" irchat-my-user irchat-my-host))
	(setq irchat-my-user-server
	      (format "%s@%s" irchat-my-user irchat-my-server))
	(setq irchat-my-user-host-server
	      (format "%s%%%s@%s" irchat-my-user irchat-my-host
		      irchat-my-server)))
    (setq irchat-my-user "")
    (setq irchat-my-host "")
    (setq irchat-my-user-host "")
    (setq irchat-my-user-server "")
    (setq irchat-my-user-host-server "")))

(defun irchat-handle-002 (prefix me msg &rest void) ; RPL_YOURHOST
  (irchat-insert-special (list "002" prefix me msg)))

(defun irchat-handle-003 (prefix me msg &rest void) ; RPL_CREATED
  (irchat-insert-special (list "003" prefix me msg)))

(defun irchat-handle-004 (prefix me &optional server version mode1 mode2 &rest void) ; RPL_MYINFO
  (if version
      (setq irchat-my-server-version version))
  (if mode1
      (setq irchat-my-server-mode-user mode1))
  (if mode2
      (setq irchat-my-server-mode-channel mode1))
  (irchat-insert (list "004" prefix me server version mode1 mode2)))

(defun irchat-handle-005 (prefix me msg &rest maplist) ; RPL_xxx
  (cond
   ((or (string= msg "MAP")
	(string= msg "RFC2812")
	(string= msg "PENALTY")) ;;; RPL_ISUPPORT
    (let ((mapstr ""))
      (while maplist
	(setq mapstr (format "%s %s" mapstr (car maplist))
	      maplist (cdr maplist)))
      (irchat-insert (list "005m" prefix me msg mapstr))))
   (t 				 ;;; RPL_BOUNCE
    (if (string-match "^Try server \\([^ ,]+\\), port \\([^ ]+\\)" msg)
	(progn
	  (setq irchat-bounce-server (matching-substring msg 1))
	  (setq irchat-bounce-port (matching-substring msg 2))))
    (irchat-insert (list "005b" prefix me msg)))))

(defun irchat-handle-042 (prefix me uid msg &rest void) ; RPL_YOURID
  (setq irchat-uid uid)
  (irchat-insert-special (list "042" prefix me uid msg)))

;;;
;;; 200 replies
;;;
(defun irchat-handle-200s (number prefix me &rest args)
  (irchat-handle-x00s number prefix me args))

(defun irchat-handle-200 (prefix me status version dest next &optional ver sec q1 q2 &rest void) ; RPL_TRACELINK
  (irchat-insert (list 200 prefix me status version dest next
		       (or ver "") (or sec "0") (or q1 "0") (or q2 "0"))))

(defun irchat-handle-201 (prefix me status class who &rest void) ;  RPL_TRACECONNECTING
  (irchat-insert (list 201 prefix me status class who)))

(defun irchat-handle-202 (prefix me status class who &rest void) ; RPL_TRACEHANDSHAKE
  (irchat-insert (list 202 prefix me status class who)))

(defun irchat-handle-203 (prefix me status class who &rest void) ; RPL_TRACEUNKNOWN
  (irchat-insert (list 203 prefix me status class who)))

(defun irchat-handle-204 (prefix me status class who &rest void) ; RPL_TRACEOPERATOR
  (irchat-insert (list 204 prefix me status class who)))

(defun irchat-handle-205 (prefix me status class who &optional dummy time &rest void) ; RPL_TRACEUSER
  (irchat-insert (list 205 prefix me status class who)))

(defun irchat-handle-206 (prefix me status class Nserver Nclient name how &optional version &rest void) ; PL_TRACESERVER
  (irchat-insert (list 206 prefix me status class Nserver Nclient name how
		       (or version ""))))

(defun irchat-handle-207 (prefix me status class name type wants &rest void) ; RPL_TRACESERVICE
  (irchat-insert (list 207 prefix me status class name type wants)))

(defun irchat-handle-208 (prefix me status class who &rest void) ; RPL_TRACENEWTYPE
  (irchat-insert (list 208 prefix me status class who)))

(defun irchat-handle-209 (prefix me status class entries &rest void) ; RPL_TRACECLASS
  (irchat-insert (list 209 prefix me status class entries)))

(defun irchat-handle-210 (prefix me status class who &rest void) ; RPL_TRACERECONNECT
  (irchat-insert (list 210 prefix me status class who)))

(defvar irchat-stats-now nil)

(defun irchat-handle-211 (prefix me link sndq sndm sndb rcvm rcvb time &rest void) ; RPL_STATSLINKINFO
  (setq	time (string-to-int time))
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "L"))
	(irchat-insert (list '211h prefix me link sndq sndm sndb rcvm rcvb
			     time))
	(setq irchat-stats-now prefix)))
  (irchat-insert (list 211 prefix me link sndq sndm sndb rcvm rcvb time))
  (irchat-insert (list '211p prefix me link sndq sndm sndb rcvm rcvb time)))

(defun irchat-handle-212 (prefix me cmd times bytes &optional rtimes &rest void)
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "M"))
	(setq irchat-stats-now prefix)))
  (irchat-insert
   (format "%s has been used %s%s times after startup (%sbytes)\n"
	   cmd times (if rtimes (format " (remote %s)" rtimes) "") bytes)))

(defun irchat-handle-213 (prefix me cmd host pass server port class &rest void)
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "C"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "%s:%s:%s:%s:%s:%s\n"
			 cmd host pass server port class)))

(defun irchat-handle-214 (prefix me cmd host pass server port class &rest void)
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "C"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "%s:%s:%s:%s:%s:%s\n"
			 cmd host pass server port class)))

(defun irchat-handle-215 (prefix me cmd ip passwd domain port class &rest void)
  ":server.name 215 me I ip * domain port class"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "I"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s:%s:%s:%s:%s:%s\n"
			 cmd ip passwd domain port class)))

(defun irchat-handle-216 (prefix me cmd domain &rest args &rest void)
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "K"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s:%s:%s\n"
			 cmd domain
			 (let ((str ""))
			   (while args
			     (setq str (format "%s %s" str (car args))
				   args (cdr args)))
			   str))))

(defun irchat-handle-217 (prefix me cmd reason star host stuff &rest void)
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "Q"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s:%s:%s:%s:%s\n"
			 cmd reason star host stuff)))

(defun irchat-handle-218 (prefix me cmd class pfreq cfreq mlinks msendq &optional local global &rest void)
  ":server.name 218 me Y class 120 600 1 3000000"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "Y"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format 
       "*** Class %s: PingFreq %s, ConFreq %s, MaxLinks %s, MaxSendQ %s%s%s\n"
	     class pfreq cfreq mlinks msendq
	     (if local (format ", LocalLimit %s" local) "")
	     (if global (format ", GlobalLimit %s" global) ""))))

(defun irchat-handle-219 (prefix me type msg &rest void)
  "RPL_ENDOFSTATS"
  (if (not irchat-stats-now)
      (irchat-insert (format "*** Nothing (STATS %s %s)\n"
			     type prefix)))
  (setq irchat-stats-now nil))

(defun irchat-handle-221 (prefix me str &rest void) ; RPL_USERMODEIS
  (irchat-insert-special (list 221 prefix me str)))

(defun irchat-handle-234 (prefix me service server mask type hop msg &rest void)
  (irchat-insert (format "Service %30s (%s)\n" (add-space 30 service) msg))
  (irchat-insert (format "     at %30s %5s [type %10s] (%s hops)\n"
			 (add-space 30 server) (add-space 5 mask) type hop)))

(defun irchat-handle-235 (prefix me mask type msg &rest void) ; SERVICE END
  nil)

(defun irchat-handle-241 (prefix me cmd hostmask star server maxdepth etc &rest void)
  ":irc.tokyo.wide.ad.jp 241 nick L * * irc.leaf.server 0 -1"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "H"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s:%s:%s:%s:%s:%s\n"
			 cmd hostmask star server maxdepth etc)))

(defun irchat-handle-242 (prefix me msg &rest void)
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "U"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s\n" msg)))

(defun irchat-handle-243 (prefix me cmd &rest args)
  ":server.name 243 me O hostname password user nazo class"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "O"))
	(setq irchat-stats-now prefix)))
  (let ((str ""))
    (while args
      (setq str (format "%s:%s" str (car args))
	    args (cdr args)))
    (irchat-insert (format "*** %s%s\n" cmd str))))

(defun irchat-handle-244 (prefix me cmd hostmask star server maxdepth etc &rest void)
  ":irc.tokyo.wide.ad.jp 244 nick H * * irc.tokai-ic.or.jp 0 -1"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "H"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s:%s:%s:%s:%s:%s\n"
			 cmd hostmask star server maxdepth etc)))

(defun irchat-handle-246 (prefix me server p1 p2 p3 p4 &rest void)
  ":server0.jp 246 nick server1.jp[*@server1.hostname] 26 26 50 15"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "P"))
	(irchat-insert (format "*** %32s %5s %5s %5s %5s\n"
			       "<server name>" "snd#" "rcv#" "ping" "pref"))
	(setq irchat-stats-now prefix)))
  (if (string-match "\\([^[]+\\)\\[[^]]+\\]" server)
      (setq server (matching-substring server 1)))
  (irchat-insert (format "*** %32s %5s %5s %5s %5s\n" server p1 p2 p3 p4)))

(defun irchat-handle-248 (prefix me msg &rest void)
  "RPL_STATSDEFINE"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "D"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s\n" msg)))

(defun irchat-handle-249 (prefix me msg &rest void)
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "T/Z"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s\n" msg)))

(defun irchat-handle-250 (prefix me cmd hostmask star server maxdepth etc &rest void)
  "RPL_STATSDLINE"
  (if (not irchat-stats-now)
      (progn
	(irchat-insert (list 'stats-header prefix "D"))
	(setq irchat-stats-now prefix)))
  (irchat-insert (format "*** %s:%s:%s:%s:%s:%s\n"
			 cmd hostmask star server maxdepth etc)))

(defun irchat-handle-251 (prefix me msg &rest void)
  "RPL_LUSERCLIENT"
  (irchat-insert (format "*** %s\n" msg)))

(defun irchat-handle-252 (prefix me count msg &rest void)
  "RPL_LUSEROP"
  (irchat-insert (format "*** %s %s\n" count msg)))

(defun irchat-handle-253 (prefix me count msg &rest void)
  "RPL_LUSERUNKNOWN"
  (irchat-insert (format "*** %s %s\n" count msg)))

(defun irchat-handle-254 (prefix me count msg &rest void)
  "RPL_LUSERCHANNELS"
  (irchat-insert (format "*** %s %s\n" count msg)))

(defun irchat-handle-255 (prefix me msg &rest void)
  "RPL_LUSERME"
  (irchat-insert (format "*** %s\n" msg)))

(defun irchat-handle-256 (prefix me msg &rest void)
  "RPL_ADMINME"
  (irchat-insert (format "*** %s\n" msg)))

(defun irchat-handle-257 (prefix me msg &rest void)
  "RPL_ADMINLOC1"
  (if (not (string= msg ""))
      (irchat-insert (list 257 prefix me msg))))

(defun irchat-handle-258 (prefix me msg &rest void)
  "RPL_ADMINLOC2"
  (if (not (string= msg ""))
      (irchat-insert (list 258 prefix me msg))))

(defun irchat-handle-259 (prefix me msg &rest void)
  "RPL_ADMINEMAIL"
  (if (not (string= msg ""))
      (irchat-insert (list 259 prefix me msg))))

(defun irchat-handle-261 (prefix me status filename port &rest void)  ; RPL_TRACELOG
  (irchat-insert (format "*** LogFile %s => %s (%s)\n"
			  prefix filename port)))

(defun irchat-handle-262 (prefix me mask version msg &optional msg2 &rest void) ; RPL_ END TRACE
  (if msg2
      (progn
	(setq version (concat version " " msg))
	(setq msg msg2)))
  (irchat-insert (list 262 prefix me mask version msg)))

(defun irchat-handle-265 (prefix me users &optional max msg &rest void)
  (cond
   ((null msg) ;;; 2.10
    (setq msg users)
    (irchat-insert (list "265o" prefix me msg)))
   (t          ;;; 2.11
    (irchat-insert (list 265 prefix me users max msg)))))

(defun irchat-handle-266 (prefix me users &optional max msg &rest void)
  (cond
   ((null msg) ;;; 2.10
    (setq msg users)
    (irchat-insert (list "266o" prefix me msg)))
   (t          ;;; 2.11
    (irchat-insert (list 266 prefix me users max msg)))))

;;;
;;; 300 replies
;;;
(defun irchat-handle-300s (number prefix me &rest args)
  (irchat-handle-x00s number prefix me args))

(defun irchat-handle-301 (prefix me nick iswhat &rest void) ; RPL_AWAY
  (if (not (string= nick irchat-auto-whois-nick))
      (irchat-insert (list 301 prefix me nick iswhat))))

(defun irchat-handle-302 (prefix me data &rest void) ; RPL_USERHOST
  (irchat-insert (list 302 prefix me data))
  (while (string-match 
	  "\\([^*=]+\\)\\([*]*\\)=\\([+-]\\)\\([^@]+\\)@\\([^ ]+\\) *\\(.*\\)" data)
    (let ((nick (matching-substring data 1))
	  (oper (matching-substring data 2))
	  (away (matching-substring data 3))
	  (user (matching-substring data 4))
	  (host (matching-substring data 5))
	  userhost)
      (setq data (matching-substring data 6))
      (setq oper (concat  (if (string= oper "") "Not ") "Operator"))
      (setq away (concat  (if (string= away "+") "Not ") "Away"))
      (setq userhost (format "%s@%s" user host))
      (irchat-update-userhost nick userhost)
      (if (and (irchat-match-my-nick nick)
	       (string= irchat-my-user-host ""))
	  (irchat-got-user-host user host)
	(irchat-insert (list '302s prefix me nick oper away user host))))))

(defun irchat-handle-303 (prefix me nicks &rest void) ; RPL_ISON
  "Handle the 303 reply, ISON reply"
  (if (string= nicks "")
      (irchat-insert (list '303n prefix me nicks))
    (irchat-insert (list 303 prefix me nicks))))

(defun irchat-handle-305 (prefix me msg &rest void) ; RPL_UNAWAY
  "Handle the 305 reply, UNAWAY reply"
  (if (string= irchat-away-indicator "A")
      (progn
	(setq irchat-away-indicator "-")
	(irchat-insert (list 305 prefix me msg)))))

(defun irchat-handle-306 (prefix me msg &rest void) ; RPL_NOWAWAY
  "Handle the 306 reply, NOWAWAY reply"
  (setq irchat-away-indicator "A")
  (irchat-insert (list 306 prefix me msg)))

(defun irchat-handle-311 (prefix me nick user host dummy name &rest void) ;; RPL_WHOISUSER
  (if (not (string= nick irchat-auto-whois-nick))
      (irchat-insert (list 311 prefix me nick user host name))))

(defun irchat-handle-312 (prefix me nick server info &rest void) ; RPL_WHOISSERVER
  ":server.name 312 me her server.name2 :server info"
  (if (not (string= nick irchat-auto-whois-nick))
      (irchat-insert (list 312 prefix me nick server info))))

(defun irchat-handle-313 (prefix me nick iswhat &rest void) ; RPL_WHOISOPERATOR
  (if (not (string= nick irchat-auto-whois-nick))
      (irchat-insert (list 313 prefix me nick iswhat))))

(defun irchat-handle-314 (prefix me nick user host star name &rest void) ; RPL_WHOWASUSER
  (message "")
  (irchat-insert (list 314 prefix me nick user host name)))

(defun irchat-handle-315 (prefix me chan msg &rest void) ; RPL_ENDOFWHO
  (irchat-insert (list 315 prefix me chan msg)))

(defun irchat-handle-317 (prefix me nick sec &optional time msg &rest void) ;RPL_WHOISIDLE
  ":server.name 317 me her 38 :seconds idle or\
   :server.name 317 me her 38 839678551 :seconds idle, signon time"
  (let (fun)
    (if (not (string= nick irchat-auto-whois-nick))
	(irchat-insert (list 317 prefix me nick sec time msg))
      (if (fboundp (setq fun (intern "irchat-whois-idle")))
	  (apply fun (list nick (string-to-int sec)))))))

(defun irchat-handle-318 (prefix me nick msg &rest void) ; RPL_ENDOFWHOIS
  (if (string= nick irchat-auto-whois-nick)
      (setq irchat-auto-whois-nick ""))
  (irchat-insert (list 318 prefix me nick msg)))

(defun irchat-handle-319 (prefix me nick rest &rest void) ; RPL_WHOISCHANNELS
  ":server.name 319 me her :chan1 chan2 chan3"
  (if (not (string= nick irchat-auto-whois-nick))
      (irchat-insert (list 319 prefix me nick rest))))

(defun irchat-handle-321 (prefix me chan msg &rest void) ; RPL_LISTSTART
  (irchat-insert (list 321 prefix me chan msg)))

(defun irchat-handle-322 (prefix me chan users topic &optional bug &rest void) ; RPL_LIST
  "Handle the 322 reply (from LIST)."
  (irchat-send-check)
  (irchat-insert (list 322 prefix me chan users topic) chan))

(defun irchat-handle-323 (prefix me msg &rest void) ; RPL_LISTEND
  (irchat-insert (list 323 prefix me msg)))

(defun irchat-handle-324 (prefix me chan &rest args) ; RPL_CHANNELMODEIS
  (irchat-insert (list 324 prefix me chan args) chan))

(defun irchat-handle-331 (prefix me chan msg &rest void) ; RPL_NOTOPIC
  (irchat-insert (list 331 prefix me chan msg) chan))

(defun irchat-handle-332 (prefix me chan topic &rest void) ; RPL_TOPIC
  (irchat-insert (list 332 prefix me chan topic) chan))

(defun irchat-handle-333 (prefix me chan nickuserhost time &rest void) ; RPL_TOPICWHOTIME
  (irchat-insert (list 333 prefix me chan nickuserhost time) chan))

(defun irchat-handle-341 (prefix me nick chan &rest void) ; RPL_INVITING
  (irchat-insert (list 341 prefix me nick chan) chan))

(defun irchat-handle-346 (prefix me chan ban &rest void) ; RPL_INVITELIST
  (irchat-insert (list 346 prefix me chan ban) chan))

(defun irchat-handle-347 (prefix me chan msg &rest void) ; RPL_ENDOFINVITELIST
  (irchat-insert (list 347 prefix me chan msg) chan))

(defun irchat-handle-351 (prefix me version server sid &optional mode &rest void) ; RPL_VERSION
  (if (not (null mode))
      (irchat-insert (list 351 prefix me version server sid mode))
    (setq mode sid)
    (irchat-insert (list "351o" prefix me version server mode))))

(defun irchat-handle-352 (prefix me chan user host serv nick oper name &rest void)
  (irchat-send-check)
  (let (hops)
    (if (string-match "\\([0-9]*\\) *\\(.*\\)" name)
	(setq hops (matching-substring name 1)
	      name (matching-substring name 2)))
    (irchat-insert (list 352 prefix me
			 chan user host serv nick oper hops name) chan)))

(defun irchat-handle-353 (prefix me flag chan nicks &rest void) ; RPL_NAMREPLY
  (irchat-send-check)
  (irchat-insert (list 353 prefix me flag chan nicks) chan)
  (irchat-add-chan-to-alist chan)
  (if (get (intern chan) 'init)
      (irchat-add-to-channel nicks chan)))

(defun irchat-handle-361 (prefix me who msg &rest void) ; RPL_KILLDONE
  (irchat-insert (list 361 prefix me who msg)))

(defun irchat-handle-364 (prefix me server next msg &rest void) ; RPL_LINKS
  (setq irchat-links-reply-count (1+ irchat-links-reply-count))
  (let ((hops "?"))
    (if (string-match "^\\([^ ]+\\) \\(\\[[^]]+\\]\\)? *\\(.*\\)" msg)
	(progn
	  (setq hops (matching-substring msg 1))
	  (setq msg (matching-substring msg 3))))
    (if irchat-how-to-show-links-reply
	(irchat-insert (list '364u prefix me server next hops msg))
      (irchat-insert (list 364 prefix me server next hops msg)))))

(defun irchat-handle-365 (prefix me mask msg &rest void) ; RPL_ENDOFLINKS
  (if (= 0 irchat-links-reply-count)
      (irchat-insert (list 365 prefix me mask msg)))
  (setq irchat-links-reply-count 0))

(defun irchat-handle-366 (prefix me chan msg &rest void) ; RPL_ENDOFNAMES
  (if (get (intern chan) 'init)
      (put (intern chan) 'init nil))
  (irchat-insert (list 366 prefix me chan msg) chan))

(defun irchat-handle-367 (prefix me chan ban &rest void) ; RPL_BANLIST
  (setq irchat-ban-reply-count (1+ irchat-ban-reply-count))
  (irchat-insert (list 367 prefix me chan ban) chan))

(defun irchat-handle-368 (prefix me chan msg &rest void) ; RPL_ENDOFBANLIST
  (if (= 0 irchat-ban-reply-count)
      (irchat-insert (list 368 prefix me chan msg)))
  (setq irchat-ban-reply-count 0))

(defun irchat-handle-369 (prefix me nick msg &rest void) ; RPL_ENDOFWHOWAS
  (irchat-insert (list 369 prefix me nick msg)))

(defun irchat-handle-371 (prefix me msg &rest void) ; RPL_INFO
  (irchat-insert (list 371 prefix me msg)))

(defun irchat-handle-372 (prefix me msg &rest void) ; RPL_MOTD
  (irchat-insert (list 372 prefix me msg)))

(defun irchat-handle-373 (prefix me msg &rest void) ; RPL_INFOSTART
  (irchat-insert (list 373 prefix me msg)))

(defun irchat-handle-374 (prefix me msg &rest void) ; RPL_ENDOFINFO
  (irchat-insert (list 374 prefix me msg)))

(defun irchat-handle-375 (prefix me msg &rest void) ; RPL_MOTDSTART
  (irchat-insert (list 375 prefix me msg)))

(defun irchat-handle-376 (prefix me msg &rest void) ; RPL_ENDOFMOTD
  (irchat-insert (list 376 prefix me msg)))

(defun irchat-handle-381 (prefix me msg &rest void) ; RPL_YOUREOPER
  ":server.name 381 me :Good afternoon, gentleman. I am a HAL 9000 computer."
  (irchat-insert (list 381 prefix me msg)))

(defun irchat-handle-382 (prefix me file msg &rest void) ; RPL_REHASHING
  (irchat-insert (list 382 prefix me file msg)))

(defun irchat-handle-391 (prefix me server time &rest void) ; RPL_TIME
  ":server.name 391 me server.name :Monday August 5 1996 -- 01:39 +09:00"
  (irchat-insert (list 391 prefix me server time)))

;;;
;;; 400 replies -- ERRORS
;;; 
(defun irchat-handle-400s (number prefix me &rest args)
  (irchat-handle-x00s number prefix me args))

(defun irchat-handle-401 (prefix me nick msg &rest void) ; ERR_NOSUCHNICK
  (if (irchat-ischannel nick)
      (irchat-insert (format "*** No such channel (%s)\n"
			     (irchat-chan-virtual nick)))
    (if (string-match "@" nick)
	(irchat-insert (format "*** No such user (%s)\n" nick))
      (if (not (string= nick irchat-auto-whois-nick))
	  (irchat-send "WHOWAS %s" nick)
	(setq irchat-auto-whois-nick "")))))

(defun irchat-handle-402 (prefix me nick msg &rest void) ; ERR_NOSUCHSERVER
  ":server.name 402 me servername :No such server"
  (if (string-match "^[^.*]+$" nick)
      (if (not (string= nick irchat-auto-whois-nick))
	  (irchat-insert
	   (format "*** Error: No such nick. (%s)\n" nick))
	(setq irchat-auto-whois-nick ""))
    (irchat-insert
     (format "*** Error: No such server. (%s)\n" nick))))

(defun irchat-handle-403 (prefix me chan msg &rest void)
  (irchat-insert (format "*** %s (%s)\n" msg (irchat-chan-virtual chan))))

(defun irchat-handle-404 (prefix me chan msg &rest void)
  (irchat-insert (format "*** %s (%s)\n" msg (irchat-chan-virtual chan))))

(defun irchat-handle-405 (prefix me chan msg &rest void)
  (irchat-insert (format "*** %s (%s)\n" msg (irchat-chan-virtual chan))))

(defun irchat-handle-406 (prefix me nick msg &rest void)
  (irchat-insert (format "*** %s (%s)\n" msg nick))
  (let ((userhost (irchat-get-userhost nick))
	(lastchan (irchat-get-lastchan nick)))
    (if userhost
	(irchat-insert (format "*** %s = <%s>\n" nick userhost)))
    (if lastchan
	(irchat-insert (format "*** %s = last message to %s [%s]\n" nick
			       (irchat-chan-virtual lastchan)
			       (irchat-past-time1
				(irchat-get-lasttime nick)))))))

(defun irchat-handle-407 (prefix me msg1 msg2 &optional msg3 &rest void)
  (if msg3
      (progn
	(setq msg1 (concat msg1 " " msg2))
	(setq msg2 msg3)
	(setq msg3 nil)))
  (if (string-match "^\\([^ ]+\\)\\( recipients.*\\)" msg2)
      (progn
	(setq msg3 (matching-substring msg2 1))
	(setq msg2 (concat msg1 (matching-substring msg2 2)))
	(setq msg1 msg3)))
  (irchat-insert (list 407 prefix me msg1 msg2)))

(defun irchat-handle-412 (prefix me msg &rest void)
  (message "IRCHAT: No text to send"))

(defun irchat-handle-421 (prefix me cmd msg &rest void)
  (irchat-insert (format "*** Unknown command (%s)\n" cmd)))

(defun irchat-handle-422 (prefix me msg &rest void)
  (irchat-insert (format "*** No message of the day in %s\n" prefix)))

(defun irchat-handle-432 (prefix me nick msg &rest void) ;;; ERR_ERRONICK?
  "Handle the 432 reply (erroneus nickname)"
  (save-excursion
    (set-buffer irchat-Command-buffer)
    (if irchat-no-configure-windows
	(setq irchat-nickname-erroneus t)
      (message
       "IRCHAT: Erroneus Nickname.  Choose a new one with %s."
       (substitute-command-keys "\\[irchat-Command-nickname]")))))

(defun irchat-handle-433 (prefix me nick msg &rest void) ;;; ERR_NICKNAMEINUSE
  "Handle the 433 reply (nickname already in use)"
  (save-excursion
    (set-buffer irchat-Command-buffer)
    (if irchat-no-configure-windows
	(setq irchat-nickname-already-in-use t)
      (message
       "IRCHAT: Nickname %s already in use.  Choose a new one with %s." nick
       (substitute-command-keys "\\[irchat-Command-nickname]")))))

(defun irchat-handle-437 (prefix me chan msg &rest void)
  "437 ERR_UNAVAILRESOURCE nick|chan :Nick/channel is temporarily unavailable"
  (irchat-insert (format "*** %s (%s)\n" msg (irchat-chan-virtual chan))))

(defun irchat-handle-441 (prefix me nick chan msg &rest void)
  "441 ERR_USERNOTINCHANNEL nick chan :They aren't on that channel"
  (irchat-insert (list 441 prefix me nick chan msg)))

(defun irchat-handle-442 (prefix me chan msg &rest void)
  "442 ERR_NOTONCHANNEL chan :You're not on that channel"
  (irchat-insert (format "*** %s (%s)%s\n" msg (irchat-chan-virtual chan)
			  (if (string= prefix irchat-my-server) ""
			      (format " from %s" prefix)))))

(defun irchat-handle-451 (prefix &rest args)
  (if irchat-after-registration
      (irchat-insert (format "*** You have not registered.\n"))))

(defun irchat-handle-464 (prefix me msg &rest void)
  (irchat-insert (format 
       "*** Password incorrect from %s. Try again with password.\n" prefix))
  (setq irchat-reconnect-with-password t))

(defun irchat-handle-471 (prefix me chan msg &rest void) ; ERR_CHANNELISFULL
  ":server.name 471 me chan :Sorry, cannot join channel."
  (setq irchat-invited-channel chan)
  (irchat-insert (format "*** Error: Sorry, channel %s is full.\n"
			 (irchat-chan-virtual chan))))

(defun irchat-handle-472 (prefix me char msg &rest void) ; ERR_UNKNOWNMODE
  ":server.name 472 me char :is unknown mode char to me"
  (irchat-insert (format "*** Error: '%s' %s.\n" char msg)))

(defun irchat-handle-473 (prefix me chan msg &rest void) ; ERR_INVITEONLYCHAN
  ":server.name 473 me chan :Sorry, cannot join channel."
  (setq irchat-invited-channel chan)
  (irchat-insert (format "*** Error: Sorry, channel %s is invited only.\n"
			 (irchat-chan-virtual chan))))

(defun irchat-handle-474 (prefix me chan msg &rest void) ; ERR_BANNEDFROMCHAN
  ":server.name 474 me chan :Sorry, cannot join channel."
  (setq irchat-invited-channel chan)
  (irchat-insert (format "*** Error: Sorry, you are banned from channel %s.\n"
			 (irchat-chan-virtual chan))))

(defun irchat-handle-475 (prefix me chan msg &rest void) ; ERR_BADCHANNELKEY
  ":server.name 475 me chan :Sorry, cannot join channel."
  (setq irchat-invited-channel chan)
  (irchat-insert (format "*** Error: Sorry, incorrect key for channel %s.\n"
			 (irchat-chan-virtual chan))))

(defun irchat-handle-476 (prefix me chan msg &rest void)
  "476 ERR_BADCHANMASK chan :Bad Channel Mask"
  (irchat-insert (list 476 prefix me chan msg)))

(defun irchat-handle-477 (prefix me chan msg &rest void)
  ":server.name 477 me chan :Channel doesn't support modes"
  (irchat-insert (format "*** %s (%s)\n" msg
			 (irchat-chan-virtual chan))))

(defun irchat-handle-482 (prefix me chan msg &rest void)
  (message "IRCHAT: You are not a channel operator on %s"
	   (irchat-chan-virtual chan)))

(defun irchat-handle-484 (prefix me msg &rest void)
  ":server.name 484 me :Your connection is restricted!"
  (irchat-insert (format "*** %s\n" msg)))

;;;
;;; 500 replies -- ERRORS
;;;
(defun irchat-handle-500s (number prefix me &rest args)
  (irchat-handle-x00s number prefix me args))

(defun irchat-handle-x00s (number prefix me args)
  (cond
   ((= (length args) 1)
    (irchat-insert (format "*** %s\n" (car args))))
   ((= (length args) 2)
    (irchat-insert (format "*** %s (%s)\n" (nth 1 args)
			   (irchat-chan-virtual (car args)))))
   ((= (length args) 3)
    (irchat-insert (format "*** %s %s (%s)\n" (car args) (nth 2 args)
			   (irchat-chan-virtual (nth 1 args)))))
   (t
    (irchat-insert (format "IRCHAT: Strange %s reply = %s" number args)))))

(defun irchat-print-nick-userhost (nick)
  (let (userhost)
    (if (and (or (eq irchat-print-userhost-when 'always)
		 (and (eq irchat-print-userhost-when 'new)
		      (irchat-get-newuserhost nick)))
	     (setq userhost (irchat-get-userhost nick)))
	(progn
	  (irchat-put-newuserhost nick nil)
	  (format "%s <%s>" nick userhost))
      nick)))

(defun irchat-msg-from-ignored (src chan msg)
  (save-excursion
    (let ((buf (current-buffer)))
      (set-buffer irchat-IGNORED-buffer)
      (goto-char (point-max))
      (insert (format "%s::%s %s\n" src chan msg))
      (set-buffer buf)
      t)))

(provide 'irchat-handle)
