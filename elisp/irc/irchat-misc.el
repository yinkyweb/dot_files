;;;
;;; irchat-send
;;;
(defun irchat-send (&rest args)
  (let ((msg (apply 'format args)))
    (irchat-update-time)
    (setq irchat-send-time irchat-time)
    (irchat-send-to-process irchat-server-process msg)
    (irchat-debug msg)))

(defun irchat-send-to-process (process msg)
  (process-send-string process (concat msg "\n")))

(defun irchat-send-gaga ()
  (irchat-update-time)
  (if (and (= irchat-seq-ping -1)
	   (or (irchat-send-full (+ 512 20))
	       (> irchat-time (+ irchat-send-time 60))))
      (progn
	(irchat-send-real "NOTICE %s :\007%d" irchat-nickname
			  (+ irchat-seq 20))
	(setq irchat-seq-ping irchat-seq))))

(defun irchat-send-real (&rest args)
  (let ((msg (apply 'format args)) len)
    (irchat-update-time)
    (setq len (length msg))
    (setq irchat-seq (+ irchat-seq len))
    (setq irchat-send-time irchat-time)
    (irchat-send-to-process irchat-server-process msg)))

(defun irchat-send-full (len)
  (> (+ irchat-seq len) (+ irchat-seq-ok 1000)))

(defun irchat-send-gege (seq)
  (setq irchat-seq-ping -1)
  (setq irchat-seq-ok seq))
  

(defun irchat-send-check ()
  (if (or (null irchat-send-time)
	  (< (+ irchat-send-time 60) irchat-time))
      (irchat-send-pong)))

(defun irchat-send-pong ()
  (irchat-send "PONG :%s" irchat-my-server))

(defun irchat-send-privmsg (dst msg &optional notsend)
  (irchat-handle-message irchat-nickname dst msg 'MYMSG)
  (if (null notsend)
      (if (string-match "^=\\([^ ]+\\)" dst)
	  (irchat-dcc-chat-send (matching-substring dst 1) msg)
	(irchat-send "PRIVMSG %s :%s" dst msg))))

(defun irchat-debug (msg)
  (if (and irchat-debug-buffer (get-buffer irchat-debug-buffer))
      (let ((obuf (current-buffer)) opoint)
	(set-buffer irchat-debug-buffer)
	(setq opoint (point))
	(goto-char (point-max))
	(insert (concat msg "\n"))
	(goto-char opoint)
	(set-buffer obuf))))

(defun irchat-DEBUG (&rest args)
  (irchat-insert (concat "DEBUG: " (apply 'format args))))
; (irchat-debug (concat "DEBUG: " (apply 'format args))))

;;;
;;; irchat-format
;;;
(defun irchat-format (&rest args)
  (if (car args)
      (let ((form (car args)) (str "") (len 0)
	    num cmd arg pos rev sep func argstr tmpstr)
	(while (string-match "^\\([^%]*\\)%\\(.\\)\\(.?\\)\\(.*\\)" form)
	  (setq str (concat str (matching-substring form 1)))
	  (setq num (matching-substring form 2))
	  (setq cmd (matching-substring form 3))
	  (setq form (matching-substring form 4))
	  (setq pos "")
	  (setq len 0)
	  (setq rev nil)
	  (setq sep "")
	  (setq func 'irchat-decode-message)
	  (if (or (string= cmd "+") (string= cmd "-"))
	      (progn
		(setq pos cmd)
		(string-match "^\\([0-9]*\\)\\(.?\\)\\(.*\\)" form)
		(setq len (matching-substring form 1))
		(setq cmd (matching-substring form 2))
		(setq form (matching-substring form 3))
		(setq len (string-to-int len))))
	  (if (string= num "%")
	      (setq str (concat str "%")
		    form (concat cmd form))
	    (setq num (string-to-char num))
	    (if (and (< 96 num) (< num 103))
		(setq num (- num 87)))
	    (if (and (< 47 num) (< num 58))
		(setq num (- num 48)))
	    (if (< 16 num)
		(setq str (concat str "%" num)
		      form (concat cmd form))
	      (setq arg (nth num args))
	      (cond
	       ((string= cmd "{")
		(string-match "^\\([^}]+\\)}\\(.*\\)" form)
		(setq func (intern (matching-substring form 1)))
		(setq form (matching-substring form 2)))
	       ((string= cmd "c") ;; channel
		(setq sep irchat-format-separator-channel)
		(setq func 'irchat-chan-virtual))
	       ((string= cmd "C") ;; channel
		(setq rev t)
		(setq sep irchat-format-separator-channel)
		(setq func 'irchat-chan-virtual))
	       ((string= cmd "n")  ;; nick
		(setq sep irchat-format-separator-nick))
	       ((string= cmd "N") ;; nick
		(setq rev t)
		(setq sep irchat-format-separator-nick))
	       ((string= cmd "o") ;; mode
		(setq rev t)
		(setq sep irchat-format-separator-mode)
		(setq func 'irchat-decode-str-if-reversible))
	       ((string= cmd "m") ;; message
		(setq func 'irchat-decode-message))
	       ((string= cmd "u") ;; upcase message
		(setq func 'irchat-upcase-decode-message))
	       ((string= cmd "d") ;; downcase message
		(setq func 'irchat-downcase-decode-message))
	       ((string= cmd "r") ;; message if reversible
		(setq func 'irchat-decode-str-if-reversible))
	       ((string= cmd "t") ;; time (relative)
		(setq func 'irchat-convert-seconds1))
	       ((string= cmd "a") ;; time (relative -> absolute)
		(setq func 'irchat-convert-seconds0))
	       ((string= cmd "T") ;; time (absolute)
		(setq func 'irchat-convert-time))
	       (t
		(setq func 'irchat-dummy)))
	      (setq tmpstr nil)
	      (setq arg (if (listp arg) arg (list arg)))
	      (while arg
		(setq argstr (if func (apply func (car arg) nil) (car arg)))
		(if tmpstr
		    (if rev
			(setq tmpstr (concat tmpstr sep argstr))
		      (setq tmpstr (concat argstr sep tmpstr)))
		  (setq tmpstr argstr))
		(setq arg (cdr arg)))
	      (if (and (not (eq len 0))
		       (< 0 (setq len (- len (length tmpstr)))))
		  (if (string= pos "+")
		      (setq tmpstr (concat tmpstr (make-string len 32)))
		    (setq tmpstr (concat (make-string len 32) tmpstr))))
	      (setq str (concat str tmpstr)))))
	(concat str form))
    nil))
  
(defun irchat-upcase-decode-message (str)
  (upcase (irchat-decode-message str)))

(defun irchat-downcase-decode-message (str)
  (downcase (irchat-decode-message str)))

(defun irchat-decode-str-if-reversible (str)
  (let (decoded)
    (if (and str (setq decoded (irchat-decode str))
	     (string= str (irchat-encode decoded))) decoded str)))
(defun irchat-decode-str (str)
  (if str (or (irchat-decode str) str) nil))
(defun irchat-encode-str (str)
  (if str (or (irchat-encode str) str) nil))

(fset 'irchat-decode-chan 'irchat-decode-str-if-reversible)
(fset 'irchat-encode-chan 'irchat-encode-str)

(fset 'irchat-decode-var 'irchat-decode-str)
(fset 'irchat-encode-var 'irchat-encode-str)

(defun irchat-dummy (str)
  "(irchat-format ERROR)")

(defun irchat-convert-seconds1 (time)
  "Convert seconds to printable string." ;; ex. 650 => 10 minutes 50 seconds
  (if (null time) (setq time 0))
  (let* ((seconds (if (stringp time) (string-to-int time) time))
	 (minutes (/ seconds 60))
	 (seconds (if minutes (% seconds 60) seconds))
	 (hours (/ minutes 60))
	 (minutes (if hours (% minutes 60) minutes))
	 (days (/ hours 24))
	 (hours (if days (% hours 24) hours))
	 (ds (and (/= 0 days)
		  (format "%d day%s, " days
			  (if (> days 1) "s" ""))))
	 (hs (and (/= 0 hours)
		  (format "%d hour%s, " hours
			  (if (> hours 1) "s" ""))))
	 (ms (and (/= 0 minutes)
		  (format "%d minute%s " minutes
			  (if (> minutes 1) "s" ""))))
	 (ss (format "%d seconds" seconds)))
    (concat ds hs ms (if seconds ss ""))))

(defun irchat-convert-seconds2 (time &optional th)
  "Convert seconds to printable string." ;; ex. 2669820 => 30th 21:37:00
  (if (null time) (setq time 0))
  (let* ((seconds (if (stringp time) (string-to-int time) time))
	 (minutes (/ seconds 60))
	 (seconds (if minutes (% seconds 60) seconds))
	 (hours (/ minutes 60))
	 (minutes (if hours (% minutes 60) minutes))
	 (days (/ hours 24))
	 (hours (if days (% hours 24) hours)))
    (format "%d%s %02d:%02d:%02d" days
	    (if th
		(if (= days 1) "st"
		  (if (= days 2) "nd"
		    (if (= days 3) "rd" "th")))
	      (if (or (= days 0) (= days 1)) "day " "days"))
	    hours minutes seconds)))

(defun irchat-past-time1 (time)
  (irchat-convert-seconds2 time t))

(defun irchat-convert-seconds0 (difftimestr)
  (let (curtime curhigh curlow difftime diffhigh difflow high low)
    (setq curtime (current-time))
    (setq curhigh (car curtime))
    (setq curlow (car (cdr curtime)))
    (setq difftime (convert-high-low difftimestr))
    (setq diffhigh (car difftime))
    (setq difflow (cdr difftime))
    (if (> curlow difflow)
	(setq low (- curlow difflow))
      (setq curhigh (- curhigh 1))
      (setq curlow (+ curlow 65536))
      (setq low (- curlow difflow)))
    (setq high (- curhigh diffhigh))
    (format-time-string "%a %b %e %H:%M:%S %Y" (cons high low))))

(defun irchat-convert-time (utimestr)
  (if (stringp utimestr)
      (format-time-string "%a %b %e %H:%M:%S %Y" (convert-high-low utimestr))
    "unknown"))

(defun convert-high-low (utimestr)
  ;; "1101606139" => (16809 . 11515)
  (let ((len (length utimestr)) high low)
    (setq high (string-to-number (substring utimestr 0 (- len 2))))
    (setq low (string-to-number (substring utimestr (- len 2) len)))
    (setq low (+ (* (% high 65536) 100) low))
    (setq high (+ (* (/ high 65536) 100) (/ low 65536)))
    (setq low (% low 65536))
    (cons high low)))

(defun irchat-whoischannels (str)
  (let (msg oper chan)
    (while (string-match "^\\([@+]?\\)\\([^ ]+\\) +\\(.*\\)$" str)
      (setq oper (matching-substring str 1))
      (setq chan (matching-substring str 2))
      (setq str (matching-substring str 3))
      (if (not (irchat-ischannel chan))
	  (setq chan (concat oper chan)
		oper ""))
      (setq chan (irchat-format "%1c" chan))
      (if msg
	  (setq msg (concat msg " " oper chan))
	(setq msg (concat oper chan))))
    msg))

;;;
;;; irchat-insert
;;;
(defun irchat-insert-special (msg &optional pattern)
  (irchat-insert msg 'private pattern))

(defun irchat-insert-allchan (msg &optional pattern)
  (irchat-insert msg (cons 'private (irchat-get-chans irchat-nickname))
		 pattern))

(defun irchat-insert (msg &optional chans pattern)
  (let ((obuf (current-buffer)) opoint chan buf win
	visible suppress non-suppress)
    (if (not irchat-handling)
	(irchat-update-time))
    (if (and msg (listp msg))
	(progn
	  (setq pattern msg)
	  (setq msg nil)))
    (while chans
      (setq chan (if (listp chans) (car chans) chans))
      (setq chans (if (listp chans) (cdr chans) nil))
      (if (setq buf (if (bufferp chan) chan (irchat-chanbuf-get chan)))
	  (progn
	    (set-buffer buf)
	    (setq opoint (point))
	    (goto-char (point-max))
	    (if (setq win (get-buffer-window buf))
		  (if (or irchat-buggy-emacs-pos-visible-in-window-p
			  (pos-visible-in-window-p (point-max) win))
		      (setq visible t)))
	    (irchat-insert-sub msg pattern
			       t (not (eq buf irchat-Private-buffer)))
	    (if irchat-freeze-local
		(goto-char opoint))
	    (if (setq win (get-buffer-window buf))
		(progn
		  (if (or irchat-buggy-emacs-pos-visible-in-window-p
			  (pos-visible-in-window-p (point-max) win))
		      (setq visible t))
		  (if (not irchat-freeze-local)
		      (set-window-point win (point-max)))))
	    (if irchat-suppress-local
		(setq suppress t))
	    (if (not irchat-suppress-local)
		(setq non-suppress t))
	    (set-buffer obuf))))
    (if (and (not visible) (or non-suppress (not suppress)))
	(progn
	  (set-buffer irchat-Others-buffer)
	  (if (> (point-max) irchat-others-high-watermark)
	      (let ((buffer-read-only nil))
		(delete-region 1 (- (point-max) irchat-others-low-watermark))))
	  (irchat-insert-sub msg pattern nil nil)
	  (if (setq win (get-buffer-window irchat-Others-buffer))
	      (progn
		(set-window-point win (point-max))
		(let ((owin (selected-window)))
		  (select-window win)
		  (recenter (- (window-height) 1))
		  (select-window owin))))
	  (set-buffer obuf)))
    (set-buffer irchat-Dialogue-buffer)
    (let ((opoint (point)))
      (goto-char (point-max))
      (irchat-insert-sub msg pattern nil nil)
      (if (setq win (get-buffer-window irchat-Dialogue-buffer))
	  (if irchat-freeze
	      (goto-char opoint)
	    ;;(set-window-point win (point-max))
	    (if (not (pos-visible-in-window-p (point-max) win))
		(let ((owin (selected-window)))
		  (select-window win)
		  (goto-char (point-max))
		  (recenter (- (window-height) 1))
		  (select-window owin))))))
    (set-buffer obuf)))

(defun irchat-insert-sub (msgs pattern local chanbuf)
  (let ((buffer-read-only nil) (replace nil) msg)
    (if (null msgs)
	(let (result)
	  (setq result (irchat-insert-pattern pattern irchat-previous-pattern
					      chanbuf))
	  (setq replace (nth 0 result))
	  (setq irchat-previous-pattern (nth 1 result))
	  (setq msgs (nth 2 result)))	
      (setq irchat-previous-pattern pattern))
    (goto-char (point-max)) ; again
    (if replace
	(progn
	  (backward-char 1)
	  (beginning-of-line)
	  (delete-region (point) (point-max))))
    (if (and local irchat-beep-local)
	(let ((cmd (if (listp pattern) (car pattern) pattern))
	      (cmds irchat-beep-command-list))
	  (while cmds
	    (if (eq cmd (car cmds))
		(beep))
	    (setq cmds (cdr cmds)))))
    (while msgs
      (setq msg (if (listp msgs) (car msgs) msgs))
      (setq msgs (if (listp msgs) (cdr msgs) nil))
      (if msg
	  (progn
	    (if (not (string-match "\n" msg))
		(setq msg (concat msg "\n")))
	    (if irchat-print-time
		(insert (substring irchat-time-string 11 16) " "))
	    (insert msg))))))

(defun irchat-insert-pattern (c o chanbuf)
  (let (time diff new add ok nicklist nick-o nick-O
	     flag nick flags nicks badflags badnicks)
    (setq time (+ (* 60 irchat-hour) irchat-minute))
    (if (and (listp o) (car o))
	(progn
	  (setq diff (- time (car o)))
	  (setq o (cdr o)))
      (setq diff -1)) ;; this means o is invalid.
    (if (and (or (= diff 0) (= diff 1) (= diff -1439)) ; within 1min
	     (listp o))
	(cond
	 ((eq (car c) 'JOIN)
	  (cond
	   ((and (eq (car o) 'JOIN)
		 (not (string-match " " (nth 1 c)))
		 (not (string-match " " (nth 1 o)))
		 (string= (nth 2 c) (nth 2 o)))
            (setq new (list 'JOIN1 (list (nth 1 c) (nth 1 o)) (nth 2 c))))
	   ((and (eq (car o) 'JOIN)
		 (or (and (string-match "^\\(.*\\) " (nth 1 o))
			  (string= (matching-substring (nth 1 o) 1) (nth 1 c)))
		     (string= (nth 1 c) (nth 1 o))))
	    (setq new (list 'JOIN2 (nth 1 o) (list (nth 2 c) (nth 2 o)))))
	   ((and (eq (car o) 'JOIN1)
		 (not (string-match ">$" (nth 1 c)))
		 (string= (nth 2 c) (nth 2 o)))
            (setq new (list 'JOIN1 (cons (nth 1 c) (nth 1 o)) (nth 2 c))))
	   ((and (eq (car o) 'JOIN2)
		 (or (and (string-match "^\\(.*\\) " (nth 1 o))
			  (string= (matching-substring (nth 1 o) 1) (nth 1 c)))
		     (string= (nth 1 c) (nth 1 o))))
	    (setq new (list 'JOIN2 (nth 1 o) (cons (nth 2 c) (nth 2 o)))))))
	 ((eq (car c) 'PART)
	  (cond
	   ((and (eq (car o) 'PART)
		 (not (string-match ">$" (nth 1 c)))
		 (not (string-match ">$" (nth 1 o)))
		 (string= (nth 2 c) (nth 2 o))
		 (string= (nth 3 c) (nth 3 o)))
	    (setq new (list 'PART1 (list (nth 1 c) (nth 1 o)) (nth 1 c)
			    (nth 3 c))))
	   ((and (eq (car o) 'PART)
		 (or (and (string-match "^\\(.*\\) " (nth 1 o))
			  (string= (matching-substring (nth 1 o) 1) (nth 1 c)))
		     (string= (nth 1 c) (nth 1 o)))
		 (string= (nth 3 c) (nth 3 o)))
	    (setq new (list 'PART2 (nth 1 o) (list (nth 2 c) (nth 2 o))
			    (nth 3 c))))
	   ((and (eq (car o) 'PART1)
		 (not (string-match ">$" (nth 1 c)))
		 (string= (nth 2 c) (nth 2 o))
		 (string= (nth 3 c) (nth 3 o)))
	    (setq new (list 'PART1 (cons (nth 1 c) (nth 1 o)) (nth 1 c)
			    (nth 3 c))))
	   ((and (eq (car o) 'PART2)
		 (or (and (string-match "^\\(.*\\) " (nth 1 o))
			  (string= (matching-substring (nth 1 o) 1) (nth 1 c)))
		     (string= (nth 1 c) (nth 1 o)))
		 (string= (nth 3 c) (nth 3 o)))
	    (setq new (list 'PART2 (nth 1 o) (cons (nth 2 c) (nth 2 o))
			    (nth 3 c))))))
	 ((eq (car c) 'MODE-ME)
	  (cond
	   ((eq (car o) 'MODE-ME)
	    (setq new (list 'MODE-ME (append (nth 1 o) (nth 1 c)))))))
	 ((eq (car c) 'MODE)
	  (cond
	   ((and (eq (car o) 'MODE)
		 (string= (nth 1 c) (nth 1 o))
		 (string= (nth 2 c) (nth 2 o)))
	    (setq new (list 'MODE (nth 1 c) (nth 2 c)
			    (append (nth 3 o) (nth 3 c)))))
	   ((and (or (eq (car o) 'JOIN) (eq (car o) 'JOIN1))
		 (string= (nth 2 c) (nth 2 o))
		 (string-match "\\." (nth 1 c))
		 (string-match "^\\+[Oov]+$" (car (nth 3 c))))
	    (setq nicklist (if (listp (nth 1 o))
			       (nth 1 o)
			     (list (nth 1 o))))
	    (setq flags (car (nth 3 c)))
	    (setq nicks (cdr (nth 3 c)))
	    (setq badflags "+")
	    (setq badnicks nil)
	    (while (and (string-match "^\+\\([Oov]\\)\\(.*\\)" flags)
			(stringp (car nicks)))
	      (setq flag (matching-substring flags 1))
	      (setq flags (format "+%s" (matching-substring flags 2)))
	      (setq nick (car nicks))
	      (setq nicks (cdr nicks))
	      (if (string= flag "v")
		  (setq nick-O (format "@@%s" nick)
			nick-o (format "@%s" nick)))
	      (setq ok nil)
	      (setq nicklist (mapcar
			      '(lambda (x)
				 (let (xx)
				   (cond
				    ((string= flag "O")
				     (if (string= x nick)
					 (setq xx (format "@@%s" nick))))
				    ((string= flag "o")
				     (if (string= x nick)
					 (setq xx (format "@%s" nick))))
				    ((string= flag "v")
				     (cond
				      ((string= x nick-O)
				       (setq xx (format "@@+%s" nick)))
				      ((string= x nick-o)
				       (setq xx (format "@+%s" nick)))
				      ((string= x nick)
				       (setq xx (format "+%s" nick))))))
				   (if (null xx)
				       x
				     (setq ok t)
				     xx)))
			      nicklist))
	      (if (not ok)
		  (setq badflags (format "%s%s" badflags flag)
			badnicks (append badnicks (list nick)))))
	    (setq new (list 'JOIN1 nicklist (nth 2 o)))
	    (if (not (string= badflags "+"))
		(setq add (list 'MODE (nth 1 c) (nth 2 c)
				(cons badflags badnicks)))))))
	 ((eq (car c) 'KICK)
	  (cond
	   ((and (eq (car o) 'KICK)
		 (string= (nth 1 c) (nth 1 o))
		 (string= (nth 3 c) (nth 3 o))
		 (string= (nth 4 c) (nth 4 o)))
	    (setq new (list 'KICK2 (nth 1 c) (list (nth 2 c) (nth 2 o))
			    (nth 3 c) (nth 4 c))))
	   ((and (eq (car o) 'KICK)
		 (string= (nth 1 c) (nth 1 o))
		 (string= (nth 2 c) (nth 2 o))
		 (string= (nth 4 c) (nth 4 o)))
	    (setq new (list 'KICK3 (nth 1 c) (nth 2 c)
			    (list (nth 3 c) (nth 3 o)) (nth 4 c))))
	   ((and (eq (car o) 'KICK2)
		 (string= (nth 1 c) (nth 1 o))
		 (string= (nth 3 c) (nth 3 o))
		 (string= (nth 4 c) (nth 4 o)))
	    (setq new (list 'KICK2 (nth 1 c) (cons (nth 2 c) (nth 2 o))
			    (nth 3 c) (nth 4 c))))
	   ((and (eq (car o) 'KICK3)
		 (string= (nth 1 c) (nth 1 o))
		 (string= (nth 2 c) (nth 2 o))
		 (string= (nth 4 c) (nth 4 o)))
	    (setq new (list 'KICK3 (nth 1 c) (nth 2 c)
			    (cons (nth 3 c) (nth 3 o)) (nth 4 c))))))
	 ((eq (car c) 'QUIT)
	  (cond
	   ((and (eq (car o) 'QUIT)
		 (not (string-match ">$" (nth 1 c)))
		 (not (string-match ">$" (nth 1 o)))
		 (string= (nth 2 c) (nth 2 o)))
	    (setq new (list 'QUIT1 (list (nth 1 c) (nth 1 o)) (nth 2 c))))
	   ((and (eq (car o) 'QUIT1)
		 (not (string-match ">$" (nth 1 c)))
		 (string= (nth 2 c) (nth 2 o)))
	    (setq new (list 'QUIT1 (cons (nth 1 c) (nth 1 o)) (nth 2 c))))))))
    (if new
	(if add
	    (setq replace t
		  pattern add
		  message (list (irchat-make-message new chanbuf)
				(irchat-make-message add chanbuf)))
	  (setq replace t
		pattern new
		message (irchat-make-message new chanbuf)))
      (setq replace nil
	    pattern c
	    message (irchat-make-message c chanbuf)))
    (list replace (cons time pattern) message)))

(defun irchat-make-message (pat chanbuf)
  (let ((cmd (car pat)) (form t) name)
    (cond
     ((or (eq cmd 'JOIN) (eq cmd 'JOIN1) (eq cmd 'JOIN2))
      (if chanbuf
	  (setq form irchat-format-joinx)
	(setq form irchat-format-join)))
     ((or (eq cmd 'PART) (eq cmd 'PART1) (eq cmd 'PART2))
      (if chanbuf
	  (setq form irchat-format-partx)
	(setq form irchat-format-part)))
     ((or (eq cmd 'KICK) (eq cmd 'KICK2) (eq cmd 'KICK3))
      (if chanbuf
	  (setq form irchat-format-kickx)
	(setq form irchat-format-kick)))
     ((eq cmd 'MODE)
      (if chanbuf
	  (setq form irchat-format-modex)
	(setq form irchat-format-mode)))
     ((eq cmd 'TOPIC)
      (if chanbuf
	  (setq form irchat-format-topicx)
	(setq form irchat-format-topic)))
     ((eq cmd 'MODE-ME)
      (setq form irchat-format-mode-me))
     ((eq cmd 'KICK-ME)
      (setq form irchat-format-kick-me))
     ((eq cmd 'NICK)
      (setq form irchat-format-nick))
     ((or (eq cmd 'QUIT) (eq cmd 'QUIT1))
      (setq form irchat-format-quit))
     ((eq cmd 'WALLOPS)
      (setq form irchat-format-wallops))
     ((eq cmd 'INVITE)
      (setq form irchat-format-invite))
     ((eq cmd 'KILL)
      (setq form irchat-format-kill))
     ((eq cmd 'ERROR)
      (setq form irchat-format-error))
     ((eq cmd 'MYMSG)
      (if (irchat-ischannel (nth 2 pat))
	  (if (irchat-user-on-this-channel (nth 1 pat) (nth 2 pat))
	      (if chanbuf
		  (setq form irchat-format-message-by-me1x)
		(setq form irchat-format-message-by-me1))
	    (if chanbuf
		(setq form irchat-format-message-by-me2x)
	      (setq form irchat-format-message-by-me2)))
	(setq form irchat-format-message-by-me0)))
     ((eq cmd 'MYACT)
      (if (irchat-ischannel (nth 2 pat))
	  (if chanbuf
	      (setq form irchat-format-action-by-me1x)
	    (setq form irchat-format-action-by-me1))
	(setq form irchat-format-action-by-me0)))
     ((or (eq cmd 'PRIVMSG) (eq cmd 'DCCMSG) (eq cmd 'NOTICE))
      (if (irchat-match-my-nick (nth 2 pat))
	  (setq form irchat-format-message-to-me0)
	(if (irchat-equal (nth 2 pat) irchat-my-user-server)
	    (setq form irchat-format-message-to-me1)
	  (if (irchat-equal (nth 2 pat) irchat-my-user-host-server)
	      (setq form irchat-format-message-to-me2)
	    (if (irchat-user-on-this-channel (nth 1 pat) (nth 2 pat))
		(if (eq cmd 'PRIVMSG)
		    (if chanbuf
			(setq form irchat-format-message-to-channel-normalx)
		      (setq form irchat-format-message-to-channel-normal))
		  (if chanbuf
		      (setq form irchat-format-message-to-channel-noticex)
		    (setq form irchat-format-message-to-channel-notice)))
	      (if chanbuf
		  (setq form irchat-format-message-to-channel-nonmemx)
		(setq form irchat-format-message-to-channel-nonmem)))))))
     ((eq cmd 'ACTION)
      (if (irchat-match-me (nth 2 pat))
	  (setq form irchat-format-action-to-me)
	(if chanbuf
	    (setq form irchat-format-action-to-channelx)
	  (setq form irchat-format-action-to-channel))))
     (t
      (if (not (stringp cmd))
	  (setq cmd (prin1-to-string cmd)))
      (if (and chanbuf
	       (boundp (setq name (intern (concat "irchat-format-" cmd "x")))))
	  (setq form (eval name))
	(if (boundp (setq name (intern (concat "irchat-format-" cmd))))
	    (setq form (eval name))
	  (setq form t)))))
    (if (eq form t)
	(format "irchat-make-message: %s" pat)
      (apply 'irchat-format form (cdr pat)))))

;;;
;;; irchat-read
;;;
(defun irchat-read (prompt table &optional default)
  (let ((input (completing-read
		(if (and default (not (string= "" default)))
		    (format "%s(default %s) " prompt default)
		  (format "%s" prompt))
		table '(lambda (s) t) nil nil)))
    (if (and default (string= input ""))
	default
      input)))

(defun irchat-read-chan (prompt table &optional default)
  (irchat-chan-real (irchat-read prompt table (irchat-chan-virtual default))))

(defun irchat-read-nick (prompt &optional default table)
  (irchat-read prompt (or table irchat-nick-alist) default))

(defun irchat-read-server (prompt &optional default table)
  (irchat-read prompt (or table irchat-server-alist) default))

(defun irchat-read-word (prompt &optional default table)
  (irchat-encode-message (irchat-read prompt
				      table (irchat-decode-message default))))

(defun irchat-read-message (prompt &optional default)
  (irchat-encode-message (read-string prompt (irchat-decode-message default))))

(defun irchat-read-from-line ()
  (let (start stop)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq stop (point))
    (irchat-next-line 1)
    (irchat-encode-message (buffer-substring start stop))))

;;; this function from ange-ftp.el by Andy Norman (ange@hplb.hpl.hp.com)
(defun irchat-read-passwd (prompt)
  "Read a password from the user. Echos a . for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out."
  (let ((pass "")
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?.))
      (setq c (read-char))
      (if (and (/= c ?\b) (/= c ?\177))
	  (setq pass (concat pass (char-to-string c)))
	(if (> (length pass) 0)
	    (setq pass (substring pass 0 -1)))))
    (substring pass 0 -1)))

;;;
;;; coding system handling -- no conversion (coding system independent)
;;;
(defun irchat-define-service-coding-system (service)
  (if (fboundp 'modify-coding-system-alist)
      (modify-coding-system-alist 'network service
				  (cons 'no-conversion 'no-conversion))
    (if (fboundp 'define-service-coding-system)
	(define-service-coding-system service nil (cons *noconv* *noconv*))
      (if (fboundp 'define-service-kanji-code)
	  (define-service-kanji-code service nil 0)))))

(defun irchat-set-process-coding-system (proc)
  (if (fboundp 'modify-coding-system-alist)
      (set-process-coding-system proc 'no-conversion 'no-conversion)
    (if (fboundp 'set-process-coding-system)
	(set-process-coding-system proc *noconv* *noconv*)
      (if (fboundp 'set-process-kanji-code)
	  (set-process-kanji-code proc 0)))))

(defun irchat-decode-noconv (str) str)
(defun irchat-encode-noconv (str) str)
(defun irchat-decode-message-noconv (str) str)
(defun irchat-encode-message-noconv (str) str)

;;;
;;; misc.
;;;
(defun irchat-update-time ()
  (setq irchat-time-string (current-time-string))
  (setq irchat-day (string-to-int (substring irchat-time-string 8 10)))
  (setq irchat-hour (string-to-int (substring irchat-time-string 11 13)))
  (setq irchat-minute (string-to-int (substring irchat-time-string 14 16)))
  (setq irchat-second (string-to-int (substring irchat-time-string 17 19)))
  (setq irchat-time (+ irchat-second (* 60 irchat-minute)
		       (* 3600 irchat-hour) (* 86400 irchat-day))))

(defun irchat-shell-escape (string)
  (if (string-match "^\\([^()<>'`;$#&!?\\]*\\)\\([()<>'`;$#&!\?\\]\\)\\(.*\\)$" string)
      (concat (matching-substring string 1)
	      "\\"
	      (matching-substring string 2)
	      (irchat-shell-escape (matching-substring string 3)))
    string))

;; sample
(defun irchat-exec-netscape (url &optional new-window)
  (while (string-match "," url)
    (setq url (concat (substring url 0 (match-beginning 0))
                      "%2c"
                      (substring url (match-end 0)))))
  (if new-window
      (start-process (concat (concat "browser " url)) nil "netscape" "-noraise"
		      "-remote" (format "openURL(%s,new-window)" url))
    (start-process (concat (concat "browser " url)) nil "netscape"
		    "-remote" (format "openURL(%s)" url))))

;;; caesar-region written by phr@prep.ai.mit.edu  Nov 86
;;; modified by tower@prep Nov 86
;;; Modified by umerin@flab.flab.Fujitsu.JUNET for ROT47.
(defun irchat-caesar-region (&optional n)
  "Caesar rotation of region by N, default 13, for decrypting netnews.
ROT47 will be performed for Japanese text in any case."
  (interactive (if current-prefix-arg	; Was there a prefix arg?
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil)))
  (cond ((not (numberp n)) (setq n 13))
	((< n 0) (setq n (- 26 (% (- n) 26))))
	(t (setq n (% n 26))))		;canonicalize N
  (if (not (zerop n))		; no action needed for a rot of 0
      (progn
	(if (or (not (boundp 'caesar-translate-table))
		(/= (aref caesar-translate-table ?a) (+ ?a n)))
	    (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
	      (message "Building caesar-translate-table...")
	      (setq caesar-translate-table (make-vector 256 0))
	      (while (< i 256)
		(aset caesar-translate-table i i)
		(setq i (1+ i)))
	      (setq lower (concat lower lower) upper (upcase lower) i 0)
	      (while (< i 26)
		(aset caesar-translate-table (+ ?a i) (aref lower (+ i n)))
		(aset caesar-translate-table (+ ?A i) (aref upper (+ i n)))
		(setq i (1+ i)))
	      ;; ROT47 for Japanese text.
	      ;; Thanks to ichikawa@flab.fujitsu.junet.
	      (setq i 161)
	      (let ((t1 (logior ?O 128))
		    (t2 (logior ?! 128))
		    (t3 (logior ?~ 128)))
		(while (< i 256)
		  (aset caesar-translate-table i
			(let ((v (aref caesar-translate-table i)))
			  (if (<= v t1) (if (< v t2) v (+ v 47))
			    (if (<= v t3) (- v 47) v))))
		  (setq i (1+ i))))
	      (message "Building caesar-translate-table... done")))
	(let ((from (region-beginning))
	      (to (region-end))
	      (i 0) str len)
	  (setq str (buffer-substring from to))
	  (setq len (length str))
	  (while (< i len)
	    (aset str i (aref caesar-translate-table (aref str i)))
	    (setq i (1+ i)))
	  (goto-char from)
	  (delete-region from to)
	  (insert str)))))

(provide 'irchat-misc)
