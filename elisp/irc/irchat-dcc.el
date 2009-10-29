;; irchat-dcc.el  (for irchat-jp version)
;; Copyright (C) 1995,1996,1999 Takahiro Kikuchi

;; Author:   Takahiro Kikuchi <kick@wide.ad.jp>
;; Created:  Mar 19, 1995
;; Bug Fixed: Aug 8, 1996  special thanks to ohm@wide.ad.jp

(defvar irchat-dcc-list nil)
(defvar irchat-dcc-partner nil)

(defun irchat-dcc-request (from to rest)
  "DCC request from user"
  (cond
   ((null rest)
    (irchat-insert (format "*** Bad format DCC from %s to %s\n" from to))
    (irchat-ctcp-send-errmsg from "DCC" "bad format"))
   ((string-match "^SEND \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)" rest)
    (let ((file (matching-substring rest 1))
	  (host (matching-substring rest 2))
	  (port (matching-substring rest 3))
	  (size (matching-substring rest 4)))
      (irchat-dcc-add-object
       (list 'GET 'Offered nil (current-time-string)
	     from host port file size file))
      (irchat-insert
       (format "*** DCC SEND request from %s: %s (%s bytes)\n"
	       from file size) (irchat-chanbuf-priv to) 'DCC)
      (if irchat-dcc-auto-get-file
	  (irchat-Command-dcc-get))))
   ((string-match "^CHAT \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)" rest)
    (let ((file (matching-substring rest 1))
	  (host (matching-substring rest 2))
	  (port (matching-substring rest 3)))
      (irchat-dcc-add-object
       (list 'CHAT 'Offered nil (current-time-string)
	     from host port file))
      (irchat-insert-special (format "*** DCC CHAT request from %s\n"
			     from) 'DCC)))
   ((string-match "^CANCEL \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)" rest)
    (let ((file (matching-substring rest 1))
	  (host (matching-substring rest 2))
	  (port (matching-substring rest 3)))
      (irchat-dcc-cancel from host port file)))
   (t
    (irchat-insert (format "*** Unknown DCC from %s to %s: %s\n"
			   from to rest))
    (irchat-ctcp-send-errmsg from "DCC" "bad command"))))

(defun irchat-dcc-cancel (from host port file)
  "DCC cancel request from user"
  (let ((dcc-list irchat-dcc-list) (num 0) dcc-object found)
    (while (and dcc-list (not found))
      (setq dcc-object (car dcc-list)
	    dcc-list (cdr dcc-list))
      (if (and (string= (nth 4 dcc-object) from)
	       (string= (nth 5 dcc-object) host)
	       (string= (nth 6 dcc-object) port)
	       (string= (nth 7 dcc-object) file))
	  (setq found dcc-object)))
    (if found
	(progn
	  (irchat-insert irchat-dcc-list-header)
	  (irchat-insert (irchat-dcc-list-object 0 dcc-object))
	  (irchat-insert (format "*** DCC canceled from %s\n" from))
	  (if (nth 2 dcc-object)
	      (delete-process (nth 2 dcc-object)))
	  (irchat-dcc-delete-object dcc-object)))))
    

(defun irchat-Command-dcc-send ()
  "Send file to user via DCC"
  (interactive)
  (let (proc (completion-ignore-case t)
	     (file (expand-file-name
		    (read-file-name "File to send: " default-directory nil))))
    (setq irchat-dcc-partner
	  (irchat-read-nick "To whom: " irchat-dcc-partner))
    (setq proc (start-process irchat-dcc-program nil irchat-dcc-program
			      "file" "send" file))
    (irchat-set-process-coding-system proc)
    (irchat-dcc-add-object
     (list 'SEND 'Setting proc (current-time-string)
	   irchat-dcc-partner nil nil file nil))
    (set-process-buffer proc
            (get-buffer-create (format " DCC:%s" (process-id proc))))
    (set-process-filter proc 'irchat-dcc-filter)
    (set-process-sentinel proc 'irchat-dcc-sentinel)))

(defun irchat-dcc-filter (process output)
  (let ((obuf (current-buffer)) (pbuf (process-buffer process)) msg)
    (set-buffer pbuf)
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (while (looking-at ".*\n")
      (goto-char (match-end 0))
      (setq msg (buffer-substring (point-min) (1- (point))))
      (delete-region (point-min) (point))
      (set-buffer obuf)
      (irchat-dcc-filter-sub process msg)
      (set-buffer pbuf))
    (set-buffer obuf)))

(defun irchat-dcc-filter-sub (process msg)
  (let* ((dcc-object (irchat-dcc-get-process-object process)) filename
	 (nick (nth 4 dcc-object))
	 (file (nth 7 dcc-object))
	 (size (nth 8 dcc-object)))
    (if (null dcc-object)
	(delete-process process)   ;; garbage process
      (cond
       ((eq (nth 1 dcc-object) 'Active)
	(irchat-dcc-chat-loop-filter-sub process msg))
       ((string-match "^DCC GETTING" msg)
	;; 'Connect -> 'Getting
	(setcar (nthcdr 1 dcc-object) 'Getting)
	(irchat-insert (format "*** DCC getting file %s (%s bytes) from %s\n"
			       file size nick)))
       ((string-match "^DCC SEND \\([^ ]+\\) \\([^ ]+\\) \\(.+\\)$" msg)
	;; 'Setting -> 'Waiting
	(let ((host (matching-substring msg 1))
	      (port (matching-substring msg 2))
	      (size (matching-substring msg 3)))
	  (setcar (nthcdr 1 dcc-object) 'Waiting)
	  (setcar (nthcdr 5 dcc-object) host)
	  (setcar (nthcdr 6 dcc-object) port)
	  (setcar (nthcdr 8 dcc-object) size)
	  (setq filename (if (string-match ".*/\\(.*\\)" file)
			     (matching-substring file 1) file))
	  (irchat-send "PRIVMSG %s :DCC SEND %s %s %s %s"
		       nick filename host port size)
	  (irchat-insert
	   (format "*** DCC SEND request file %s (%s bytes) to %s\n"
		   file size nick) (irchat-chanbuf-priv nick))))
       ((string-match "^DCC SENDING" msg)
	;; 'Waiting -> 'Sending
	(setcar (nthcdr 1 dcc-object) 'Sending)
	(irchat-insert (format "*** DCC sending file %s (%s bytes) to %s\n"
			       file size nick)))
       ((string-match "^DCC CHAT \\([^ ]+\\) \\([^ ]+\\)$" msg)
	;; 'Setting -> 'Waiting
	(let ((host (matching-substring msg 1))
	      (port (matching-substring msg 2)))
	  (setcar (nthcdr 1 dcc-object) 'Waiting)
	  (setcar (nthcdr 5 dcc-object) host)
	  (setcar (nthcdr 6 dcc-object) port)
	  (irchat-send "PRIVMSG %s :DCC CHAT chat %s %s"
		       nick host port)
	  (irchat-insert (format "*** DCC CHAT request to %s\n" nick))))
       ((string-match "^DCC CHATTING" msg)
	;; 'Waiting or 'Connect -> 'Active
	(set-process-filter process 'irchat-dcc-chat-loop-filter)
	(setcar (nthcdr 1 dcc-object) 'Active)
	(irchat-insert-special
	 (format "*** DCC CHAT connection (with %s) established.\n" nick)
	 'DCC))
       ((string-match "^DCC REPORT \\(.+\\)" msg)
	(message "DCC report: %s" (matching-substring msg 1)))
       ((string-match "^DCC ERROR \\(.+\\)" msg)
	(irchat-insert (format "*** DCC ERROR: %s\n"
			       (matching-substring msg 1))))
       ((string-match "^DCC ERROR1 \\(.+\\)" msg)
	(irchat-dcc-add-object 
	 (list 'GET 'Offered nil
	       (nth 3 dcc-object) (nth 4 dcc-object) (nth 5 dcc-object)
	       (nth 6 dcc-object) (nth 9 dcc-object) (nth 8 dcc-object)))
	(irchat-insert (format "*** DCC ERROR: %s\n"
			       (matching-substring msg 1))))
       (t
	(irchat-insert (format "*** DCC FATAL ERROR: %s\n"
			       (matching-substring msg 1)))
	nil)))))

(defun irchat-dcc-sentinel (process output)
  (let* ((dcc-object (irchat-dcc-get-process-object process))
	 (type (nth 0 dcc-object))
	 (nick (nth 4 dcc-object))
	 (file (nth 7 dcc-object))
	 (size (nth 8 dcc-object)))
    (if (null dcc-object) 
	(delete-process process)   ;; garbage process
      (cond
       ((and (string-match "^finished" output) (eq type 'SEND))
	;; 'Sending -> 'Done
	(irchat-insert (format "*** DCC sent file %s (%s bytes) to %s\n"
			       file size nick))
	(message ""))
       ((and (string-match "^finished" output) (eq type 'GET))
	;; 'Getting -> 'Done
	(irchat-insert (format "*** DCC got file %s (%s bytes) from %s\n"
			       file size nick))
	(message ""))
       ((and (string-match "^finished" output) (eq type 'CHAT))
	;; 'Active -> 'Done
	(irchat-insert-special
	 (format "*** DCC CHAT connection (with %s) finished.\n" nick) 'DCC))
       (t
	(irchat-insert (format "*** DCC ERROR process (%s %s %s %s) is %s\n"
			       ; type file to/from/with nick
			       (prin1-to-string (nth 0 dcc-object))
			       (if (nth 7 dcc-object) (nth 7 dcc-object) "")
			       (cond
				((eq (nth 0 dcc-object) 'SEND) "to")
				((eq (nth 0 dcc-object) 'GET) "from")
				((eq (nth 0 dcc-object) 'CHAT) "with"))
			       (nth 4 dcc-object)
			       (substring output 0 (1- (length output)))))))
      (irchat-dcc-delete-object dcc-object))))

(defun irchat-Command-dcc-kill ()
  "Kill DCC process and object"
  (interactive)
  (let (dcc-object number filename)
    (if (numberp current-prefix-arg)
	(setq number current-prefix-arg)
      (setq number nil))
    (if number
	(setq dcc-object (irchat-dcc-get-object number))
      (setq dcc-object (irchat-dcc-get-offered-object)))
    (if (null dcc-object)
	(if number
	    (irchat-insert (format "*** DCC No.%d --- not found\n" number))
	  (irchat-insert (format "*** There is no offered DCC\n")))
      (irchat-insert irchat-dcc-list-header)
      (irchat-insert (irchat-dcc-list-object number dcc-object))
      (if (y-or-n-p "Kill this DCC? ")
	  (if (irchat-dcc-match-object dcc-object) ;;still alive?
	      (let ((nick (nth 4 dcc-object))
		    (host (nth 5 dcc-object))
		    (port (nth 6 dcc-object))
		    (file (nth 7 dcc-object)))
		(setq filename (if (string-match ".*/\\(.*\\)" file)
				   (matching-substring file 1) file))
		(irchat-send "PRIVMSG %s :DCC CANCEL %s %s %s"
			     nick file host port)
		(if (nth 2 dcc-object)
		    (delete-process (nth 2 dcc-object)))
		(irchat-dcc-delete-object dcc-object)
		(irchat-insert (format "*** DCC killed.\n"))))))))

(defun irchat-Command-dcc-get ()
  "Get offered file from list."
  (interactive)
  (let (dcc-object number)
    (if (numberp current-prefix-arg)
	(setq number current-prefix-arg)
      (setq number nil))
    (if number
	(setq dcc-object (irchat-dcc-get-object number))
      (setq dcc-object (irchat-dcc-get-offered-object)))
    (if (null dcc-object)
	(if number
	    (irchat-insert (format "*** DCC No.%d --- not found\n" number))
	  (irchat-insert (format "*** There is no offered DCC SEND\n")))
      (if (not (eq (nth 1 dcc-object) 'Offered))
	  (irchat-insert (format "*** DCC No.%d --- not offered\n" number))
	(cond
	 ((eq (nth 0 dcc-object) 'GET)
	  (let (proc dir
		(nick (nth 4 dcc-object))
		(host (nth 5 dcc-object))
		(port (nth 6 dcc-object))
		(file (nth 7 dcc-object))
		(size (nth 8 dcc-object)))
	    (while (string-match "\\(.*\\)[/~]\\(.*\\)" file)
	      (setq file (format "%s-%s"  (matching-substring file 1)
				          (matching-substring file 2))))
	    (if (file-directory-p irchat-dcc-directory)
		(setq dir irchat-dcc-directory)
	      (irchat-insert
	       (format "*** irchat-dcc-directory [%s] is not directory!!\n"
		       irchat-dcc-directory))
	      (setq dir "/tmp"))
	    (setq file (format "%s/%s" dir file))
	    (if (file-attributes (expand-file-name file dir))
		(progn
		  (irchat-insert
		   (format "*** file[%s] already exist. Do you overwrite it?\n"
			   file))
		  (setq file (read-file-name "Write file: " file file))))
	    (setcar (nthcdr 7 dcc-object) file)
	    (setq file (expand-file-name file dir))
	    (setq proc
		  (start-process irchat-dcc-program nil irchat-dcc-program 
				 "file" "get" host port size file))
	    (irchat-set-process-coding-system proc)
	    (setcar (nthcdr 1 dcc-object) 'Connect)
	    (setcar (nthcdr 2 dcc-object) proc)
	    (set-process-buffer proc
                    (get-buffer-create (format " DCC:%s" (process-id proc))))
	    (set-process-sentinel proc 'irchat-dcc-sentinel)
	    (set-process-filter proc 'irchat-dcc-filter)))
	 ((eq (nth 0 dcc-object) 'CHAT)
	  (let (proc
		(nick (nth 4 dcc-object))
		(host (nth 5 dcc-object))
		(port (nth 6 dcc-object)))
	    (setq proc
		  (start-process irchat-dcc-program nil irchat-dcc-program 
				 "chat" "connect" host port))
	    (irchat-set-process-coding-system proc)
	    (setcar (nthcdr 1 dcc-object) 'Connect)
	    (setcar (nthcdr 2 dcc-object) proc)
	    (set-process-buffer proc
                    (get-buffer-create (format " DCC:%s" (process-id proc))))
	    (set-process-sentinel proc 'irchat-dcc-sentinel)
	    (set-process-filter proc 'irchat-dcc-filter)))
	 (t
	  (irchat-insert "Fatal error! in Command-dcc-get\n")
	  nil))))))

(defun irchat-Command-dcc-chat ()
  "send DCC CHAT request"
  (interactive)
  (let (proc (completion-ignore-case t))
    (setq irchat-dcc-partner
	  (irchat-read-nick "With whom: " irchat-dcc-partner))
    (setq proc (start-process irchat-dcc-program nil irchat-dcc-program
			      "chat" "listen"))
    (irchat-set-process-coding-system proc)
    (irchat-dcc-add-object
     (list 'CHAT 'Setting proc (current-time-string)
	   irchat-dcc-partner nil nil "chat"))
    (set-process-buffer proc
            (get-buffer-create (format " DCC:%s" (process-id proc))))
    (set-process-filter proc 'irchat-dcc-filter)
    (set-process-sentinel proc 'irchat-dcc-sentinel)))

(defun irchat-dcc-chat-send (dst msg)
  (let ((dcc-list irchat-dcc-list) dcc-object found not-active)
    (while (and dcc-list (not found))
      (setq dcc-object (car dcc-list)
	    dcc-list (cdr dcc-list))
      (if (and (eq (nth 0 dcc-object) 'CHAT)
	       (irchat-equal (nth 4 dcc-object) dst))
	  (if (not (eq (nth 1 dcc-object) 'Active))
	      (setq not-active t)
	    (irchat-send-to-process (nth 2 dcc-object) msg)
	    (setq found t))))
    (if (not found)
	(if not-active
	    (irchat-insert (format "*** DCC CHAT with %s is not active\n" dst))
	  (irchat-insert (format "*** There is no DCC CHAT with %s\n" dst))))))
  
(defun irchat-dcc-chat-loop-filter (process output)
  (let ((obuf (current-buffer)) (pbuf (process-buffer process)) msg)
    (set-buffer pbuf)
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (while (looking-at ".*\n")
      (goto-char (match-end 0))
      (setq msg (buffer-substring (point-min) (1- (point))))
      (delete-region (point-min) (point))
      (set-buffer obuf)
      (irchat-dcc-chat-loop-filter-sub process msg)
      (set-buffer pbuf))
    (set-buffer obuf)))

(defun irchat-dcc-chat-loop-filter-sub (process msg)
  (let* ((dcc-object (irchat-dcc-get-process-object process))
	 (state (nth 1 dcc-object))
	 (nick (nth 4 dcc-object)))
    (if (null dcc-object)
	(delete-process process)   ;; garbage process
      (irchat-handle-dcc-chat nick irchat-nickname msg))))

(defun irchat-Command-dcc-list ()
  "show DCC list"
  (interactive)
  (if (null irchat-dcc-list)
      (irchat-insert "*** There is no DCC list.\n")
    (irchat-insert irchat-dcc-list-header)
    (let ((dcc-list irchat-dcc-list) (num 0) dcc-object)
      (while dcc-list
	(setq dcc-object (car dcc-list)
	      dcc-list (cdr dcc-list))
	(setq num (+ num 1))
	(irchat-insert (irchat-dcc-list-object num dcc-object))))))

(setq irchat-dcc-list-header
      "*** DCC Received Time Type Status  Nick      filename/chat\n")
     ;"01: [Mar 18 12:34:56] SEND Waiting abcdefghi /hoge/hoge (123 bytes)"

(defun irchat-dcc-list-object (num dcc-object)
  (cond
   ((or (eq (car dcc-object) 'SEND) (eq (car dcc-object) 'GET))
    (format "%02d: [%s] %4s %7s %9s %s (%s bytes)\n"
	    (if num num (irchat-dcc-match-object dcc-object))
	    (substring (nth 3 dcc-object) 4 19) ; time
	    (add-space 4 (prin1-to-string (nth 0 dcc-object))) ; type
	    (add-space 7 (prin1-to-string (nth 1 dcc-object))) ; status
	    (add-space 9 (nth 4 dcc-object)) ; nick
	    (nth 7 dcc-object) ; file
	    (nth 8 dcc-object))) ; size
   ((eq (car dcc-object) 'CHAT)
    (format "%02d: [%s] %4s %7s %9s <chat>\n"
	    (if num num (irchat-dcc-match-object dcc-object))
	    (substring (nth 3 dcc-object) 4 19) ; time
	    (add-space 4 (prin1-to-string (nth 0 dcc-object))) ; type
	    (add-space 7 (prin1-to-string (nth 1 dcc-object))) ; status
	    (add-space 9 (nth 4 dcc-object)))) ; nick
   (t
    "00: Unknown\n")))

(defun irchat-dcc-add-object (dcc-object)
  (setq irchat-dcc-list (append irchat-dcc-list (list dcc-object))))

(defun irchat-dcc-delete-object (dcc-object)
  (let ((num (irchat-dcc-match-object dcc-object)))
    (if num
	(let ((nth (1- num)))
	  (if (= 0 nth)
	      (setq irchat-dcc-list (cdr irchat-dcc-list))
	    (setcdr (nthcdr (1- nth) irchat-dcc-list)
		    (nthcdr (1+ nth) irchat-dcc-list)))))))

(defun irchat-dcc-get-object (num)
  "return object"
  (nth (1- num) irchat-dcc-list))

(defun irchat-dcc-get-offered-object ()
  "return the first offered object"
  (let ((dcc-list irchat-dcc-list) (num 0) dcc-object found)
    (while (and dcc-list (not found))
      (setq dcc-object (car dcc-list)
	    dcc-list (cdr dcc-list))
      (setq num (1+ num))
      (if (eq (nth 1 dcc-object) 'Offered)
	  (setq found dcc-object)))
    found))

(defun irchat-dcc-get-process-object (process)
  "return object"
  (let ((dcc-list irchat-dcc-list) (num 0) dcc-object found)
    (while (and dcc-list (not found))
      (setq dcc-object (car dcc-list)
	    dcc-list (cdr dcc-list))
      (setq num (+ num 1))
      (if (eq (nth 2 dcc-object) process)
	  (setq found dcc-object)))
    found))

(defun irchat-dcc-match-object (dcc-object)
  "what number?"
  (let ((dcc-list irchat-dcc-list) (num 0) obj found)
    (while (and dcc-list (not found))
      (setq obj (car dcc-list)
	    dcc-list (cdr dcc-list))
      (setq num (1+ num))
      (if (eq dcc-object obj)
	  (setq found num)))
    found))

(provide 'irchat-dcc)
