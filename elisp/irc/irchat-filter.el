;;; see file irchat-copyright.el for change log and copyright info

(defun irchat-filter (process output)
  "Filter function for IRC server process."
  (let ((obuf (current-buffer))
	(data (match-data))
	line)
    ;;
    ;; C-c D creates debug buffer for incoming messages...
    ;;
    (if (and irchat-debug-buffer (get-buffer irchat-debug-buffer))
	(progn
	  (set-buffer irchat-debug-buffer)
	  (let* ((dbgwin (get-buffer-window irchat-debug-buffer))
		 (wp (if dbgwin (window-point dbgwin) nil))
		 (pm (point-max)))
	    (save-excursion
	      (goto-char (point-max))
	      (insert output))
	    (if (and wp (>= wp pm))
		(irchat-scroll-if-visible dbgwin)))))
    ;;
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (setq irchat-handling t)
    (while (looking-at ".*\n")
      (goto-char (match-end 0))
      (setq line (buffer-substring (point-min) (1- (point))))
      (delete-region (point-min) (point))
      (irchat-handle line))
    (setq irchat-handling nil)
    (set-buffer obuf)
    (store-match-data data)))

(defun irchat-sentinel (proc status)
  "Sentinel function for IRC server process."
  (if (and irchat-server-process
	   (not (irchat-server-opened)))
      (if (not (or irchat-reconnect-automagic irchat-reconnect-with-password))
	  (if irchat-fatal-error-message
	      (error (format "IRC ERROR: %s" irchat-fatal-error-message))
	    (if (process-id proc)
		(irchat-sentinel2 proc status)
	      (error (format "IRC ERROR: Connection closed. (%s)"
			     (substring status 0 (1- (length status)))))))
	(if (and irchat-grow-tail (not irchat-reconnect-with-password))
	    (progn
	      (setq irchat-nickname (concat irchat-nickname irchat-grow-tail))
	      (irchat 'always))
	  (irchat)))))

(defun irchat-sentinel2 (proc status)
  (if (not (string-match "^exited abnormally with code \\([0-9]+\\)" status))
      (error (format "IRC ERROR: Connection closed. (%s)"
		     (substring status 0 (1- (length status)))))
    (let ((status (string-to-int (matching-substring status 1))))
      (cond
       ((= 99 status) ;; unsupported command
	(error (format "IRC ERROR: Please use a newer \"%s\"."
		       irchat-dcc-program)))
       ((= 98 status) ;; bad argment number
	(error (format "IRC ERROR: Please use a newer \"%s\"."
		       irchat-dcc-program)))
       ((= 97 status)
	(error (format "IRC ERROR: Cannot connect to IRC server.")))
       (t
	(error (format "IRC ERROR: Server connection closed.")))))))

(defun irchat-handle (line)
  "Called when we have at least one line of output from the IRC server."
  (let ((obuf (current-buffer))
	prefix userhost (cmd "") arg)
    ;;
    (let ((old-minute irchat-minute)
	  (old-hour irchat-hour)
	  fun)
      (irchat-update-time)
      (if (/= irchat-minute old-minute)
	  (progn
	    (if (< irchat-minute old-minute)
		(progn
		  (if (< irchat-hour old-hour)
		      (progn
			(irchat-insert-allchan 
			 (concat (substring irchat-time-string 0 11)
				 "00:00:00"
				 (substring irchat-time-string 19 24)))
			(if (fboundp (setq fun (intern "irchat-daily")))
			    (apply fun nil))))
		  (if (fboundp (setq fun (intern "irchat-hourly")))
		      (apply fun nil))))
	    (if (fboundp (setq fun (intern "irchat-minutely")))
		(apply fun nil)))))
    ;;
    (while (string-match "^\\([^\r]*\\)\r+\\(.*\\)" line)
      (setq line (concat (matching-substring line 1)
			 (matching-substring line 2))))
    (if (string-match "^:\\([^ ]+\\) *\\(.*\\)" line)
	(setq prefix (matching-substring line 1)
	      line (matching-substring line 2)))
    (if (and prefix (string-match "^\\([^!]*\\)!\\(.*\\)" prefix))
	(setq userhost (matching-substring prefix 2)
	      prefix (matching-substring prefix 1)))
    (if (string-match "^ *\\([^ ]+\\) *\\(.*\\)" line)
	(setq arg (list (matching-substring line 1))
	      line (matching-substring line 2)))
    (while (string-match "^\\([^:][^ ]*\\) +\\(.*\\)" line)
      (nconc arg (list (matching-substring line 1)))
      (setq line (matching-substring line 2)))
    (if (not (string= line ""))
	(if (string-match "^:\\(.*\\)" line)
	    (nconc arg (list (matching-substring line 1)))
	  (nconc arg (list line))))
    (irchat-update-userhost prefix userhost)
    (if (car arg)
	(setq cmd (downcase (car arg))
	      arg (cdr arg)))
    (if (and (boundp (setq hook
			   (intern (concat "irchat-handle-" cmd "-hook"))))
	     (eval hook)
	     (eval (cons (eval hook) (cons prefix arg))))
	nil
      (setq fun (intern (concat "irchat-handle-" cmd)))
      (if (fboundp fun)
	  (apply fun prefix arg)
	(if irchat-debugging
	    (irchat-insert-special (format "No handle-%s (%s) %s\n"
					   cmd prefix
					   (prin1-to-string arg))))
	(let* ((cmd-number (string-to-int cmd))
	       (default-number (/ cmd-number 100)) tmp)
	  (setq fun (intern (concat "irchat-handle-"
				    (format "%d00s" default-number))))
	  (if (and (> cmd-number 0)
		   (fboundp fun))
	      (apply fun cmd-number prefix arg)
	    (setq tmp (format "Unknown MESSAGE: [%s] [%s] [%s]\n" 
				    prefix cmd (prin1-to-string arg)))
	    (irchat-insert tmp)))))
    (if (and (boundp (setq hook
			   (intern (concat "irchat-handle-" cmd "-hook2"))))
	     (eval hook))
	(eval (cons (eval hook) (cons prefix arg))))
    (set-buffer obuf)))

(provide 'irchat-filter)
