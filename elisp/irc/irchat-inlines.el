;;; see file irchat-copyright.el for change log and copyright info

(defsubst irchat-upper0 (str)
  (if (string-match "[{}\|]" str)
      (let (char (ret ""))
	(while (string-match "^\\([^{}\|]*\\)\\([{}\|]\\)\\(.*\\)" str)
	  (setq char (string-to-char (substring str (match-beginning 2)
						(match-end 2))))
	  (setq ret (concat ret
			    (upcase (substring str (match-beginning 1)
					       (match-end 1)))
			    (char-to-string (cond ((eq char ?{) ?\[)
						  ((eq char ?|) ?\\)
						  ((eq char ?}) ?\])))))
	  (setq str (substring str (match-beginning 3) (match-end 3))))
	(concat ret (upcase str)))
    (upcase str)))

(defsubst irchat-upper (str)
  (or (get (intern str) 'upper)
      (let ((up (irchat-upper0 str)))
	(put (intern str) 'upper up)
	up)))

(defsubst irchat-equal (str1 str2)
  (string= (irchat-upper str1) (irchat-upper str2)))

(defsubst irchat-member (str list)
  (let ((item (car list))
	(ustr (irchat-upper str)))
    (while (and item (not (string= ustr (irchat-upper item))))
      (setq item (car list)
	    list (cdr list)))
    item))

(defsubst irchat-delete (str list)
  (let ((ustr (irchat-upper str))
	(item (car list))
	(result nil))
    (while item
      (if (not (string= (irchat-upper item) ustr))
	  (setq result (nconc result (list item))))
      (setq list (cdr list)
	    item (car list)))
    result))

(defsubst list-to-assoclist (list)
  (let ((result nil) (item (car list)))
    (while item
      (setq result (cons (list item) result)
	    list (cdr list)
	    item (car list)))
    result))

(defsubst matching-substring (string arg)
  (substring string (match-beginning arg) (match-end arg)))

(defsubst nth1 (nn str)
  (nth (- nn 1) str))

(defsubst nth1cdr (nn str)
  (nthcdr (- nn 1) str))

(defsubst add-space (length string)
  (if (< (length string) length)
      (format "%s%s" string (make-string (- length (length string)) 32))
    string))

(defsubst irchat-next-line (&optional n)
  (if (= (point) (point-max))
      (newline))
  (forward-line n))

(defsubst irchat-match-me (str)
  (or (irchat-equal str irchat-nickname)
      (irchat-equal str irchat-my-user-server)
      (irchat-equal str irchat-my-user-host-server)))

(defsubst irchat-match-my-nick (str)
  (irchat-equal str irchat-nickname))

(defsubst irchat-get-lastchan (nick)
  (get (intern nick) 'lastchan))

(defsubst irchat-put-lastchan (nick chan)
  (put (intern nick) 'lastchan chan))

(defsubst irchat-get-lasttime (nick)
  (get (intern nick) 'lasttime))

(defsubst irchat-put-lasttime (nick time)
  (put (intern nick) 'lasttime time))

(defsubst irchat-get-userhost (nick)
  (get (intern nick) 'userhost))

(defsubst irchat-put-userhost (nick userhost)
  (put (intern nick) 'userhost userhost))

(defsubst irchat-get-newuserhost (nick)
  (get (intern nick) 'newuserhost))

(defsubst irchat-put-newuserhost (nick new)
  (put (intern nick) 'newuserhost new))

(defsubst irchat-get-chans (nick)
  (get (intern nick) 'chans))

(defsubst irchat-put-chans (nick new)
  (put (intern nick) 'chans new))

(provide 'irchat-inlines)
