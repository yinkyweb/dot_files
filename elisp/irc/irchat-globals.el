;;; see file irchat-copyright.el for change log and copyright info

(defvar irchat-debug-buffer)
(defvar irchat-Command-buffer)
(defvar irchat-Dialogue-buffer)
(defvar irchat-Channel-buffer)
(defvar irchat-WALLOPS-buffer)
(defvar irchat-IGNORED-buffer)
(defvar irchat-KILLS-buffer)

(defvar irchat-server-name nil)
(defvar irchat-my-user "")
(defvar irchat-my-host "")
(defvar irchat-my-server "")
(defvar irchat-my-user-host "")
(defvar irchat-my-user-server "")
(defvar irchat-my-user-host-server "")
(defvar irchat-my-server-version "")
(defvar irchat-my-server-mode-user "")
(defvar irchat-my-server-mode-channel "")

(defvar irchat-current-target "")
(defvar irchat-chanbuf-indicator "")
(defvar irchat-current-channels nil)
(defvar irchat-partners nil)

(defvar irchat-ignore-changes)

(defvar irchat-links-reply-count 0
  "*the number of LINKS reply.")

(defvar irchat-ban-reply-count 0
  "*the number of BAN reply.")

(defvar irchat-no-configure-windows t
  "*not yet configure windows")

(defvar irchat-fatal-error-message nil
  "*ERROR message")

(defvar irchat-server-process)
(defvar irchat-handling)

(defvar irchat-freeze)
(defvar irchat-ignore-nickname)
(defvar irchat-invited-channel)

(defvar irchat-away-indicator)
(defvar irchat-nick-alist)
(defvar irchat-trying-nickname)
(defvar irchat-previous-pattern nil )

(defvar irchat-privmsg-partner nil
  "The person who got your last private message.")

(defvar irchat-current-channels nil
  "The channels you have currently joined.")

(defvar irchat-chanbuf-num 0
  "The channel buffer you currently have selected.")

(defvar irchat-chanbuf-list nil
  "The channel buffers list you have currently joined.")

(defvar irchat-chanbuf-indicator "Private"
  "The current channel buffer, \"pretty-printed.\"")

(defvar irchat-chanbufs-indicator "No channel"
  "The channel buffers list, \"pretty-printed.\"")

(defvar irchat-away-indicator)
(defvar irchat-freeze-indicator)

(defvar irchat-trying-nickname nil
  "the nickname that I'm trying to be.")

(defvar irchat-old-window-configuration nil
  "the window configuration before starting irchat.")

(defvar irchat-after-registration nil
  "after my registration")

(defvar irchat-debugging nil
  "for debugging only")

(defvar irchat-chan-target nil)

(defvar irchat-nick-alist nil
  "An alist containing the nicknames of users known to currently be on IRC.
Each element in the list is a list containing a nickname.")

(defvar irchat-chan-alist nil 
  "An alist containing the channels on IRC.  Each element in the list is 
a list containing a channel name.")

(defvar irchat-time-string)
(defvar irchat-time)
(defvar irchat-day)
(defvar irchat-hour)
(defvar irchat-minute)
(defvar irchat-second)

(defvar irchat-ctcp-lastcommand nil)
(defvar irchat-ctcp-target nil)

(provide 'irchat-globals)
