;;;
;;; iso-2022-jp
;;;
(defun irchat-decode-iso-2022-jp-type1 (str)
  (decode-coding-string str *iso-2022-jp*))
(defun irchat-encode-iso-2022-jp-type1 (str)
  (encode-coding-string str *iso-2022-jp*))

(defun irchat-decode-iso-2022-jp-type2 (str)
  (decode-coding-string str 'iso-2022-jp))
(defun irchat-encode-iso-2022-jp-type2 (str)
  (encode-coding-string str 'iso-2022-jp))

(defun irchat-decode-iso-2022-jp-type3 (str)
  (code-convert-string str *junet* *internal*))
(defun irchat-encode-iso-2022-jp-type3 (str)
  (code-convert-string str *internal* *junet*))

(defun irchat-decode-iso-2022-jp-type4 (str)
  (convert-string-kanji-code str 2 3))
(defun irchat-encode-iso-2022-jp-type4 (str)
  (let ((orig-to-ascii-process to-ascii-process)
	(orig-to-kanji-process to-kanji-process))
    (setq to-ascii-process ?B)
    (setq to-kanji-process ?B)
    (setq str (convert-string-kanji-code str 3 2))
    (setq to-ascii-process orig-to-ascii-process)
    (setq to-kanji-process orig-to-kanji-process)
    str))

(if (fboundp 'decode-coding-string)
    (if (boundp '*iso-2022-jp*)
	(fset 'irchat-decode-iso-2022-jp 'irchat-decode-iso-2022-jp-type1)
      (fset 'irchat-decode-iso-2022-jp 'irchat-decode-iso-2022-jp-type2))
  (if (fboundp 'code-convert-string)
      (fset 'irchat-decode-iso-2022-jp 'irchat-decode-iso-2022-jp-type3)
    (if (fboundp 'convert-string-kanji-code)
	(fset 'irchat-decode-iso-2022-jp 'irchat-decode-iso-2022-jp-type4)
      (fset 'irchat-decode-iso-2022-jp 'irchat-decode-noconv))))
  
(if (fboundp 'encode-coding-string)
    (if (boundp '*iso-2022-jp*)
	(fset 'irchat-encode-iso-2022-jp 'irchat-encode-iso-2022-jp-type1)
      (fset 'irchat-encode-iso-2022-jp 'irchat-encode-iso-2022-jp-type2))
  (if (fboundp 'code-convert-string)
      (fset 'irchat-encode-iso-2022-jp 'irchat-encode-iso-2022-jp-type3)
    (if (fboundp 'convert-string-kanji-code)
	(fset 'irchat-encode-iso-2022-jp 'irchat-encode-iso-2022-jp-type4)
      (fset 'irchat-encode-iso-2022-jp 'irchat-encode-noconv))))

(defun irchat-decode-message-iso-2022-jp (str)
  (if str
      (or (irchat-decode str)
	  (irchat-decode (concat str "\033(B"))
	  str)
    nil))

(defun irchat-encode-message-iso-2022-jp (str)
  (if str
      (or (irchat-encode str)
	  str)
    nil))

(provide 'irchat-iso-2022-jp)
