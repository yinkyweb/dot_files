;; -*- coding: utf-8; mode: Emacs-Lisp -*-
;;
;; gca.el - Gauche code assist
;; 
;;   Copyright (c) 2006 KOGURO, Naoki (naoki@koguro.net)
;;   All rights reserved.
;;
;; [What is it?]
;;  Gaucheのコードを書くときのためのユーティリティ集です。
;;  現在のところ以下の機能を持っています。
;;   - カーソルのあるS式の先頭にあるシンボルのドキュメント(info)を自動的に検索する
;;   - カーソルのある位置のシンボルのドキュメント(info)を検索する
;;   - シンボルを補完する(infoのインデックスに載っているものが対象です)
;;   - (use module) をソースコードの先頭の方にインサートする
;;     なお、run-schemeでSchemeを起動している場合、そちらでも(use module)が
;;     自動的に実行されます。
;;   - stub作成時のクラス定義(define-cclass, boxer, unboxerなど)に必要なひな形を
;;     生成する。
;;   - run-schemeでの実行結果をもとにテストケースを作成する (tgosh.scmを使う必要があります)
;;
;; [Installation]
;;  本elisp(gca.el)をload-pathの通った位置にコピーして、以下の式を~/.emacsに追加
;;  してください。(キーは好みに応じて変更してください)
;;
;;   (require 'gca)
;;   ;; C-c C-uで(use module)をインサートする
;;   (define-key scheme-mode-map "\C-c\C-u" 'gca-insert-use) 
;;   (let ((m (make-sparse-keymap)))
;;     ;; C-c C-d h でドキュメントを検索する
;;     (define-key m "h" 'gca-show-info)
;;     ;; C-c C-d i でauto-info-modeの切り替えを行う。(ONの場合自動的にinfoを表示します)
;;     (define-key m "i" 'auto-info-mode)
;;     (define-key scheme-mode-map "\C-c\C-d" m))
;;   ;; C-c C-,でinfoの次のトピックを表示します(検索結果が複数あった場合)
;;   (define-key scheme-mode-map [(control c) (control ,)] 'gca-info-next)
;;   ;; C-. でシンボルを補完する
;;   (define-key scheme-mode-map [(control .)] 'gca-completion-current-word)
;;   ;; C-c C-. でコードのひな形をインサートする
;;   (define-key scheme-mode-map [(control c) (control .)] 'gca-insert-template)
;;   (define-key c-mode-map [(control c) (control .)] 'gca-insert-template)
;;   ;; C-c t でテストケースをインサートする (run-schemeでtgosh.scmを使う必要があります)
;;   ;; C-u で引数を与えると、その番号で実行された結果をもとにしてテストケースを作成します。
;;   ;; 省略時は直前の実行結果が使われます。
;;   (define-key scheme-mode-map "\C-ct" 'gca-make-test)
;;   ;; C-c h で履歴をみる (run-schemeでtgosh.scmを使う必要があります)
;;   (define-key scheme-mode-map "\C-ch" 'gca-show-history)
;;
;;   単体テストケース作成の支援(gca-make-test, gca-show-history)を使うには
;;   run-schemeで付属のtgosh.scmが起動するようにしてください。
;;   (setq scheme-program-name "gosh /path/to/tgosh.scm")
;;
;;   なお、gca本体の設定はcustomizeから行うことができます。
;;
;; [Templates]
;;  コードのひな形のインサート(C-c C-.)で指定できるテンプレート名は以下の通りです
;;  - [c-header] stubで使用するCのソースコードのヘッダを作成する。Cのタイプ名が
;;    尋ねられるので、それを入力するとクラス定義、boxer, unboxerマクロがインサート
;;    されます。
;;    なお、Cのタイプ名はtypdefされている場合は"Foo"のように、structの場合は
;;    "struct Foo"のように入力してください(以下同様)。
;;  - [c-boxer] Cのタイプ名を入力すると、finalizerとboxerのコードがインサートされます。
;;  - [define-type] Cのタイプ名を入力すると、stubの (define-type ...) 
;;    のコードがインサートされます。
;;  - [define-cclass] Cのタイプ名を入力すると、stubの (define-cclass ...) 
;;    のコードがインサートされます。
;;  - [define-module] モジュール名を入力すると、(define-module ...) (provide ...)
;;    のコードがインサートされます。
;;
;; [Tips]
;;  - gca-info-listにある "gauche-refe.info.gz" の代わりに以下のS式を設定すると
;;    日本語のドキュメントが参照できます。
;;    なお、他のinfoファイル(gauche-gl-refj.info.gzなど)ではインデックスの
;;    ノード名が異なるので適宜書き換えてください
;;       ("/usr/local/info/gauche-refj.info.gz" 
;;         ("Index - 手続きと構文索引" 
;;          "Index - モジュール索引" 
;;          "Index - 変数索引") 
;;         ("Index - クラス索引"))
;;
;;   $Id: gca.el 337 2006-09-03 05:13:49Z naoki $
;;

(require 'cmuscheme)
(require 'cc-mode)
(require 'info)

(defgroup gca nil "Gauche Code Assist")

(defcustom gca-info-list '("gauche-refe.info.gz")
  "ドキュメント検索やシンボル補完で使用するinfoファイル

リストの各要素はinfoファイルのファイル名を表す文字列もしくは(ファイル名 (関数などのインデックス ...) (クラスのインデックス ...))を指定してください。クラスのインデックスに含まれるトピックは\"<\"\">\"で囲まれた形で補完を行います(例えば、クラスインデックスに\"vector\"とあった場合は、\"<vector>\"と補完できます)。
なお、ファイル名のみを指定した場合、内部では(ファイル名 (\"Function and Syntax Index\" \"Module Index\" \"Variable Index\") (\"Class Index\"))と指定したものとして扱われます。
"
  :type '(repeat sexp)
  :set (lambda (symbol value)
         (setq gca-info-complete-alist-cache nil)
         (set symbol value))
  :group 'gca)

(defcustom gca-idle-delay 0.5
  "auto-info-modeの時にアイドル状態になってからドキュメント検索を開始するまでの秒数"
  :type 'number
  :group 'gca)

(defcustom gca-module-db-filename "~/.emacs.d/module-db"
  "モジュールとシンボルの対応関係をキャッシュしておくためのファイルの名前"
  :type 'file
  :set (lambda (symbol value)
         (setq gca-module-db nil)
         (set symbol value))
  :group 'gca)

(defcustom gca-gosh "gosh"
  "goshのプログラム名。パスが通っていない場合はフルパスで設定してください。"
  :type 'file
  :group 'gca)

(defcustom gca-info-window-height-ratio 0.3
  "infoを表示するときの画面の高さの全体に対する比率"
  :type 'number
  :group 'gca)

(defcustom gca-auto-info-exclude-topics
  '("define" "lambda" "let" "list" "cons" "car" "cdr")
  "auto-info-modeでの検索を行わないシンボル名のリスト"
  :type '(repeat string)
  :group 'gca)

;; Show info
(defvar gca-info-complete-alist-cache nil)
(defvar gca-info-topic-dict nil)

(defvar gca-timer nil)
(defvar gca-current-idle-delay gca-idle-delay)
(defvar gca-last-word nil)

(define-minor-mode auto-info-mode
  "Toggle AutoInfo mode turn on or off.

In AutoInfo mode, an *info* buffer shows information about a function in the current sexp."
  :group 'auto-info :lighter " AutoInfo"
  (if auto-info-mode
      (progn
        (or (get-buffer-window-and-frame "*info*")
            (let ((curwin (selected-window)))
              (select-window (split-window-vertically
                              (floor (* (window-height)
                                        (- 1 gca-info-window-height-ratio)))))
              (info)
              (setq gca-last-word nil)
              (select-window curwin)))
        (add-hook 'post-command-hook 'gca-schedule-timer nil t))
    (remove-hook 'post-command-hook 'gca-schedule-timer)))

(defun gca-schedule-timer ()
  (or (and gca-timer
           (memq gca-timer timer-idle-list))
      (setq gca-timer (run-with-idle-timer gca-idle-delay t
                                               'gca-auto-info)))
  (cond ((not (= gca-idle-delay gca-current-idle-delay))
         (setq gca-current-idle-delay gca-idle-delay)
         (timer-set-idle-time gca-timer gca-idle-delay t))))

(defun selected-window-and-frame ()
  (list (selected-window) (selected-frame)))

(defun select-window-and-frame (window frame)
  (select-frame frame)
  (select-window window))

(defun get-buffer-window-and-frame (buffer)
  (let ((frames (visible-frame-list))
        (result nil))
    (while frames
      (if (get-buffer-window buffer (car frames))
          (prog1
              (setq result
                    (list (get-buffer-window buffer (car frames)) (car frames)))
            (setq frames ()))
        (setq frames (cdr frames))))
    result))

(defun gca-auto-info ()
  (if (and auto-info-mode
           (eq major-mode 'scheme-mode)
           (get-buffer-window-and-frame "*info*"))
      (let ((word (gca-fnname-in-current-sexp)))
        (when (and (not (member word gca-auto-info-exclude-topics))
                   (not (string= gca-last-word word)))
          (gca-info-topic word)
          (setq gca-last-word word)))))

(defun gca-info-window ()
  (or (get-buffer-window "*info*")
      (select-window (split-window-vertically
                              (floor (* (window-height)
                                        (- 1 gca-info-window-height-ratio)))))))

(defun gca-index-nodes (filename)
  (let ((lst gca-info-list)
        (result nil))
    (while lst
      (let ((elt (car lst)))
        (cond
         ((and (stringp elt)
               (string= elt filename))
          (setq result '("Function and Syntax Index"
                         "Module Index"
                         "Variable Index"
                         "Class Index")
                lst nil))
         ((and (listp elt)
               (string= (car elt) filename))
          (setq result (append (nth 1 elt) (nth 2 elt))
                lst nil))
         (t
          (setq lst (cdr lst))))))
    result))
          
(defun gca-info-topic (topic)
  (let ((filename (gethash topic (gca-info-topic-dict))))
    (if filename
        (let* ((curwinframe (selected-window-and-frame))
               (infowinframe (or (get-buffer-window-and-frame "*info*")
                                 (list
                                  (select-window (split-window-vertically
                                                  (* (/ (window-height) 3) 2)))
                                  (selected-frame))))
               (Info-index-nodes nil))
          (unwind-protect
              (progn
                (apply #'select-window-and-frame infowinframe)
                (info filename)
                (setq Info-index-nodes (list (cons Info-current-file
                                                   (gca-index-nodes filename))))
                (Info-index (if (string-match "^<.*>$" topic)
                                (substring topic 1 -1)
                              topic)))
            (apply #'select-window-and-frame curwinframe))
          t)
      nil)))

(defun gca-show-info (topic)
  "Show info."
  (interactive (list (completing-read "Index topic: "
                                      (gca-info-complete-alist)
                                      nil
                                      nil
                                      (gca-current-word))))
  (or (gca-info-topic topic)
      (error "No topic: %s" topic)))

(defun gca-info-next ()
  "Show next topic."
  (interactive)
  (let ((curwinframe (selected-window-and-frame))
        (infowinframe (get-buffer-window-and-frame "*info*")))
    (when infowinframe
      (unwind-protect
          (progn
            (apply #'select-window-and-frame infowinframe)
            (Info-index-next 1))
        (apply #'select-window-and-frame curwinframe)))))

(defun gca-info-topic-dict ()
  (or gca-info-complete-alist-cache
      (gca-info-complete-alist))
  gca-info-topic-dict)

(defun gca-info-complete-alist ()
  (or gca-info-complete-alist-cache
      (progn
        (setq gca-info-topic-dict (make-hash-table :test 'equal)
              gca-info-complete-alist-cache
              (let ((result ()))
                (mapcar #'(lambda (elt)
                            (setq result
                                  (apply #'gca-append-info-complete-alist
                                         result
                                         (if (stringp elt)
                                             (list elt
                                                   '("Function and Syntax Index"
                                                     "Module Index"
                                                     "Variable Index")
                                                   '("Class Index"))
                                           elt))))
                        gca-info-list)
                (sort result #'(lambda (v1 v2)
                                 (string< (car v1) (car v2))))))
        gca-info-complete-alist-cache)))

(defun gca-append-info-complete-alist (lst filename fnvar-idx class-idx)
  (let ((buf (get-buffer-create " *gauche-info-complete-menu*")))
    (unwind-protect
        (with-current-buffer buf
          (Info-mode)
          (let ((pattern (concat "\n\\* +\\(\\(?:[^:]\\|:[^:,.;() \t\n]\\)*\\):[ \t]*\\(\\(([^)]+)\\)?\\([^.,:]*[^.,: ]\\|\\)\\)[,:.]")))
            (mapc (lambda (node)
                    (Info-find-node filename node)
                    (goto-char (point-min))
                    (search-forward "\n* Menu:")
                    (while (re-search-forward pattern nil t)
                      (let ((topic (match-string-no-properties 1)))
                        (unless (or (string-match "\\s " topic)
                                    (eq (length topic) 0))
                          (push (cons topic filename) lst)
                          (puthash topic filename gca-info-topic-dict)))))
                  fnvar-idx)
            (mapc (lambda (node)
                    (Info-find-node filename node)
                    (goto-char (point-min))
                    (search-forward "\n* Menu:")
                    (while (re-search-forward pattern nil t)
                      (let ((topic (match-string-no-properties 1)))
                        (unless (or (string-match "\\s " topic)
                                    (eq (length topic) 0))
                          (push (cons (concat "<" topic ">") filename) lst)
                          (puthash (concat "<" topic ">")
                                   filename gca-info-topic-dict)))))
                  class-idx)
            lst))
      (kill-buffer buf))))

(defun gca-current-word ()
  (let ((p (point))
        (parse-sexp-ignore-comments t))
    (unwind-protect
        (progn
          (gca-backward-sexp)
          (if (= (or (char-after (1- (point))) 0) ?\")
              nil
            (let ((c (char-after (point))))
              (and c
                   (memq (char-syntax c) '(?w ?_))
                   (current-word)))))
      (goto-char p))))

;; Completion current word
(defvar gca-save-window-conf nil)
(defvar gca-completion-current-word nil)

(defun gca-completion-current-word ()
  "Complete current word with info index."
  (interactive)
  (let (word (p (point)))
    (gca-backward-sexp)
    (setq word (buffer-substring (point) p))
    (if (and (string= gca-completion-current-word word)
             (eq last-command 'gca-completion-current-word))
        (gca-completion-scroll-up)
      (let ((result (try-completion word (gca-info-complete-alist))))
        (setq gca-completion-current-word nil)
        (cond ((eq result t)
               (goto-char p)
               (message "Sole completion"))
              ((eq result nil)
               (goto-char p)
               (message "No match in completion list, so expand previous word")
               (call-interactively 'dabbrev-expand))
              ((string= result word)
               (goto-char p)
               (gca-completion-popup-window
                result (all-completions word (gca-info-complete-alist))))
              (t (delete-region (point) p)
                 (insert result)
                 (let ((lst (all-completions result
                                             (gca-info-complete-alist))))
                   (cond ((= (length lst) 1)
                          (gca-completion-restore)
                          (message "Sole completion"))
                         ((member result lst)
                          (gca-completion-popup-window result lst)
                          (message "Sole completion, but not unique"))
                         (t (gca-completion-popup-window result lst)
                          (message "Not unique"))))))))))

(defun gca-backward-sexp ()
  (condition-case err
      (forward-sexp -1)
    (error nil)))

(defun gca-forward-sexp ()
  (condition-case err
      (forward-sexp 1)
    (error nil)))

(defun gca-completion-pre-command ()
  (unless (or (eq this-command 'gca-completion-current-word)
              (and gca-save-window-conf
                   (eq this-command 'self-insert-command)
                   (string-match "^[!$-&*-:<-_a-~]$" (this-command-keys))))
    (gca-completion-restore)))

(defun gca-completion-post-command ()
  (cond ((eq this-command 'gca-completion-current-word) nil)
        ((and gca-save-window-conf
              (eq this-command 'self-insert-command)
              (string-match "^[!$-&*-:<-_a-~]$" (this-command-keys)))
         (let ((p (point)))
           (unwind-protect
               (progn
                 (gca-backward-sexp)
                 (let ((lst (all-completions (buffer-substring (point) p)
                                             (gca-info-complete-alist))))
                   (if lst
                       (with-output-to-temp-buffer "*Completions*"
                         (display-completion-list lst))
                     (progn
                       (gca-completion-restore)
                       (beep)
                       (message "No match")))))
             (goto-char p))))))

(defun gca-completion-popup-window (word comp-list)
  (unless gca-save-window-conf
    (setq gca-save-window-conf (current-window-configuration)))
  (setq gca-completion-current-word word)
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list comp-list))
  (add-hook 'pre-command-hook 'gca-completion-pre-command)
  (add-hook 'post-command-hook 'gca-completion-post-command))

(defun gca-completion-restore ()
  (if gca-save-window-conf
      (progn
        (set-window-configuration gca-save-window-conf)
        (setq gca-save-window-conf nil)))
  (setq gca-completion-current-word nil)
  (remove-hook 'pre-command-hook 'gca-completion-pre-command)
  (remove-hook 'post-command-hook 'gca-completion-post-command))

(defun gca-completion-scroll-up ()
  (let ((cur (selected-window))
        (win (get-buffer-window (get-buffer "*Completions*"))))
    (when win
      (unwind-protect
          (progn
            (select-window win)
            (condition-case err
                (scroll-up)
              (error (goto-char (point-min)))))
        (select-window cur)
        (goto-char p)))))

;; Insert use
(defun gca-fnname-in-current-sexp ()
  (let ((p (point))
        (parse-sexp-ignore-comments t))
    (unwind-protect
        (progn
          (condition-case err
              (while (progn
                       (forward-sexp -1)
                       (or (= (char-before) ?\")
                           (> (point) (point-min)))))
            (error nil))
          (if (= (or (char-after (1- (point))) 0) ?\")
              nil
            (let ((c (char-after (point))))
              (and c
                   (memq (char-syntax c) '(?w ?_))
                   (current-word)))))
      (goto-char p))))

(defun gca-module-alist ()
  (unless gca-module-alist
    (gca-make-module-db))
  gca-module-alist)

(defun gca-module-db ()
  (unless gca-module-db
    (gca-make-module-db))
  gca-module-db)

(defun gca-insert-use (pkg)
  "Insert use."
  (interactive (list
                (let* ((fnname (gca-fnname-in-current-sexp))
                       (modules (if fnname
                                    (mapcar #'symbol-name
                                            (gethash (intern fnname)
                                                     (gca-module-db)
                                                     nil))
                                  nil))
                       (module 
                        (completing-read 
                         (format "Use module %s: "
                                 (if modules
                                     (concat 
                                      "(" (mapconcat (lambda (x) x)
                                                     (cons (concat 
                                                            "[" (car modules) "]")
                                                           (cdr modules))
                                                     ", ") ")")
                                          "")) 
                         (gca-module-alist) nil nil nil)))
                  (if (string= module "")
                      (or (car modules)
                          nil)
                    module))))
  (if pkg
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat "(\\s *use\\s +" pkg "\\s *)") nil t)
            (message "(use %s) is already exists." pkg)
          (progn
            (goto-char (point-min))
            (cond ((re-search-forward "^\\s *(\\s *use\\s \\w.*)" nil t)
                   (goto-char (match-end 0))
                   (while (re-search-forward "^\\s *(\\s *use\\s \\w.*)" nil t)
                     (goto-char (match-end 0)))
                   (next-line 1))
                  ((re-search-forward "^\\s *(\\s *define-module\\s +\\w+" nil t)
                   (goto-char (match-end 0))
                   (while (re-search-forward "^\\s *(\\s *use\\s \\w.*)" nil t)
                     (goto-char (match-end 0)))
                   (next-line 1))
                  ((re-search-forward "^(" nil t)
                   (goto-char (match-beginning 0))
                   (insert "\n")
                   (previous-line 1))
                  ((re-search-forward "^[^#;]" nil t)
                   (goto-char (match-beginning 0))
                   (insert "\n")))
            (beginning-of-line)
            (insert (format "(use %s)\n" pkg))
            (previous-line 1)
            (lisp-indent-line)
	    (let ((proc (get-buffer-process scheme-buffer)))
	      (if proc
		  (comint-send-string proc (format "(use %s)\n" pkg))))
            (message "Inserted (use %s)" pkg))))
    (message "You didn't specify a module name")))

(defvar gca-module-db nil)
(defvar gca-module-alist nil)

(defun gca-eval-scheme (sexp)
  (with-temp-buffer
    (let ((tempfile (make-temp-file "gca-module")))
      (unwind-protect
	  (progn
	    (with-temp-file tempfile
	      (insert (prin1-to-string sexp t)))
	    (call-process gca-gosh nil '(t nil) nil tempfile)
	    (goto-char (point-min))
	    (read (current-buffer)))
	(delete-file tempfile)))))

(defun gca-scheme-make-module-db (filename)
  (gca-eval-scheme
   `(begin
     (use srfi-1)
     (use srfi-2)
     (use srfi-13)
     (use file.util)
     (use gauche.config)
            
     (define nil "()")
            
     (define (module-exports* mod-sym)
       (receive (in out) (sys-pipe)
                (if (= (sys-fork) 0)
                    (begin
                     (guard (e (else (write \#f out)
                                     (newline out)
                                     (flush out)))
                            (eval "`"(begin
                                      (use ","mod-sym)
                                      (write (module-exports 
                                              (find-module "',"mod-sym)) 
                                             ","out)
                                      (newline ","out)
                                      (flush ","out))
                                  (interaction-environment)))
                     (sys-exit 0))
                  (let ((result (read in)))
                    (sys-wait)
                    result))))
            
     (define (directory-list-rec dir)
       (let ((wd (current-directory)))
         (sys-chdir dir)
         (begin0
          (directory-fold "\".\""
                          (lambda (child result)
                            (if (string=? (or (path-extension child)
                                              "\"\"") 
                                          "\"scm\"")
                                (let ((str (path-sans-extension child)))
                                  (cons
                                   (list (substring str 2 
                                                    (string-length str))
                                         (file-mtime child))
                                   result))
                              result))
                          ())
          (sys-chdir wd))))

     (define-syntax append-db
       (syntax-rules '()
                     ((_ db record) (let ((v record))
                                      (if (list-ref v 1)
                                          (push! db v))))))
     
     (define (main args)
       (let ((filename ,(concat "\""
				(expand-file-name gca-module-db-filename)
				"\""))
	     (module-db ())
             (new-db ()))
         (if (file-exists? filename)
             (set! module-db (call-with-input-file filename
                                                   (lambda (in)
                                                     (read in)))))
         (if (not (list? module-db))
             (set! module-db '()))
         (call-with-output-file 
          filename
          (lambda (out)
            (write
             (reverse
              (begin
               (for-each 
                (lambda (elt)
                  (receive (path mtime) 
                           (apply values elt)
                           (unless (string-contains path "\".\"")
                             (let ((module-name 
                                    (path->module-name 
                                     path)))
                               (or (and-let* ((record (assq module-name module-db)))
                                             (if (= (list-ref record 2) mtime)
                                                 (append-db new-db record)
                                               \#f))
                                   (append-db new-db
                                              (list module-name
                                                    (module-exports* module-name)
                                                    mtime)))))))
                (append (directory-list-rec
                         (gauche-config "\"--syslibdir\""))
                        (directory-list-rec
                         (gauche-config "\"--sitelibdir\""))))
               new-db))
             out)))
	 (print (call-with-input-file filename
				      (lambda (in)
					(port->string in)))))
       0))))

(defun gca-make-module-db ()
  (message "make module-db ...")
  (let ((module-db (gca-scheme-make-module-db
                    (expand-file-name gca-module-db-filename))))
    (setq gca-module-db (make-hash-table))
    (setq gca-module-alist nil)
    (mapcar (lambda (elt)
              (push (list (symbol-name (car elt))) gca-module-alist)
              (mapcar (lambda (sym)
                        (puthash sym
                                 (cons (car elt)
                                       (gethash sym gca-module-db ()))
                                 gca-module-db))
                      (cadr elt)))
            module-db)
  (message "make module-db ...done")))

;; Code template  
(defvar gca-template-alist ())

(defun gca-split-name (name)
  (let ((state nil)
         (result ())
         (word ()))
    (defun push-word ()
      (when word
        (push (concat word) result)
        (setq word ())))
    (defun push-char (c)
      (push c word))
    (mapcar (lambda (c)
	      (cond ((= c ?_)
                     (push-word))
                    ((and (<= ?a c) (<= c ?z))
                     (if (eq state 'upper)
                         (push-word))
                     (push-char c)
                     (setq state 'lower))
                    ((and (<= ?A c) (<= c ?Z))
                     (push-char c)
                     (if (eq state 'lower)
                         (push-word))
                     (setq state 'upper))
                    (t (push-char c))))
	    (reverse (string-to-list
                      (replace-regexp-in-string "^\\(struct\\|union\\)\\s +" 
                                                "" 
                                                name))))
    (push-word)
    result))

(defun gca-camelcase-name (name-parts)
  (apply #'concat (mapcar #'capitalize name-parts)))

(defun gca-upcase-name (name-parts)
  (mapconcat #'upcase name-parts "_"))

(defun gca-scheme-name (name-parts)
  (mapconcat #'downcase name-parts "-"))

(defun gca-add-template (name template)
  (push (cons name template) gca-template-alist))

(defun gca-insert-template (name)
  "Insert code template."
  (interactive (list
                (let ((completion-ignore-case t))
                  (completing-read "Template name: "
                                   gca-template-alist
                                   nil
                                   t))))
  (let ((kv (assoc name gca-template-alist)))
    (unless kv
      (error "Template name '%s' is not found" name))
    (cond ((stringp (cdr kv)) (insert kv))
          ((functionp (cdr kv)) (apply (cdr kv) ()))
          (t (error "Template must be string or function, but got %s"
                    (cdr kv))))))

(gca-add-template
 "c-header"
 (lambda ()
   (let* ((type_name (read-string "C-type name: "))
          (name-parts (gca-split-name type_name))
          (TypeName (gca-camelcase-name name-parts))
          (TYPE_NAME (gca-upcase-name name-parts)))
     (insert "/* " type_name " */\n")
     (insert "typedef struct Scm" TypeName "Rec {\n")
     (insert "    SCM_HEADER;\n")
     (insert "    " type_name " *data;\n")
     (insert "} Scm" TypeName ";\n")
     (insert "\n")
     (insert "SCM_CLASS_DECL(Scm_" TypeName "Class);\n")
     (insert "#define SCM_CLASS_" TYPE_NAME " (&Scm_" TypeName "Class)\n")
     (insert "#define SCM_" TYPE_NAME "(obj) ((Scm" TypeName "*)(obj))\n")
     (insert "#define SCM_" TYPE_NAME "P(obj) SCM_XTYPEP(obj, SCM_CLASS_"
             TYPE_NAME ")\n")
     (insert "#define SCM_" TYPE_NAME "_DATA(obj) (SCM_" TYPE_NAME "(obj)->data)\n")
     (insert "#define SCM_MAKE_" TYPE_NAME "(data) (Scm_Make" TypeName "(data))\n")
     (insert "\n")
     (insert "extern ScmObj Scm_Make" TypeName "(" type_name " *data);\n"))))

(gca-add-template
 "c-boxer"
 (lambda ()
   (let* ((type_name (read-string "C-type name: "))
          (name-parts (gca-split-name type_name))
          (TypeName (gca-camelcase-name name-parts))
          (TYPE_NAME (gca-upcase-name name-parts)))
     (insert "static void Scm_finalize_" TypeName "(ScmObj obj, void *data)\n")
     (insert "{\n")
     (insert "    Scm" TypeName " *z = SCM_" TYPE_NAME "(obj);\n")
     (insert "    if (z->data) {\n")
     (insert "        /* call destructor */\n")
     (insert "        z->data = NULL;\n")
     (insert "    }\n")
     (insert "}\n")
     (insert "\n")
     (insert "ScmObj Scm_Make" TypeName "(" type_name " *data)\n")
     (insert "{\n")
     (insert "    Scm" TypeName " *z = SCM_NEW(Scm" TypeName ");\n")
     (insert "    SCM_SET_CLASS(z, SCM_CLASS_" TYPE_NAME ");\n")
     (insert "    Scm_RegisterFinalizer(SCM_OBJ(z), Scm_finalize_" TypeName ", NULL);\n")
     (insert "    z->data = data;\n")
     (insert "\n")
     (insert "    SCM_RETURN(SCM_OBJ(z));\n")
     (insert "}\n")
     (insert "\n"))))

(gca-add-template
 "define-type"
 (lambda ()
   (let* ((type_name (read-string "C-type name: "))
          (name-parts (gca-split-name type_name))
          (TypeName (gca-camelcase-name name-parts))
          (TYPE_NAME (gca-upcase-name name-parts))
          (type-name (gca-scheme-name name-parts)))
     (insert "(define-type <" type-name ">\n")
     (insert "  \"" type_name "*\"         ; c-type\n") 
     (insert "  \"" type_name "\"          ; description\n")
     (insert "  \"SCM_" TYPE_NAME "P\"     ; c-predicate\n")
     (insert "  \"SCM_" TYPE_NAME "_DATA\" ; unboxer\n")
     (insert "  \"SCM_MAKE_" TYPE_NAME "\" ; boxer\n")
     (insert "  )\n")
     (insert "\n"))))

(gca-add-template
 "define-cclass"
 (lambda ()
   (let* ((type_name (read-string "C-type name: "))
          (name-parts (gca-split-name type_name))
          (TypeName (gca-camelcase-name name-parts))
          (TYPE_NAME (gca-upcase-name name-parts))
          (type-name (gca-scheme-name name-parts)))
     (insert "(define-cclass <" type-name ">\n")
     (insert "  ;; qualifier := :base | :built-in\n")
     (insert "  :built-in\n")
     (insert "\n")
     (insert "  ;; c-typename c-class-name\n")
     (insert "  \"Scm" TypeName "*\" \"Scm_" TypeName "Class\"\n")
     (insert "\n")
     (insert "  ;; cpa := (<string> ...)\n")
     (insert "  ()\n")
     (insert "\n")
     (insert "  (\n")
     (insert "   ;; slot-spec := slot-name\n")
     (insert "   ;;           |  (slot-name\n")
     (insert "   ;;                [:type <type>]\n")
     (insert "   ;;                [:c-name <c-name>]\n")
     (insert "   ;;                [:c-spec <c-cpec>]\n")
     (insert "   ;;                [:getter <c-code> | (c <c-name>) | #f | #t]\n")
     (insert "   ;;                [:setter <c-code> | (c <c-name>) | #f | #t])\n")
     (insert "   ))\n"))))

(gca-add-template
 "define-module"
 (lambda ()
   (let ((module-name (read-string "module name: ")))
     (insert "(define-module " module-name "\n")
     (insert "  )\n")
     (insert "\n\n")
     (insert "(provide \"" (replace-regexp-in-string "\\."
                                                     "/"
                                                     module-name) "\")\n")
     (previous-line 2))))

(defun gca-current-line ()
  ""
  (1+ (count-lines 1 (point))))

(defun gca-make-test (&optional arg)
  ""
  (interactive "P")
  (let ((bufname (buffer-name))
        (start nil)
        (end nil)
        (teststr nil)
        (l nil))
    (set-buffer scheme-buffer)
    (goto-char (point-max))
    (setq l (gca-current-line))
    (comint-send-string (scheme-proc) 
                        (if arg
                            (format "(make-test %d)\n" arg)
                          "(make-test)\n"))
    (while (= l (gca-current-line))
      (sleep-for 1))

    (previous-line 1)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (let ((str (buffer-substring-no-properties start end)))
      (if (or (string-match "^[0-9]+:\\w+> \\*\\*\\* " str)
              (not (string-match "^[0-9]+:\\w+> " str)))
          (error "No test case")))

    (backward-sexp)
    (setq start (point))
    (forward-sexp)
    (setq end (point))
    (setq teststr (buffer-substring-no-properties start end))
    (set-buffer bufname)
    (insert-string teststr)))

(defun gca-show-history (&optional arg)
  ""
  (interactive "P")
  (comint-send-string (scheme-proc) 
                      (if arg
                          (format "(history %d)\n" arg)
                        "(history)\n")))

;; Key binding

(provide 'gca)

