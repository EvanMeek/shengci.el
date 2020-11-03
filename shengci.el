; -*- lexical-binding: t -*-

;;; shengci.el --- Record unfamiliar English words
;; Copyright (C) 2020 Evan Meek
;; Author: EvanMeek <the_lty_mail@foxmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.0") (youdao-dictionary "0.4") (f "0.20.0") )
;; License: GPL-3.0
;; Repository: https://github.com/EvanMeek/shengci.el

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;; shengci可以让你在使用emacs的过程中随时记录生词，并且管理你的生词。

;;; TODO:
;;* 研究text properties.


;;; Code:
(require 'json)
(require 'request)
(require 'cl-lib)
(require 'pcase)
(require 'async)
(require 'youdao-dictionary)
(require 'f)


(defgroup shengci nil
  "shengci group"
  :group 'applications
  :prefix 'shengci-)

(defvar shengci-buffer-name "*shengci*" "The name of shengci buffer.")

(defvar shengci-record-buffer-name "*shengci-record*" "The name of shengci-record buffer.")

(defvar shengci-memorized-buffer-name "*shengci-memorized*" "The name of shengci-memorized buffer.")

(defcustom shengci-word-info nil
  "The info of word.
单词的信息"
  :type 'string
  :group 'shengci)

(defcustom shengci-cache-word-dir-path (expand-file-name "shengci/" user-emacs-directory)
  "The words cahe directory path.
shengci插件的缓存目录路径"
  :type 'string
  :group 'shengci)

(defcustom shengci-cache-word-file-path-format nil
  "The words cache file path.
所有已记录单词的缓存文件路径"
  :type 'string
  :group 'shengci)

(defcustom shengci-cache-word-delete nil
  "Word to be deleted.
将被删除掉的单词"
  :type 'string
  :group 'shengci)

(defcustom shengci-cache-word-file-path (concat shengci-cache-word-dir-path "all-words-table.json")
  "All recorded the words.
所有已记录的单词"
  :type 'string
  :group 'shengci)

(defcustom shengci-cache-memorized-word-file-path (concat shengci-cache-word-dir-path "memorized-word-table.json")
  "All memorized the words.
所有已记住的单词"
  :type 'string
  :group 'shengci)

;;;###autoload
(defun shengci ()
  "Initialize shengci buffer when shengci-mode is activated
激活shengci-mode时初始化shengci buffer"
  (if (get-buffer shengci-buffer-name)
	  (switch-to-buffer shengci-buffer-name)
	(unless (buffer-live-p (get-buffer shengci-buffer-name))
	  (switch-to-buffer shengci-buffer-name))
	(shengci-interface-init)))

(define-namespace shengci-

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and
       the
       `*Messages*` buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun check-path ()
  "check cache directory and memorized words and words cache file is exists? if not found, create they.
检查缓存路径、已记住单词缓存文件、已记录单词缓存文件是否存在，如果不存在则创建它们。"
  ;; check shengci cache directory path
  ;; 检查shengci缓存目录路径
  (when (not (file-exists-p shengci-cache-word-dir-path))
	(make-directory shengci-cache-word-dir-path))
  
  ;; check all the cache recorded words file path
  ;; 检查所有已记录单词缓存文件路径
  (when (not (file-exists-p shengci-cache-word-file-path))
	(f-write "" 'utf-8 shengci-cache-word-file-path))
  
  ;; check all the cache memorized words file path
  ;; 检查所有已记住单词缓存文件路径
  (when (not (file-exists-p shengci-cache-memorized-word-file-path))
	(f-write "" 'utf-8 shengci-cache-memorized-word-file-path)))

(defun capture-word-and-save (&optional word)
  (interactive)
  "capture new word and save to all recorded word cache file.
捕获新的生词，并且保存到生词缓存文件中"
  (shengci-check-path)
  (let* ((word-info (youdao-dictionary--request (if (null word)
													(thing-at-point 'word)
												  word)))
		 (word-eng (cdr (assoc 'query word-info)))
		 (word-basic (cdr (assoc 'basic word-info)))
		 (word-phonetic (cdr (assoc 'us-phonetic word-basic)))
		 (word-explains (cdr (assoc 'explains word-basic)))
		 (all-words-cache (if (string= (f-read-text shengci-cache-word-file-path) "")
							  nil
							(json-read-file shengci-cache-word-file-path))))
	;; (message "english: %s\nphonetic: %s\n explains: %s\n web: %s" word-eng word-phonetic word-explains word-web)
	
	(setq shengci-cache-word-file-path-format (concat  shengci-cache-word-dir-path word-eng "-cache.json"))
	
	;; if word cache file not found, create it.
	;; 如果word这个单词缓存文件不存在，则创建它。
	(when (not (file-exists-p shengci-cache-word-file-path-format))
	  (f-write-text "" 'utf-8 shengci-cache-word-file-path-format))
	
	;; insert word info to cache file.
	;; 插入单词信息到缓存文件中。
	(f-append-text (concat "{\n"
						   "\"english\" : \"" word-eng "\",\n"
						   "\"start-time\" : \"" (current-time-string) "\",\n"
						   "\"end-time\" : \"" (json-encode nil) "\",\n"
						   "\"phonetic\" : \"" word-phonetic "\",\n"
						   "\"explains\" : " (json-encode-array word-explains)  "\n"
						   "}\n") 'utf-8 shengci-cache-word-file-path-format)

	;; delete shengci-cache-word-file-path's content
	;; 删除shengci-cache-word-file-path的内容
	(with-temp-file shengci-cache-word-file-path
	  (mark-whole-buffer)
	  (delete-active-region))
	
	;; Add the current file path to the total word cache file
	;; 添加当前文件路径到所有单词缓存文件中。
	(f-append-text (json-serialize
					(json-add-to-object all-words-cache word-eng shengci-cache-word-file-path-format)) 'utf-8 shengci-cache-word-file-path)))

(defun remove-word-forever (word)
  "delete a recorded or memorized word forever.
永久删除一个已记录或已记住的单词"
  (let ((all-cache-words (if (string= (f-read-text shengci-cache-word-file-path) "")
							 nil
						   (json-read-file shengci-cache-word-file-path)))
		(all-memorized-cache-words (if (string= (f-read-text shengci-cache-memorized-word-file-path) "")
									   nil
									 (json-read-file shengci-cache-memorized-word-file-path)))
		(delete-word-file-path (concat shengci-cache-word-dir-path word "-cache.json")) )
	(if (string= (read-minibuffer (format "确定要删除%s吗(y/n)? :" word)) "y")
		(progn
		  ;; 将word从所有已记录单词列表中移除
		  (setq all-cache-words (map-delete all-cache-words (intern word)))
		  (with-temp-file shengci-cache-word-file-path
			(mark-whole-buffer)
			(delete-active-region)
			(save-buffer))
		  (f-append-text (json-serialize all-cache-words) 'utf-8 shengci-cache-word-file-path)

		  ;; 将word从所有已记住单词列表中移除
		  (setq all-memorized-cache-words (map-delete all-memorized-cache-words (intern word)))
		  (with-temp-file shengci-cache-memorized-word-file-path
			(mark-whole-buffer)
			(delete-active-region)
			(save-buffer))
		  (f-append-text (json-serialize all-memorized-cache-words) 'utf-8 shengci-cache-memorized-word-file-path)

		  ;; Delete the cache file corresponding to word
		  ;; 最后删除word对因的单词缓存文件
		  (when (delete-file delete-word-file-path)
			(message ("删除成功"))))
	  nil)))

(defun memorized-word (word)
  "let word was memorized
让单词改变为记住的"
  (let* ((memorized-word-file-path (if (file-exists-p (concat shengci-cache-word-dir-path word "-cache.json"))
									   (concat shengci-cache-word-dir-path word "-cache.json")
									 nil))
		 (cache-word-json-data (if (null memorized-word-file-path)
								   (progn (message "不存在此单词.")
										  nil)
								 (json-read-file memorized-word-file-path)))
		 ;; 记录所有已记住单词更改
		 (all-the-memorized-words (if (string= (f-read-text shengci-cache-memorized-word-file-path) "")
									  nil
									(json-read-file shengci-cache-memorized-word-file-path))))
	(when (not (null cache-word-json-data))
	  ;; Delete the contents of all recorded word cache files first
	  ;; 先将所有已记录单词缓存文件内容删除
	  (with-temp-file memorized-word-file-path
		(mark-whole-buffer)
		(delete-active-region)
		(save-buffer))
	  
	  ;; Second, reset the end-time object of the word to the current time
	  ;; 其次将单词的 end-time 对象重置为当前时间
	  (map-put! cache-word-json-data 'end-time (current-time-string))
	  
	  ;; Re-add the modified data to the remembered word cache file
	  ;; 将修改好的数据重新加入到已记住单词缓存文件中
	  (f-append-text (json-serialize cache-word-json-data) 'utf-8 memorized-word-file-path)

	  (with-temp-file shengci-cache-memorized-word-file-path
		(mark-whole-buffer)
		(delete-active-region))
	  ;; add the word to memorized-word-table
	  (f-append-text (json-serialize
					  (json-add-to-object all-the-memorized-words word memorized-word-file-path))
					 'utf-8 shengci-cache-memorized-word-file-path))))

(defun re-record-word (word)
  "re-record word
重新记录单词"
  (let ((all-memorized-word-json-data (if (string= (f-read-text shengci-cache-memorized-word-file-path) "")
										  nil
										(json-read-file shengci-cache-memorized-word-file-path)))
		(word-json-data (if (file-exists-p (concat shengci-cache-word-dir-path word "-cache.json"))
							(json-read-file (concat shengci-cache-word-dir-path word "-cache.json")))))
	(when (and
		   all-memorized-word-json-data
		   word-json-data)
	  ;; 处理所有已记录单词列表
	  (with-temp-file shengci-cache-memorized-word-file-path
		(mark-whole-buffer)
		(delete-active-region))
	  (setq all-memorized-word-json-data (map-delete all-memorized-word-json-data (intern word)))
	  (f-append-text (json-serialize all-memorized-word-json-data) 'utf-8 shengci-cache-memorized-word-file-path)

	  ;; 处理单词end-time
	  (map-put! word-json-data 'end-time "null")
	  (f-write-text (json-serialize word-json-data) 'utf-8 (concat shengci-cache-word-dir-path word "-cache.json")))))

(defun get-all-recorded-word ()
  "Get all the words that have been recorded
获取所有已记录的单词"
  (let ((all-cache-words (if (string= (f-read-text shengci-cache-word-file-path) "")
							 nil
						   (json-read-file shengci-cache-word-file-path)))
		(all-recorded-words nil))
	
	(mapc
	 (lambda (word-file-path) 
	   (if (string= (map-elt (json-read-file word-file-path) 'end-time) "null")
		   (push word-file-path all-recorded-words)
		 nil))
	 (map-values all-cache-words))
	all-recorded-words))

(defun get-all-memorized-word ()
  "Get all the words that have been memorized.
获取所有已记住的单词"
  (let ((all-cache-words (if (string= (f-read-text shengci-cache-word-file-path) "")
							 nil
						   (json-read-file shengci-cache-word-file-path)))
		(all-memorized-words nil))
	;; Traverse all the cached words to determine whether they are have been memorized words
	;; 遍历所有缓存的单词以确定它们是否已被记住
	(mapc
	 (lambda (word-file-path) 
	   (if (string= (map-elt (json-read-file word-file-path) 'end-time) "null")
		   nil
		 (push word-file-path all-memorized-words)))
	 (map-values all-cache-words))
	all-memorized-words))

(defun get-word-info (word-path)
  "获取单词信息
get word info"
  (json-read-file word-path))

(defun show-word (type)
  "显示已记录或者是已背熟单词."
  (when (symbolp type)
	(setq type (symbol-name type))
	(message "type:%s" type))
  (let ((buf (get-buffer-create (cond ((string= type "memorized") shengci-memorized-buffer-name)
									  ((string= type "recorded") shengci-record-buffer-name))))
		word-info)
	(pop-to-buffer buf)
	(setq buffer-read-only nil)
	(erase-buffer)
	(mapcar (lambda (word-path)
			  (let* ((word-info (shengci-get-word-info word-path))
					 (word-info-explains (map-elt word-info 'explains))
					 (word-info-eng (map-elt word-info 'english))
					 (word-info-insert-mark-beg)
					 (word-info-insert-mark-end)
					 (hide-state t))
				(with-current-buffer buf
				  ;; Style Exmaple:
				  ;; ==== Skeleton ====
				  ;; 朗读    背熟    删除
				  ;; 记录时间: Mon Nov  2 19:47:23 2020
				  ;; 音标: [ˈskelɪtn]
				  ;; 翻译:
				  ;;     - n. 骨架，骨骼；纲要；骨瘦如柴的人
				  ;;     - adj. 骨骼的；骨瘦如柴的；概略的
				  
				  
 				  (insert "_________________________ ")
				  (insert-button word-info-eng
								 'action (lambda (_)
										   (setq word-info-insert (concat "记录时间: " (map-elt word-info 'start-time) "\n"
																		  "音标: [" (map-elt word-info 'phonetic) "]\n"
																		  "翻译:\n"
																		  (mapconcat (lambda (word)
																					   (concat "\t" "- " word))
																					 word-info-explains
																					 "\n")
																		  "\n"))
										   
										   (setq buffer-read-only nil)
										   ;; 如果当前是隐藏状态
										   (if hide-state
											   (progn
												 (save-excursion
												   (next-line)
												   (next-line)
												   (beginning-of-line)
												   (setq word-info-insert-mark-beg (point-marker)
														 word-info-insert-mark-end word-info-insert-mark-beg)
												   (string-insert-rectangle (marker-position word-info-insert-mark-beg) (marker-position word-info-insert-mark-beg) word-info-insert))
												 (setq hide-state nil
													   word-info-insert-mark-end (point-marker)))
											 
											 (progn
											   (save-excursion
												 (replace-string  word-info-insert ""))
											   (setq hide-state t
													 word-info-insert-mark-end nil)))
										   (setq buffer-read-only t))
								 
								 'follow-link t)
				  (insert "\n")
				  ;; 插入按钮
				  (insert-button ""
								 'action (lambda (_) (youdao-dictionary--play-voice word-info-eng))
								 'follow-link t)
				  (insert "\t")
				  (cond ((string= type "recorded") (insert-button ""
																  'action (lambda (_)
																			(shengci-memorized-word word-info-eng)
																			(shengci-refresh-all-buffer-content))
																  'follow-link t))
						((string= type "memorized") (insert-button ""
																   'action (lambda (_)
																			 (shengci-re-record-word word-info-eng)
																			 (shengci-refresh-all-buffer-content))
																   'follow-lint t)))
				  (insert "\t")
				  (insert-button ""
								 'action (lambda (_)
										   (shengci-remove-word-forever word-info-eng)
										   (cond ((string= type "memorized") (shengci-refresh-buffer-content))
												 ((string= type "recorded") (shengci-refresh-buffer-content))))) 
				  (insert "\n"))))
			(cond ((string= type "memorized") (shengci-get-all-memorized-word))
				  ((string= type "recorded") (shengci-get-all-recorded-word))))
	(setq buffer-read-only t)
	(beginning-of-buffer)))

(defun refresh-buffer-content ()
  "Refresh record buffer or memorzied buffer content."
  (cond ((string= (buffer-name) shengci-record-buffer-name) 
		 (shengci-show-word "recorded"))
		((string= (buffer-name) shengci-memorized-buffer-name)
		 (shengci-show-word "memorized"))))

(defun refresh-all-buffer-content ()
  "Refrsh record buffer and memorized buffer content."
  (shengci-show-word "recorded")
  (shengci-show-word "memorized"))

(defun show-recorded-word ()
  "Show all recorded-word , Does not contain memorized word."
  (interactive)
  (shengci-show-word "recorded"))

(defun show-memorized-word ()
  "Show all memorized word"
  (interactive)
  (shengci-show-word "memorzied"))

)

(provide 'shengci)
;;; shengci.el ends here.
