;; -*- lexical-binding: t -*-
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

;;; Code:
(require 'json)
(require 'cl-lib)
(require 'youdao-dictionary)
(require 'f)

(defgroup shengci nil
  "shengci group"
  :group 'applications
  :prefix 'shengci-)

(defvar shengci-buffer-name "*shengci*" "The name of shengci buffer.")

(defvar shengci-guess-recorded-word-buffer-name "*shengci-guess-recorded-word*" "The name of write recorded word form memory buffer.")

(defvar shengci-guess-memorized-word-buffer-name "*shengci-guess-memorized-word*" "The name of write memorized word form memory buffer.")

(defvar shengci-record-buffer-name "*shengci-record*" "The name of shengci-record buffer.")

(defvar shengci-memorized-buffer-name "*shengci-memorized*" "The name of shengci-memorized buffer.")

;; 临时单词的哈系表集合。
(defvar shengci-temp-words-hash-table nil "Temp words cache information. Hash Table")

;; 猜单词的成绩，数据例子: (单词A 1 单词B 0 单词C 1)，其中value为1代表正确，0代表错误。
(defvar shengci-guess-word-score (make-hash-table :test 'equal) "The socre for guess word game.")

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
;; (defun shengci ()
;;   "Initialize shengci buffer when shengci-mode is activated.
;; 激活shengci-mode时初始化shengci buffer"
;;   (if (get-buffer shengci-buffer-name)
;;       (switch-to-buffer shengci-buffer-name)
;;     (unless (buffer-live-p (get-buffer shengci-buffer-name))
;;       (switch-to-buffer shengci-buffer-name))
;;     (shengci-interface-init)))

(define-namespace shengci-

;; (defmacro with-suppressed-message (&rest body)
;;   "Suppress new messages temporarily in the echo area and
;;        the
;;        `*Messages*` buffer while BODY is evaluated."
;;   (declare (indent 0))
;;   (let ((message-log-max nil))
;;     `(with-temp-message (or (current-message) "") ,@body)))

;;;###autoload
(defun -check-path ()
  "Check cache directoryand memorized words and words cache file is exists.
if not found, create they.
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

;;;###autoload
(defun capture-word-and-save (&optional word)
  "Capture new word and save to all recorded word cache file.
捕获新的生词，并且保存到生词缓存文件中
WORD 要保存的单词"
  (interactive)
  (shengci--check-path)
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
      (f-write-text "" 'utf-8 shengci-cache-word-file-path-format)
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
                      (json-add-to-object all-words-cache word-eng shengci-cache-word-file-path-format)) 'utf-8 shengci-cache-word-file-path))))
;;;###autoload
(defun remove-word-forever (word)
  "Delete a recorded or memorized word forever.
永久删除一个已记录或已记住的单词
WORD 要删除的单词"
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

;;;###autoload
(defun -memorized-word (word)
  "Let word was memorized.
让单词改变为背熟地。
WORD 要跟改为背熟的单词."
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
      
      ;; Initialize the last review time to nil
      ;; 初始化上次复习时间为null
      (map-put cache-word-json-data 'review-time "null")
      
      ;; 初始化复习等级为0
      ;; Initialize the last review time to nil
      (map-put cache-word-json-data 'review-level 0)
      
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

;;;###autoload
(defun re-record-word (word)
  "Re-record  the word.
重新记录单词。
WORD 要重记的单词。"
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
      ;; 处理单词review-time
      (setq word-json-data (map-delete word-json-data 'review-time))
      ;; 处理单词review-level
      (setq word-json-data (map-delete word-json-data 'review-level)) 
      (f-write-text (json-serialize word-json-data) 'utf-8 (concat shengci-cache-word-dir-path word "-cache.json")))))

;;;###autoload
(defun -get-all-recorded-word ()
  "Get all the words that have been recorded
获取所有已记录的单词"
  (let* ((all-cache-words (if (string= (f-read-text shengci-cache-word-file-path) "")
                              nil
                            (json-read-file shengci-cache-word-file-path)))
         (all-recorded-words))
    (mapcar
     (lambda (word-file-path)
       (when (string= (map-elt (json-read-file word-file-path) 'end-time) "null")
         (push word-file-path all-recorded-words)))
     (map-values all-cache-words))
    all-recorded-words))

;;;###autoload
(defun -get-all-memorized-word ()
  "Get all the words that have been memorized.
获取所有已记住的单词"
  (let ((all-cache-words (if (string= (f-read-text shengci-cache-word-file-path) "")
                             nil
                           (json-read-file shengci-cache-word-file-path)))
        (all-memorized-words ))
    ;; Traverse all the cached words to determine whether they are have been memorized words
    ;; 遍历所有缓存的单词以确定它们是否已被记住
    (mapc
     (lambda (word-file-path)
       (if (string= (map-elt (json-read-file word-file-path) 'end-time) "null")
           nil
         (push word-file-path all-memorized-words)))
     (map-values all-cache-words))
    all-memorized-words))

;;;###autoload
(defun -get-word-info (word-path)
  "Get word info.
获取单词信息
WORD-PATH is the cache path corresponding to the word.
WORD-PATH 是单词对应的缓存文件路径。"
  (json-read-file word-path))

;;;###autoload
(defun show-word (type)
  "Show recored or memorized word.
显示已记录或者是已背熟单词.

The value of TYPE should be a memorized or recoreded
with symbol or character string.

memorized means to display the learned words, and recored
means to display the recorded words.

TYPE的值应该是memorized或recoreded符号或字符串。
memorized意味着显示显示已背熟单词，recored意味显示已记录单词."
  (shengci--check-path)
  ;; 当type是符号时转换
  (when (symbolp type)
    (setq type (symbol-name type)))
  (let ((buf (get-buffer-create (cond ((string= type "memorized") shengci-memorized-buffer-name)
                                      ((string= type "recorded") shengci-record-buffer-name))))
        word-info)
    (pop-to-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (mapcar (lambda (word-path)
              (when word-path
                (let* ((word-info (shengci--get-word-info word-path))
                       (word-info-explains (map-elt word-info 'explains))
                       (word-info-eng (map-elt word-info 'english))
                       (word-info-insert-mark-beg)
                       (word-info-insert-mark-end)
                       (hide-state t))
                  (with-current-buffer buf
                    ;; Style Exmaple:
                    ;;                  signal
                    ;;        
                    ;; 开始时间: Mon Nov  2 19:47:23 2020
                    ;; 美式音标: [ˈsɪɡnəl]
                    ;; 中文翻译:
                    ;;  - n. 信号；暗号；导火线
                    ;;  - vt. 标志；用信号通知；表示
                    ;;  - adj. 显著的；作为信号的
                    ;;  - vi. 发信号
                    ;;  - n. (Signal)人名；(瑞典)西格纳尔

                    (defun center-string (string size)
                      (let* ((padding (/ (- size (length string)) 2))
                             (lpad (+ (length string) padding))
                             (lformat (format "%%%ds" lpad))
                             (rformat (format "%%%ds" (- size))))
                        (format rformat (format lformat string))))
                    (insert-button
                     (center-string word-info-eng (window-width))
                     'face (list :underline "RoyalBlue1" :overline "RoyalBlue1" :foreground "MediumOrchid1")
                     'action (lambda (_)
                               (setq word-info-insert
                                     (concat "加入时间: " (map-elt word-info 'start-time) "\n"
                                             "美式音标: [" (map-elt word-info 'phonetic) "]\n"
                                             "中文翻译:\n"
                                             (mapconcat (lambda (word)
                                                          (concat "\t" "- " word))
                                                        word-info-explains
                                                        "\n")
                                             "\n"))
                               (setq buffer-read-only nil)
                               ;; If the hide-state is not nil.
                               ;; 如果当前是隐藏状态
                               (if hide-state
                                   (progn
                                     (save-excursion
                                       (forward-line)
                                       (forward-line)
                                       (beginning-of-line)
                                       (setq word-info-insert-mark-beg (point-marker)
                                             word-info-insert-mark-end word-info-insert-mark-beg)
                                       ;; insert about word info data.
                                       ;; 插入于单词信息相关的数据。
                                       (string-insert-rectangle
                                        (marker-position word-info-insert-mark-beg)
                                        (marker-position word-info-insert-mark-beg)
                                        word-info-insert))
                                     (setq hide-state nil
                                           word-info-insert-mark-end (point-marker)))
                                 ;; 如果不隐藏是状态，则将其隐藏
                                 (progn
                                   (save-excursion
                                     (replace-string  word-info-insert ""))
                                   (setq hide-state t
                                         word-info-insert-mark-end nil)))
                               (setq buffer-read-only t))

                     'follow-link nil)
                    (insert "\n")
                    ;; 插入按钮
                    (insert-button "朗读"
                                   'help-echo "播放"
                                   'follow-link t
                                   'action (lambda (_) (youdao-dictionary--play-voice word-info-eng))
                                   'face (list :underline nil :foreground "green"))
                    (insert "\t")
                    (cond ((string= type "recorded") (insert-button "背熟"
                                                                    'action (lambda (_)
                                                                              (shengci--memorized-word word-info-eng)
                                                                              (shengci-refresh-all-buffer-content))
                                                                    'follow-link t
                                                                    'help-echo "重记"
                                                                    'face (list :underline nil :foreground "coral")))
                          ((string= type "memorized") (insert-button "重记"
                                                                     'action (lambda (_)
                                                                               (shengci-re-record-word word-info-eng)
                                                                               (shengci-refresh-all-buffer-content))
                                                                     'follow-lint t
                                                                     'help-echo "背熟"
                                                                     'face (list :underline nil :foreground "coral"))))
                    (insert "\t")
                    (insert-button "删除"
                                   'action (lambda (_)
                                             (shengci-remove-word-forever word-info-eng)
                                             (cond ((string= type "memorized") (shengci-refresh-buffer-content))
                                                   ((string= type "recorded") (shengci-refresh-buffer-content))))
                                   ;; 'help-echo "删除"
                                   'face (list :underline nil :foreground "VioletRed1"))
                    (insert "\n"))
                  )))
            (cond ((string= type "memorized") (shengci--get-all-memorized-word))
                  ((string= type "recorded") (shengci--get-all-recorded-word))))
    (setq buffer-read-only t)
    (goto-char (point-min))))

;;;###autoload
(defun refresh-buffer-content ()
  "Refresh record buffer or memorzied buffer content."
  (cond ((string= (buffer-name) shengci-record-buffer-name)
         (shengci-show-word "recorded"))
        ((string= (buffer-name) shengci-memorized-buffer-name)
         (shengci-show-word "memorized"))))

;;;###autoload
(defun refresh-all-buffer-content ()
  "Refrsh record buffer and memorized buffer content."
  (shengci-show-word "recorded")
  (shengci-show-word "memorized"))

;;;###autoload
(defun show-recorded-word ()
  "Show all recorded-word , Does not contain memorized word."
  (interactive)
  (shengci-show-word "recorded"))

;;;###autoload
(defun show-memorized-word ()
  "Show all memorized word."
  (interactive)
  (shengci-show-word "memorized"))

;;;###autoload
(defun -set-all-word (type)
  "Set all word hash table.
设置单词哈系表。
The value of TYPE should be memorized or recorded
"
  ;; 如果type是一个符号，将其转换为string
  (when (symbolp type)
    (setq type (symbol-name type)))
  (let ((all-cache-words (json-read-file shengci-cache-word-file-path)))
    (setq shengci-temp-words-hash-table nil)
    (setq shengci-temp-words-hash-table (make-hash-table :test 'equal))
    (mapcar (lambda (word)
              (cond ((string= type "memorized") (when (not (string= (map-elt (json-read-file (cdr word)) 'end-time) "null"))
                                                  (puthash (car word) (cdr word) shengci-temp-words-hash-table))) 
                    ((string= type "recoreded") (when (string= (map-elt (json-read-file (cdr word)) 'end-time) "null")
                                                  (puthash (car word) (cdr word) shengci-temp-words-hash-table)))))
            all-cache-words)))

;;;###autoload
(defun -insert-score ()
  "Insert the score.
插入成绩"
  (let ((true 0)
        (false 0))
    (maphash (lambda (key value)
               (if (string= value "1")
                   (setq true (1+ true))
                 (setq false (1+ false))))
             shengci-guess-word-score)
    (insert "正确: " (number-to-string true) "\t" "错误: " (number-to-string false))))


;;;###autoload
(defun -guess-word-main (hash-table type &optional level)
  (when (symbolp type)
    (setq type (symbol-name type)))
  (setq ovs nil)
  (maphash (lambda (key value)
             (erase-buffer)
             (shengci--insert-score)
             (let* ((word-info (json-read-file value))
                    (word-info-eng (map-elt word-info 'english))
                    (word-info-explains (map-elt word-info 'explains))
                    (beg)
                    (end)
                    (ov))
               (insert "\n")
               (setq beg (point))
               (insert word-info-eng "\n")
               (setq end (point))
               (message "beg: %s ::: end: %s" beg end)
               (mapcar (lambda (word)
                         (insert "\t" "- " word "\n"))
                       word-info-explains)
               ;; 隐藏word-info-eng部分单词
               (dotimes (i (length word-info-eng))
                 ;; 算法是隔两个字母隐藏一个字母
                 (when (= (% i 3) 1)
                   (progn
                     (setq ov (make-overlay (+ beg i) (1+ (+ beg i))))
                     (overlay-put ov 'face '(:underline t))
                     (overlay-put ov 'display (make-string 1 ?\s))
                     (push ov ovs))))
               (if (string= key (read-string "英文(C-g取消练习): "))
                   (progn
                     ;; 将单词设置为已背熟
                     (cond ((string= type "recorded") (shengci--memorized-word key))
                           ((string= type "memorized") (let ((word-cache (json-read-file value)))
                                                         (with-temp-file value
                                                           (call-interactively #'mark-whole-buffer)
                                                           (delete-active-region)
                                                           (save-buffer))
                                                         (when (= level 0)
                                                           (map-put! word-cache 'review-level (1+ level)))
                                                         (when (string= (map-elt word-cache 'review-time) "null")
                                                           (map-put! word-cache 'review-time (current-time-string)))
                                                         (f-append-text (json-serialize word-cache) 'utf-8 value))))
                     (puthash key "1" shengci-guess-word-score))
                 (puthash key "0" shengci-guess-word-score)))) 
           hash-table))

;;;###autoload
(defun practice-guess-recorded-word ()
  "Practice write recorded word from memory.
练习默写已记录单词。"
  (interactive)
  (shengci--check-path)
  (shengci--set-all-word "recorded")
  (let ((buf (get-buffer-create shengci-guess-recorded-word-buffer-name)))
    (pop-to-buffer buf)
    (setq shengci-guess-word-score nil
          shengci-guess-word-score (make-hash-table :test 'equal))
    (with-current-buffer buf
      (shengci--guess-word-main shengci-temp-words-hash-table "recorded")
      (erase-buffer)
      (insert "默写完成!\n")
      (shengci--insert-score))))

;;;###autoload
(defun practice-guess-memorized-word ()
  "Practice write memorized word from memory.
练习默写已背熟的单词."
  (interactive)
  (shengci--check-path)
  (shengci--set-all-word "memorized")
  (let* ((buf (get-buffer-create shengci-guess-memorized-word-buffer-name))
         (level-lst (list "0 - 从未复习过" "1 - 二十分钟至一小时前复习过" "2 - 一小时至九小时前复习过" "3 - 九小时至一天前复习过" "4 - 一天至两天前复习过" "5 - 两天前至六天复习过" "6 - 大于六天前复习过"))
         (level (string-to-number (completing-read "请选择复习等级: " level-lst)))
         (guess-memorized-word-hash-table (make-hash-table :test 'equal)))
    (pop-to-buffer buf)
    (cond
     ;; 当level为0，需要过滤review-time为null的单词
     ((= level 0) (maphash (lambda (key val)
                             ;; 从未复习过
                             (when (string= (map-elt (json-read-file val) 'review-time) "null")
                               (puthash key val guess-memorized-word-hash-table)))
                           shengci-temp-words-hash-table))
     ((= level 1) (maphash (lambda (key val)
                             (let* ((word-info (json-read-file val))
                                    (word-review-time (map-elt word-info 'review-time))
                                    (time-interval (time-subtract
                                                    (time-convert (current-time) 'integer)
                                                    (time-convert (if (string= word-review-time "null")
                                                                      (current-time)
                                                                    (date-to-time word-review-time)) 'integer))))
                               ;; 20分钟~1小时之间前复习过
                               (when (and (>= time-interval 1)
                                          (< time-interval (* 20 60)))
                                 (message "20分钟~1小时")
                                 (puthash key val guess-memorized-word-hash-table))))
                           shengci-temp-words-hash-table))
     ((= level 2) (maphash (lambda (key val)
                             (let* ((word-info (json-read-file val))
                                    (word-review-time (map-elt word-info 'review-time))
                                    (time-interval (time-subtract
                                                    (time-convert (current-time) 'integer)
                                                    (time-convert (if (string= word-review-time "null")
                                                                      (current-time)
                                                                    (date-to-time word-review-time)) 'integer))))
                               ;; 1小时~9小时之间前复习过
                               (when (and (>= time-interval (* 60 60))
                                          (< time-interval (* (* 60 60) 9)))
                                 (puthash key val guess-memorized-word-hash-table))))
                           shengci-temp-words-hash-table))
     ((= level 3) (maphash (lambda (key val)
                             (let* ((word-info (json-read-file val))
                                    (word-review-time (map-elt word-info 'review-time))
                                    (time-interval (time-subtract
                                                    (time-convert (current-time) 'integer)
                                                    (time-convert (if (string= word-review-time "null")
                                                                      (current-time)
                                                                    (date-to-time word-review-time)) 'integer))))
                               ;; 9小时~一天之间前复习过
                               (when (and (>= time-interval (* (* 60 60) 9))
                                          (< time-interval (* (* 60 60) 24)))
                                 (puthash key val guess-memorized-word-hash-table))))
                           shengci-temp-words-hash-table))
     ((= level 4) (maphash (lambda (key val)
                             (let* ((word-info (json-read-file val))
                                    (word-review-time (map-elt word-info 'review-time))
                                    (time-interval (time-subtract
                                                    (time-convert (current-time) 'integer)
                                                    (time-convert (if (string= word-review-time "null")
                                                                      (current-time)
                                                                    (date-to-time word-review-time)) 'integer))))
                               ;; 一天~两天之间前复习过
                               (when (and (>= time-interval (* (* 60 60) 24))
                                          (< time-interval (* (* (* 60 60) 24) 2)))
                                 (puthash key val guess-memorized-word-hash-table))))
                           shengci-temp-words-hash-table))
     ((= level 5) (maphash (lambda (key val)
                             (let* ((word-info (json-read-file val))
                                    (word-review-time (map-elt word-info 'review-time))
                                    (time-interval (time-subtract
                                                    (time-convert (current-time) 'integer)
                                                    (time-convert (if (string= word-review-time "null")
                                                                      (current-time)
                                                                    (date-to-time word-review-time)) 'integer))))
                               ;; 两天~六天之间前复习过
                               (when (and (>= time-interval (* (* (* 60 60) 24) 2))
                                          (< time-interval (* (* (* 60 60) 24) 6)))
                                 (puthash key val guess-memorized-word-hash-table))))
                           shengci-temp-words-hash-table))

     ((= level 6) (maphash (lambda (key val)
                             (let* ((word-info (json-read-file val))
                                    (word-review-time (map-elt word-info 'review-time))
                                    (time-interval (time-subtract
                                                    (time-convert (current-time) 'integer)
                                                    (time-convert (if (string= word-review-time "null")
                                                                      (current-time)
                                                                    (date-to-time word-review-time)) 'integer))))
                               ;; 大于六天前复习过的
                               (when (>= time-interval (* (* (* 60 60) 24) 6))
                                 (puthash key val guess-memorized-word-hash-table))))
                           shengci-temp-words-hash-table)))
    (setq shengci-guess-word-score nil
          shengci-guess-word-score (make-hash-table :test 'equal))
    (with-current-buffer buf
      (shengci--guess-word-main guess-memorized-word-hash-table "memorized" level)
      (erase-buffer)
      (insert "默写完成!\n")
      (shengci--insert-score)
      ))))

(provide 'shengci)
;;; shengci.el ends here
