;;; Code:
;; (map! :leader
;;       :mode (org-mode latex-mode)
;;       "e e" #'xenops-mode
;;       "e r" #'xenops-regenerate
;;       )

;;(use-package! org-elp
  ;;:config
  ;;(setq	org-elp-idle-time 0.5
        ;;org-elp-split-fraction 0.25
        ;;org-elp-buffer-name "*Equation Live*"
   ;;))
;;;;
;;(map! :leader
      ;;:mode (org-mode latex-mode)
      ;;"e w" #'org-elp-mode)
;; org-preview 生成矢量图
;; (setq org-preview-latex-default-process "dvisvgm")
; 使用xelatex
(setq org-latex-compiler "xelatex")
; 垂直对齐，让公式和文本基线对其
(defun my/org-latex-preview-vertical-align (beg end &rest _)
  "Vertically align the LaTeX preview image with the text baseline."
  (when-let* ((ov (car (overlays-at (/ (+ beg end) 2))))
              (display-prop (overlay-get ov 'display))
              (img-spec (and (eq (car-safe display-prop) 'image)
                             (cdr display-prop))))
    (let ((new-img (plist-put img-spec :ascent 95))) ; 95% 的图片在基线之上，可自行修改
      (overlay-put ov 'display (cons 'image new-img)))))
(advice-add 'org--make-preview-overlay
            :after #'my/org-latex-preview-vertical-align)
;; 水平对齐
;; 可改为 'center' 'right' 'left' 对齐方式
(setq org-format-latex-options (plist-put org-format-latex-options :justify 'center))
(defun my/org-latex-preview-justify (beg end &rest _)
  "Justify block-level LaTeX previews according to `org-format-latex-options`."
  (when-let* ((position (plist-get org-format-latex-options :justify))
              (ov (car (overlays-at (/ (+ beg end) 2))))
              ;; 仅当公式片段位于行首时才进行对齐处理。
              (width (and (= beg (line-beginning-position))
                          (car (image-size (overlay-get ov 'display) t)))))
    (let (offset)
      (cond
       ((eq 'center position)
        (setq offset (floor (- (/ (float fill-column) 2)
                               (/ (float width) 2))))
        (when (< offset 0) (setq offset 0))
        (overlay-put ov 'before-string (make-string offset ?\s)))
       ((eq 'right position)
        (setq offset (floor (- fill-column width)))
        (when (< offset 0) (setq offset 0))
        (overlay-put ov 'before-string (make-string offset ?\s)))))))
(advice-add 'org--make-preview-overlay
            :after #'my/org-latex-preview-justify)

;; 公式的正确编号
(defun my/org-renumber-latex-environment (orig-func &rest args)
  "Provide continuous equation numbering for LaTeX previews in Org mode."
  (let ((results '())
        (counter -1) ; LaTeX 计数器从0开始
        (numberp))
    ;; 1. 扫描整个buffer, 为所有可编号环境计算出正确序号
    (setq results
          (cl-loop for (begin . env)
                   in (org-element-map (org-element-parse-buffer) 'latex-environment
                        (lambda (env)
                          (cons (org-element-property :begin env)
                                (org-element-property :value env))))
                   collect
                   (cond
                    ;; equation 环境增加一个编号
                    ((and (string-match "\\\\begin{equation}" env)
                          (not (string-match "\\\\tag{" env)))
                     (cl-incf counter)
                     (cons begin counter))
                    ;; align 至少增加一个编号
                    ((string-match "\\\\begin{align}" env)
                     (prog1 (cons begin (cl-incf counter)) ; 先记录当前编号再计算内部换行
                       (with-temp-buffer
                         (insert env)
                         (goto-char (point-min))
                         ;; 每个 '\\' 换行符增加一个编号
                         (cl-incf counter (count-matches "\\\\"))
                         ;; 每个 '\nonumber' or '\notag' 减少一个编号
                         (goto-char (point-min))
                         (cl-decf counter (count-matches "\\\\nonumber"))
                         (goto-char (point-min))
                         (cl-decf counter (count-matches "\\\\notag")))))
                    (t
                     (cons begin nil)))))
    ;; 2. 如果当前光标处公式需要编号，则修改传递给LaTeX 代码
    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             ;; 注入 \setcounter 命令，将计数器设置为正确的编号
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))
  ;; 3. 调用原始的预览生成函数
  (apply orig-func args))
(advice-add 'org-create-formula-image
            :around #'my/org-renumber-latex-environment)

(provide '+eqt)

;;; +eqt.el
