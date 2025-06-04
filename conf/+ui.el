;;; Personal UI Settings for Doom Emacs
;;; Code:

;; Doom 主题设置
(setq doom-theme 'doom-acario-light)

;; 字体设置
(setq main-font-size 18 )

(defun font-exists-p (font-family)
  "Check if the specified font family exists in the system."
  (member font-family (font-family-list)))

(let ((default-font "Hack Nerd Font")
      (variable-font "思源黑体 CN")
      (serif-font "CodeNewRoman Nerd Font"))
  (when (font-exists-p default-font)
    (setq doom-font (font-spec :family default-font :size main-font-size :weight 'regular)))
  (when (font-exists-p variable-font)
    (setq doom-variable-pitch-font (font-spec :family variable-font :size main-font-size :weight 'regular)))
  (when (font-exists-p serif-font)
    (setq doom-serif-font (font-spec :family serif-font :size main-font-size :weight 'light)
          doom-big-font (font-spec :family serif-font :size main-font-size :weight 'regular))))

;; 显示相对行号
(setq display-line-numbers-type 'relative)  ; nil 为关闭，t 为默认行号

;; 窗口居中
(add-to-list 'initial-frame-alist
             '(top . 0.3))
(add-to-list 'initial-frame-alist
             '(left . 0.3))

;; 窗口大小
(pushnew! initial-frame-alist '(width . 150) '(height . 40))

;; 退出插入模式时禁用光标移动
;;(setq evil-move-cursor-back nil)

;;; Org-mode 相关设置
(setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
(setq org-ellipsis "⬇️")

;; 代码块和符号美化
(setq prettify-symbols-alist '(("#+BEGIN_SRC" . "")
                              ("#+END_SRC" . "†")
                              ("#+begin_src" . "†")
                              ("#+end_src" . "†")
                              (">=" . "≥")
                              ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Org-mode 钩子
(add-hook 'org-mode-hook
          (lambda ()
            (prettify-symbols-mode 1)
            (org-num-mode 1)))

;;; Module End
(message "Personal UI settings have been loaded")
(provide '+ui)
