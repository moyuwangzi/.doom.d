;;; Personal UI Settings for Doom Emacs
;;; Code:

;; Doom 主题设置
(setq doom-theme 'doom-acario-light)

;; 字体设置
;;(defun font-exists-p (font-family)
;;  "Check if the specified font family exists in the system."
;;  (member font-family (font-family-list)))

;; (let (
      ;; (default-font "Maple Mono Normal NF CN")
      ;; (variable-font "Maple Mono Normal NF CN")
      ;; (serif-font "Maple Mono Normal NF CN")
      ;; (default-font "Sarasa Term SC Nerd")
      ;; (variable-font "Sarasa Term SC Nerd")
      ;; (serif-font "Sarasa Term SC Nerd")
      ;; (default-font "Hack Nerd Font")
      ;; (variable-font "思源黑体 CN")
      ;; (serif-font "CodeNewRoman Nerd Font")
  ;;     )
  ;; (when (font-exists-p default-font)
  ;;   (setq doom-font (font-spec :family default-font :size main-font-size :weight 'regular)))
  ;; (when (font-exists-p variable-font)
  ;;   (setq doom-variable-pitch-font (font-spec :family variable-font :size main-font-size :weight 'regular)))
  ;; (when (font-exists-p serif-font)
  ;;   (setq doom-serif-font (font-spec :family serif-font :size main-font-size :weight 'light)
  ;;         doom-big-font (font-spec :family serif-font :size main-font-size :weight 'bold)))
;; )
;; 参考别人的字体设置顺序
;; 字体存在性检查函数
(defvar cached-font-families nil
  "Cached list of font families to avoid repeated calls to `font-family-list`.")
(defun font-exists-p (font-family)
  "Check if the specified font family exists in the system.
FONT-FAMILY is the name of the font family to check.
Returns t if the font exists, nil otherwise."
  (unless cached-font-families
    (setq cached-font-families (font-family-list)))
  (or (find-font (font-spec :family font-family))
      (member font-family cached-font-families)))

;; 定义字体名称作为全局变量
(defvar my-en-font "Maple Mono Normal NF CN"
  "Default English font family.")
(defvar my-cn-font "Maple Mono Normal NF CN"
  "Default Chinese font family.")
(defvar my-sym-font "Symbola"
  "Default symbol font family.")
(defvar my-main-font-size 18
  "Default font size.")

;; 检查并设置英文字体
(when (font-exists-p my-en-font)
  (setq doom-font (font-spec :family my-en-font :size my-main-font-size))
  (setq doom-serif-font doom-font)
  (message "English font %s applied with size %d." my-en-font my-main-font-size))
;; 检查并设置中文字体
(when (font-exists-p my-cn-font)
  (setq doom-variable-pitch-font (font-spec :family my-cn-font :weight 'bold))
  (message "Chinese font %s applied." my-cn-font))

;; 如果不把这玩意设置为 nil, 会默认去用 fontset-default 来展示, 配置无效
(setq use-default-font-for-symbols nil)

;; Doom 的字体加载顺序问题, 如果不设定这个 hook, 配置会被覆盖失效
(add-hook! 'after-setting-font-hook
  ;; 英文字符集 (latin)
  (when (font-exists-p my-en-font)
    (set-fontset-font t 'latin (font-spec :family my-en-font))
    (message "English font %s applied for Latin characters via hook." my-en-font))
  ;; 符号、数学符号和表情字符集
  (when (font-exists-p my-sym-font)
    (set-fontset-font t 'symbol (font-spec :family my-sym-font))
    (set-fontset-font t 'mathematical (font-spec :family my-sym-font))
    (set-fontset-font t 'emoji (font-spec :family my-sym-font))
    (message "Symbol font %s applied via hook." my-sym-font))
  ;; 中文相关字符集 (han, cjk-misc, kana)
  (when (font-exists-p my-cn-font)
    (set-fontset-font t 'han (font-spec :family my-cn-font))
    (set-fontset-font t 'cjk-misc (font-spec :family my-cn-font))
    (set-fontset-font t 'kana (font-spec :family my-cn-font))
    (message "chinese font %s applied for cjk characters via hook." my-cn-font))
)

;; 显示相对行号
;;(setq display-line-numbers-type 'relative)
(setq display-line-numbers-type 't)  ; nil 为关闭，t 为默认行号

;; 窗口居中
(add-to-list 'initial-frame-alist
             '(top . 0.3))
(add-to-list 'initial-frame-alist
             '(left . 0.3))

;; 窗口大小
(pushnew! initial-frame-alist '(width . 100) '(height . 30))

;; 退出插入模式时禁用光标移动
(setq evil-move-cursor-back nil)


;; 代码块和符号美化
(setq prettify-symbols-alist '(("#+BEGIN_SRC" . "")
                              ("#+END_SRC" . "†")
                              ("#+begin_src" . "†")
                              ("#+end_src" . "†")
                              (">=" . "≥")
                              ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Org-mode 钩子
(require 'org-superstar)
(add-hook 'org-mode-hook
          (lambda ()
            (prettify-symbols-mode 1)
            (org-num-mode 1)
            (org-superstar-mode 1)
            ))

;;; Org-mode 相关设置
(with-eval-after-load 'org-superstar
  (set-face-attribute 'org-superstar-item nil :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  (set-face-attribute 'org-superstar-leading nil :height 1.3))
(setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" ))
(setq org-superstar-cycle-headline-bullets nil)
(setq org-superstar-item-bullet-alist
      '(
        (?* . #x25C9) ; * -> 带圈圈的实心圆点
        (?+ . #x27A4) ; + -> 立体箭头
        (?- . #x21E2) ; - -> 虚线箭头
        )
      )
(setq org-ellipsis "⬇️")
(setq org-superstar-leading-fallback ?\s)

;;; Module End
(message "Personal UI settings have been loaded")
(provide '+ui)
