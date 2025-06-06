;;; Personal UI Settings for Doom Emacs
;;; Code:

;; Doom 主题设置
(setq doom-theme 'doom-acario-light)

;; 字体设置
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
;; Maple Mono Normal NF CN; Sarasa Term SC Nerd
(defvar my-en-font "Sarasa Term SC Nerd"
  "Default English font family.")
(defvar my-cn-font "Sarasa Term SC Nerd"
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

;; 软换行
(global-visual-line-mode t)

;;; Module End
(message "Personal UI settings have been loaded")
(provide '+ui)
