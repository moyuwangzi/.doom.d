(require 'rime)
(setq default-input-method "rime")
;;; 详细说明请看 https://github.com/DogLooksGood/emacs-rime
;;; 將霧凇拼音git到下面doom文件夾
;; (setq rime-user-data-dir "~/.doom.d/rime-ice")
;; 或者和系统公用一个配置
(setq rime-user-data-dir "~/.local/share/fcitx5/rime")
;; 样式设置
(setq rime-show-candidate 'posframe) ;; 跟随展示
;;(setq rime-posframe-style 'horizontal) ;;水平显示，垂直选vertical
(setq rime-posframe-style 'vertical)
;; 自动切换中英文
(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-tex-math-or-command-p
        rime-predicate-space-after-cc-p
        rime-predicate-current-uppercase-letter-p
        rime-predicate-org-latex-mode-p
        rime-predicate-prog-in-code-p))
;;; 具体参考 mode-line-mule-info 默认值，其中可能有其它有用信息
(setq mode-line-mule-info '((:eval (rime-lighter))))
;;; 基于 Rime inline ascii 模式的临时英文
;; (setq rime-inline-predicates
;;       '(
;;         rime-predicate-current-uppercase-letter-p
;;         rime-predicate-tex-math-or-command-p))
;;; support shift-l, shift-r, control-l, control-r
(setq rime-inline-ascii-trigger 'shift-l)
(define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
(define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)

;;       测试输入法傻人有傻福，傻B没有,配钥匙，三块钱一把，十块钱三把，你配几把

(message "rime配置成功")
(provide '+rime)
