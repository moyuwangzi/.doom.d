;;; Code:
;;(use-package! org-elp
  ;;;;:after org  ; 确保在 org-mode 加载后再加载 org-elp
  ;;:hook (org-mode . org-elp-mode)  ; 自动在 org-mode 中启用 org-elp-mode
  ;;:config
  ;;;; 自定义 org-elp 的设置
  ;;(setq org-elp-split-fraction 0.2)  ; 设置分割窗口的比例为 0.2
  ;;(setq org-elp-buffer-name "*temp equation*")  ; 设置渲染缓冲区名称
  ;;(setq org-elp-idle-time 0.5)  ; 设置空闲延迟时间为 0.5 秒
  ;;;; 定义快捷键，使用 Doom 的 map! 宏
  ;;)
;;
;;;; 定义快捷键，使用 Doom 的 map! 宏
;;(map! :map org-elp-mode-map
      ;;:leader
      ;;:prefix ("o" . "org")
      ;;:desc "Toggle org-elp mode" "e" #'org-elp-mode)

(use-package! xenops
  :after (org latex)
         )

;;检查是否存在main-font-size
(if (boundp 'my-main-font-size)
    (setq xenops-math-image-scale-factor (/ my-main-font-size 24.0))
  (progn
    (message "Warning: main-font-size not defined, using default value 12")
    (setq xenops-math-image-scale-factor 1.0 )))

;; (add-hook 'org-mode-hook #'xenops-mode 'append)
;; (after! (org latex)
;;   (add-hook 'org-mode-hook #'xenops-mode)
;;   (add-hook 'latex-mode-hook #'xenops-mode)
;;   (add-hook 'LaTeX-mode-hook #'xenops-mode)
;; )
;; 与org 的其他插件冲突似乎，加一个快捷键来手动触发吧
(map! :leader
      :mode (org-mode latex-mode)
      "e e" #'xenops-mode
      "e r" #'xenops-regenerate
      )
;; (remove-hook 'org-mode-hook #'xenops-mode)
;; (remove-hook 'latex-mode-hook #'xenops-mode)
(provide '+eqt)

;;; +eqt.el
