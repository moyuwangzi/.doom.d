;;; conf/+star.el -*- lexical-binding: t; -*-


;; 代码块和符号美化
;; (setq prettify-symbols-unprettify-at-point 'right-edge)
;; 关闭modern 包对superstar 的影响
(setq org-modern-todo nil
      org-modern-list nil
      org-modern-hide-stars nil
      org-modern-checkbox nil
      org-modern-star nil
      ;; org-modern-star '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" )
      org-modern-block-name '("" . "")
      ;; org-modern-timestamp nil
      ;; org-modern-keyword '(("" . ""))
      )

;; (after! org
;;   (setq org-startup-indented nil)
;;   (remove-hook 'org-mode-hook #'org-indent-mode)
;;   )
;; org-modern-indent 带缩进的modern 包
(setq org-startup-indented t)
(use-package! org-modern-indent
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  )
;; Org-mode 钩子
(require 'org-superstar)
(add-hook 'org-mode-hook
          (lambda ()
            (prettify-symbols-mode t)
            (org-num-mode t)
            (org-superstar-mode t)
            ;; (org-modern-indent-mode t)
            ;; (+org-pretty-mode t)
            ))

;;; Org-mode 相关设置
;; (with-eval-after-load 'org-superstar
  ;; (set-face-attribute 'org-superstar-item nil :height 1.2)
  ;; (set-face-attribute 'org-superstar-header-bullet nil :height 1.5)
  ;；(set-face-attribute 'org-superstar-leading nil :height 1.3)
  ;; )
(setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" ))
(setq org-superstar-cycle-headline-bullets t)
(setq org-superstar-item-bullet-alist
      '(
        (?* . #x25C9) ; * -> 带圈圈的实心圆点
        (?+ . #x27A4) ; + -> 立体箭头
        (?- . #x21E2) ; - -> 虚线箭头
        )
      )
(setq org-ellipsis "...️")
(setq org-superstar-leading-fallback ?\s)
;;|标题字体设置
(set-face-attribute 'outline-1 nil :height 1.8 )
  (set-face-attribute 'outline-2 nil :height 1.6 )
  (set-face-attribute 'outline-3 nil :height 1.4 )
  (set-face-attribute 'outline-4 nil :height 1.3 )
  (set-face-attribute 'outline-5 nil :height 1.2 )
  (set-face-attribute 'outline-6 nil :height 1.1 )
  (set-face-attribute 'org-document-title nil :height 2.5 :bold t)
  (set-face-attribute 'org-document-info nil :height 1.8 :bold t)
  (set-face-attribute 'org-document-info-keyword nil
     :inherit 'org-document-info)
  (set-face-attribute 'org-block nil
    :extend t :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil
                      :background (face-background 'default) :height 0.9 :inherit nil)

;； end
(provide '+star)
