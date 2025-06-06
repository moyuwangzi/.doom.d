;;; conf/+star.el -*- lexical-binding: t; -*-

;;|标题字体设置
(defun my/set-org-custom-faces ()
  "change the font size of org mode"
  (let ((faces '((org-level-1 . 1.4)
                 (org-level-2 . 1.3)
                 (org-level-3 . 1.2)
                 (org-level-4 . 1.1)
                 (org-level-5 . 1.0)
                 (org-level-6 . 1.0)
                 (org-level-7 . 1.0)
                 (org-level-8 . 1.0)
                 ;; (org-document-info-keyword . 1.45)
                 (org-document-title . 1.45)
                 (org-document-info . 1.2)
                 ;; (org-meta-line . 1.45)
                 ;; (org-list-dt . 1.35)
                 )))
    (dolist (face faces)
      (set-face-attribute (car face) nil :height (cdr face)))
    (set-face-attribute 'org-block nil :background 'unspecified)))

(defun my/refresh-org-buffer ()
  "reflash the buffer"
  (font-lock-flush)
  (redisplay))

(defun my/org-custom-face-setup ()
  "..."
  (my/set-org-custom-faces)
  ;;
  (run-at-time 0 nil #'my/refresh-org-buffer))

;;add a hook
(add-hook 'org-mode-hook #'my/org-custom-face-setup 100)

;; 代码块和符号美化
;; 关闭modern 包对superstar 的影响
(after! org
  (remove-hook 'org-mode-hook #'org-modern-mode))

;; 使用svg-tag-mode
(load! "+svg-tag.el")

;; org-modern-indent 带缩进的modern 包
(setq org-startup-indented t)
(use-package! org-modern-indent
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  )
;
(with-eval-after-load 'org-modern-indent
  (let ((vars '((org-modern-indent-begin   . "\u250C")  ;  ⎡23A1
                (org-modern-indent-guide   . "\u2502")  ;  ⎢23A2
                (org-modern-indent-end     . "\u2514")))) ;⎣23A3
    (dolist (var vars)
      (set (car var) (cdr var))
      (unless (eq (symbol-value (car var)) (cdr var))
        (message "change the block style failed: %S" (car var))))))
;; Org-mode 钩子
(require 'org-superstar)
(add-hook 'org-mode-hook
          (lambda ()
            (prettify-symbols-mode)
            (org-num-mode t)
            (org-superstar-mode t)
            ;; (org-modern-indent-mode t)
            ))

;;; Org-mode 相关设置
;; (with-eval-after-load 'org-superstar
  ;; (set-face-attribute 'org-superstar-item nil :height 1.2)
  ;; (set-face-attribute 'org-superstar-header-bullet nil :height 1.5)
  ;；(set-face-attribute 'org-superstar-leading nil :height 1.3)
  ;; )
;; (setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" ))
(setq org-superstar-headline-bullets-list '(#x2630 #x2631 #x2632  #x2633  #x2634  #x2635  #x2636   #x2637))
(setq org-superstar-cycle-headline-bullets t)
(setq org-superstar-item-bullet-alist
      '(
        (?* . #x269D) ; * -> 25C9带圈圈的实心圆点
        (?+ . #x27A2) ; + -> 立体箭头 27A2,27A3
        (?- . #x25C9) ; - -> 21E2虚线箭头
        )
      )
(setq org-ellipsis "...️")
(setq org-superstar-leading-fallback ?\s)

;; table modify
(use-package! org-pretty-table
  :config
  (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))
  (setq org-pretty-table-charset "\u250c\u2510\u2514\u2518\u252c\u2524\u2534\u251c\u253c\u2500\u2502") ;┌┐└┘┬┤┴├┼─│
  )

;； end
(provide '+star)
