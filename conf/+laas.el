;; (add-to-list 'load-path "~/.emacs.d/site-lisp/LaTeX-auto-activating-snippets")

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :hook (org-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'org-inside-LaTeX-fragment-p
                    ;; bind to functions!
                    ;;snippets
                    ;;Ïmy snippets
                    ";s" "\\sigma"
                    ";a" "\\alpha"
                    "eps" "\\varepsilon"
                    "pi" "\pi"
                    ;; upscri downscri
                    "tp" (lambda () (interactive)
                            (yas-expand-snippet "^{$1} $0"))
                    "td" (lambda () (interactive)
                            (yas-expand-snippet "_{$1} $0"))
                    ;;
                    "sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    "lim" (lambda () (interactive)
                            (yas-expand-snippet "\\lim_{$1}^{$2} $0"))
                    "sqrt" (lambda () (interactive)
                             (yas-expand-snippet "\\sqrt{$1} $0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "hsq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))


(message "laas has been loaded")
(provide '+laas)
