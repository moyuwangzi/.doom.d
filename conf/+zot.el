;;; conf/+zot.el -*- lexical-binding: t; -*-

(setq zot_bib '("~/Document/zotero/review.bib" "~/Document/zotero/all_paper.bib" "~/Document/zotero/book.bib");; zotero 的 bib 文件信息
      org_roam  "~/Document/roam";; roam 放置的默认文件夹
      zot_pdf "~/Document/pdfs") ;; PDF 放置的默认文件夹

(use-package! helm-bibtex
  :custom
  (bibtex-completion-notes-path org_roam)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf)
  )

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (("C-c n k" . orb-insert-link) ;; 插入文件笔记
         ("C-c n a" . orb-note-action)) ; 打开操作菜单
  :custom
  (orb-insert-interface 'helm-bibtex) ; 和上面的保持一致
  (orb-insert-link-description 'citekey) ; 插入的连接描述，默认是title
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf"))
  )
; org-roam 笔记模板
(setq ref-template
(concat "#+FILETAGS: reading research \n"
        "- tags :: %^{keywords} \n"
        "* %^{title}\n"
        ":PROPERTIES:\n"
        ":Custom_ID: %^{citekey}\n"
        ":URL: %^{url}\n"
        ":AUTHOR: %^{author-or-editor}\n"
        ":NOTER_DOCUMENT: ~/Document/pdfs/%^{title}.pdf\n"
        ":NOTER_PAGE:\n"
        ":END:"))
(add-to-list 'org-roam-capture-templates
        `("r" "Zotero template" plain
                ,ref-template
                :target
                (file+head "roam/${title}.org" "#+title: ${title}\n")
                ))
