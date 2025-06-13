;;; conf/+svg-tag.el -*- lexical-binding: t; -*-


(require 'svg-tag-mode)

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (save-match-data
   (svg-image (svg-lib-concat
               (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                 nil :margin 0 :stroke 2 :radius 6 :padding 2 :width 11)
               (svg-lib-tag (concat value "%")
                            nil :stroke 0 :margin 0)) :ascent 'center)))

(defun svg-progress-count (value)
  (save-match-data
    (let* ((seq (split-string value "/"))
           (count (if (stringp (car seq))
                      (float (string-to-number (car seq)))
                    0))
           (total (if (stringp (cadr seq))
                      (float (string-to-number (cadr seq)))
                    1000)))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 6 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center))))

(setq svg-tag-tags
      `(
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority
                                            :beg 2 :end -1 :margin 0))))

        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
        ;; Meta line and duc info
        ("\\#\\+TITLE:" . ((lambda (tag) (svg-tag-make "Title" :face 'org-document-title))))
        ("\\#\\+AUTHOR:" . ((lambda (tag) (svg-tag-make "Author" :face 'org-document-info-keyword))))
        ("\\#\\+EMAIL:" . ((lambda (tag) (svg-tag-make "Email" :face 'org-document-info-keyword))))
        ("\\#\\+STARTUP:" . ((lambda (tag) (svg-tag-make "Startup" :face 'org-meta-line))))
        ("\\#\\+RESULTS:" . ((lambda (tag) (svg-tag-make "output" :face 'org-meta-line))))

        ("\\(\\#\\+begin_[A-Za-z]+\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :margin 0
                                                        :beg 8
                                                        :inverse t
                                                        :face 'org-block-end-line
                                                        ))))
        ;;
        ("\\(\\#\\+end_[A-Za-z]+\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :margin 0
                                                        :beg 6
                                                        :inverse t
                                                        :face 'org-block-end-line))))
        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))


        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

        ;; ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))
        ))


;; (svg-tag-mode t)
(add-hook 'org-mode-hook #'svg-tag-mode)

(map! :leader
      :mode (org-mode)
      "e s" #'svg-tag-mode)
