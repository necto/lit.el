;;; lit-parse.el --- Parse lit-tester spec comments -*- lexical-binding: t; -*-
;; Author: Arseniy Zaostrovnykh <arseniy.zaostrovnykh@sonarsource.com>

;; Created: 25.06.23

;;; Commentary:
;;; Code:

(defun lit-parse--line-offset-parse (line-offset-str)
  "Parse the (smart) offset specification.
It understands nil, for :same,
+N, -N, for regular relative offsets
+, - for smart offsets."
  (cond ((null line-offset-str) '(:same))
        ((string-match "^ *\\+$" line-offset-str) '(:smart-next))
        ((string-match "^ *-$" line-offset-str) '(:smart-prev))
        (t (let ((offset (string-to-number line-offset-str)))
             (if (< 0 offset)
                 `(:next ,offset)
               `(:prev ,(- offset)))))))

(defconst lit-parse--regular-spec-src-regex
  "// \\([A-Z]+\\)\\(\\[[^]]+\\]\\)?\\( +\\)\\([+-][0-9]*\\)?:\\([0-9]+\\) ?\\([+-][0-9]*\\)?:\\([0-9]+\\)"
  "Regex for CHECK, SECONDARY, and DATAFLOW specs.")
(defconst lit-parse--regular-spec-indices
  '( :keyword 1
     :begin-line-offset 4
     :begin-col 5
     :end-line-offset 6
     :end-col 7)
  "Indices of the matched regex corresponding to different semantic elements of the regular spec.")
(defconst lit-parse--edit-src-regex "// \\(EDIT\\) [^ ]+ \\([+-][0-9]*\\)?:\\([0-9]+\\) ?\\([+-][0-9]*\\)?:\\([0-9]+\\)"
  "Regex for EDIT specs")
(defconst lit-parse--edit-indices
  '( :keyword 1
     :begin-line-offset 2
     :begin-col 3
     :end-line-offset 4
     :end-col 5)
  "Indices of the matched regex corresponding to different semantic elements of the EDIT spec.")
(defconst lit-parse--other-spec-src-regex "// \\(FIX\\|DATAFLOW DESCRIPTION\\|COMMENT\\|NOEXECFLOW\\|NOFIX\\)"
  "Regex for all the specs that do not feature range to highlight.
Needed to properly skip lines when resolving smart offsets.")

(defun lit-parse--string-match-spec (line)
  "Match `LINE' against all the spec regexes.
Returns nil for no match,
  or a plist with :offset of the match and :indices where applicable."
  (if-let ((offset (string-match lit-parse--regular-spec-src-regex line)))
      `(:offset ,offset :indices ,lit-parse--regular-spec-indices)
    (if-let ((offset (string-match lit-parse--edit-src-regex line)))
        `(:offset ,offset :indices ,lit-parse--edit-indices)
      (if-let ((offset (string-match lit-parse--other-spec-src-regex line)))
          `(:offset ,offset :indices nil)
        nil))))

(defun lit-parse-is-pure-lit-spec (line)
  "Check if `LINE' a pure spec line: contains only spec data or whitespace. "
  (if-let ((offset-indices (lit-parse--string-match-spec line)))
      (string-match "^ *$" (substring line 0 (plist-get offset-indices :offset)))
    nil))

(defun lit-parse-get-lit-spec-range (spec-line)
  "Parse a lit specification string SPEC-LINE and return the property-list.
The returned list specifies the relative target range offsets,
keyword column range and range-spec column range."
  (if-let ((offset-indices (lit-parse--string-match-spec spec-line)))
      (if-let ((indices (plist-get offset-indices :indices)))
          (let ((keyword-begin-col (match-beginning (plist-get indices :keyword)))
                (keyword-end-col (match-end (plist-get indices :keyword)))
                (range-spec-begin-col (or (match-beginning (plist-get indices :begin-line-offset))
                                          (1- (match-beginning (plist-get indices :begin-col)))))
                (range-spec-end-col (match-end (plist-get indices :end-col)))
                (begin-line-offset-str (match-string (plist-get indices :begin-line-offset) spec-line))
                (begin-col-offset-str (match-string (plist-get indices :begin-col) spec-line))
                (end-line-offset-str (match-string (plist-get indices :end-line-offset) spec-line))
                (end-col-offset-str (match-string (plist-get indices :end-col) spec-line)))
            (let ((begin-line-offset (lit-parse--line-offset-parse begin-line-offset-str))
                  (begin-col-offset (string-to-number begin-col-offset-str))
                  (end-line-offset (lit-parse--line-offset-parse end-line-offset-str))
                  (end-col-offset (string-to-number end-col-offset-str)))
              `( :begin-line-offset ,begin-line-offset
                 :begin-col ,begin-col-offset
                 :end-line-offset ,end-line-offset
                 :end-col ,end-col-offset
                 :keyword-begin-col ,keyword-begin-col
                 :keyword-end-col ,keyword-end-col
                 :range-spec-begin-col ,range-spec-begin-col
                 :range-spec-end-col ,range-spec-end-col))))
    nil))

(defconst lit-parse--tester-filename-regex "^ */[^:.\"']+\\.[^:.\"']+$"
  "Matches the filename-only line that precedes every issue in tester output.")

(defun lit-parse--match-filename (first-line)
  (if (string-match lit-parse--tester-filename-regex first-line)
      (match-string 0 first-line)
    nil))

(defconst lit-parse--tester-primary-regex "\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\) \\([^:]+\\):\\(.*\\)")

(defun lit-parse--primary-spec (spec-line)
  (if (string-match lit-parse--tester-primary-regex spec-line)
      (let ((begin-line (string-to-number (match-string 1 spec-line)))
            (begin-col (string-to-number (match-string 2 spec-line)))
            (end-line (string-to-number (match-string 3 spec-line)))
            (end-col (string-to-number (match-string 4 spec-line)))
            (rule-id (match-string 5 spec-line))
            (message (match-string 6 spec-line)))
        `( :rule-id ,rule-id
           :primary ( :begin (:line ,begin-line :col ,begin-col)
                      :end (:line ,end-line :col ,end-col)
                      :message ,message)))
    nil))

(defconst lit-parse--tester-secondary-regex
  "\\(/[^:]+\\):    \\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\(.*\\)")
(defconst lit-parse--tester-d-dataflows-regex "[0-9]+ data flows:")
(defconst lit-parse--tester-dataflow-descr-regex "// DATAFLOW DESCRIPTION:\\(.*\\)$")
(defconst lit-parse--tester-fix-descr-regex "// FIX \\(.*\\)$")
(defconst lit-parse--tester-edit-regex "// EDIT \\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\) `\\([^`]*\\)`")

(defun lit-parse--secondary-spec (spec-line)
  (if (string-match lit-parse--tester-secondary-regex spec-line)
      (let ((filename (match-string 1 spec-line))
            (begin-line (string-to-number (match-string 2 spec-line)))
            (begin-col (string-to-number (match-string 3 spec-line)))
            (end-line (string-to-number (match-string 4 spec-line)))
            (end-col (string-to-number (match-string 5 spec-line)))
            (message (match-string 6 spec-line)))
        `( :filename ,filename
           :secondary ( :begin (:line ,begin-line :col ,begin-col)
                        :end (:line ,end-line :col ,end-col)
                        :message ,message)))
    nil))

(defun lit-parse--split-into-secondary-dataflow-fix-specs (lines)
  (let ((secondary-specs '())
        (dataflow-specs '())
        (fix-specs '())
        (now-collecting :secondaries))
    (dolist (line lines)
      (cl-case now-collecting
        (:secondaries
         (if (string-match-p lit-parse--tester-d-dataflows-regex line)
             ;; This particular line is useless
             (setq now-collecting :dataflows)
           (if (string-match-p lit-parse--tester-fix-descr-regex line)
               (progn
                 (setq now-collecting :fixes)
                 (push line fix-specs))
             (push line secondary-specs))))
        (:dataflows
         (if (string-match-p lit-parse--tester-fix-descr-regex line)
             (progn
               (setq now-collecting :fixes)
               (push line fix-specs))
           (push line dataflow-specs)))
        (:fixes
         (if (string-match-p lit-parse--tester-d-dataflows-regex line)
             ;; This particular line is useless
             (setq new-collecting :dataflows)
           (push line fix-specs)))
        (t (error "Unexpected state %s" now-collecting))))
    (list (reverse secondary-specs)
          (reverse dataflow-specs)
          (reverse fix-specs))))

(defun lit-parse--not-dataflow-descr (l)
  (not (string-match-p lit-parse--tester-dataflow-descr-regex l)))

(defun lit-parse--dataflows (dataflow-specs)
  (let ((dataflows '())
        (dataflow-specs dataflow-specs))
    (while dataflow-specs
      (let ((it-is-descr (string-match lit-parse--tester-dataflow-descr-regex (car dataflow-specs))))
        (cl-assert it-is-descr)
        (let* ((description (match-string 1 (car dataflow-specs)))
               (step-specs (seq-take-while #'lit-parse--not-dataflow-descr (cdr dataflow-specs)))
               (steps (mapcar #'lit-parse--secondary-spec step-specs)))
          (setq dataflow-specs (seq-drop-while #'lit-parse--not-dataflow-descr (cdr dataflow-specs)))
          (push `( :description ,description
                   :steps ,(mapcar (lambda (step) (plist-get step :secondary)) steps))
                dataflows))))
    (reverse dataflows)))

(defun lit-parse--not-fix-descr (l)
  (not (string-match-p lit-parse--tester-fix-descr-regex l)))

(defun lit-parse--edit-spec (spec-line)
  (if (string-match lit-parse--tester-edit-regex spec-line)
      (let ((begin-line (string-to-number (match-string 1 spec-line)))
            (begin-col (string-to-number (match-string 2 spec-line)))
            (end-line (string-to-number (match-string 3 spec-line)))
            (end-col (string-to-number (match-string 4 spec-line)))
            (message (match-string 5 spec-line)))
        `( :begin (:line ,begin-line :col ,begin-col)
           :end (:line ,end-line :col ,end-col)
           :message ,message))
    nil))

(defun lit-parse--fixes (fix-specs)
  (let ((fixes '())
        (fix-specs fix-specs))
    (while fix-specs
      (let ((it-is-descr (string-match lit-parse--tester-fix-descr-regex (car fix-specs))))
        (cl-assert it-is-descr)
        (let* ((description (match-string 1 (car fix-specs)))
               (edit-specs (seq-take-while #'lit-parse--not-fix-descr (cdr fix-specs)))
               (edits (mapcar #'lit-parse--edit-spec edit-specs)))
          (cl-assert (cl-every #'consp edits)) ; All edits are parsed
          (setq fix-specs (seq-drop-while #'lit-parse--not-fix-descr (cdr fix-specs)))
          (push `( :description ,description :edits ,edits)
                fixes))))
    (reverse fixes)))

(defun lit-parse--join-multilines (lines)
  (let ((joined '())
        (accumulated-multiline ""))
    (dolist (line lines)
      (if (string-suffix-p "\\" line)
          (setq accumulated-multiline (concat accumulated-multiline line "\n"))
        (push (concat accumulated-multiline line) joined)
        (setq accumulated-multiline "")))
    (unless (string-empty-p accumulated-multiline)
      (push accumulated-multiline joined))
    (reverse joined)))

(defun lit-parse--1-observed (observed-report)
  "Parse the observed issue report as printed by tester"
  (let ((lines (lit-parse--join-multilines (split-string observed-report "\n" t))))
    (if-let ((filename (string-trim-left (lit-parse--match-filename (car lines))))
             (primary-spec (lit-parse--primary-spec (cadr lines))))
        (cl-destructuring-bind (secondary-specs dataflow-specs fix-specs)
            (lit-parse--split-into-secondary-dataflow-fix-specs (cddr lines))
          (let ((secondaries (mapcar #'lit-parse--secondary-spec secondary-specs))
                (dataflows (lit-parse--dataflows dataflow-specs))
                (fixes (lit-parse--fixes fix-specs)))
            (cl-assert (cl-every #'consp secondaries)) ; All secondaries parsed
            (mapc (lambda (secondary) (cl-assert (equal filename (plist-get secondary :filename))))
                  secondaries)
            `( :file ,filename
               :rule-id ,(plist-get primary-spec :rule-id)
               :primary ,(plist-get primary-spec :primary)
               :secondaries ,(mapcar (lambda (secondary) (plist-get secondary :secondary)) secondaries)
               :dataflows ,dataflows
               :fixes ,fixes)))
      (print "failed")
      nil)))

(defun lit-parse--chop-string (string separator)
  "Same as (split-string STRING separator) but preserves the separators in the cut strings.
Modifies MATCH DATA."
  (let ((result '())
        (begin-cur-str 0)
        (end-last-match 0))
    (while (when-let ((end-cur-str (string-match separator string end-last-match)))
      (push (substring string begin-cur-str end-cur-str) result)
      (setq begin-cur-str end-cur-str
            end-last-match (match-end 0))))
    (push (substring string begin-cur-str) result)
    (setq result (reverse result))
    (if (and (string-empty-p (car result)) (cdr result))
        (cdr result)
      result)))

(defun lit-parse-all-observed (observed-multiisue-report)
  "Parse the observed report of multiple issues as printed by tester"
  (let* ((observed-single-reports (lit-parse--chop-string observed-multiisue-report lit-parse--tester-filename-regex))
         (issue-specs (mapcar #'lit-parse--1-observed observed-single-reports)))
    ;; Make sure all issues parsed
    (dolist (issue-spec issue-specs) (cl-assert issue-spec))
    issue-specs))

(provide 'lit-parse)
;;; lit-parse.el ends here
