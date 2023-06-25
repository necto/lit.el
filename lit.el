;;; lit.el --- Manipulate lit-tester spec comments -*- lexical-binding: t; -*-

;; Author: Arseniy Zaostrovnykh <arseniy.zaostrovnykh@sonarsource.com>

;; Created: 17.06.23

;;; Commentary:
;; Easy the manipulation of the lit-tester style specification comments.
;; Comment have the form // KEYWORD ...
;; E.g. // CHECK :1 :2 S100:Message
;; Insert, remove, and readjust them.
;; Defines two user-level functions:
;; * li-insert-issues
;; * li-delete-spec

;;; Code:


(defun lit-highlight-cur-spec-range ()
  "The main call of the mode. Should be called every time the point lands on a new line.
It identifies the range specification and highlights it in the same buffer."
  (if lit-mode
      (save-excursion
        (lit-move-to-start-of-multiline)
        (unless (and lit-last-line-highlihted
                     (= (line-number-at-pos) lit-last-line-highlihted))
          (setq lit-last-line-highlihted (line-number-at-pos))
          (if-let ((range-spec (lit-get-cur-lit-spec-range))
                   (target-range (lit-resolve-range-offsets range-spec))
                   (keyword-range (lit-resolve-keyword-range-offset range-spec)))
              (lit-add-range target-range keyword-range))))))

(defface lit-default-face
  `((((class color) (background light))
     (:background "yellow"))
    (((class color) (background dark))
     (:background "DodgerBlue1"))
    (t :inverse-video t))
  "Face used for most-recent lit-range highlight")

(defface lit-tail-face1
  `((((class color) (background light))
     (:background "gold1"))
    (((class color) (background dark))
     (:background "DodgerBlue2"))
    (t :inverse-video t))
  "Face used for 2nd-most-recent lit-range highlight")

(defface lit-tail-face2
  `((((class color) (background light))
     (:background "wheat2"))
    (((class color) (background dark))
     (:background "DodgerBlue3"))
    (t :inverse-video t))
  "Face used for 3rd-most-recent lit-range highlight")

(defconst lit-faces '(lit-default-face lit-tail-face1 lit-tail-face2)
  "List of all highlight faces used to highlight the N most-recent ranges")

(defvar lit-highlights '()
  "List of all highlight-overlay currently displayed")
(make-variable-buffer-local 'lit-highlights)

(defvar lit-last-line-highlihted nil
  "Last line that was highlighted.
Used to avoid duplicating highlight on the same line.
This happens with mouse-click events because
they coprise of mouse-down + mouse-up.")
(make-variable-buffer-local 'lit-last-line-highlihted)

(defun lit-add-range (target-range keyword-range)
  "Add highlight for at the position specified by `TARGET-RANGE' and `KEYWORD-RANGE'"
  (let* ((target-begin (plist-get target-range :begin))
         (target-end (max (1+ target-begin) (plist-get target-range :end)))
         (target-hl (lit---make-hl target-begin target-end nil 'lit-default-face))
         (keyword-hl (lit---make-hl (plist-get keyword-range :begin) (plist-get keyword-range :end) nil 'lit-default-face)))
    (push (cons target-hl keyword-hl) lit-highlights)
    (lit---remove-old-hl (length lit-faces))
    (lit-cycle-colors)))

(defun lit-cycle-colors ()
  "Cycle highlight colors to match the lit-faces list.
One color per highlighted range."
  (let ((priority (length lit-highlights)))
    (cl-mapcar (lambda (hls face)
                 (let ((hl1 (car hls)) (hl2 (cdr hls)))
                   (overlay-put hl1 'face face)
                   (overlay-put hl1 'priority priority)
                   (overlay-put hl2 'face face)
                   (overlay-put hl2 'priority priority)
                   (cl-decf priority)))
               lit-highlights
               lit-faces)))

(defun lit---make-hl (beg end buf face)
  "Make a highlight at the position specified by `BEG' and `END'."
  (let (hl)
    (setq hl (make-overlay beg end buf))
    (overlay-put hl 'face face)
    (overlay-put hl 'priority 1)
    hl))

(defun lit---remove-hl (hl)
  "Clear highlight."
  (when (overlayp hl)
    (delete-overlay hl)))

(defun lit---remove-old-hl (&optional max-hls)
  "Remove and deactivate all old highlights. Keep `MAX-HLS' newest highlights."
  (unless max-hls (setq max-hls (length (lit-faces))))
  (let ((oldest (reverse lit-highlights)))
    (dolist (highlight-pair (butlast oldest max-hls))
      (let ((target-hl (car highlight-pair))
            (keyword-hl (cdr highlight-pair)))
        (lit---remove-hl target-hl)
        (lit---remove-hl keyword-hl)))
    (setq lit-highlights (reverse (last oldest max-hls)))))

(defun lit-line-offset-parse (line-offset-str)
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

(defconst lit-regular-spec-src-regex "// \\([A-Z]+\\)\\(\\[[^]]+\\]\\)?\\( +\\)\\([+-][0-9]*\\)?:\\([0-9]+\\) ?\\([+-][0-9]*\\)?:\\([0-9]+\\)"
  "Regex for CHECK, SECONDARY, and DATAFLOW specs.")
(defconst lit-regular-spec-indices
  '( :keyword 1
     :begin-line-offset 4
     :begin-col 5
     :end-line-offset 6
     :end-col 7)
  "Indices of the matched regex corresponding to different semantic elements of the regular spec.")
(defconst lit-edit-src-regex "// \\(EDIT\\) [^ ]+ \\([+-][0-9]*\\)?:\\([0-9]+\\) ?\\([+-][0-9]*\\)?:\\([0-9]+\\)"
  "Regex for EDIT specs")
(defconst lit-edit-indices
  '( :keyword 1
     :begin-line-offset 2
     :begin-col 3
     :end-line-offset 4
     :end-col 5)
  "Indices of the matched regex corresponding to different semantic elements of the EDIT spec.")
(defconst lit-other-spec-src-regex "// \\(FIX\\|DATAFLOW DESCRIPTION\\|COMMENT\\|NOEXECFLOW\\|NOFIX\\)"
  "Regex for all the specs that do not feature range to highlight.
Needed to properly skip lines when resolving smart offsets.")

(defun lit-string-match-spec (line)
  "Match `LINE' agains all the spec regexes.
Returns nil for no match,
  or a plist with :offset of the match and :indices where applicable."
  (if-let ((offset (string-match lit-regular-spec-src-regex line)))
      `(:offset ,offset :indices ,lit-regular-spec-indices)
    (if-let ((offset (string-match lit-edit-src-regex line)))
        `(:offset ,offset :indices ,lit-edit-indices)
      (if-let ((offset (string-match lit-other-spec-src-regex line)))
          `(:offset ,offset :indices nil)
        nil))))

(defun lit-is-pure-lit-spec (line)
  "Check if `LINE'. I.e. contains only spec data or whitespace. "
  (if-let ((offset-indices (lit-string-match-spec line)))
      (string-match "^ *$" (substring line 0 (plist-get offset-indices :offset)))
    nil))

(defun lit-is-cur-line-pure-lit-spec ()
  "Check if current line is part of a pure specification
(i.e. contains no non-spec data other than whitespace).
Handles '\'-extended lines. "
  (save-excursion
    (lit-move-to-start-of-multiline)
    (lit-is-pure-lit-spec (thing-at-point 'line t))))

(defun lit-get-lit-spec-range (spec-line)
  "Parse a lit specification string SPEC-LINE and return the property-list
specifying the begin/end line and column offsets"
  (if-let ((offset-indices (lit-string-match-spec spec-line)))
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
            (let ((begin-line-offset (lit-line-offset-parse begin-line-offset-str))
                  (begin-col-offset (string-to-number begin-col-offset-str))
                  (end-line-offset (lit-line-offset-parse end-line-offset-str))
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

(defun lit-move-to-start-of-multiline ()
  "Move the point to the first line in a sequence of '\'-extended lines."
  (let ((ret (thing-at-point 'line t))
        (line-moved t))
    (while (and line-moved (< 1 (line-number-at-pos)))
      (forward-line -1)
      (let ((prev-line (thing-at-point 'line t)))
        (when (not (string-suffix-p "\\\n" prev-line))
            (setq line-moved nil)
            (forward-line))))))

(defun lit-get-cur-lit-spec-range ()
  "Get the parsed range specifiation from the current line if it contains one.
Return nil otherwise."
  (lit-get-lit-spec-range (thing-at-point 'line t)))

(defun lit-point-at-col-and-line (col line-offset)
  "Calculate the point if current position was shifted according to `LINE-OFFSET'.
And moved to the `COL' horisontal column."
  (save-excursion
    (cond ((equal (car line-offset) :prev) (forward-line (- 0 (cadr line-offset))))
          ((equal (car line-offset) :next) (forward-line (cadr line-offset)))
          ((equal (car line-offset) :smart-prev)
           (while (lit-is-cur-line-pure-lit-spec) (forward-line -1)))
          ((equal (car line-offset) :smart-next)
           (while (lit-is-cur-line-pure-lit-spec) (forward-line)))
          (t t))
    (move-to-column (- col 1))
    (point)))

(defun lit-resolve-range-offsets (range-spec)
  "Transform the parsed range specification `RANGE-SPEC'
into a plist of point positions :begin :end."
  (let ((begin (lit-point-at-col-and-line (plist-get range-spec :begin-col)
                                          (plist-get range-spec :begin-line-offset)))
        (end (lit-point-at-col-and-line (plist-get range-spec :end-col)
                                        (plist-get range-spec :end-line-offset))))
    `( :begin ,begin
       :end ,end)))

(defun lit-resolve-keyword-range-offset (range-spec)
  "Extract the column range of the keyword of the current spec from `RANGE-SPEC',
and transform it to the point values :begin and :end plist."
  (lit-resolve-range-offsets `( :begin-line-offset (:same) :begin-col ,(+ (plist-get range-spec :keyword-begin-col) 1)
                                :end-line-offset (:same) :end-col ,(+ (plist-get range-spec :keyword-end-col) 1))))

(defadvice evil-next-line (after lit-highlight-ranges-evil-next)
  (lit-highlight-cur-spec-range))

(defadvice next-line (after lit-highlight-ranges-next)
  (lit-highlight-cur-spec-range))

(defadvice previous-line (after lit-highlight-ranges-prev)
  (lit-highlight-cur-spec-range))

(defadvice evil-previous-line (after lit-highlight-ranges-evil-prev)
  (lit-highlight-cur-spec-range))

(defadvice mouse-set-point (after lit-highlight-ranges-mouse-set-point)
  (lit-highlight-cur-spec-range))

;;;###autoload
(define-minor-mode lit-mode
  "Highlight ranges referenced from lit specification comments"
  :lighter nil
  (lit---remove-old-hl 0))

(if (fboundp 'evil-next-line)
    (progn
      (ad-enable-advice 'evil-next-line 'after 'lit-highlight-ranges-evil-next)
      (ad-activate 'evil-next-line))
  (ad-enable-advice 'next-line 'after 'lit-highlight-ranges-next)
  (ad-activate 'next-line))

(if (fboundp 'evil-previous-line)
    (progn
      (ad-enable-advice 'evil-previous-line 'after 'lit-highlight-ranges-evil-prev)
      (ad-activate 'evil-previous-line))
  (ad-enable-advice 'previous-line 'after 'lit-highlight-ranges-prev)
  (ad-activate 'previous-line))

(ad-enable-advice 'mouse-set-point 'after 'lit-highlight-ranges-mouse-set-point)
(ad-activate 'mouse-set-point)


(provide 'lit-highlight-mode)

(defface lit-hint-preview '((t :foreground "dark green" :height .7 :extend t))
  "Face used for the preview generated around the target location when choosing the location of the spec.")

(defface lit-target-range '((t :background "yellow"))
  "Face used to highlight the target region for an insertion.")

(defface lit-currently-inserting '((t :slant italic :height 1.0 :weight bold :background "yellow"))
  "Face used to highlight the target region for an insertion.")

(defface lit-cancelled-line '((t :slant normal :weight normal
                                :height 1.0 :background nil :foreground "gray" :strike-through t))
  "Face used to highlight the target region for an insertion.")

(defface lit-processed-line '((t :weight normal :slant normal :height 1.0 :foreground "forest green"))
  "Face used to indicate that a line was already inserted.")

(defface lit-hint-keyword '((t :height .5 :box t))
  "Face used to distinguish the keywords prefixing the insertion options.")

(defface lit-hint-key '((t :height .5 :weight bold :box t))
  "Face used to distinguish the hotkey associated with the insertion-option keyword.")

(defun lit-record-current-line-if-dumb ()
  (if-let ((dumb-range-pair (lit-make-dumb-range-spec-overlay-pair)))
      (push dumb-range-pair lit-dumb-range-overlays)))

(defvar lit-dumb-range-overlays '()
  "List of pairs of overlays connecting a range specification with its target range.")
(make-local-variable 'lit-dumb-range-overlays)


(defconst lit-tester-filename-regex "^ */[^:.\"']+\\.[^:.\"']+$"
  "Matches the filename-only line that precedes every issue in tester output.")

(defun lit-match-filename (first-line)
  (if (string-match lit-tester-filename-regex first-line)
      (match-string 0 first-line)
    nil))

(defconst lit-tester-primary-regex "\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\) \\([^:]+\\):\\(.*\\)")

(defun lit-parse-primary-spec (spec-line)
  (if (string-match lit-tester-primary-regex spec-line)
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

(defconst lit-tester-secondary-regex "\\(/[^:]+\\):    \\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\(.*\\)")
(defconst lit-tester-d-dataflows-regex "[0-9]+ data flows:")
(defconst lit-tester-dataflow-descr-regex "// DATAFLOW DESCRIPTION:\\(.*\\)$")

(defun lit-parse-secondary-spec (spec-line)
  (if (string-match lit-tester-secondary-regex spec-line)
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

(defun lit-split-into-secondary-dataflow-fix-specs (lines)
  (let ((secondary-specs '())
        (dataflow-specs '())
        (fix-specs '())
        (now-collecting :secondaries))
    (dolist (line lines)
      (cl-case now-collecting
        (:secondaries
         (if (string-match-p lit-tester-d-dataflows-regex line)
             ;; This particular line is useless
             (setq now-collecting :dataflows)
           (if (string-match-p lit-tester-fix-descr-regex line)
               (progn
                 (setq now-collecting :fixes)
                 (push line fix-specs))
             (push line secondary-specs))))
        (:dataflows
         (if (string-match-p lit-tester-fix-descr-regex line)
             (progn
               (setq now-collecting :fixes)
               (push line fix-specs))
           (push line dataflow-specs)))
        (:fixes
         (if (string-match-p lit-tester-d-dataflows-regex line)
             ;; This particular line is useless
             (setq new-collecting :dataflows)
           (push line fix-specs)))
        (t (error "Unexpected state %s" now-collecting))))
    (list (reverse secondary-specs)
          (reverse dataflow-specs)
          (reverse fix-specs))))

(defun lit-not-dataflow-descr (l)
  (not (string-match-p lit-tester-dataflow-descr-regex l)))

(defun lit-parse-dataflows (dataflow-specs)
  (let ((dataflows '())
        (dataflow-specs dataflow-specs))
    (while dataflow-specs
      (let ((it-is-descr (string-match lit-tester-dataflow-descr-regex (car dataflow-specs))))
        (cl-assert it-is-descr)
        (let* ((description (match-string 1 (car dataflow-specs)))
               (step-specs (seq-take-while #'lit-not-dataflow-descr (cdr dataflow-specs)))
               (steps (mapcar #'lit-parse-secondary-spec step-specs)))
          (setq dataflow-specs (seq-drop-while #'lit-not-dataflow-descr (cdr dataflow-specs)))
          (push `( :description ,description
                   :steps ,(mapcar (lambda (step) (plist-get step :secondary)) steps))
                dataflows))))
    (reverse dataflows)))

(defconst lit-tester-fix-descr-regex "// FIX \\(.*\\)$")
(defconst lit-tester-edit-regex "// EDIT \\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\) `\\([^`]*\\)`")

(defun lit-not-fix-descr (l)
  (not (string-match-p lit-tester-fix-descr-regex l)))

(defun lit-parse-edit-spec (spec-line)
  (if (string-match lit-tester-edit-regex spec-line)
      (let ((begin-line (string-to-number (match-string 1 spec-line)))
            (begin-col (string-to-number (match-string 2 spec-line)))
            (end-line (string-to-number (match-string 3 spec-line)))
            (end-col (string-to-number (match-string 4 spec-line)))
            (message (match-string 5 spec-line)))
        `( :begin (:line ,begin-line :col ,begin-col)
           :end (:line ,end-line :col ,end-col)
           :message ,message))
    nil))

(defun lit-parse-fixes (fix-specs)
  (let ((fixes '())
        (fix-specs fix-specs))
    (while fix-specs
      (let ((it-is-descr (string-match lit-tester-fix-descr-regex (car fix-specs))))
        (cl-assert it-is-descr)
        (let* ((description (match-string 1 (car fix-specs)))
               (edit-specs (seq-take-while #'lit-not-fix-descr (cdr fix-specs)))
               (edits (mapcar #'lit-parse-edit-spec edit-specs)))
          (cl-assert (cl-every #'consp edits)) ; All edits are parsed
          (setq fix-specs (seq-drop-while #'lit-not-fix-descr (cdr fix-specs)))
          (push `( :description ,description :edits ,edits)
                fixes))))
    (reverse fixes)))

(defun lit-join-multilines (lines)
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

(defun lit-parse-1-observed (observed-report)
  "Parse the observed issue report as printed by tester"
  (print observed-report)
  (let ((lines (lit-join-multilines (split-string observed-report "\n" t))))
    (if-let ((filename (string-trim-left (lit-match-filename (car lines))))
             (primary-spec (lit-parse-primary-spec (cadr lines))))
        (cl-destructuring-bind (secondary-specs dataflow-specs fix-specs)
            (lit-split-into-secondary-dataflow-fix-specs (cddr lines))
          (let ((secondaries (mapcar #'lit-parse-secondary-spec secondary-specs))
                (dataflows (lit-parse-dataflows dataflow-specs))
                (fixes (lit-parse-fixes fix-specs)))
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

(defun lit-chop-string (string separator)
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
  (let* ((observed-single-reports (lit-chop-string observed-multiisue-report lit-tester-filename-regex))
         (issue-specs (mapcar #'lit-parse-1-observed observed-single-reports)))
    ;; Make sure all issues parsed
    (dolist (issue-spec issue-specs) (cl-assert issue-spec))
    issue-specs))

(defun lit-insert-next (line overlay)
  (goto-char (overlay-end overlay))
  (beginning-of-line)
  (forward-line)
  (insert line)
  (forward-line -1))

(defun lit-insert-next-indent (line overlay)
  (lit-insert-next line overlay)
  (indent-according-to-mode))

(defun lit-insert-prev (line overlay)
  (goto-char (overlay-start overlay))
  (beginning-of-line)
  (insert line)
  (forward-line -1))

(defun lit-insert-prev-indent (line overlay)
  (lit-insert-prev line overlay)
  (indent-according-to-mode))

(defun lit-insert-at-end (line overlay)
  (goto-char (overlay-end overlay))
  (end-of-line)
  (when (< (point) (point-max))
    ; Delete the new-line character
    (delete-char 1))
  (insert line)
  (forward-line -1))

(defun lit-can-put-spec-on-the-same-line ()
  (not (string-match-p "//" (thing-at-point 'line t))))

(defconst lit-hint-prev (concat (propertize "(p)" 'face 'lit-hint-key)
                               (propertize "rev" 'face 'lit-hint-keyword)))

(defconst lit-hint-next (concat (propertize "(n)" 'face 'lit-hint-key)
                               (propertize "ext" 'face 'lit-hint-keyword)))

(defconst lit-hint-same (concat (propertize "(s)" 'face 'lit-hint-key)
                               (propertize "ame" 'face 'lit-hint-keyword)))

(defconst lit-hint-yes (concat (propertize "(y)" 'face 'lit-hint-key)
                              (propertize "es" 'face 'lit-hint-keyword)))

(defun lit-short-preview (prefix str)
  (let* ((single-line-str (replace-regexp-in-string "\n" "" str))
         (sample (if (< (length single-line-str) 70)
                     single-line-str
                   (concat (substring single-line-str 0 67) "..."))))
    (concat prefix (propertize sample 'face 'lit-hint-preview))))

(defun lit-same-string-spec-hint (render-spec)
  (if-let ((can (lit-can-put-spec-on-the-same-line))
           (spec (funcall render-spec :same)))
      (lit-short-preview lit-hint-same spec)
    ""))

(defvar lit---uncleared-overlays '()
  "Holds a list of all the temporary overlays that exist and should be cleaned up at the end")

(defun lit-make-overlay (&rest args)
  (let ((overlay (apply #'make-overlay args)))
    (push overlay lit---uncleared-overlays)
    overlay))

(defun lit-copy-overlay (source)
  (let ((copy (copy-overlay source)))
    (push copy lit---uncleared-overlays)
    copy))

(defun lit-clear-overlay (overlay)
  (delete-overlay overlay)
  (setq lit---uncleared-overlays (delete overlay lit---uncleared-overlays)))

(defun lit-clear-overlays ()
  (mapc #'delete-overlay lit---uncleared-overlays)
  (setq lit---uncleared-overlays nil))

(defun lit-goto-line (n)
  (goto-char (point-min))
  (forward-line (1- n)))

(defvar-local lit-range-spec-overlays '()
  "Overlay pairs that connect range specifications with their target ranges")

(defun lit-make-dumb-range-spec-overlay-pair ()
  (if-let* ((offsets (lit-get-cur-lit-spec-range))
            (is-not-smart (not (null (cdr (plist-get offsets :begin-line-offset)))))
            (target-range (lit-resolve-range-offsets offsets)))
      (cons (lit-make-overlay (+ (line-beginning-position) (plist-get offsets :range-spec-begin-col))
                             (+ (line-beginning-position) (plist-get offsets :range-spec-end-col)))
            (lit-make-overlay (plist-get target-range :begin) (plist-get target-range :end) nil t))
    nil))

(defun lit-make-all-dumb-range-spec-overlays ()
  (save-excursion
    (let ((overlay-pairs '()))
      (beginning-of-buffer)
      (while (< (point) (point-max))
        (if-let ((overlay-pair (lit-make-dumb-range-spec-overlay-pair)))
            (push overlay-pair overlay-pairs))
        (forward-line))
      overlay-pairs)))

(defun lit-overlay-pair-affected-p (overlay-pair inserted-begin inserted-end)
  (let ((spec-begin (overlay-start (car overlay-pair)))
        (target-begin (overlay-start (cdr overlay-pair)))
        (target-end (overlay-end (cdr overlay-pair))))
    (or (< spec-begin inserted-begin target-end)
        (< spec-begin inserted-end target-end)
        (< target-begin inserted-begin spec-begin)
        (< target-begin inserted-end spec-begin))))

(defun lit-column-number-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (- pos (point))))

(defun lit-render-dumb-spec (spec-line target-begin target-end)
  (let ((target-begin-line (line-number-at-pos target-begin))
        (target-begin-col (lit-column-number-at-pos target-begin))
        (target-end-line (line-number-at-pos target-end))
        (target-end-col (lit-column-number-at-pos target-end)))
    (format "%+d:%d %+d:%d"
            (- target-begin-line spec-line)
            (1+ target-begin-col)
            (- target-end-line spec-line)
            (1+ target-end-col))))

(defun lit-rewrite-spec-for-pair (overlay-pair)
  (let* ((spec-begin (overlay-start (car overlay-pair)))
         (spec-end (overlay-end (car overlay-pair)))
         (spec-line (line-number-at-pos spec-begin))
         (target-begin (overlay-start (cdr overlay-pair)))
         (target-end (overlay-end (cdr overlay-pair)))
         (new-spec (lit-render-dumb-spec spec-line target-begin target-end)))
      (replace-region-contents spec-begin spec-end (lambda () new-spec))))

(defun lit-adjust-range-specs-after-insertion (overlay-pairs inserted-begin inserted-end)
  (mapc #'lit-rewrite-spec-for-pair
        (seq-filter (lambda (pair)
                      (lit-overlay-pair-affected-p pair inserted-begin inserted-end))
                    overlay-pairs)))

(defun lit-make-overlay-covering-lines-of (target-overlay)
  (save-excursion
    (goto-char (overlay-start target-overlay))
    (let ((from (line-beginning-position)))
      (goto-char (overlay-end target-overlay))
      (lit-make-overlay from (line-end-position)))))

(defun lit-display-options-temporarily (render-spec overlay disable-same)
  (let ((target-line-overlay (lit-make-overlay-covering-lines-of overlay) ))
    (overlay-put target-line-overlay 'before-string
                 (concat (lit-short-preview lit-hint-prev (funcall render-spec :prev)) "\n"))
    (overlay-put target-line-overlay 'after-string
        (concat (if disable-same ""
                  (lit-same-string-spec-hint render-spec)) "\n"
                (lit-short-preview lit-hint-next (funcall render-spec :next))))
    target-line-overlay))


(defun lit-choose-prev-next-same (loc render-spec overlay disable-same)
  (let ((options '(("prev" ?p "Insert the spec on the line above")
                   ("next" ?n "Insert the spec on the line below"))))
    (when (and (not disable-same) (lit-can-put-spec-on-the-same-line) (funcall render-spec :same))
      (push '("same" ?s "Insert the spec on the same line") options))
    (let ((options-overlay (lit-display-options-temporarily render-spec overlay disable-same))
          (target-overlay (lit-copy-overlay (plist-get loc :overlay))))
      (overlay-put target-overlay 'face 'lit-target-range)
      (lit-considering-loc loc)
      (let ((read-answer-short t))
        (let* ((choice (pcase (read-answer "Where to insert? " options)
                         ("prev" :prev)
                         ("next" :next)
                         ("same" :same))))
          (lit-clear-overlay options-overlay)
          (lit-clear-overlay target-overlay)
          (lit-done-loc loc)
          choice)))))

(defun lit-smart-loc-spec (overlay loc pos)
  (let ((start-line (line-number-at-pos (overlay-start overlay)))
        (start-col (plist-get (plist-get loc :begin) :col))
        (end-line (line-number-at-pos (overlay-end overlay)))
        (end-col (plist-get (plist-get loc :end) :col)))
    (if (= start-line end-line)
        (pcase pos
          (:prev (format "+:%d +:%d" start-col end-col))
          (:next (format "-:%d -:%d" start-col end-col))
          (:same (format ":%d :%d" start-col end-col)))
      (pcase pos
        (:prev (format "+1:%d +%d:%d" start-col (1+ (- end-line start-line)) end-col))
        (:next (format "-%d:%d -1:%d" (1+ (- end-line start-line)) start-col end-col))
        (:same nil)))))

(defun lit-compose-flow-ids (issue-id data-flow-ids)
  (if (or issue-id data-flow-ids)
      (format "(%s)" (mapconcat (lambda (id) (if id id "_")) (cons issue-id data-flow-ids) ","))
    ""))

(defun lit-insert-primary (rule-id primary issue-id data-flow-ids)
  (let ((overlay (plist-get primary :overlay))
        (message (plist-get primary :message))
        (flow-ids (lit-compose-flow-ids issue-id data-flow-ids)))
    (goto-char (overlay-start overlay))
    (beginning-of-line)
    (cl-flet ((render-spec (pos)
                           (if-let ((loc-spec (lit-smart-loc-spec overlay primary pos)))
                               (format " // CHECK %s %s%s:%s\n" loc-spec rule-id flow-ids message)
                             nil)))
      (let* ((pos (lit-choose-prev-next-same primary #'render-spec overlay nil))
             (spec (render-spec pos)))
        (pcase pos
          (:prev (lit-insert-prev-indent spec overlay))
          (:next (lit-insert-next-indent spec overlay))
          (:same (lit-insert-at-end      spec overlay)))
        (lit-record-current-line-if-dumb)
        (lit-make-overlay (line-beginning-position) (line-end-position) nil t)))))

(defun lit-insert-secondary (secondary issue-id keyword count)
  (let ((overlay (plist-get secondary :overlay))
        (message (plist-get secondary :message))
        (count-str (if count (format ",%d" count) "")))
    (goto-char (overlay-start overlay))
    (beginning-of-line)
    (cl-flet ((render-spec (pos)
                           (if-let ((loc-spec (lit-smart-loc-spec overlay secondary pos)))
                               (format " // %s %s %s%s:%s\n" keyword loc-spec issue-id count-str message)
                           nil)))
      (let* ((pos (lit-choose-prev-next-same secondary #'render-spec overlay nil))
             (spec (render-spec pos)))
        (pcase pos
          (:prev (lit-insert-prev-indent spec overlay))
          (:next (lit-insert-next-indent spec overlay))
          (:same (lit-insert-at-end      spec overlay)))
        (lit-record-current-line-if-dumb)))))

(defun lit-insert-edit (edit fix-id)
  (let ((overlay (plist-get edit :overlay))
        (message (plist-get edit :message)))
    (goto-char (overlay-start overlay))
    (beginning-of-line)
    (cl-flet ((render-spec (pos)
                           (if-let ((loc-spec (lit-smart-loc-spec overlay edit pos)))
                               (format "// EDIT %s %s `%s`\n" fix-id loc-spec message)
                             nil)))
      (let* ((pos (lit-choose-prev-next-same edit #'render-spec overlay t))
             (spec (render-spec pos)))
        (pcase pos
          (:prev (lit-insert-prev spec overlay))
          (:next (lit-insert-next spec overlay)))
        (lit-move-to-start-of-multiline)
        (lit-record-current-line-if-dumb)))))

(defconst lit-fun-decl-regex "^[[:alnum:]_&*<>]+ \\([[:alnum:]_]+\\).*{")

(defun lit-find-closest-defined-identifier (prim-target-overlay)
  (save-excursion
    (goto-char (overlay-start prim-target-overlay))
    (if (re-search-backward lit-fun-decl-regex nil t)
        (substring-no-properties (match-string 1))
      nil)))

(defun lit-valid-identifier (str)
  (string-match-p "^[^ :()\t\n'\"]*$" str))

(defun lit-read-valid-identifier-or-empty (prompt default)
  (let ((identifier (read-from-minibuffer prompt nil nil nil 'hh default)))
    (while (not (lit-valid-identifier identifier))
      (setq identifier (read-from-minibuffer
                        (format "'%s' is not a valid identifier. \nEnter valid identifier or nothing: "
                                identifier)
                        nil nil nil 'hh default)))
    identifier))

(defun lit-ask-exec-flow-id (issue-spec default-id)
  (if (plist-get issue-spec :secondaries)
        (let ((target-overlay (lit-copy-overlay
                               (plist-get (plist-get issue-spec :primary) :overlay))))
          (mapc #'lit-considering-loc (plist-get issue-spec :secondaries))
          (goto-char (overlay-start target-overlay))
          (overlay-put target-overlay 'face 'lit-target-range)
          (let ((issue-id (lit-read-valid-identifier-or-empty "Enter issue id (empty string to omit secondaries): " default-id)))
            (lit-clear-overlay target-overlay)
            (if (not (or (string-empty-p issue-id) (string= issue-id "_")))
                (progn
                  (mapc #'lit-unhighlight-loc (plist-get issue-spec :secondaries))
                  issue-id)
              (mapc #'lit-cancel-loc (plist-get issue-spec :secondaries))
              nil)))
        nil))

(defun lit-ask-data-flow-ids (issue-spec default-id)
  (let ((ids (mapcar (lambda (data-flow)
                       (lit-considering-loc data-flow)
                       (mapc #'lit-considering-loc (plist-get data-flow :steps))
                       (let ((id (lit-read-valid-identifier-or-empty
                                  (format "Id for dataflow \"%s\": " (plist-get data-flow :description))
                                  default-id)))
                         (lit-unhighlight-loc data-flow)
                         (mapc #'lit-unhighlight-loc (plist-get data-flow :steps))
                         (when (string-empty-p id)
                           (lit-cancel-loc data-flow)
                           (mapc #'lit-cancel-loc (plist-get data-flow :steps)))
                         id))
                     (plist-get issue-spec :dataflows))))
    (if (cl-every #'string-empty-p ids)
        nil ; no need to specify the snake tail S100(xxx,_,_,_) = S100(xxx)
      (mapcar (lambda (id) (if (string-empty-p id) nil id)) ids))))

(defun lit-ask-fix-ids (issue-spec default-id)
  (let ((ids (mapcar (lambda (fix)
                       (lit-considering-loc fix)
                       (mapc #'lit-considering-loc (plist-get fix :edits))
                       (let ((id (lit-read-valid-identifier-or-empty
                                  (format "Id for fix \"%s\": " (plist-get fix :description))
                                  default-id)))
                         (lit-unhighlight-loc fix)
                         (mapc #'lit-unhighlight-loc (plist-get fix :edits))
                         (when (string-empty-p id)
                           (lit-cancel-loc fix)
                           (mapc #'lit-cancel-loc (plist-get fix :edits)))
                         id))
                     (plist-get issue-spec :fixes))))
    (mapcar (lambda (id) (if (string-empty-p id) nil id)) ids)))

(defun lit-insert-dataflow-description (data-flow id primary-overlay)
  (goto-char (overlay-start primary-overlay))
  (beginning-of-line)
  (let* ((description (plist-get data-flow :description))
         (descr-spec (format "// DATAFLOW DESCRIPTION %s:%s\n" id description))
         (target-line-overlay (lit-make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put target-line-overlay 'after-string
                 (concat "\n" (lit-short-preview lit-hint-yes descr-spec)))
    (lit-considering-loc data-flow)
    (let ((do-insert (y-or-n-p (format "Insert description for dataflow \"%s\"?" description))))
      (lit-clear-overlay target-line-overlay)
      (if do-insert
          (progn
            (lit-done-loc data-flow)
            (lit-insert-next-indent descr-spec primary-overlay)
            (lit-record-current-line-if-dumb))
        (lit-cancel-loc data-flow)))))

(defun lit-insert-fix-description (fix id primary-overlay)
  (goto-char (overlay-start primary-overlay))
  (beginning-of-line)
  (let* ((description (plist-get fix :description))
         (descr-spec (format "// FIX %s:%s\n" id description))
         (target-line-overlay (lit-make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put target-line-overlay 'after-string
                 (concat "\n" (lit-short-preview lit-hint-yes descr-spec)))
    (lit-considering-loc fix)
    (let ((do-insert (y-or-n-p (format "Insert FIX \"%s\"?" description))))
      (lit-clear-overlay target-line-overlay)
      (when do-insert
        (lit-done-loc fix)
        (lit-insert-next-indent descr-spec primary-overlay)
        (lit-record-current-line-if-dumb))
      do-insert)))

(defun lit-insert-dataflows (dataflows dataflow-ids primary-overlay)
  (cl-mapcar
   (lambda (data-flow id)
     (when id
       (let ((step-count 0))
         (lit-insert-dataflow-description data-flow id primary-overlay)
         (dolist (step (plist-get data-flow :steps))
           (lit-insert-secondary step id "DATAFLOW" step-count)
           (cl-incf step-count)))))
   dataflows dataflow-ids))

(defun lit-cancel-fix (fix)
  (lit-cancel-loc fix)
  (dolist (edit (plist-get fix :edits))
    (lit-cancel-loc edit)))

(defun lit-insert-fixes (fixes fix-ids primary-overlay)
  (let ((last-header-spec-overlay (lit-copy-overlay primary-overlay)))
    (cl-mapcar
     (lambda (fix id)
       (when id
         (if (lit-insert-fix-description fix id last-header-spec-overlay)
             (progn
               (move-overlay last-header-spec-overlay
                             (line-beginning-position) (line-end-position))
               (dolist (edit (plist-get fix :edits))
                 (lit-insert-edit edit id)))
           (lit-cancel-fix fix))))
     fixes fix-ids)))

(defun lit-make-target-range-overlay (loc)
  (save-excursion
    (lit-goto-line (plist-get (plist-get loc :begin) :line))
    (move-to-column (1- (plist-get (plist-get loc :begin) :col)))
    (let ((begin (point)))
      (lit-goto-line (plist-get (plist-get loc :end) :line))
      (move-to-column (1- (plist-get (plist-get loc :end) :col)))
      (let ((end (point)))
        (when (= begin end)
          (if (eq ?\n (char-after))
              (setq begin (1- begin))
            (setq end (1+ end))))
        (lit-make-overlay begin end nil t)))))

(defun lit-generate-dataflow-overlays (dataflows)
  (mapcar (lambda (data-flow)
            (plist-put data-flow :steps
                       (mapcar (lambda (step)
                                 (plist-put step :overlay (lit-make-target-range-overlay step))
                                 step)
                               (plist-get data-flow :steps)))
            data-flow)
          dataflows))

(defun lit-generate-fix-overlays (fixes)
  (mapcar (lambda (fix)
            (plist-put fix :edits
                       (mapcar (lambda (edit)
                                 (plist-put edit :overlay (lit-make-target-range-overlay edit))
                                 edit)
                               (plist-get fix :edits)))
            fix)
          fixes))

(defun lit-generate-overlays (issue-spec)
  (setf (plist-get (plist-get issue-spec :primary) :overlay)
        (lit-make-target-range-overlay (plist-get issue-spec :primary)))
  (setf (plist-get issue-spec :secondaries)
        (mapcar (lambda (sec)
                  (plist-put sec :overlay (lit-make-target-range-overlay sec))
                  sec)
                (plist-get issue-spec :secondaries)))
  (setf (plist-get issue-spec :dataflows)
        (lit-generate-dataflow-overlays (plist-get issue-spec :dataflows)))
  (setf (plist-get issue-spec :fixes)
        (lit-generate-fix-overlays (plist-get issue-spec :fixes)))
  issue-spec)

(defun lit-insert-issue-spec-with-overlays (issue-spec)
  (atomic-change-group
    (setq lit-dumb-range-overlays (lit-make-all-dumb-range-spec-overlays))
    (let* ((default-id (lit-find-closest-defined-identifier
                        (plist-get (plist-get issue-spec :primary) :overlay)))
           (exec-id (lit-ask-exec-flow-id issue-spec default-id))
           (dataflow-ids (lit-ask-data-flow-ids issue-spec default-id))
           (fix-ids (lit-ask-fix-ids issue-spec default-id))
           (prim-spec-overlay (lit-insert-primary
                               (plist-get issue-spec :rule-id)
                               (plist-get issue-spec :primary)
                               exec-id
                               dataflow-ids)))
      (when exec-id
        (let ((secondaries (plist-get issue-spec :secondaries)))
          (dolist (secondary secondaries)
            (lit-insert-secondary secondary exec-id "SECONDARY" nil))))
      (when dataflow-ids
        (lit-insert-dataflows (plist-get issue-spec :dataflows) dataflow-ids prim-spec-overlay))
      (lit-insert-fixes (plist-get issue-spec :fixes) fix-ids prim-spec-overlay)
      (mapc #'lit-rewrite-spec-for-pair lit-dumb-range-overlays))))

(defun lit-insert-issue-spec (issue-spec)
  (unwind-protect
      (let ((issue-spec (lit-generate-overlays issue-spec)))
        (lit-insert-issue-spec-with-overlays issue-spec))
    (lit-clear-overlays)))

(defun lit-insert-issue-specs (issue-specs)
  (unwind-protect
      (let ((issue-specs (mapcar #' lit-generate-overlays issue-specs)))
        (mapc #'lit-insert-issue-spec-with-overlays issue-specs))
    (lit-clear-overlays)))

(defun lit-run-no-questions-asked (initial-buffer-content default-answer fun)
  (with-temp-buffer
    (insert initial-buffer-content)
    (let ((fix-cnt 0))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t))
                ((symbol-function 'read-answer) (lambda (&rest args) default-answer))
                ((symbol-function 'read-from-minibuffer)
                 (lambda (prompt &rest args)
                   (cond ((string-match-p "issue id" prompt) "issue-id")
                         ((string-match-p "id for dataflow" prompt) "df-id")
                         ((string-match-p "id for fix" prompt) (cl-incf fix-cnt) (format "fix-id%d" fix-cnt))
                         (t "unexpected")))))
        (funcall fun))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lit-run-insert-issue-spec-batch (initial-content issue-spec default-answer)
  (lit-run-no-questions-asked initial-content default-answer (lambda () (lit-insert-issue-spec issue-spec))))

(defun lit-run-insert-issue-specs-batch (initial-content issue-specs default-answer)
  (lit-run-no-questions-asked initial-content default-answer (lambda () (lit-insert-issue-specs issue-specs))))

(defun lit-render-primary (primary rule-id)
  (plist-put primary :pos-in-list (line-number-at-pos))
  (insert
   (format "%d:%d %d:%d %s:%s\n"
           (plist-get (plist-get primary :begin) :line)
           (plist-get (plist-get primary :begin) :col)
           (plist-get (plist-get primary :end) :line)
           (plist-get (plist-get primary :end) :col)
           rule-id
           (plist-get primary :message)))
    primary)

(defun lit-render-secondary (secondary)
  (plist-put secondary :pos-in-list (line-number-at-pos))
  (insert
   (format "    %d:%d %d:%d:%s\n"
           (plist-get (plist-get secondary :begin) :line)
           (plist-get (plist-get secondary :begin) :col)
           (plist-get (plist-get secondary :end) :line)
           (plist-get (plist-get secondary :end) :col)
           (plist-get secondary :message)))
  secondary)

(defun lit-render-dataflow (dataflow)
  (plist-put dataflow :pos-in-list (line-number-at-pos))
  (insert (format "      // DATAFLOW DESCRIPTION:%s\n" (plist-get dataflow :description)))
  (plist-put dataflow :steps (mapcar (lambda (step)
                                       (insert "    ")
                                       (lit-render-secondary step))
                                     (plist-get dataflow :steps)))
  dataflow)

(defun lit-render-edit (edit)
  (plist-put edit :pos-in-list (line-number-at-pos))
  (insert
   (format "    // EDIT %d:%d %d:%d `%s`\n"
           (plist-get (plist-get edit :begin) :line)
           (plist-get (plist-get edit :begin) :col)
           (plist-get (plist-get edit :end) :line)
           (plist-get (plist-get edit :end) :col)
           (plist-get edit :message)))
  edit)

(defun lit-render-fix (fix)
  (plist-put fix :pos-in-list (line-number-at-pos))
  (insert (format "    // FIX %s\n" (plist-get fix :description)))
  (plist-put fix :edits (mapcar (lambda (edit) (lit-render-edit edit))
                                (plist-get fix :edits)))
  fix)

(defun lit-render-issue-spec (issue-spec)
  (plist-put issue-spec :primary
             (lit-render-primary (plist-get issue-spec :primary)
                                (plist-get issue-spec :rule-id)))
  (plist-put issue-spec :secondaries
             (mapcar #'lit-render-secondary (plist-get issue-spec :secondaries)))
  (plist-put issue-spec :dataflows
             (mapcar #'lit-render-dataflow (plist-get issue-spec :dataflows)))
  (plist-put issue-spec :fixes
             (mapcar #'lit-render-fix (plist-get issue-spec :fixes)))
  issue-spec)

(defconst lit-aux-buffer-name "*issues-to-add*"
  "The name of the buffer holding some the issues to be added and indicating your progress.")

(defun lit-render-issue-specs-dedicated-buffer (issue-specs)
  (let ((prev-buffer (current-buffer))
        (buffer (switch-to-buffer-other-window lit-aux-buffer-name)))
    (erase-buffer)
    (let ((specs-with-lines (mapcar #'lit-render-issue-spec issue-specs)))
      (switch-to-buffer-other-window prev-buffer)
      specs-with-lines)))

(defun lit-modify-line-with-its-continuation-in-buffer (lino buf modif)
  (when lino
    (with-current-buffer buf
      (lit-goto-line lino)
      (when-let ((window (get-buffer-window buf)))
        (print window)
        (set-window-point window (point)))
      (funcall modif)
      (while (and (< (point) (point-max))
                  (string-suffix-p "\\\n" (thing-at-point 'line)))
        (forward-line)
        (funcall modif)))))

(defun lit-mark-line-in-buffer (lino buf face)
  (lit-modify-line-with-its-continuation-in-buffer
   lino buf (lambda () (set-text-properties (line-beginning-position) (line-end-position) `(face ,face)))))

(defun lit-unhighlight-line-in-buffer (lino buf)
  (lit-modify-line-with-its-continuation-in-buffer
   lino buf (lambda () (remove-text-properties (line-beginning-position) (line-end-position) '(face nil)))))

(defun lit-cancel-loc (loc)
  (lit-mark-line-in-buffer (plist-get loc :pos-in-list) lit-aux-buffer-name 'lit-cancelled-line))

(defun lit-done-loc (loc)
  (lit-mark-line-in-buffer (plist-get loc :pos-in-list) lit-aux-buffer-name 'lit-processed-line))

(defun lit-considering-loc (loc)
  (lit-mark-line-in-buffer (plist-get loc :pos-in-list) lit-aux-buffer-name 'lit-currently-inserting))

(defun lit-unhighlight-loc (loc)
  (lit-unhighlight-line-in-buffer (plist-get loc :pos-in-list) lit-aux-buffer-name))

;;;###autoload
(defun lit-insert-issues (observed-report)
  "Parse OBSERVED-REPORT, and insert the corresponding spec comments into current buffer.
The OBSERVED-REPORT must be taken verbatim from the 'unexpected'
section of the tester output. It will then be split into
individual issues, parsed and inserted interactively, asking for
the placement of each issue primary and secondary locations, data
flows, and fixes.
"
  (interactive "sInsert full 'unexpected' output: ")
  (if-let ((issue-specs (lit-parse-all-observed observed-report)))
      (lit-insert-issue-specs (lit-render-issue-specs-dedicated-buffer issue-specs))
    (print "malformed 'unexpected' output")))

(defconst lit-removable-spec-src-regex
  " *// \\(\\(CHECK\\|FIX\\|EDIT\\|DATAFLOW\\|DATAFLOW DESCRIPTION\\|SECONDARY\\|NOEXECFLOW\\)\\(\\[.*\\]\\)? .*\\|NOFIX\\)$"
  "All the lit spec comments that should be removed when requested.
Does not include COMMENT, because it cannot be automatically recovered from tester output.")

(defun lit-delete-spec-on-the-line ()
  (let ((cur-point (point))
        (case-fold-search nil))
    (atomic-change-group
      (lit-move-to-start-of-multiline)
      (if (search-forward-regexp lit-removable-spec-src-regex (line-end-position) t)
          (let ((line (match-string-no-properties 0)))
            (backward-char (length line))
            (delete-region (point) (line-end-position))
            (if (string-equal "\n" (thing-at-point 'line))
                (delete-char 1)
              (forward-char))
            (while (string-suffix-p "\\" line)
              (setq line (thing-at-point 'line))
              (delete-region (line-beginning-position) (line-end-position))
              (kill-whole-line))
            t)
        (goto-char cur-point)
        nil))))

;;;###autoload
(defun lit-delete-spec ()
  "Delete a lit-tester specification comment(s).
Delete the spec comment from current line, and delete the entire
line if it contains nothing else (ignoring whitespace). If a
region is selected, delete all spec comments frm the selected
region."
  (interactive)
  (if (not (use-region-p))
      (lit-delete-spec-on-the-line)
    (save-excursion
      (let ((reg (lit-make-overlay (region-beginning) (region-end))))
        (goto-char (region-beginning))
        (while (< (point) (overlay-end reg))
          (unless (lit-delete-spec-on-the-line)
            (forward-line)))
        (lit-clear-overlay reg)))))

;; (defun lit-run-tester ()
;;   (let ((buffer (switch-to-buffer-other-window "*tester-output*")))
;;     (erase-buffer)
;;     (setq major-mode #'c++-mode)
;;     (shell-command "/home/arseniy/proj/sonar-cpp/build/asserts/tester -include-unchecked/home/arseniy/proj/sonar-cpp/test/std-mock -ignore=S799,S878,S1005,S1908 -no-color -diff=false /home/arseniy/proj/sonar-cpp/test/checks/SymbolicExecution/ReclaimedTemporaryChecker.cpp" buffer)))

; (ert "lit-.*")

(provide 'lit)

;;; lit.el ends here
