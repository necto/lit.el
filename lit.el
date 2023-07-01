;;; lit.el --- Manipulate lit-tester spec comments -*- lexical-binding: t; -*-

;; Author: Arseniy Zaostrovnykh <arseniy.zaostrovnykh@sonarsource.com>

;; Created: 17.06.23

;;; Commentary:
;; Ease the display and manipulation of the lit-tester style specification comments.
;; Comment have the form // KEYWORD ...
;; E.g. // CHECK :1 :2 S100:Message
;; Insert, remove, and readjust them.
;; Defines a minor mode LIT-MODE that highlights target ranges for lit specification commants
;; and adds two user-level functions:
;; * LIT-INSERT-ISSUES
;; * LIT-DELETE-SPEC

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lit-parse)

(defface lit-default-face
  `((((class color) (background light))
     (:background "yellow"))
    (((class color) (background dark))
     (:background "blue3"))
    (t :inverse-video t))
  "Face used for most-recent lit-range highlight.")

(defface lit-tail-face1
  `((((class color) (background light))
     (:background "gold1"))
    (((class color) (background dark))
     (:background "#161630"))
    (t :inverse-video t))
  "Face used for 2nd-most-recent lit-range highlight.")

(defface lit-tail-face2
  `((((class color) (background light))
     (:background "wheat2"))
    (((class color) (background dark))
     (:background "gray7"))
    (t :inverse-video t))
  "Face used for 3rd-most-recent lit-range highlight.")

(defface lit-hint-preview '((t :foreground "dark green" :height .7 :extend t))
  "Face used for the preview generated around the target location when choosing the location of the spec.")

(defface lit-target-range '((((class color) (background dark))
                             (:background "blue2"))
                            (t :background "yellow"))
  "Face used to highlight the target region for an insertion.")

(defface lit-currently-inserting '((((class color) (background dark))
                                    (:slant italic :height 1.0 :weight bold :background "blue2"))
                                   (t :slant italic :height 1.0 :weight bold :background "yellow"))
  "Face used to highlight the target region for an insertion.")

(defface lit-cancelled-line '((((class color) (background dark))
                               (:slant normal :weight normal
                                 :height 1.0 :background nil :foreground "dark gray" :strike-through t))
                              (t :slant normal :weight normal
                                 :height 1.0 :background nil :foreground "gray" :strike-through t))
  "Face used to highlight the target region for an insertion.")

(defface lit-processed-line '((t :weight normal :slant normal :height 1.0 :foreground "forest green"))
  "Face used to indicate that a line was already inserted.")

(defface lit-hint-keyword '((t :height .5 :box t :foreground "black"))
  "Face used to distinguish the keywords prefixing the insertion options.")

(defface lit-hint-key '((t :height .5 :weight bold :box t :foreground "black"))
  "Face used to distinguish the hotkey associated with the insertion-option keyword.")

(defconst lit--highlight-faces '(lit-default-face lit-tail-face1 lit-tail-face2)
  "List of all highlight faces used to highlight the N most-recent ranges.")

(defvar-local lit--highlights '()
  "List of all highlight-overlay currently displayed.")

(defvar-local lit--last-line-highlihted nil
  "Last line that was highlighted.
Used to avoid duplicating highlight on the same line.
This happens with mouse-click events because
they coprise of mouse-down + mouse-up.")

(defun lit--cycle-colors ()
  "Cycle highlight colors to match the lit--highlight-faces list.
One color per highlighted range."
  (let ((priority (length lit--highlights)))
    (cl-mapcar (lambda (hls face)
                 (let ((hl1 (car hls)) (hl2 (cdr hls)))
                   (overlay-put hl1 'face face)
                   (overlay-put hl1 'priority priority)
                   (overlay-put hl2 'face face)
                   (overlay-put hl2 'priority priority)
                   (cl-decf priority)))
               lit--highlights
               lit--highlight-faces)))

(defun lit--add-range (target-range keyword-range)
  "Add highlight for at the position specified by TARGET-RANGE and KEYWORD-RANGE."
  (let* ((target-begin (plist-get target-range :begin))
         (target-end (max (1+ target-begin) (plist-get target-range :end)))
         (target-hl (lit--make-hl target-begin target-end nil 'lit-default-face))
         (keyword-hl (lit--make-hl (plist-get keyword-range :begin) (plist-get keyword-range :end) nil 'lit-default-face)))
    (push (cons target-hl keyword-hl) lit--highlights)
    (lit--remove-old-hl (length lit--highlight-faces))
    (lit--cycle-colors)))

(defun lit--make-hl (beg end buf face)
  "Make a highlight at the position specified by BEG and END."
  (let (hl)
    (setq hl (make-overlay beg end buf))
    (overlay-put hl 'face face)
    (overlay-put hl 'priority 1)
    hl))

(defun lit--remove-hl (hl)
  "Clear highlight HL."
  (when (overlayp hl)
    (delete-overlay hl)))

(defun lit--remove-old-hl (max-hls)
  "Remove and deactivate all old highlights. Keep MAX-HLS newest highlights."
  (let ((oldest (reverse lit--highlights)))
    (dolist (highlight-pair (butlast oldest max-hls))
      (let ((target-hl (car highlight-pair))
            (keyword-hl (cdr highlight-pair)))
        (lit--remove-hl target-hl)
        (lit--remove-hl keyword-hl)))
    (setq lit--highlights (reverse (last oldest max-hls)))))

(defun lit--move-to-start-of-multiline ()
  "Move the point to the first line in a sequence of '\'-extended lines."
  (let ((line-moved t))
    (while (and line-moved (< 1 (line-number-at-pos)))
      (forward-line -1)
      (let ((prev-line (thing-at-point 'line t)))
        (when (not (string-suffix-p "\\\n" prev-line))
            (setq line-moved nil)
            (forward-line))))))

(defun lit--is-cur-line-pure-lit-spec ()
  "Check if current line is part of a pure specification.
I.e. contains no non-spec data other than whitespace.
Handles '\'-extended lines. "
  (save-excursion
    (lit--move-to-start-of-multiline)
    (lit-parse-is-pure-lit-spec (thing-at-point 'line t))))

(defun lit--get-cur-lit-spec-range ()
  "Get the parsed range specifiation from the current line if it contains one.
Return nil otherwise."
  (lit-parse-get-lit-spec-range (thing-at-point 'line t)))

(defun lit--point-at-col-and-line (col line-offset)
  "Calculate the point if current position was shifted according to LINE-OFFSET.
And moved to the COL horisontal column."
  (save-excursion
    (pcase line-offset
      (`(:prev ,delta) (forward-line (- 0 delta)))
      (`(:next ,delta) (forward-line delta))
      (`(:smart-prev) (while (lit--is-cur-line-pure-lit-spec) (forward-line -1)))
      (`(:smart-next) (while (lit--is-cur-line-pure-lit-spec) (forward-line)))
      (_ t))
    (move-to-column (- col 1))
    (point)))

(defun lit--resolve-range-offsets (range-spec)
  "Transform the parsed range specification `RANGE-SPEC'
into a plist of point positions :begin :end."
  (let ((begin (lit--point-at-col-and-line (plist-get range-spec :begin-col)
                                          (plist-get range-spec :begin-line-offset)))
        (end (lit--point-at-col-and-line (plist-get range-spec :end-col)
                                        (plist-get range-spec :end-line-offset))))
    `( :begin ,begin
       :end ,end)))

(defun lit--resolve-keyword-range-offset (range-spec)
  "Extract the column range of the keyword of the current spec from RANGE-SPEC,
and transform it to the point values :begin and :end plist."
  (lit--resolve-range-offsets
   `( :begin-line-offset (:same)
      :begin-col ,(+ (plist-get range-spec :keyword-begin-col) 1)
      :end-line-offset (:same)
      :end-col ,(+ (plist-get range-spec :keyword-end-col) 1))))

(defface lit--step-marker '((t :height .8 :weight bold
                             :box (:line-width (4 . -4)
                                   :color "rose"
                                   :style released-button)
                             :background "dark red"
                             :foreground "white"))
  "Face used for the little markers on the side of each secondary step.")

(defun lit-highlight-cur-spec-range ()
  "The main call of the LIT-MODE.
Should be called every time the point lands on a new line.
It identifies the range specification and highlights it in the same buffer."
  (if lit-mode
      (save-excursion
        (lit--move-to-start-of-multiline)
        (unless (and lit--last-line-highlihted
                     (= (line-number-at-pos) lit--last-line-highlihted))
          (setq lit--last-line-highlihted (line-number-at-pos))
          (if-let ((range-spec (lit--get-cur-lit-spec-range))
                   (target-range (lit--resolve-range-offsets range-spec))
                   (keyword-range (lit--resolve-keyword-range-offset range-spec)))
              (lit--add-range target-range keyword-range))))
    (when lit--tester-output-mode
      (lit--highlight-cur-spec-target-ranges))))

(defvar lit--numbered-overlays '()
  "Keep track of all the overlays used to number secondary steps.")

(defun lit--highlight-cur-spec-target-ranges ()
  (lit--unhighlight-targets-for-lines)
  (lit--unnumber-lines)
  (when-let* ((lino (save-excursion
                      (lit--move-to-start-of-multiline)
                      (line-number-at-pos)))
              (target-loc
               (gethash
                lino
                lit--output-unexpected-issues-line-to-loc-ht))
              (sec-num 0))
    (dolist (loc (gethash lino lit--output-headers-to-steps))
      (let ((num-mark (propertize (number-to-string sec-num)
                                  'face 'lit--step-marker)))
        (lit--number-line (plist-get loc :pos-in-list) num-mark)
        (lit--highlight-target
         loc nil `((face . lit-tail-face1)
                   (priority . 1)
                   (before-string . ,num-mark)))
        (cl-incf sec-num)))
    (lit--highlight-target
     target-loc t '((face . lit-default-face)
                    (priority . 2)))))

;;;###autoload
(define-minor-mode lit-mode
  "Highlight ranges referenced from lit specification comments."
  :lighter nil
  (lit--remove-old-hl 0))

;;;###autoload
(define-minor-mode lit--tester-output-mode
  "Add some interactivity to the tester output."
  :lighter "LIT"
  :interactive nil
  (lit--unhighlight-targets-for-lines))

(defun lit--highlight-cur-spec-range-advice (&rest _args)
  (lit-highlight-cur-spec-range))

(advice-add 'line-move :after #'lit--highlight-cur-spec-range-advice)
(advice-add 'mouse-set-point :after #'lit--highlight-cur-spec-range-advice)

(defun lit-record-current-line-if-dumb ()
  (if-let ((dumb-range-pair (lit--make-dumb-range-spec-overlay-pair)))
      (push dumb-range-pair lit--dumb-range-overlays)))

(defvar-local lit--dumb-range-overlays '()
  "List of pairs of overlays connecting a range specification with its target range.")

(defun lit--insert-next (line overlay)
  (goto-char (overlay-end overlay))
  (beginning-of-line)
  (forward-line)
  (insert line)
  (forward-line -1))

(defun lit--insert-next-indent (line overlay)
  (lit--insert-next line overlay)
  (indent-according-to-mode))

(defun lit--insert-prev (line overlay)
  (goto-char (overlay-start overlay))
  (beginning-of-line)
  (insert line)
  (forward-line -1))

(defun lit--insert-prev-indent (line overlay)
  (lit--insert-prev line overlay)
  (indent-according-to-mode))

(defun lit--insert-at-end (line overlay)
  (goto-char (overlay-end overlay))
  (end-of-line)
  (when (< (point) (point-max))
    ; Delete the new-line character
    (delete-char 1))
  (insert line)
  (forward-line -1))

(defun lit--can-put-spec-on-the-same-line-p ()
  (not (string-match-p "//" (thing-at-point 'line t))))

(defconst lit--hint-prev (concat (propertize "(p)" 'face 'lit-hint-key)
                                 (propertize "rev" 'face 'lit-hint-keyword)))

(defconst lit--hint-next (concat (propertize "(n)" 'face 'lit-hint-key)
                                 (propertize "ext" 'face 'lit-hint-keyword)))

(defconst lit--hint-same (concat (propertize "(s)" 'face 'lit-hint-key)
                                 (propertize "ame" 'face 'lit-hint-keyword)))

(defconst lit--hint-yes (concat (propertize "(y)" 'face 'lit-hint-key)
                                (propertize "es" 'face 'lit-hint-keyword)))

(defun lit--short-preview (prefix str)
  (let* ((single-line-str (replace-regexp-in-string "\n" "" str))
         (sample (if (< (length single-line-str) 70)
                     single-line-str
                   (concat (substring single-line-str 0 67) "..."))))
    (concat prefix (propertize sample 'face 'lit-hint-preview))))

(defun lit--same-string-spec-hint (render-spec)
  (if-let ((can (lit--can-put-spec-on-the-same-line-p))
           (spec (funcall render-spec :same)))
      (lit--short-preview lit--hint-same spec)
    ""))

(defvar lit--uncleared-overlays '()
  "Holds a list of all the temporary overlays that exist and should be cleaned up at the end")

(defun lit--make-overlay (&rest args)
  "Same as make-overlay but keeps a reference to it for a later cleanup."
  (let ((overlay (apply #'make-overlay args)))
    (push overlay lit--uncleared-overlays)
    overlay))

(defun lit--copy-overlay (source)
  "Same as copy-overlay but keeps a reference to the copy for a later cleanup."
  (let ((copy (copy-overlay source)))
    (push copy lit--uncleared-overlays)
    copy))

(defun lit--clear-overlay (overlay)
  "Delete the overlay from the buffer and from the cleanup list."
  (delete-overlay overlay)
  (setq lit--uncleared-overlays (delete overlay lit--uncleared-overlays)))

(defun lit--clear-overlays ()
  "Delete all overlays in the cleanup list from the buffer and empty the list."
  (mapc #'delete-overlay lit--uncleared-overlays)
  (setq lit--uncleared-overlays nil))

(defun lit--goto-line (n)
  "Move to the line N reliably and non-interactively."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun lit--make-dumb-range-spec-overlay-pair ()
  "Return a pair or overlays connecting range spec with its target range or nil.
If the current line contains a non-smart range spec, create and return a pair of
overlays covering the target range and the range spec.
Return nil otherwise."
  (if-let* ((offsets (lit--get-cur-lit-spec-range))
            (is-not-smart (not (null (cdr (plist-get offsets :begin-line-offset)))))
            (target-range (lit--resolve-range-offsets offsets)))
      (cons (lit--make-overlay (+ (line-beginning-position) (plist-get offsets :range-spec-begin-col))
                             (+ (line-beginning-position) (plist-get offsets :range-spec-end-col)))
            (lit--make-overlay (plist-get target-range :begin) (plist-get target-range :end) nil t))
    nil))

(defun lit--make-all-dumb-range-spec-overlays ()
  (save-excursion
    (let ((overlay-pairs '()))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (if-let ((overlay-pair (lit--make-dumb-range-spec-overlay-pair)))
            (push overlay-pair overlay-pairs))
        (forward-line))
      overlay-pairs)))

(defun lit--overlay-pair-affected-p (overlay-pair inserted-begin inserted-end)
  "Is the spec-target pair of overlays affacted by the insertion."
  (let ((spec-begin (overlay-start (car overlay-pair)))
        (target-begin (overlay-start (cdr overlay-pair)))
        (target-end (overlay-end (cdr overlay-pair))))
    (or (< spec-begin inserted-begin target-end)
        (< spec-begin inserted-end target-end)
        (< target-begin inserted-begin spec-begin)
        (< target-begin inserted-end spec-begin))))

(defun lit--column-number-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (- pos (point))))

(defun lit--render-dumb-spec (spec-line target-begin target-end)
  (let ((target-begin-line (line-number-at-pos target-begin))
        (target-begin-col (lit--column-number-at-pos target-begin))
        (target-end-line (line-number-at-pos target-end))
        (target-end-col (lit--column-number-at-pos target-end)))
    (format "%+d:%d %+d:%d"
            (- target-begin-line spec-line)
            (1+ target-begin-col)
            (- target-end-line spec-line)
            (1+ target-end-col))))

(defun lit--rewrite-spec-for-pair (overlay-pair)
  "Regenerate the non-smart spec in-place of the range spec of the pair."
  (let* ((spec-begin (overlay-start (car overlay-pair)))
         (spec-end (overlay-end (car overlay-pair)))
         (spec-line (line-number-at-pos spec-begin))
         (target-begin (overlay-start (cdr overlay-pair)))
         (target-end (overlay-end (cdr overlay-pair)))
         (new-spec (lit--render-dumb-spec spec-line target-begin target-end)))
      (replace-region-contents spec-begin spec-end (lambda () new-spec))))

(defun lit--adjust-range-specs-after-insertion (overlay-pairs inserted-begin inserted-end)
  "Adjust all affected non-smart specs after an insertion."
  (mapc #'lit--rewrite-spec-for-pair
        (seq-filter (lambda (pair)
                      (lit--overlay-pair-affected-p pair inserted-begin inserted-end))
                    overlay-pairs)))

(defun lit--make-overlay-covering-lines-of (target-overlay)
  "Make another overlay that covers entire lines and the TARGET-OVERLAY."
  (save-excursion
    (goto-char (overlay-start target-overlay))
    (let ((from (line-beginning-position)))
      (goto-char (overlay-end target-overlay))
      (lit--make-overlay from (line-end-position)))))

(defun lit--display-options-temporarily (render-spec overlay disable-same)
  "Display prev: same: next: insertion options and return the overlay.
Deleting the overlay removes these options."
  (let ((target-line-overlay (lit--make-overlay-covering-lines-of overlay) ))
    (overlay-put target-line-overlay 'before-string
                 (concat (lit--short-preview lit--hint-prev (funcall render-spec :prev)) "\n"))
    (overlay-put target-line-overlay 'after-string
        (concat (if disable-same ""
                  (lit--same-string-spec-hint render-spec)) "\n"
                (lit--short-preview lit--hint-next (funcall render-spec :next))))
    target-line-overlay))


(defun lit--choose-prev-next-same (loc render-spec overlay disable-same)
  "Let the user choose where to insert: prev, same, or next line.
Provides a visual demonstration for each option.
OVERLAY must cover the target range of the inserted spec.
LOC is the line number of the corresponding spec in the auxilarry buffer.
Displays the same-line option only if it is feasible and not DISABLE-SAME. "
  (let ((options '(("prev" ?p "Insert the spec on the line above")
                   ("next" ?n "Insert the spec on the line below"))))
    (when (and (not disable-same) (lit--can-put-spec-on-the-same-line-p) (funcall render-spec :same))
      (push '("same" ?s "Insert the spec on the same line") options))
    (let ((options-overlay (lit--display-options-temporarily render-spec overlay disable-same))
          (target-overlay (lit--copy-overlay (plist-get loc :overlay))))
      (overlay-put target-overlay 'face 'lit-target-range)
      (lit--considering-loc loc)
      (let ((read-answer-short t))
        (let* ((choice (pcase (read-answer "Where to insert? " options)
                         ("prev" :prev)
                         ("next" :next)
                         ("same" :same))))
          (lit--clear-overlay options-overlay)
          (lit--clear-overlay target-overlay)
          (lit--done-loc loc)
          choice)))))

(defun lit--smart-loc-spec (overlay loc pos)
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

(defun lit--compose-flow-ids (issue-id data-flow-ids)
  (if (or issue-id data-flow-ids)
      (format "(%s)" (mapconcat (lambda (id) (if id id "_")) (cons issue-id data-flow-ids) ","))
    ""))

(defun lit--insert-primary (rule-id primary issue-id data-flow-ids)
  "Insert the CHECK specification for the primary location of the issue."
  (let ((overlay (plist-get primary :overlay))
        (message (plist-get primary :message))
        (flow-ids (lit--compose-flow-ids issue-id data-flow-ids)))
    (goto-char (overlay-start overlay))
    (beginning-of-line)
    (cl-flet ((render-spec (pos)
                           (if-let ((loc-spec (lit--smart-loc-spec overlay primary pos)))
                               (format " // CHECK %s %s%s:%s\n" loc-spec rule-id flow-ids message)
                             nil)))
      (let* ((pos (lit--choose-prev-next-same primary #'render-spec overlay nil))
             (spec (render-spec pos)))
        (pcase pos
          (:prev (lit--insert-prev-indent spec overlay))
          (:next (lit--insert-next-indent spec overlay))
          (:same (lit--insert-at-end      spec overlay)))
        (lit-record-current-line-if-dumb)
        (lit--make-overlay (line-beginning-position) (line-end-position) nil t)))))

(defun lit--insert-secondary (secondary issue-id keyword order)
  "Insert the SECONDARY or DATAFLOW spec for a secondary location.
KEYWORD parameter allows you to specify which keyword is inserted.
Optional ORDER allows specifying the order of the secondary location."
  (let ((overlay (plist-get secondary :overlay))
        (message (plist-get secondary :message))
        (count-str (if order (format ",%d" order) "")))
    (goto-char (overlay-start overlay))
    (beginning-of-line)
    (cl-flet ((render-spec (pos)
                           (if-let ((loc-spec (lit--smart-loc-spec overlay secondary pos)))
                               (format " // %s %s %s%s:%s\n" keyword loc-spec issue-id count-str message)
                           nil)))
      (let* ((pos (lit--choose-prev-next-same secondary #'render-spec overlay nil))
             (spec (render-spec pos)))
        (pcase pos
          (:prev (lit--insert-prev-indent spec overlay))
          (:next (lit--insert-next-indent spec overlay))
          (:same (lit--insert-at-end      spec overlay)))
        (lit-record-current-line-if-dumb)))))

(defun lit--insert-edit (edit fix-id)
  "Insert the EDIT spec."
  (let ((overlay (plist-get edit :overlay))
        (message (plist-get edit :message)))
    (goto-char (overlay-start overlay))
    (beginning-of-line)
    (cl-flet ((render-spec (pos)
                           (if-let ((loc-spec (lit--smart-loc-spec overlay edit pos)))
                               (format "// EDIT %s %s `%s`\n" fix-id loc-spec message)
                             nil)))
      (let* ((pos (lit--choose-prev-next-same edit #'render-spec overlay t))
             (spec (render-spec pos)))
        (pcase pos
          (:prev (lit--insert-prev spec overlay))
          (:next (lit--insert-next spec overlay)))
        (lit--move-to-start-of-multiline)
        (lit-record-current-line-if-dumb)))))

(defconst lit--fun-decl-regex "^[[:alnum:]_&*<>]+ \\([[:alnum:]_]+\\).*{"
  "Regex matching a C or C++ function or a class definition.")

(defun lit--find-closest-defined-identifier (prim-target-overlay)
  "Find the closest function or class definition to the given location.
Used to suggest the issue id for the inserted issue."
  (save-excursion
    (goto-char (overlay-start prim-target-overlay))
    (if (re-search-backward lit--fun-decl-regex nil t)
        (substring-no-properties (match-string 1))
      nil)))

(defun lit--valid-identifier-p (str)
  "Check if the given issue-id is valid for an issue specification."
  (string-match-p "^[^ :()\t\n'\"]*$" str))

(defun lit--read-valid-identifier-or-empty (prompt default)
  (let ((identifier (read-from-minibuffer prompt nil nil nil 'hh default)))
    (while (not (lit--valid-identifier-p identifier))
      (setq identifier (read-from-minibuffer
                        (format "'%s' is not a valid identifier. \nEnter valid identifier or nothing: "
                                identifier)
                        nil nil nil 'hh default)))
    identifier))

(defun lit--ask-exec-flow-id (issue-spec default-id)
  (if (plist-get issue-spec :secondaries)
        (let ((target-overlay (lit--copy-overlay
                               (plist-get (plist-get issue-spec :primary) :overlay))))
          (mapc #'lit--considering-loc (plist-get issue-spec :secondaries))
          (goto-char (overlay-start target-overlay))
          (overlay-put target-overlay 'face 'lit-target-range)
          (let ((issue-id (lit--read-valid-identifier-or-empty "Enter issue id (empty string to omit secondaries): " default-id)))
            (lit--clear-overlay target-overlay)
            (if (not (or (string-empty-p issue-id) (string= issue-id "_")))
                (progn
                  (mapc #'lit--unhighlight-loc (plist-get issue-spec :secondaries))
                  issue-id)
              (mapc #'lit--cancel-loc (plist-get issue-spec :secondaries))
              nil)))
        nil))

(defun lit--ask-data-flow-ids (issue-spec default-id)
  (let ((ids (mapcar (lambda (data-flow)
                       (lit--considering-loc data-flow)
                       (mapc #'lit--considering-loc (plist-get data-flow :steps))
                       (let ((id (lit--read-valid-identifier-or-empty
                                  (format "Id for dataflow \"%s\": " (plist-get data-flow :description))
                                  default-id)))
                         (lit--unhighlight-loc data-flow)
                         (mapc #'lit--unhighlight-loc (plist-get data-flow :steps))
                         (when (string-empty-p id)
                           (lit--cancel-loc data-flow)
                           (mapc #'lit--cancel-loc (plist-get data-flow :steps)))
                         id))
                     (plist-get issue-spec :dataflows))))
    (if (cl-every #'string-empty-p ids)
        nil ; no need to specify the snake tail S100(xxx,_,_,_) = S100(xxx)
      (mapcar (lambda (id) (if (string-empty-p id) nil id)) ids))))

(defun lit--ask-fix-ids (issue-spec default-id)
  (let ((ids (mapcar (lambda (fix)
                       (lit--considering-loc fix)
                       (mapc #'lit--considering-loc (plist-get fix :edits))
                       (let ((id (lit--read-valid-identifier-or-empty
                                  (format "Id for fix \"%s\": " (plist-get fix :description))
                                  default-id)))
                         (lit--unhighlight-loc fix)
                         (mapc #'lit--unhighlight-loc (plist-get fix :edits))
                         (when (string-empty-p id)
                           (lit--cancel-loc fix)
                           (mapc #'lit--cancel-loc (plist-get fix :edits)))
                         id))
                     (plist-get issue-spec :fixes))))
    (mapcar (lambda (id) (if (string-empty-p id) nil id)) ids)))

(defun lit--insert-dataflow-description (data-flow id primary-overlay)
  (goto-char (overlay-start primary-overlay))
  (beginning-of-line)
  (let* ((description (plist-get data-flow :description))
         (descr-spec (format "// DATAFLOW DESCRIPTION %s:%s\n" id description))
         (target-line-overlay (lit--make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put target-line-overlay 'after-string
                 (concat "\n" (lit--short-preview lit--hint-yes descr-spec)))
    (lit--considering-loc data-flow)
    (let ((do-insert (y-or-n-p (format "Insert description for dataflow \"%s\"?" description))))
      (lit--clear-overlay target-line-overlay)
      (if do-insert
          (progn
            (lit--done-loc data-flow)
            (lit--insert-next-indent descr-spec primary-overlay)
            (lit-record-current-line-if-dumb))
        (lit--cancel-loc data-flow)))))

(defun lit--insert-fix-description (fix id primary-overlay)
  (goto-char (overlay-start primary-overlay))
  (beginning-of-line)
  (let* ((description (plist-get fix :description))
         (descr-spec (format "// FIX %s:%s\n" id description))
         (target-line-overlay (lit--make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put target-line-overlay 'after-string
                 (concat "\n" (lit--short-preview lit--hint-yes descr-spec)))
    (lit--considering-loc fix)
    (let ((do-insert (y-or-n-p (format "Insert FIX \"%s\"?" description))))
      (lit--clear-overlay target-line-overlay)
      (when do-insert
        (lit--done-loc fix)
        (lit--insert-next-indent descr-spec primary-overlay)
        (lit-record-current-line-if-dumb))
      do-insert)))

(defun lit--insert-dataflows (dataflows dataflow-ids primary-overlay)
  (cl-mapcar
   (lambda (data-flow id)
     (when id
       (let ((step-count 0))
         (lit--insert-dataflow-description data-flow id primary-overlay)
         (dolist (step (plist-get data-flow :steps))
           (lit--insert-secondary step id "DATAFLOW" step-count)
           (cl-incf step-count)))))
   dataflows dataflow-ids))

(defun lit--cancel-fix (fix)
  (lit--cancel-loc fix)
  (dolist (edit (plist-get fix :edits))
    (lit--cancel-loc edit)))

(defun lit--insert-fixes (fixes fix-ids primary-overlay)
  (let ((last-header-spec-overlay (lit--copy-overlay primary-overlay)))
    (cl-mapcar
     (lambda (fix id)
       (when id
         (if (lit--insert-fix-description fix id last-header-spec-overlay)
             (progn
               (move-overlay last-header-spec-overlay
                             (line-beginning-position) (line-end-position))
               (dolist (edit (plist-get fix :edits))
                 (lit--insert-edit edit id)))
           (lit--cancel-fix fix))))
     fixes fix-ids)))

(defun lit--make-target-range-overlay (loc)
  "Make an overlay pointed to by the location specification LOC."
  (save-excursion
    (lit--goto-line (plist-get (plist-get loc :begin) :line))
    (move-to-column (1- (plist-get (plist-get loc :begin) :col)))
    (let ((begin (point)))
      (lit--goto-line (plist-get (plist-get loc :end) :line))
      (move-to-column (1- (plist-get (plist-get loc :end) :col)))
      (let ((end (point)))
        (when (= begin end)
          (if (eq ?\n (char-after))
              (setq begin (1- begin))
            (setq end (1+ end))))
        (lit--make-overlay begin end nil t)))))

(defun lit--generate-dataflow-overlays (dataflows)
  (mapcar (lambda (data-flow)
            (plist-put data-flow :steps
                       (mapcar (lambda (step)
                                 (plist-put step :overlay (lit--make-target-range-overlay step))
                                 step)
                               (plist-get data-flow :steps)))
            data-flow)
          dataflows))

(defun lit--generate-fix-overlays (fixes)
  (mapcar (lambda (fix)
            (plist-put fix :edits
                       (mapcar (lambda (edit)
                                 (plist-put edit :overlay (lit--make-target-range-overlay edit))
                                 edit)
                               (plist-get fix :edits)))
            fix)
          fixes))

(defun lit--generate-overlays (issue-spec)
  "Generate overlays for all locations mention in ISSUE-SPEC.
Modifies ISSUE-SPEC.
Returns same ISSUE-SPEC with :overlay properties inserted for each location."
  (setf (plist-get (plist-get issue-spec :primary) :overlay)
        (lit--make-target-range-overlay (plist-get issue-spec :primary)))
  (setf (plist-get issue-spec :secondaries)
        (mapcar (lambda (sec)
                  (plist-put sec :overlay (lit--make-target-range-overlay sec))
                  sec)
                (plist-get issue-spec :secondaries)))
  (setf (plist-get issue-spec :dataflows)
        (lit--generate-dataflow-overlays (plist-get issue-spec :dataflows)))
  (setf (plist-get issue-spec :fixes)
        (lit--generate-fix-overlays (plist-get issue-spec :fixes)))
  issue-spec)

(defun lit--insert-issue-spec-with-overlays (issue-spec)
  "Insert interactively the specification comments for ISSUE-SPEC.
ISSUE-SPEC must feature :overlay properties for each location for target ranges.
These are produced by lit--generate-overlays.
For each location the function asks the position next to the target where it should be inserted.
For optional elements - DATAFLOW DESCRIPTION, or entire FIX/EDITs it asks confirmation."
  (atomic-change-group
    (setq lit--dumb-range-overlays (lit--make-all-dumb-range-spec-overlays))
    (let* ((default-id (lit--find-closest-defined-identifier
                        (plist-get (plist-get issue-spec :primary) :overlay)))
           (exec-id (lit--ask-exec-flow-id issue-spec default-id))
           (dataflow-ids (lit--ask-data-flow-ids issue-spec default-id))
           (fix-ids (lit--ask-fix-ids issue-spec default-id))
           (prim-spec-overlay (lit--insert-primary
                               (plist-get issue-spec :rule-id)
                               (plist-get issue-spec :primary)
                               exec-id
                               dataflow-ids)))
      (when exec-id
        (let ((secondaries (plist-get issue-spec :secondaries)))
          (dolist (secondary secondaries)
            (lit--insert-secondary secondary exec-id "SECONDARY" nil))))
      (when dataflow-ids
        (lit--insert-dataflows (plist-get issue-spec :dataflows) dataflow-ids prim-spec-overlay))
      (lit--insert-fixes (plist-get issue-spec :fixes) fix-ids prim-spec-overlay)
      (mapc #'lit--rewrite-spec-for-pair lit--dumb-range-overlays))))

(defun lit-insert-issue-spec (issue-spec)
  "Insert a single issue specification according to ISSUE-SPEC.
Interactively asks user to make choices on every step."
  (unwind-protect
      (let ((issue-spec (lit--generate-overlays issue-spec)))
        (lit--insert-issue-spec-with-overlays issue-spec))
    (lit--clear-overlays)))

(defun lit--insert-issue-specs (issue-specs)
  "Insert multiple issues from ISSUE-SPECS properly adjusting the target ranges.
Interactively asks user to make choices on every step."
  (unwind-protect
      (let ((issue-specs (mapcar #' lit--generate-overlays issue-specs)))
        (lit--unnumber-lines)
        (lit--unhighlight-targets-for-lines)
        (lit--remove-old-hl 0)
        (mapc #'lit--insert-issue-spec-with-overlays issue-specs))
    (lit--clear-overlays)))

(defun lit--render-primary (primary rule-id)
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

(defun lit--render-secondary (secondary)
  (plist-put secondary :pos-in-list (line-number-at-pos))
  (insert
   (format "    %d:%d %d:%d:%s\n"
           (plist-get (plist-get secondary :begin) :line)
           (plist-get (plist-get secondary :begin) :col)
           (plist-get (plist-get secondary :end) :line)
           (plist-get (plist-get secondary :end) :col)
           (plist-get secondary :message)))
  secondary)

(defun lit--render-dataflow (dataflow)
  (plist-put dataflow :pos-in-list (line-number-at-pos))
  (insert (format "      // DATAFLOW DESCRIPTION:%s\n" (plist-get dataflow :description)))
  (plist-put dataflow :steps (mapcar (lambda (step)
                                       (insert "    ")
                                       (lit--render-secondary step))
                                     (plist-get dataflow :steps)))
  dataflow)

(defun lit--render-edit (edit)
  (plist-put edit :pos-in-list (line-number-at-pos))
  (insert
   (format "    // EDIT %d:%d %d:%d `%s`\n"
           (plist-get (plist-get edit :begin) :line)
           (plist-get (plist-get edit :begin) :col)
           (plist-get (plist-get edit :end) :line)
           (plist-get (plist-get edit :end) :col)
           (plist-get edit :message)))
  edit)

(defun lit--render-fix (fix)
  (plist-put fix :pos-in-list (line-number-at-pos))
  (insert (format "    // FIX %s\n" (plist-get fix :description)))
  (plist-put fix :edits (mapcar (lambda (edit) (lit--render-edit edit))
                                (plist-get fix :edits)))
  fix)

(defun lit--render-issue-spec (issue-spec)
  "Produce a tester-output-like text specification in the current buffer.
Return ISSUE-SPEC with each location with line numbers of the
produced specification."
  (plist-put issue-spec :primary
             (lit--render-primary (plist-get issue-spec :primary)
                                  (plist-get issue-spec :rule-id)))
  (plist-put issue-spec :secondaries
             (mapcar #'lit--render-secondary (plist-get issue-spec :secondaries)))
  (plist-put issue-spec :dataflows
             (mapcar #'lit--render-dataflow (plist-get issue-spec :dataflows)))
  (plist-put issue-spec :fixes
             (mapcar #'lit--render-fix (plist-get issue-spec :fixes)))
  issue-spec)

(defun lit--add-locs-to-hashtable (issue-spec line-to-loc)
  "Add all locations from ISSUE-SPEC to LINE-TO-LOC hashtable.
Use the corresponding :POS-IN-LIST as the key."
  (cl-labels ((add-loc (loc)
                       (when (consp loc)
                         (mapc #'add-loc loc)
                         (when-let ((pos-in-list (plist-get loc :pos-in-list)))
                           (puthash pos-in-list loc line-to-loc)))))
    (add-loc issue-spec)))

(defun lit--map-headers-to-primary-loc (issue-specs line-to-loc)
  (dolist (issue-spec issue-specs)
    (when-let ((primary (plist-get issue-spec :primary)))
      (dolist (dataflow (plist-get issue-spec :dataflows))
        (when-let ((header-line (plist-get dataflow :pos-in-list)))
          (puthash header-line primary line-to-loc)))
      (dolist (fix (plist-get issue-spec :fixes))
        (when-let ((header-line (plist-get fix :pos-in-list)))
          (puthash header-line primary line-to-loc))))))

(defun lit--make-hashtable-line-to-loc (issue-specs)
  (let ((ht (make-hash-table)))
    (lit--add-locs-to-hashtable issue-specs ht)
    (lit--map-headers-to-primary-loc issue-specs ht)
    ht))

(defun lit--make-hashtable-header-line-to-dependants (issue-specs)
  (let ((ht (make-hash-table)))
    (dolist (issue-spec issue-specs)
      (when-let ((secondaries (plist-get issue-spec :secondaries)))
        (puthash (plist-get (plist-get issue-spec :primary) :pos-in-list)
                 secondaries
                 ht))
      (dolist (dataflow (plist-get issue-spec :dataflows))
        (when-let ((pos (plist-get dataflow :pos-in-list))
                   (steps (plist-get dataflow :steps)))
          (puthash pos steps ht)))
      (dolist (fix (plist-get issue-spec :fixes))
        (when-let ((pos (plist-get fix :pos-in-list))
                   (edits (plist-get fix :edits)))
          (puthash pos edits ht))))
    ht))

(defvar lit--highlighted-targets-for-lines '()
  "A list of all target overlays highlighted")

(defun lit--unnumber-lines ()
  (mapc #'delete-overlay lit--numbered-overlays)
  (setq lit--numbered-overlays '()))

(defun lit--number-line (lino num)
  (save-excursion
    (lit--goto-line lino)
    (lit--move-to-start-of-multiline)
    (goto-char (line-end-position))
    (re-search-backward "[0-9]:" (line-beginning-position))
    (forward-char)
    (let ((overlay (make-overlay (point) (line-end-position))))
      (push overlay lit--numbered-overlays)
      (overlay-put overlay 'before-string (concat " " num))
      (overlay-put overlay 'face 'lit-tail-face1))))

(defun lit--unhighlight-targets-for-lines ()
  (dolist (overlay lit--highlighted-targets-for-lines)
    (overlay-put overlay 'face nil)
    (overlay-put overlay 'before-string nil))
  (setq lit--highlighted-targets-for-lines '()))

(defun lit--highlight-target (loc focus attributes)
  (when-let* ((overlay (plist-get loc :overlay))
              (target-buffer (overlay-buffer overlay))
              (prev-buffer (current-buffer)))
    (switch-to-buffer-other-window target-buffer)
    (lit--remove-old-hl 0)
    (when focus
      (goto-char (overlay-start overlay)))
    (switch-to-buffer-other-window prev-buffer)
    (dolist (attribute attributes)
      (overlay-put overlay (car attribute) (cdr attribute)))
    (push overlay lit--highlighted-targets-for-lines)))

(defconst lit--aux-buffer-name "*tester-output*"
  "The name of the buffer holding some the issues to be added and indicating your progress.")

(defun lit--render-issue-specs-dedicated-buffer (issue-specs dedicated-buffer)
  "Produce a tester-output-like text specification in the dedicated buffer.
Opens the buffer in another window.
Return ISSUE-SPEC with each location with line numbers of the
produced specification."
  (let ((prev-buffer (current-buffer)))
    (switch-to-buffer-other-window dedicated-buffer)
    (erase-buffer)
    (let ((specs-with-lines (mapcar #'lit--render-issue-spec issue-specs)))
      (switch-to-buffer-other-window prev-buffer)
      specs-with-lines)))

(defun lit--modify-line-with-its-continuation-in-buffer (lino buf modif)
  "Apply MODIF when focused on line LINO and next line if it ends with \ in BUF."
  (when lino
    (with-current-buffer buf
      (lit--goto-line lino)
      (when-let ((window (get-buffer-window buf)))
        (set-window-point window (point)))
      (funcall modif)
      (while (and (< (point) (point-max))
                  (string-suffix-p "\\\n" (thing-at-point 'line)))
        (forward-line)
        (funcall modif)))))

(defun lit--mark-line-in-buffer (lino buf face)
  (lit--modify-line-with-its-continuation-in-buffer
   lino buf (lambda () (set-text-properties (line-beginning-position) (line-end-position) `(face ,face)))))

(defun lit--unhighlight-line-in-buffer (lino buf)
  (lit--modify-line-with-its-continuation-in-buffer
   lino buf (lambda () (remove-text-properties (line-beginning-position) (line-end-position) '(face nil)))))

(defun lit--cancel-loc (loc)
  (lit--mark-line-in-buffer (plist-get loc :pos-in-list) lit--aux-buffer-name 'lit-cancelled-line))

(defun lit--done-loc (loc)
  (lit--mark-line-in-buffer (plist-get loc :pos-in-list) lit--aux-buffer-name 'lit-processed-line))

(defun lit--considering-loc (loc)
  (lit--mark-line-in-buffer (plist-get loc :pos-in-list) lit--aux-buffer-name 'lit-currently-inserting))

(defun lit--unhighlight-loc (loc)
  (lit--unhighlight-line-in-buffer (plist-get loc :pos-in-list) lit--aux-buffer-name))

;;;###autoload
(defun lit-insert-issues (observed-report)
  "Parse OBSERVED-REPORT, and insert the corresponding spec comments into current buffer.
The OBSERVED-REPORT must be taken verbatim from the 'unexpected'
section of the tester output. It will then be split into
individual issues, parsed and inserted interactively, asking for
the placement of each issue primary and secondary locations, data
flows, and fixes.
Function also opens an auxiliary buffer in a separate window that displays
the specification and highlights inserted, cancelled, and current locs.
"
  (interactive "sInsert full 'unexpected' output: ")
  (if-let ((issue-specs (lit-parse-all-observed observed-report)))
      (lit--insert-issue-specs (lit--render-issue-specs-dedicated-buffer issue-specs lit--aux-buffer-name))
    (print "malformed 'unexpected' output")))

(defvar lit--output-unexpected-issues '()
  "A pre-parsed, pre-indexed list of the unexpected issues from last lit run.")

(defvar lit--output-unexpected-issues-line-to-loc-ht nil
  "Cached hashtable mapping lines in the rendered list of issues to locs.")

(defvar lit--output-headers-to-steps nil
  "Cached hashtable mapping the line of a header to a list of steps.
A header is any of:
- primary location (mapped to secondaries)
- dataflow description (mapped to dataflow steps)
- fix description (mapped to edits)")

;;;###autoload
(defun lit-insert-issues-from-run ()
  "Insert the unexpected issues reported by the last lit run."
  (interactive)
  (when lit--output-unexpected-issues
    (when-let* ((file (plist-get (car lit--output-unexpected-issues) :file))
                (buffer (find-buffer-visiting file)))
      (unless (equal (buffer-file-name) file)
        (switch-to-buffer-other-window buffer))
      (lit--insert-issue-specs lit--output-unexpected-issues))))

(defconst lit--removable-spec-src-regex
  " *// \\(\\(CHECK\\|FIX\\|EDIT\\|DATAFLOW\\|DATAFLOW DESCRIPTION\\|SECONDARY\\|NOEXECFLOW\\)\\(\\[.*\\]\\)? .*\\|NOFIX\\)$"
  "All the lit spec comments that should be removed when requested.
Does not include COMMENT, because it cannot be automatically recovered from tester output.")

(defun lit--delete-spec-on-the-line ()
  (let ((cur-point (point))
        (case-fold-search nil))
    (atomic-change-group
      (lit--move-to-start-of-multiline)
      (if (search-forward-regexp lit--removable-spec-src-regex (line-end-position) t)
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
      (lit--delete-spec-on-the-line)
    (save-excursion
      (let ((reg (lit--make-overlay (region-beginning) (region-end))))
        (goto-char (region-beginning))
        (while (< (point) (overlay-end reg))
          (unless (lit--delete-spec-on-the-line)
            (forward-line)))
        (lit--clear-overlay reg)))))

(defcustom lit-exec-path
  "/home/arseniy/proj/sonar-cpp/build/asserts/lit"
  "Path to the lit script that is used to run the tests."
  :type 'file)

(defun lit-parse-full-report (report)
  (let* ((unexpected-start (string-match "[0-9]+ unexpected in .* mode:\n" report))
         (unexpected-end (match-end 0))
         (missing-start (string-match "[0-9]+ missing in .* mode:\n" report))
         (missing-end (match-end 0)))
    (if unexpected-start
        `(:expected-header ,(substring report 0 unexpected-start)
          :unexpected-header ,(substring report unexpected-start unexpected-end)
          :unexpected ,(lit-parse-all-observed (substring report unexpected-end missing-start))
          ,@(if missing-start
                `(:missing-header ,(substring report missing-start missing-end)
                  :missing ,(lit-parse-all-observed (substring report missing-end)))
              '()))
      (cl-assert missing-start)
      `(:expected-header ,(substring report 0 missing-start)
        :missing-header ,(substring report missing-start missing-end)
        :missing ,(lit-parse-all-observed (substring report missing-end))))))

(defun lit--cut-and-parse-lit-output (lit-output)
  (if-let* ((report-start (string-match "[0-9]+ expected messages" lit-output))
            (report-end (string-match "error: command failed with exit status" lit-output))
            (preamble (substring lit-output 0 report-start))
            (full-report (substring lit-output report-start report-end))
            (postamble (substring lit-output report-end)))
      `( :preamble ,preamble
         :report ,(lit-parse-full-report full-report)
         :postamble ,postamble)
    (let* ((pass-start (string-match "PASS:.*\n" lit-output))
           (pass-end (match-end 0)))
      `( :preamble ,(substring lit-output 0 pass-start)
         :pass ,(substring lit-output pass-start pass-end)
         :postamble ,(substring lit-output pass-end)))))

(defface lit-irrelevant-output '((t :height .5 :weight extra-light))
  "Face used to render lit output that is of no relevance")

(defface lit-expected-heading '((t :foreground "forest green"))
  "Face used to render the 'NN expected messages' line.")

(defface lit-unexpected-heading '((t :foreground "orange red" :weight bold))
  "Face used to render the 'NN unexpected messages' line.")

(defface lit-missing-heading '((t :foreground "orange red" :weight bold))
  "Face used to render the 'NN missing messages' line.")

(defun lit--rerender-lit-output (cut-lit-output)
  (erase-buffer)
  (let ((preamble (plist-get cut-lit-output :preamble))
        (postamble (plist-get cut-lit-output :postamble))
        (ret '()))
    (insert (propertize preamble 'face 'lit-irrelevant-output))
    (if-let ((report (plist-get cut-lit-output :report)))
        (progn
          (insert (propertize (plist-get report :expected-header) 'face 'lit-expected-heading))
          (when-let ((unexpected-header (plist-get report :unexpected-header)))
            (insert (propertize unexpected-header 'face 'lit-unexpected-heading))
            (setq ret (mapcar #'lit--render-issue-spec (plist-get report :unexpected))))
          (when-let ((missing-header (plist-get report :missing-header)))
            (insert (propertize missing-header 'face 'lit-missing-heading))
                                        ; Ignore the missing issues for now. just render them but do not preserve their spec
            (mapc #'lit--render-issue-spec (plist-get report :missing))))
      (insert (propertize (plist-get cut-lit-output :pass) 'face 'lit-expected-heading)))
    (insert (propertize postamble 'face 'lit-irrelevant-output))
    ret))

(defun lit-run-tester (&optional test-file)
  "Run lit script on the file and display processed output in a dedicated buffer."
  (interactive)
  (let* ((prev-buffer (current-buffer))
        (test-file (or test-file (buffer-file-name)))
        (buffer (switch-to-buffer-other-window "*tester-output*"))
        (command (concat lit-exec-path " -DNODIFF -DNOCOLOR -v --no-progress-bar " test-file)))
    (erase-buffer)
    (lit--tester-output-mode)
    (shell-command command buffer)
    (let ((unexpected-issues (lit--rerender-lit-output
                              (lit--cut-and-parse-lit-output
                               (substring-no-properties (buffer-string))))))
      (switch-to-buffer-other-window prev-buffer)
      (setq lit--output-unexpected-issues
            (mapcar #'lit--generate-overlays
                    unexpected-issues))
      (setq lit--output-unexpected-issues-line-to-loc-ht
            (lit--make-hashtable-line-to-loc lit--output-unexpected-issues))
      (setq lit--output-headers-to-steps
            (lit--make-hashtable-header-line-to-dependants
             lit--output-unexpected-issues)))))

(provide 'lit)
;;; lit.el ends here
