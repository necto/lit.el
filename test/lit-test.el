;;; lit-test.el --- Test the lit package -*- lexical-binding: t; -*-
;; Author: Arseniy Zaostrovnykh <arseniy.zaostrovnykh@sonarsource.com>

;; Created: 17.06.23

;;; Commentary:
;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el" (:report-format 'lcov) (:send-report nil)))

(add-to-list 'load-path "..")
(require 'lit)
(require 'cl-lib)

(ert-deftest lit-move-to-start-of-multiline-test ()
  "Test moving the point to the beginning of '\'-extended chain of lines."
  (with-temp-buffer
    (insert
     " s1\n"
     " s2\\\n"
     " s3  \\\n"
     "s4 \\ \n"
     "s5\n")
    (lit--goto-line 1)
    (lit--move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 1))
    (lit--goto-line 2)
    (lit--move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 2))
    (lit--goto-line 3)
    (lit--move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 2))
    (lit--goto-line 4)
    (lit--move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 2))
    (lit--goto-line 5)
    (lit--move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 5))))

(ert-deftest lit-insert-dataflows-test-nil-id ()
  (should
   (equal
    (with-temp-buffer
      (insert "
line 2
line 3 // CHECK
line 4
")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t))
                ((symbol-function 'read-answer) (lambda (&rest args) "next"))
                (primary-overlay (lit--make-overlay 7 20 nil t)))
        (lit--insert-dataflows (lit--generate-dataflow-overlays
                                '(( :description "Description 1"
                                    :steps (( :begin (:line 2 :col 12) :end (:line 2 :col 16)
                                                     :message "This buffer access overflows")))
                                  ( :description "Description 2"
                                                 :steps (( :begin (:line 4 :col 17) :end (:line 4 :col 21)
                                                                  :message "This size argument overflows the buffer")))))
                               '("included" nil)
                               primary-overlay)
        (lit--clear-overlay primary-overlay))
      (buffer-substring-no-properties (point-min) (point-max)))
    "
line 2
// DATAFLOW -:12 -:16 included,0:This buffer access overflows
line 3 // CHECK
// DATAFLOW DESCRIPTION included:Description 1
line 4
")))

(defun lit-run-no-questions-asked (initial-buffer-content y-or-n default-answer minibuffer-answers fun)
  (with-temp-buffer
    (insert initial-buffer-content)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) y-or-n))
              ((symbol-function 'read-answer) (lambda (&rest _args) default-answer))
              ((symbol-function 'read-from-minibuffer)
               (lambda (prompt &rest _args) (funcall minibuffer-answers prompt))))
      (funcall fun))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lit-run-no-questions-asked-all-ids (initial-buffer-content default-answer fun)
  (let ((fix-cnt 0))
    (cl-flet ((all-ids (prompt)
                       (cond ((string-match-p "issue id" prompt) "issue-id")
                             ((string-match-p "id for dataflow" prompt) "df-id")
                             ((string-match-p "id for fix" prompt) (cl-incf fix-cnt) (format "fix-id%d" fix-cnt))
                             (t "unexpected"))))
      (lit-run-no-questions-asked initial-buffer-content t default-answer #'all-ids fun))))

(defun lit-run-insert-issue-spec-batch (initial-content issue-spec default-answer)
  (lit-run-no-questions-asked-all-ids initial-content default-answer
                                      (lambda () (lit-insert-issue-spec issue-spec))))

(defun lit-run-insert-issue-specs-batch (initial-content issue-specs default-answer)
  (lit-run-no-questions-asked-all-ids initial-content default-answer
                                      (lambda () (lit--insert-issue-specs issue-specs))))


(ert-deftest lit-insert-issue-spec-test-primary-only ()
  (should (equal
           (lit-run-insert-issue-spec-batch
            "
line 2
line 3
line 4
"
            '( :file "file.c"
               :rule-id "S100"
               :primary ( :begin (:line 3 :col 3) :end (:line 3 :col 5)
                          :message "The primary msg")
               :secondaries ()
               :dataflows ()
               :fixes ())
            "prev")
           "
line 2
// CHECK +:3 +:5 S100:The primary msg
line 3
line 4
")))

(ert-deftest lit-insert-issue-specs-test-two-primaries-second-updates-first ()
  (should (equal
           (lit-run-insert-issue-specs-batch
            "
line 2
line 3
line 4
"
            '(( :file "file.c"
                :rule-id "S100"
                :primary ( :begin (:line 2 :col 1) :end (:line 3 :col 5)
                           :message "Primary1")
                :secondaries ()
                :dataflows ()
                :fixes ())
              ( :file "file.c"
                :rule-id "S1234"
                :primary ( :begin (:line 3 :col 1) :end (:line 3 :col 5)
                           :message "Primary2")
                :secondaries ()
                :dataflows ()
                :fixes ()))
            "prev")
           "
// CHECK +1:1 +3:5 S100:Primary1
line 2
// CHECK +:1 +:5 S1234:Primary2
line 3
line 4
")))

(ert-deftest lit-insert-issue-specs-test-two-primaries-second-updated ()
  (should (equal
           (lit-run-insert-issue-specs-batch
            "
line 2
line 3
line 4
"
            '(( :file "file.c"
                :rule-id "S100"
                :primary ( :begin (:line 3 :col 1) :end (:line 3 :col 5)
                           :message "Primary1")
                :secondaries ()
                :dataflows ()
                :fixes ())
              ( :file "file.c"
                :rule-id "S1234"
                :primary ( :begin (:line 2 :col 1) :end (:line 3 :col 5)
                           :message "Primary2")
                :secondaries ()
                :dataflows ()
                :fixes ()))
            "prev")
           "
// CHECK +1:1 +3:5 S1234:Primary2
line 2
// CHECK +:1 +:5 S100:Primary1
line 3
line 4
")))

(ert-deftest lit-insert-issue-spec-test-data-flows ()
  (should (equal
           (lit-run-insert-issue-spec-batch
            "
line 2
line 3
line 4
"
            '( :file "file.c"
               :rule-id "S100"
               :primary ( :begin (:line 3 :col 3) :end (:line 3 :col 21)
                          :message "The primary msg")
               :secondaries
               (( :begin (:line 2 :col 3) :end (:line 2 :col 5)
                  :message "Taking true branch"))
               :dataflows
               (( :description "Description 1"
                  :steps (( :begin (:line 2 :col 12) :end (:line 2 :col 16)
                            :message "This buffer access overflows")
                          ( :begin (:line 3 :col 13) :end (:line 3 :col 20)
                            :message "Initializing parameter to this expression")))
                ( :description "Description 2"
                  :steps (( :begin (:line 4 :col 17) :end (:line 4 :col 21)
                            :message "This size argument overflows the buffer"))))
               :fixes ())
            "prev")
           "
// SECONDARY +:3 +:5 issue-id:Taking true branch
// DATAFLOW +:12 +:16 df-id,0:This buffer access overflows
line 2
// CHECK +:3 +:21 S100(issue-id,df-id,df-id):The primary msg
// DATAFLOW DESCRIPTION df-id:Description 2
// DATAFLOW DESCRIPTION df-id:Description 1
// DATAFLOW +:13 +:20 df-id,1:Initializing parameter to this expression
line 3
// DATAFLOW +:17 +:21 df-id,0:This size argument overflows the buffer
line 4
")))

(ert-deftest lit-insert-issue-spec-test-same-lines ()
  (should (equal
           (lit-run-insert-issue-spec-batch
            "
line 2
line 3
line 4
"
            '( :file "file.c" :rule-id "S100"
               :primary ( :begin (:line 3 :col 3) :end (:line 3 :col 21) :message "Prim")
               :secondaries (( :begin (:line 2 :col 3) :end (:line 2 :col 5) :message "Sec"))
               :dataflows (( :description "Description 1" :steps ())
                           ( :description "Description 2" :steps ()))
               :fixes ())
            "same")
           "
line 2 // SECONDARY :3 :5 issue-id:Sec
line 3 // CHECK :3 :21 S100(issue-id,df-id,df-id):Prim
// DATAFLOW DESCRIPTION df-id:Description 2
// DATAFLOW DESCRIPTION df-id:Description 1
line 4
")))

(ert-deftest lit-insert-issue-spec-test-next-lines ()
  (should (equal
           (lit-run-insert-issue-spec-batch
            "
line 2
line 3
line 4
"
            '( :file "file.c" :rule-id "S100"
               :primary ( :begin (:line 3 :col 3) :end (:line 3 :col 21) :message "Prim")
               :secondaries (( :begin (:line 2 :col 3) :end (:line 2 :col 5) :message "Sec"))
               :dataflows (( :description "Description 1" :steps ()))
               :fixes
               ((:description "F1"
                 :edits ((:begin (:line 4 :col 3) :end (:line 4 :col 4)
                          :message "bread")))))
            "next")
           "
line 2
// SECONDARY -:3 -:5 issue-id:Sec
line 3
// CHECK -:3 -:21 S100(issue-id,df-id):Prim
// FIX fix-id1:F1
// DATAFLOW DESCRIPTION df-id:Description 1
line 4
// EDIT fix-id1 -:3 -:4 `bread`
")))

(defun lit-test-insert-issue-with-2-secs-on-lines (sec-line1 sec-line2)
  (lit-run-insert-issue-spec-batch
   "
line 2
line 3
line 4
"
   `( :file "file.c" :rule-id "S100"
      :primary ( :begin (:line 3 :col 3) :end (:line 3 :col 21) :message "Prim")
      :secondaries
      (( :begin (:line ,sec-line1 :col 1) :end (:line ,sec-line1 :col 2) :message "Sec1")
       ( :begin (:line ,sec-line2 :col 1) :end (:line ,sec-line2 :col 2) :message "Sec2"))
      :dataflows ()
      :fixes ())
   "prev"))

(ert-deftest lit-insert-issue-spec-test-secs-before-before ()
  (should (equal
           (lit-test-insert-issue-with-2-secs-on-lines 2 2)
           "
// SECONDARY +:1 +:2 issue-id:Sec1
// SECONDARY +:1 +:2 issue-id:Sec2
line 2
// CHECK +:3 +:21 S100(issue-id):Prim
line 3
line 4
")))

(ert-deftest lit-insert-issue-spec-test-secs-before-same ()
  (should (equal
           (lit-test-insert-issue-with-2-secs-on-lines 2 3)
           "
// SECONDARY +:1 +:2 issue-id:Sec1
line 2
// CHECK +:3 +:21 S100(issue-id):Prim
// SECONDARY +:1 +:2 issue-id:Sec2
line 3
line 4
")))

(ert-deftest lit-insert-issue-spec-test-secs-same-before ()
  (should (equal
           (lit-test-insert-issue-with-2-secs-on-lines 3 2)
           "
// SECONDARY +:1 +:2 issue-id:Sec2
line 2
// CHECK +:3 +:21 S100(issue-id):Prim
// SECONDARY +:1 +:2 issue-id:Sec1
line 3
line 4
")))

(ert-deftest lit-insert-issue-spec-test-secs-same-same ()
  (should (equal
           (lit-test-insert-issue-with-2-secs-on-lines 3 3)
           "
line 2
// CHECK +:3 +:21 S100(issue-id):Prim
// SECONDARY +:1 +:2 issue-id:Sec1
// SECONDARY +:1 +:2 issue-id:Sec2
line 3
line 4
")))

(ert-deftest lit-insert-issue-spec-test-secs-same-next ()
  (should (equal
           (lit-test-insert-issue-with-2-secs-on-lines 3 4)
           "
line 2
// CHECK +:3 +:21 S100(issue-id):Prim
// SECONDARY +:1 +:2 issue-id:Sec1
line 3
// SECONDARY +:1 +:2 issue-id:Sec2
line 4
")))

(ert-deftest lit-insert-issue-spec-test-secs-next-same ()
  (should (equal
           (lit-test-insert-issue-with-2-secs-on-lines 4 3)
           "
line 2
// CHECK +:3 +:21 S100(issue-id):Prim
// SECONDARY +:1 +:2 issue-id:Sec2
line 3
// SECONDARY +:1 +:2 issue-id:Sec1
line 4
")))

(ert-deftest lit-insert-issue-spec-test-secs-next-next ()
  (should (equal
           (lit-test-insert-issue-with-2-secs-on-lines 4 4)
           "
line 2
// CHECK +:3 +:21 S100(issue-id):Prim
line 3
// SECONDARY +:1 +:2 issue-id:Sec1
// SECONDARY +:1 +:2 issue-id:Sec2
line 4
")))

(defun lit-test-insert-issue-intersecting-ranges (sec-begin sec-end)
  (lit-run-insert-issue-spec-batch
   "
line 2
line 3
line 4
line 5
"
   `( :file "file.c" :rule-id "S100"
      :primary ( :begin (:line 3 :col 2) :end (:line 4 :col 4) :message "Prim")
      :secondaries
      (( :begin ,sec-begin :end ,sec-end :message "Sec"))
      :dataflows ()
      :fixes ())
   "prev"))

(ert-deftest lit-insert-isuee-spec-test-before-same ()
  (should (equal
           (lit-test-insert-issue-intersecting-ranges '(:line 2 :col 2) '(:line 3 :col 2))
           "
// SECONDARY +1:2 +3:2 issue-id:Sec
line 2
// CHECK +1:2 +2:4 S100(issue-id):Prim
line 3
line 4
line 5
")))

(ert-deftest lit-insert-isuee-spec-test-before-middle ()
  (should (equal
           (lit-test-insert-issue-intersecting-ranges '(:line 2 :col 2) '(:line 4 :col 2))
           "
// SECONDARY +1:2 +4:2 issue-id:Sec
line 2
// CHECK +1:2 +2:4 S100(issue-id):Prim
line 3
line 4
line 5
")))

(ert-deftest lit-insert-isuee-spec-test-middle-middle ()
  ;; CHECK breaks here, because it is inserted first, and not re-adjusted when the SECONDARY is inserted
  (should (equal
           (lit-test-insert-issue-intersecting-ranges '(:line 3 :col 3) '(:line 4 :col 2))
           "
line 2
// CHECK +2:2 +3:4 S100(issue-id):Prim
// SECONDARY +1:3 +2:2 issue-id:Sec
line 3
line 4
line 5
")))

(ert-deftest lit-insert-isuee-spec-test-middle-after ()
  ;; CHECK breaks here, because it is inserted first, and not re-adjusted when the SECONDARY is inserted
  (should (equal
           (lit-test-insert-issue-intersecting-ranges '(:line 4 :col 2) '(:line 5 :col 3))
           "
line 2
// CHECK +1:2 +3:4 S100(issue-id):Prim
line 3
// SECONDARY +1:2 +2:3 issue-id:Sec
line 4
line 5
")))

(ert-deftest lit-insert-isuee-spec-test-after-after ()
  (should (equal
           (lit-test-insert-issue-intersecting-ranges '(:line 5 :col 5) '(:line 5 :col 3))
           "
line 2
// CHECK +1:2 +2:4 S100(issue-id):Prim
line 3
line 4
// SECONDARY +:5 +:3 issue-id:Sec
line 5
")))

(ert-deftest lit-insert-isuee-spec-test-before-after ()
  (should (equal
           (lit-test-insert-issue-intersecting-ranges '(:line 2 :col 1) '(:line 5 :col 3))
           "
// SECONDARY +1:1 +5:3 issue-id:Sec
line 2
// CHECK +1:2 +2:4 S100(issue-id):Prim
line 3
line 4
line 5
")))

(ert-deftest lit-insert-issue-spec-test-fixes ()
  (should (equal
           (lit-run-insert-issue-spec-batch
            "
line 2
line 3
line 4 with some long text to get 20 column
"
            '( :file "file.c"
               :rule-id "S100"
               :primary ( :begin (:line 3 :col 3) :end (:line 3 :col 21)
                          :message "The primary msg")
               :secondaries
               (( :begin (:line 2 :col 1) :end (:line 2 :col 5)
                  :message "branch 1")
                ( :begin (:line 3 :col 1) :end (:line 3 :col 5)
                  :message "branch 2")
                ( :begin (:line 4 :col 1) :end (:line 4 :col 5)
                  :message "branch 3"))
               :dataflows ()
               :fixes
               (( :description "Fix description 1"
                  :edits (( :begin (:line 2 :col 1) :end (:line 2 :col 5)
                            :message "Insertion\\
On three\\
lines")
                          ( :begin (:line 3 :col 4) :end (:line 4 :col 20)
                            :message "")))
                ( :description "Fix description 2"
                  :edits (( :begin (:line 3 :col 1) :end (:line 3 :col 7)
                            :message "Short replacement")))))
            "prev")
           "
// SECONDARY +:1 +:5 issue-id:branch 1
// EDIT fix-id1 +:1 +:5 `Insertion\\
On three\\
lines`
line 2
// CHECK +:3 +:21 S100(issue-id):The primary msg
// FIX fix-id1:Fix description 1
// FIX fix-id2:Fix description 2
// SECONDARY +:1 +:5 issue-id:branch 2
// EDIT fix-id1 +2:4 +4:20 ``
// EDIT fix-id2 +:1 +:7 `Short replacement`
line 3
// SECONDARY +:1 +:5 issue-id:branch 3
line 4 with some long text to get 20 column
")))

;; TODO: more tests for insertion of FIXes
;; For example: indentation (it sholudn't eat the spaces in the inserted text)

(ert-deftest lit-render-primary-test ()
  (with-temp-buffer
    (insert "line1\n")
    (should (equal (lit--render-primary '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                           :message "Hello")
                                        "S100")
                   '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                      :message "Hello" :pos-in-list 2)))
    (should (equal (buffer-string)
                   "line1\n1:2 3:4 S100:Hello\n"))))

(ert-deftest lit-render-secondary-test ()
  (with-temp-buffer
    (should (equal (lit--render-secondary '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                             :message "Hello"))
                   '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                      :message "Hello" :pos-in-list 1)))
    (should (equal (lit--render-secondary '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                             :message "Hello"))
                   '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                      :message "Hello" :pos-in-list 2)))
    (should (equal (buffer-string)
                   "    1:2 3:4:Hello\n    1:2 3:4:Hello\n"))))

(ert-deftest lit-render-dataflow-test ()
  (with-temp-buffer
    (should (equal (lit--render-dataflow '( :description "Df1"
                                            :steps (( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                                             :message "df1-step1")
                                                    ( :begin (:line 5 :col 6) :end (:line 7 :col 8)
                                                             :message "df1-step2"))))
                   '( :description "Df1"
                      :steps (( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                       :message "df1-step1" :pos-in-list 2)
                              ( :begin (:line 5 :col 6) :end (:line 7 :col 8)
                                       :message "df1-step2" :pos-in-list 3))
                      :pos-in-list 1)))
    (should (equal (buffer-string)
                   "      // DATAFLOW DESCRIPTION:Df1\n        1:2 3:4:df1-step1\n        5:6 7:8:df1-step2\n"))))

(ert-deftest lit-render-fix-test ()
  (with-temp-buffer
    (insert "line1\n")
    (should (equal (lit--render-fix '( :description "quick fix"
                                     :edits (( :begin (:line 10 :col 20) :end (:line 30 :col 40)
                                               :message "edit1\\
second part")
                                             ( :begin (:line 9 :col 8) :end (:line 7 :col 6)
                                               :message "edit2\\
second part\\
third part"))))
                   '( :description "quick fix"
                      :edits (( :begin (:line 10 :col 20) :end (:line 30 :col 40)
                                :message "edit1\\
second part"
                                :pos-in-list 3)
                              ( :begin (:line 9 :col 8) :end (:line 7 :col 6)
                                :message "edit2\\
second part\\
third part"
                                :pos-in-list 5))
                      :pos-in-list 2)))
    (should (equal (buffer-string)
                   "line1
    // FIX quick fix
    // EDIT 10:20 30:40 `edit1\\
second part`
    // EDIT 9:8 7:6 `edit2\\
second part\\
third part`
"))))

(ert-deftest lit-render-issue-spec-test ()
  (with-temp-buffer
    (insert "\n")
    (should
     (equal (lit--render-issue-spec
             '( :file "file.cpp"
                :rule-id "S1828"
                :primary ( :begin (:line 1 :col 2) :end (:line 1 :col 3) :message "Prim")
                :secondaries
                (( :begin (:line 1 :col 2) :end (:line 1 :col 5) :message "Sec"))
                :dataflows
                (( :description "Dataflow"
                   :steps (( :begin (:line 8 :col 11) :end (:line 9 :col 12) :message "step"))))
                :fixes
                (( :description "Fix"
                   :edits (( :begin (:line 3 :col 1) :end (:line 18 :col 3)
                             :message "multi\\
line"))))))
            '( :file "file.cpp"
               :rule-id "S1828"
               :primary ( :begin (:line 1 :col 2) :end (:line 1 :col 3) :message "Prim" :pos-in-list 2)
               :secondaries
               (( :begin (:line 1 :col 2) :end (:line 1 :col 5) :message "Sec" :pos-in-list 3))
               :dataflows
               (( :description "Dataflow"
                  :steps (( :begin (:line 8 :col 11) :end (:line 9 :col 12) :message "step" :pos-in-list 5))
                  :pos-in-list 4))
               :fixes
               (( :description "Fix"
                  :edits (( :begin (:line 3 :col 1) :end (:line 18 :col 3)
                            :message "multi\\
line"
                            :pos-in-list 7))
                  :pos-in-list 6)))))
    (should (equal (buffer-string)
                   "
1:2 1:3 S1828:Prim
    1:2 1:5:Sec
      // DATAFLOW DESCRIPTION:Dataflow
        8:11 9:12:step
    // FIX Fix
    // EDIT 3:1 18:3 `multi\\
line`
"))))

(ert-deftest lit-modify-line-with-its-continuation-in-buffer-test ()
  (let ((tmp-buf (generate-new-buffer (generate-new-buffer-name "test-tmp")))
        (cur-buf (current-buffer)))
    (unwind-protect
        (cl-flet ((check-with-bang-to-line (lino expected)
                                           (with-current-buffer tmp-buf
                                             (erase-buffer)
                                             (insert "
line2
line3\\
line4\\
line5
line6\\"))
                                           (lit--modify-line-with-its-continuation-in-buffer
                                            lino tmp-buf (lambda () (forward-line 0) (insert "!")))
                                           (should (equal (current-buffer) cur-buf))
                                           (should (equal (with-current-buffer tmp-buf (buffer-string))
                                                          expected))))
          (check-with-bang-to-line 2 "
!line2
line3\\
line4\\
line5
line6\\")
          (check-with-bang-to-line 3 "
line2
!line3\\
!line4\\
!line5
line6\\")
          (check-with-bang-to-line 4 "
line2
line3\\
!line4\\
!line5
line6\\")
          (check-with-bang-to-line 5 "
line2
line3\\
line4\\
!line5
line6\\")
          (check-with-bang-to-line 6 "
line2
line3\\
line4\\
line5
!line6\\")
          (check-with-bang-to-line nil "
line2
line3\\
line4\\
line5
line6\\"))
      (kill-buffer tmp-buf))))

(ert-deftest lit-delete-spec-on-the-line-test ()
  (cl-flet ((check-by-deleting-line
             (line expected-line expected-column expected-text)
             (with-temp-buffer
               (insert "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
               (lit--goto-line line)
               (move-to-column 3)
               (lit--delete-spec-on-the-line)
               (should (equal (line-number-at-pos) expected-line))
               (should (equal (current-column) expected-column))
               (should (equal (buffer-string) expected-text)))))
    (check-by-deleting-line 2 2 0 "
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
    (check-by-deleting-line 3 2 0 "
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
    (check-by-deleting-line 4 4 3 "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
    (check-by-deleting-line 5 6 0 "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line;

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
    (check-by-deleting-line 6 6 0 "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
    (check-by-deleting-line 7 7 0 "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // FIX static3:Declare each variable separately
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
    (check-by-deleting-line 8 8 0 "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // EDIT static3 +:16 +:18 `;\\
  static int `
")
    (check-by-deleting-line 9 9 0 "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
")
    (check-by-deleting-line 10 9 0 "
  // EDIT static2 +:15 +:17 `;\\
  static int `
  static int c, *d;
 another line; // DATAFLOW DESCRIPTION blabla

  // CHECK +:3 +:19 S1659
  // FIX static3:Declare each variable separately
")))

(ert-deftest lit-insert-issue-spec-test-primary-adjust-existing ()
  (should (equal
           (lit-run-insert-issue-spec-batch
            "
line 2 // CHECK +1:1 +2:2 S456:Prim1
line 3
line 4 // CHECK -2:1 -1:3 S654:2priM
"
            '( :file "file.c"
               :rule-id "S100"
               :primary ( :begin (:line 3 :col 3) :end (:line 3 :col 5)
                          :message "The primary msg")
               :secondaries ()
               :dataflows ()
               :fixes ())
            "prev")
           "
line 2 // CHECK +2:1 +3:2 S456:Prim1
// CHECK +:3 +:5 S100:The primary msg
line 3
line 4 // CHECK -3:1 -1:3 S654:2priM
")))

(ert-deftest lit-insert-issue-specs-test-three-secs-middle-updates-first-and-second ()
  "Insert the issue with secondaries with intersecting ranges.
Check that ranges are all correctly updates."
  (should (equal
           (lit-run-insert-issue-specs-batch
            "
line 2
line 3
line 4
"
            '(( :file "file.c"
                :rule-id "S100"
                :primary ( :begin (:line 2 :col 1) :end (:line 3 :col 5)
                           :message "Prim1")
                :secondaries
                (( :begin (:line 3 :col 1) :end (:line 4 :col 5) :message "Sec1")
                 ( :begin (:line 2 :col 1) :end (:line 3 :col 5) :message "Sec2")
                 ( :begin (:line 3 :col 1) :end (:line 4 :col 5) :message "Sec3"))
                :dataflows ()
                :fixes ()))
            "prev")
           "
// CHECK +2:1 +5:5 S100(issue-id):Prim1
// SECONDARY +1:1 +4:5 issue-id:Sec2
line 2
// SECONDARY +2:1 +3:5 issue-id:Sec1
// SECONDARY +1:1 +2:5 issue-id:Sec3
line 3
line 4
")))

(defun lit-issue-id-second-attempt (default-answer issue-spec initial-buffer-content)
  (cl-flet ((on-second-attempt (prompt)
                               (cond ((string-match-p "issue id" prompt) "!not an Id")
                                     ((string-match-p "valid identifier" prompt) "valid_id")
                                     (t "unexpected"))))
    (lit-run-no-questions-asked initial-buffer-content t default-answer #'on-second-attempt
                                (lambda () (lit-insert-issue-spec issue-spec)))))

(ert-deftest lit-reprompt-for-valid-issue-id-test ()
  (should (equal (lit-issue-id-second-attempt
                  "next"
                  '(:file "file.c" :rule-id "S999"
                    :primary (:begin (:line 1 :col 1) :end (:line 1 :col 2) :message "P")
                    :secondaries ((:begin (:line 1 :col 1) :end (:line 1 :col 2) :message "Sec"))
                    :dataflows () :fixes ())
                  "line1\n")
                 "line1\n// SECONDARY -:1 -:2 valid_id:Sec\n// CHECK -:1 -:2 S999(valid_id):P\n")))

(defun lit-cancel-all-ids (default-answer issue-spec initial-buffer-content)
  (cl-flet ((cancel-ids (_prompt) ""))
    (lit-run-no-questions-asked initial-buffer-content t default-answer #'cancel-ids
                                (lambda () (lit-insert-issue-spec issue-spec)))))

(ert-deftest lit-cancel-all-ids-test ()
  (should (equal (lit-cancel-all-ids
                  "same"
                  '(:file "file.c" :rule-id "S999"
                    :primary (:begin (:line 1 :col 1) :end (:line 1 :col 2) :message "P")
                    :secondaries ((:begin (:line 1 :col 1) :end (:line 1 :col 2) :message "Sec"))
                    :dataflows
                    ((:description "df1"
                      :steps ((:begin (:line 2 :col 12) :end (:line 2 :col 16)
                               :message "DF step1"))))
                    :fixes ((:description "F1"
                             :edits ((:begin (:line 4 :col 3) :end (:line 4 :col 4)
                                      :message "bread")))))
                  "line1\n")
                 "line1 // CHECK :1 :2 S999:P\n")))

(defun lit-cancel-fixes-and-df-descrs (default-answer issue-spec initial-buffer-content)
  (cl-flet ((same-ids (_prompt) "id"))
    (lit-run-no-questions-asked initial-buffer-content nil default-answer #'same-ids
                                (lambda () (lit-insert-issue-spec issue-spec)))))

(ert-deftest lit-cancel-fixes-and-df-descrs-test ()
  (should (equal (lit-cancel-fixes-and-df-descrs
                  "next"
                  '(:file "file.c" :rule-id "S999"
                    :primary (:begin (:line 1 :col 1) :end (:line 1 :col 2) :message "P")
                    :secondaries ((:begin (:line 1 :col 1) :end (:line 1 :col 2) :message "Sec"))
                    :dataflows
                    ((:description "df1"
                      :steps ((:begin (:line 1 :col 3) :end (:line 1 :col 4)
                               :message "DF step"))))
                    :fixes ((:description "F1"
                             :edits ((:begin (:line 1 :col 3) :end (:line 1 :col 4)
                                      :message "bread")))))
                  "line1\n")
                 "line1
// DATAFLOW -:3 -:4 id,0:DF step
// SECONDARY -:1 -:2 id:Sec
// CHECK -:1 -:2 S999(id,id):P
")))

(ert-deftest lit-column-number-at-pos-test ()
  (with-temp-buffer
    (insert "line1
line2
linelineee3

l4")
    (should (equal (lit--column-number-at-pos 3) 2))
    (should (equal (lit--column-number-at-pos 9) 2))
    (should (equal (lit--column-number-at-pos 15) 2))
    (should (equal (lit--column-number-at-pos 23) 10))
    (should (equal (lit--column-number-at-pos 24) 11))
    (should (equal (lit--column-number-at-pos 25) 0))
    (should (equal (lit--column-number-at-pos 26) 0))
    (should (equal (lit--column-number-at-pos 27) 1))))

(ert-deftest lit-render-dumb-spec-test ()
  (with-temp-buffer
    (insert "line1
line2
line3")
    (should (equal (lit--render-dumb-spec 2 1 2) "-1:1 -1:2"))
    (should (equal (lit--render-dumb-spec 2 2 5) "-1:2 -1:5"))
    (should (equal (lit--render-dumb-spec 1 7 9) "+1:1 +1:3"))
    (should (equal (lit--render-dumb-spec 1 7 15) "+1:1 +2:3"))
    (should (equal (lit--render-dumb-spec 1 14 15) "+2:2 +2:3"))))

(ert-deftest lit-rewrite-spec-for-pair-test ()
  (with-temp-buffer
    (insert "// bla-bla 'XX:XX XX:XX'
line2")
    (let ((spec-overlay (make-overlay 13 24))
          (target-overlay (make-overlay 28 30)))
      (lit--rewrite-spec-for-pair (cons spec-overlay target-overlay))
      (should (equal (buffer-string)
                     "// bla-bla '+1:3 +1:5'
line2")))))

(ert-deftest lit--add-locs-to-hashtable-test ()
  (let ((ht (make-hash-table)))
    (lit--add-locs-to-hashtable '() ht)
    (should (equal (hash-table-count ht) 0))
    (lit--add-locs-to-hashtable '(1 2 (3 4) :here "five") ht)
    (should (equal (hash-table-count ht) 0)))
  (let ((ht (make-hash-table)))
    (lit--add-locs-to-hashtable '(:id 1 :str "hi" :pos-in-list 14) ht)
    (should (equal (hash-table-count ht) 1))
    (should (equal (gethash 14 ht) '(:id 1 :str "hi" :pos-in-list 14)))
    (lit--add-locs-to-hashtable
     '(( :name submarine :pos-in-list 0)
       ( "irrelevant" 1 43 '() ( :kind relevant :pos-in-list 3
                                       ( :kind nested :pos-in-list 4))))
     ht)
    (should (equal (hash-table-count ht) 4))
    (should (equal (gethash 0 ht) '(:name submarine :pos-in-list 0)))
    (should (equal (gethash 4 ht) '(:kind nested :pos-in-list 4)))
    (should (equal (gethash 3 ht) '(:kind relevant :pos-in-list 3
                                    (:kind nested :pos-in-list 4))))))

(ert-deftest lit--map-headers-to-primary-loc-test ()
  (let ((ht (make-hash-table)))
    (puthash 1 2 ht)
    (lit--map-headers-to-primary-loc
     '((:dataflows ((:description "gg" :pos-in-list 3 :steps ()))))
     ht)
    (should (equal (hash-table-count ht) 1))
    (lit--map-headers-to-primary-loc
     '((:dataflows ((:description "gg" :pos-in-list 3 :steps ()))
        :primary (:something :something)))
     ht)
    (should (equal (hash-table-count ht) 2))
    (should (equal (gethash 3 ht) '(:something :something)))
    (lit--map-headers-to-primary-loc
     '((:dataflows ((:pos-in-list 5) (:pos-in-list 8))
        :primary "another beast")
       (:dataflows ((:pos-in-list 50) (:pos-in-list 80))
        :primary 42))
     ht)
    (should (equal (hash-table-count ht) 6))
    (should (equal (gethash 5 ht) (gethash 8 ht)))
    (should (equal (gethash 5 ht) "another beast"))
    (should (equal (gethash 50 ht) (gethash 80 ht)))
    (should (equal (gethash 50 ht) 42))))

(ert-deftest lit--make-hashtable-header-line-to-dependants ()
  (let ((ht (lit--make-hashtable-header-line-to-dependants
             '((:primary (:message "m" :pos-in-list 13)
                :secondaries ((:message "m1" :pos-in-list 3)
                              (:arbitrary "prop"))
                :dataflows () :fixes ())
               (:primary (:pos-in-list 15)
                :secondaries (("some"))
                :dataflows ((:description "df1" :pos-in-list 34
                                          :steps (1 2 3))
                            (:description "df2" :pos-in-list 84
                                          :steps ("a" "b" "c")))
                :fixes ((:pos-in-list 1 :edits ())
                        (:pos-in-list 2 :edits (a b c))))))))
    (should (equal (gethash 13 ht)
                   '((:message "m1" :pos-in-list 3) (:arbitrary "prop"))))
    (should (equal (gethash 15 ht) '(("some"))))
    (should (equal (gethash 34 ht) '(1 2 3)))
    (should (equal (gethash 84 ht) '("a" "b" "c")))
    (should (equal (gethash 2 ht) '(a b c)))
    (should (equal (hash-table-count ht) 5))))

(defconst lit--test-str-with-specs
  "line1
line2 // CHECK :1 :2 S100:message
// COMMENT message
// SECONDARY :1 :2 id: message
// FIX fix-id:description
line6 // EDIT fix-id :1 :2 `mes\\
age'
// EDIT fix-id -:3 -:4 `l1\\
l2\\
l3'
line11")

(ert-deftest lit--is-cur-line-pure-lit-spec-test ()
  (with-temp-buffer
    (insert lit--test-str-with-specs)
    (lit--goto-line 1) (should (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 2) (should (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 3) (should-not (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 4) (should-not (equal (lit--is-cur-line-pure-lit-spec) t))
    (lit--goto-line 5) (should-not (equal (lit--is-cur-line-pure-lit-spec) t))
    (lit--goto-line 6) (should (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 7) (should (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 8) (should-not (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 9) (should-not (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 10) (should-not (equal (lit--is-cur-line-pure-lit-spec) nil))
    (lit--goto-line 11) (should (equal (lit--is-cur-line-pure-lit-spec) nil))))

(ert-deftest lit--point-at-col-and-line-test ()
  (with-temp-buffer
    (insert lit--test-str-with-specs)
    (cl-flet ((relative-to (lino col line-offset)
                           (lit--goto-line lino)
                           (goto-char (lit--point-at-col-and-line col line-offset))
                           (cons (line-number-at-pos) (current-column))))
      (should (equal (relative-to 3 3 '(:prev 2)) '(1 . 2)))
      (should (equal (relative-to 3 1 '(:next 2)) '(5 . 0)))
      (should (equal (relative-to 3 4 '(:smart-next)) '(6 . 3)))
      (should (equal (relative-to 3 2 '(:smart-prev)) '(2 . 1)))
      (should (equal (relative-to 6 2 '(:smart-prev)) '(6 . 1)))
      (should (equal (relative-to 6 2 '(:smart-next)) '(6 . 1)))
      (should (equal (relative-to 9 2 '(:smart-prev)) '(7 . 1))) ; multiline 6-7 is non-pure, stop at 7
      (should (equal (relative-to 9 2 '(:smart-next)) '(11 . 1)))
      (should (equal (relative-to 10 2 '(:smart-prev)) '(7 . 1))) ; Same as from line 9
      (should (equal (relative-to 10 2 '(:smart-next)) '(11 . 1)))))); Same as from line 9

(defun lit--buf-string-with-overlay-positions ()
  (let ((positions
         (sort (apply #'append
                (mapcar (lambda (overlay) `((:start . ,(overlay-start overlay))
                                       (:end . ,(overlay-end overlay))))
                        (overlays-in (point-min) (point-max))))
               (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
        (original-string (buffer-string)))
    (with-temp-buffer
      (insert original-string)
      (dolist (pos positions)
        (goto-char (cdr pos))
        (insert (pcase (car pos)
                  (:start "(")
                  (:end ")"))))
      (buffer-string))))

(defun lit--collect-all-highlight-faces ()
  (let ((hl-overlays (overlays-in (point-min) (point-max))))
    (mapcar (lambda (overlay) (overlay-get overlay 'face)) hl-overlays)))

(ert-deftest lit-mode-moves-test()
  (with-temp-buffer
    (insert lit--test-str-with-specs)
    (lit-mode)
    (goto-char (point-min))
    (search-forward "COMMENT")
    (previous-line) ; intentionally call interactive function
    (let ((highlight-faces (lit--collect-all-highlight-faces)))
      (should (equal (length highlight-faces) 2))
      (should (equal (length (cl-remove-duplicates highlight-faces)) 1)))
    (should (equal (lit--buf-string-with-overlay-positions)
                   "line1
(l)ine2 // (CHECK) :1 :2 S100:message
// COMMENT message
// SECONDARY :1 :2 id: message
// FIX fix-id:description
line6 // EDIT fix-id :1 :2 `mes\\
age'
// EDIT fix-id -:3 -:4 `l1\\
l2\\
l3'
line11"))
    (search-forward "age'")
    (next-line) ; intentionally call interactive function
    (let ((highlight-faces (lit--collect-all-highlight-faces)))
      (should (equal (length highlight-faces) 4))
      (should (equal (length (cl-remove-duplicates highlight-faces)) 2)))
    (should (equal (lit--buf-string-with-overlay-positions)
                   "line1
(l)ine2 // (CHECK) :1 :2 S100:message
// COMMENT message
// SECONDARY :1 :2 id: message
// FIX fix-id:description
line6 // EDIT fix-id :1 :2 `mes\\
ag(e)'
// (EDIT) fix-id -:3 -:4 `l1\\
l2\\
l3'
line11"))
    (dotimes (_ 4)
      (previous-line))
    ;; the first highlighting must've been removed
    (let ((highlight-faces (lit--collect-all-highlight-faces)))
      (should (equal (length highlight-faces) 6))
      (should (equal (length (cl-remove-duplicates highlight-faces)) 3)))
    (should (equal (lit--buf-string-with-overlay-positions)
                   "line1
line2 // CHECK :1 :2 S100:message
// COMMENT message
(/)/ (SECONDARY) :1 :2 id: message
// FIX fix-id:description
(l)ine6 // (EDIT) fix-id :1 :2 `mes\\
ag(e)'
// (EDIT) fix-id -:3 -:4 `l1\\
l2\\
l3'
line11"))))

(ert-deftest lit--find-closest-defined-identifier ()
  (cl-flet ((call (contents target)
                  (with-temp-buffer
                    (insert contents)
                    (let* ((target-begin (string-match (regexp-quote target) contents))
                           (target-end (match-end 0))
                           (target-overlay (make-overlay target-begin target-end)))
                      (goto-char (point-min))
                      (lit--find-closest-defined-identifier target-overlay)))))
    (should (equal (call "int f1() {\nint f2() {\n target\n}\n}\n" "target") "f2"))
    (should (equal (call "int f1() {\n}\n target \nint f2() {\n\n}\n" "target") "f1"))
    (should (equal (call "std::list<int> f1() {\n}\n target \nint f2() {\n\n}\n" "target") "f1"))
    (should (equal (call "class X {\nconst int& f1() {\n}\n target \n" "target") "f1"))
    (should (equal (call "class X {\ntarget\nconst int& f1() {\n}\n \n" "target") "X"))
    (should (equal (call "class X :public Y {\npublic:\n  target\n } \n" "target") "X"))
    (should (equal (call "struct X :public ns::Y<void> {\npublic:\n  target\n } \n" "target") "X"))))

(provide 'lit-test)

;;; lit-test.el ends here
