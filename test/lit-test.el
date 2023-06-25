;;; lit-test.el --- Test the lit package -*- lexical-binding: t; -*-
;; Author: Arseniy Zaostrovnykh <arseniy.zaostrovnykh@sonarsource.com>

;; Created: 17.06.23

;;; Commentary:
;;; Code:

(ignore-errors (load-file "test/undercover-init.el"))

(add-to-list 'load-path "..")
(require 'lit)

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
  (lit-run-no-questions-asked initial-content default-answer (lambda () (lit--insert-issue-specs issue-specs))))


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

(ert-deftest lit--adjust-dumb-ranges-test ()
  (with-temp-buffer
    (insert "line1
// CHECK +1:1 +1:2 S100:Message
line2")
    (lit--goto-line 2)
    (end-of-line)
    (let ((overlay-pairs (lit--make-all-dumb-range-spec-overlays))
          (inserted-begin (point)))
      (insert "
line2.5")
      (lit--adjust-range-specs-after-insertion overlay-pairs inserted-begin (point)))
    (should (equal (buffer-string)
                   "line1
// CHECK +2:1 +2:2 S100:Message
line2.5
line2"))))

(provide 'lit-test)

;;; lit-test.el ends here
