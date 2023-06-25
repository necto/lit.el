
(require 'lit)

(ert-deftest lit-line-offset-parse-test ()
  "Tests the parsing of the (smart) offset in the range spec."
  (should (equal (lit-line-offset-parse nil) '(:same)))
  (should (equal (lit-line-offset-parse "   -") '(:smart-prev)))
  (should (equal (lit-line-offset-parse "+") '(:smart-next)))
  (should (equal (lit-line-offset-parse "  +18") '(:next 18)))
  (should (equal (lit-line-offset-parse "-93") '(:prev 93))))

(ert-deftest lit-get-lit-spec-range-test ()
  "Test parsing of ranges in different specs."
  (should (equal (lit-get-lit-spec-range "// CHECK :3 :28 S5553")
                 '( :begin-line-offset (:same) :begin-col 3 :end-line-offset (:same) :end-col 28
                    :keyword-begin-col 3 :keyword-end-col 8
                    :range-spec-begin-col 9 :range-spec-end-col 15)))
  (should (equal (lit-get-lit-spec-range "// CHECK -3:3 +8:11 S110")
                 '( :begin-line-offset (:prev 3) :begin-col 3 :end-line-offset (:next 8) :end-col 11
                    :keyword-begin-col 3 :keyword-end-col 8
                    :range-spec-begin-col 9 :range-spec-end-col 19)))
  (should (equal (lit-get-lit-spec-range "// SECONDARY -:1 -:2 my-sec-id:cbveswrtg")
                 '( :begin-line-offset (:smart-prev) :begin-col 1 :end-line-offset (:smart-prev) :end-col 2
                    :keyword-begin-col 3 :keyword-end-col 12
                    :range-spec-begin-col 13 :range-spec-end-col 20)))
  (should (equal (lit-get-lit-spec-range "  // EDIT qfb1 +8:4 +13:11 ``\\\n")
                 '( :begin-line-offset (:next 8) :begin-col 4 :end-line-offset (:next 13) :end-col 11
                    :keyword-begin-col 5 :keyword-end-col 9
                    :range-spec-begin-col 15 :range-spec-end-col 26))))

(ert-deftest lit-move-to-start-of-multiline-test ()
  "Test moving the point to the beginning of '\'-extended chain of lines."
  (with-temp-buffer
    (insert
     " s1\n"
     " s2\\\n"
     " s3  \\\n"
     "s4 \\ \n"
     "s5\n")
    (goto-line 1)
    (lit-move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 1))
    (goto-line 2)
    (lit-move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 2))
    (goto-line 3)
    (lit-move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 2))
    (goto-line 4)
    (lit-move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 2))
    (goto-line 5)
    (lit-move-to-start-of-multiline)
    (should (equal (line-number-at-pos) 5))))


(ert-deftest lit-join-multilines-test ()
  (should (equal (lit-join-multilines '("a" "b" "c")) '("a" "b" "c")))
  (should (equal (lit-join-multilines '("a\\" "b" "c")) '("a\\\nb" "c")))
  (should (equal (lit-join-multilines '("a" "b\\" "c")) '("a" "b\\\nc")))
  (should (equal (lit-join-multilines '("a" "b" "c\\")) '("a" "b" "c\\\n"))))

(ert-deftest lit-chop-string-test ()
  (should (equal (lit-chop-string "" "b") '("")))
  (should (equal (lit-chop-string "acd" "b") '("acd")))
  (should (equal (lit-chop-string "abcbd" "b") '("a" "bc" "bd")))
  (should (equal (lit-chop-string "abbbbc" "b+") '("a" "bbbbc")))
  (should (equal (lit-chop-string "bbbb" "b+") '("bbbb")))
  (should (equal (lit-chop-string "bbbb" "bb?") '("bb" "bb")))
  (should (equal (lit-chop-string "bbbbc" "b+") '("bbbbc")))
  (should (equal (lit-chop-string "abbbb" "b+") '("a" "bbbb")))
  (should (equal (lit-chop-string "bbb" "bbb") '("bbb"))))

(ert-deftest lit-parse-all-observed-test-two-simple ()
  (should
   (equal
    (lit-parse-all-observed
     "  /home/arseniy/proj/sonar-cpp/test/checks/UsingInsteadOfTypedefCheck.cpp
    224:12 224:16 M23_338:Replace this enum with enum class.
  /home/arseniy/proj/sonar-cpp/test/checks/UsingInsteadOfTypedefCheck.cpp
    224:12 224:16 S3642:Replace this enum with enum class.
")
    (list
     (lit-parse-1-observed
      "  /home/arseniy/proj/sonar-cpp/test/checks/UsingInsteadOfTypedefCheck.cpp
    224:12 224:16 M23_338:Replace this enum with enum class.")
     (lit-parse-1-observed
      "/home/arseniy/proj/sonar-cpp/test/checks/UsingInsteadOfTypedefCheck.cpp
    224:12 224:16 S3642:Replace this enum with enum class.")))))

(ert-deftest lit-parse-all-observed-test-with-fixes ()
  (should
   (equal
    (lit-parse-all-observed
     "  /path/to/file.cpp
    224:1 224:28 S5416:\"using\" should be preferred to \"typedef\" for type aliasing.
      // FIX Replace by a type-alias syntax
      // EDIT 224:1 224:28 `using E2 = enum { };\\
using E2Ref = E2 &`
  /path/to/file.cpp
    224:9 224:13 M23_338:Replace this \"enum\" with \"enum class\".
  /path/to/file.cpp
    224:9 224:13 S3642:Replace this \"enum\" with \"enum class\".
")
    (list
     (lit-parse-1-observed
      "  /path/to/file.cpp
    224:1 224:28 S5416:\"using\" should be preferred to \"typedef\" for type aliasing.
      // FIX Replace by a type-alias syntax
      // EDIT 224:1 224:28 `using E2 = enum { };\\
using E2Ref = E2 &`
")
     (lit-parse-1-observed
      "  /path/to/file.cpp
    224:9 224:13 M23_338:Replace this \"enum\" with \"enum class\".
")
     (lit-parse-1-observed
      "  /path/to/file.cpp
    224:9 224:13 S3642:Replace this \"enum\" with \"enum class\".
")))))

(ert-deftest lit-parse-1-observed-test-primary-only ()
  (should
   (equal
    (lit-parse-1-observed
     "  /p/to/f.cpp
    67:3 67:22 S946:Address of stack memory associated with temporary object of type 'B' returned to caller")
    '( :file "/p/to/f.cpp"
       :rule-id "S946"
       :primary ( :begin (:line 67 :col 3) :end (:line 67 :col 22)
                  :message "Address of stack memory associated with temporary object of type 'B' returned to caller")
       :secondaries ()
       :dataflows ()
       :fixes ()))))

(ert-deftest lit-parse-1-observed-test-secondaries ()
  (should
   (equal
    (lit-parse-1-observed
     "  /path/to/file.cpp
    55:10 55:13 S5553:Accessing reclaimed temporary
      /path/to/file.cpp:    51:10 51:15:Temporary is created here
      /path/to/file.cpp:    51:10 51:22:Calling 'B::getA'
      /path/to/file.cpp:    51:10 53:22:Returning from 'B::getA'
      /path/to/file.cpp:    55:10 55:13:Accessing reclaimed temporary
")
    '( :file "/path/to/file.cpp"
       :rule-id "S5553"
       :primary ( :begin (:line 55 :col 10) :end (:line 55 :col 13)
                  :message "Accessing reclaimed temporary")
       :secondaries
       (( :begin (:line 51 :col 10) :end (:line 51 :col 15)
          :message "Temporary is created here")
        ( :begin (:line 51 :col 10) :end (:line 51 :col 22)
          :message "Calling 'B::getA'")
        ( :begin (:line 51 :col 10) :end (:line 53 :col 22)
          :message "Returning from 'B::getA'")
        ( :begin (:line 55 :col 10) :end (:line 55 :col 13)
          :message "Accessing reclaimed temporary"))
       :dataflows ()
       :fixes ()))))

(ert-deftest lit-parse-1-observed-test-dataflows ()
  (should
   (equal
    (lit-parse-1-observed
     "  /p/to/file.cpp
    48:3 48:21 S5782:The primary msg
      /p/to/file.cpp:    64:3 64:5:Taking true branch
      2 data flows:
        // DATAFLOW DESCRIPTION:Description 1
          /p/to/file.cpp:    48:12 48:16:This buffer access overflows
          /p/to/file.cpp:    66:13 66:20:Initializing parameter to this expression
        // DATAFLOW DESCRIPTION:Description 2
          /p/to/file.cpp:    48:17 50:21:This size argument overflows the buffer
")
    '( :file "/p/to/file.cpp"
       :rule-id "S5782"
       :primary ( :begin (:line 48 :col 3) :end (:line 48 :col 21)
                  :message "The primary msg")
       :secondaries
       (( :begin (:line 64 :col 3) :end (:line 64 :col 5)
          :message "Taking true branch"))
       :dataflows
       (( :description "Description 1"
          :steps (( :begin (:line 48 :col 12) :end (:line 48 :col 16)
                    :message "This buffer access overflows")
                  ( :begin (:line 66 :col 13) :end (:line 66 :col 20)
                    :message "Initializing parameter to this expression")))
        ( :description "Description 2"
          :steps (( :begin (:line 48 :col 17) :end (:line 50 :col 21)
                    :message "This size argument overflows the buffer"))))
       :fixes ()))))

(ert-deftest lit-parse-1-observed-test-only-dataflows ()
  (should
   (equal
    (lit-parse-1-observed
     "  /file.cpp
    46:3 46:25 S5782:Primary location
      2 data flows:
        // DATAFLOW DESCRIPTION:Direct dataflow
          /file.cpp:    46:12 46:19:This buffer access overflows
")
    '( :file "/file.cpp"
       :rule-id "S5782"
       :primary ( :begin (:line 46 :col 3) :end (:line 46 :col 25)
                  :message "Primary location")
       :secondaries ()
       :dataflows
       (( :description "Direct dataflow"
          :steps (( :begin (:line 46 :col 12) :end (:line 46 :col 19)
                    :message "This buffer access overflows"))))
       :fixes ()))))

(ert-deftest lit-parse-1-observed-test-with-edits ()
  (should
   (equal
    (lit-parse-1-observed
     "  /file.cpp
    10:36 10:46 S3229:Field y is initialized after field x.
      /file.cpp:    10:36 10:40:Field \"x\" is inited.
      /file.cpp:    10:42 10:46:Field \"y\" is inited.
      // FIX Reorder the member initializer list
      // EDIT 10:36 10:40 `x(a)`
      // EDIT 10:42 10:46 `y(a)`
")
    '( :file "/file.cpp"
       :rule-id "S3229"
       :primary ( :begin (:line 10 :col 36) :end (:line 10 :col 46)
                  :message "Field y is initialized after field x.")
       :secondaries
       (( :begin (:line 10 :col 36) :end (:line 10 :col 40) :message "Field \"x\" is inited.")
        ( :begin (:line 10 :col 42) :end (:line 10 :col 46) :message "Field \"y\" is inited."))
       :dataflows ()
       :fixes
       (( :description "Reorder the member initializer list"
          :edits
          (( :begin (:line 10 :col 36) :end (:line 10 :col 40) :message "x(a)")
           ( :begin (:line 10 :col 42) :end (:line 10 :col 46) :message "y(a)"))))))))

(ert-deftest lit-parse-1-observed-test-multiline-edits ()
  (should
   (equal
    (lit-parse-1-observed
     "  /file.cpp
    183:12 183:17 S1045:Exception of type Base &.
      /file.cpp:    181:12 181:15:Handler for this type.
      // FIX Swap Base & and X handlers
      // EDIT 181:4 183:4 ``
      // EDIT 185:4 185:4 ` catch (std::exception& e) {\\
    nop(e);\\
  }`
      // FIX Remove the dead handler
      // EDIT 183:5 185:5 ``
")
    '( :file "/file.cpp"
       :rule-id "S1045"
       :primary ( :begin (:line 183 :col 12) :end (:line 183 :col 17)
                  :message "Exception of type Base &.")
       :secondaries
       (( :begin (:line 181 :col 12) :end (:line 181 :col 15) :message "Handler for this type."))
       :dataflows ()
       :fixes
       (( :description "Swap Base & and X handlers"
          :edits
          (( :begin (:line 181 :col 4) :end (:line 183 :col 4) :message "")
           ( :begin (:line 185 :col 4) :end (:line 185 :col 4) :message " catch (std::exception& e) {\\
    nop(e);\\
  }")))
        ( :description "Remove the dead handler"
          :edits
          (( :begin (:line 183 :col 5) :end (:line 185 :col 5) :message ""))))))))

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
                (primary-overlay (lit-make-overlay 7 20 nil t)))
        (lit-insert-dataflows (lit-generate-dataflow-overlays
                              '(( :description "Description 1"
                                  :steps (( :begin (:line 2 :col 12) :end (:line 2 :col 16)
                                            :message "This buffer access overflows")))
                                ( :description "Description 2"
                                  :steps (( :begin (:line 4 :col 17) :end (:line 4 :col 21)
                                            :message "This size argument overflows the buffer")))))
                              '("included" nil)
                               primary-overlay)
        (lit-clear-overlay primary-overlay))
      (buffer-substring-no-properties (point-min) (point-max)))
    "
line 2
// DATAFLOW -:12 -:16 included,0:This buffer access overflows
line 3 // CHECK
// DATAFLOW DESCRIPTION included:Description 1
line 4
")))

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
    (should (equal (lit-render-primary '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                         :message "Hello")
                                      "S100")
                   '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                      :message "Hello" :pos-in-list 2)))
    (should (equal (buffer-string)
                   "line1\n1:2 3:4 S100:Hello\n"))))

(ert-deftest lit-render-primary-test ()
  (with-temp-buffer
    (should (equal (lit-render-secondary '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                           :message "Hello"))
                   '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                      :message "Hello" :pos-in-list 1)))
    (should (equal (lit-render-secondary '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                                           :message "Hello"))
                   '( :begin (:line 1 :col 2) :end (:line 3 :col 4)
                      :message "Hello" :pos-in-list 2)))
    (should (equal (buffer-string)
                   "    1:2 3:4:Hello\n    1:2 3:4:Hello\n"))))

(ert-deftest lit-render-dataflow-test ()
  (with-temp-buffer
    (should (equal (lit-render-dataflow '( :description "Df1"
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
    (should (equal (lit-render-fix '( :description "quick fix"
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
     (equal (lit-render-issue-spec
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
                                           (lit-modify-line-with-its-continuation-in-buffer
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
               (goto-line line)
               (move-to-column 3)
               (lit-delete-spec-on-the-line)
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
    (should (equal (lit-column-number-at-pos 3) 2))
    (should (equal (lit-column-number-at-pos 9) 2))
    (should (equal (lit-column-number-at-pos 15) 2))
    (should (equal (lit-column-number-at-pos 23) 10))
    (should (equal (lit-column-number-at-pos 24) 11))
    (should (equal (lit-column-number-at-pos 25) 0))
    (should (equal (lit-column-number-at-pos 26) 0))
    (should (equal (lit-column-number-at-pos 27) 1))))

(ert-deftest lit-render-dumb-spec-test ()
  (with-temp-buffer
    (insert "line1
line2
line3")
    (should (equal (lit-render-dumb-spec 2 1 2) "-1:1 -1:2"))
    (should (equal (lit-render-dumb-spec 2 2 5) "-1:2 -1:5"))
    (should (equal (lit-render-dumb-spec 1 7 9) "+1:1 +1:3"))
    (should (equal (lit-render-dumb-spec 1 7 15) "+1:1 +2:3"))
    (should (equal (lit-render-dumb-spec 1 14 15) "+2:2 +2:3"))))

(ert-deftest lit-rewrite-spec-for-pair ()
  (with-temp-buffer
    (insert "// bla-bla 'XX:XX XX:XX'
line2")
    (let ((spec-overlay (make-overlay 13 24))
          (target-overlay (make-overlay 28 30)))
      (lit-rewrite-spec-for-pair (cons spec-overlay target-overlay))
      (should (equal (buffer-string)
                     "// bla-bla '+1:3 +1:5'
line2")))))

(ert-deftest lit-adjust-dumb-ranges-test ()
  (with-temp-buffer
    (insert "line1
// CHECK +1:1 +1:2 S100:Message
line2")
    (lit-goto-line 2)
    (end-of-line)
    (let ((overlay-pairs (lit-make-all-dumb-range-spec-overlays))
          (inserted-begin (point)))
      (insert "
line2.5")
      (lit-adjust-range-specs-after-insertion overlay-pairs inserted-begin (point)))
    (should (equal (buffer-string)
                   "line1
// CHECK +2:1 +2:2 S100:Message
line2.5
line2"))))

;;; lit-test.el ends here
