;;; lit-parse-test.el --- Test the lit-arsing routines -*- lexical-binding: t; -*-
;; Author: Arseniy Zaostrovnykh <arseniy.zaostrovnykh@sonarsource.com>

;; Created: 17.06.23

;;; Commentary:
;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el" (:report-format 'lcov) (:send-report nil)))

(add-to-list 'load-path "..")
(require 'lit-parse)

(ert-deftest lit-parse--line-offset-parse-test ()
  "Tests the parsing of the (smart) offset in the range spec."
  (should (equal (lit-parse--line-offset-parse nil) '(:same)))
  (should (equal (lit-parse--line-offset-parse "   -") '(:smart-prev)))
  (should (equal (lit-parse--line-offset-parse "+") '(:smart-next)))
  (should (equal (lit-parse--line-offset-parse "  +18") '(:next 18)))
  (should (equal (lit-parse--line-offset-parse "-93") '(:prev 93))))

(ert-deftest lit-parse-get-lit-spec-range-test ()
  "Test parsing of ranges in different specs."
  (should (equal (lit-parse-get-lit-spec-range "// CHECK :3 :28 S5553")
                 '( :begin-line-offset (:same) :begin-col 3 :end-line-offset (:same) :end-col 28
                    :keyword-begin-col 3 :keyword-end-col 8
                    :range-spec-begin-col 9 :range-spec-end-col 15)))
  (should (equal (lit-parse-get-lit-spec-range "// CHECK -3:3 +8:11 S110")
                 '( :begin-line-offset (:prev 3) :begin-col 3 :end-line-offset (:next 8) :end-col 11
                    :keyword-begin-col 3 :keyword-end-col 8
                    :range-spec-begin-col 9 :range-spec-end-col 19)))
  (should (equal (lit-parse-get-lit-spec-range "// SECONDARY -:1 -:2 my-sec-id:cbveswrtg")
                 '( :begin-line-offset (:smart-prev) :begin-col 1 :end-line-offset (:smart-prev) :end-col 2
                    :keyword-begin-col 3 :keyword-end-col 12
                    :range-spec-begin-col 13 :range-spec-end-col 20)))
  (should (equal (lit-parse-get-lit-spec-range "  // EDIT qfb1 +8:4 +13:11 ``\\\n")
                 '( :begin-line-offset (:next 8) :begin-col 4 :end-line-offset (:next 13) :end-col 11
                    :keyword-begin-col 5 :keyword-end-col 9
                    :range-spec-begin-col 15 :range-spec-end-col 26))))

(ert-deftest lit-parse--join-multilines-test ()
  (should (equal (lit-parse--join-multilines '("a" "b" "c")) '("a" "b" "c")))
  (should (equal (lit-parse--join-multilines '("a\\" "b" "c")) '("a\\\nb" "c")))
  (should (equal (lit-parse--join-multilines '("a" "b\\" "c")) '("a" "b\\\nc")))
  (should (equal (lit-parse--join-multilines '("a" "b" "c\\")) '("a" "b" "c\\\n"))))

(ert-deftest lit-parse--chop-string-test ()
  (should (equal (lit-parse--chop-string "" "b") '("")))
  (should (equal (lit-parse--chop-string "acd" "b") '("acd")))
  (should (equal (lit-parse--chop-string "abcbd" "b") '("a" "bc" "bd")))
  (should (equal (lit-parse--chop-string "abbbbc" "b+") '("a" "bbbbc")))
  (should (equal (lit-parse--chop-string "bbbb" "b+") '("bbbb")))
  (should (equal (lit-parse--chop-string "bbbb" "bb?") '("bb" "bb")))
  (should (equal (lit-parse--chop-string "bbbbc" "b+") '("bbbbc")))
  (should (equal (lit-parse--chop-string "abbbb" "b+") '("a" "bbbb")))
  (should (equal (lit-parse--chop-string "bbb" "bbb") '("bbb"))))

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
     (lit-parse--1-observed
      "  /home/arseniy/proj/sonar-cpp/test/checks/UsingInsteadOfTypedefCheck.cpp
    224:12 224:16 M23_338:Replace this enum with enum class.")
     (lit-parse--1-observed
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
     (lit-parse--1-observed
      "  /path/to/file.cpp
    224:1 224:28 S5416:\"using\" should be preferred to \"typedef\" for type aliasing.
      // FIX Replace by a type-alias syntax
      // EDIT 224:1 224:28 `using E2 = enum { };\\
using E2Ref = E2 &`
")
     (lit-parse--1-observed
      "  /path/to/file.cpp
    224:9 224:13 M23_338:Replace this \"enum\" with \"enum class\".
")
     (lit-parse--1-observed
      "  /path/to/file.cpp
    224:9 224:13 S3642:Replace this \"enum\" with \"enum class\".
")))))

(ert-deftest lit-parse--1-observed-test-primary-only ()
  (should
   (equal
    (lit-parse--1-observed
     "  /p/to/f.cpp
    67:3 67:22 S946:Address of stack memory associated with temporary object of type 'B' returned to caller")
    '( :file "/p/to/f.cpp"
       :rule-id "S946"
       :primary ( :begin (:line 67 :col 3) :end (:line 67 :col 22)
                  :message "Address of stack memory associated with temporary object of type 'B' returned to caller")
       :secondaries ()
       :dataflows ()
       :fixes ()))))

(ert-deftest lit-parse--1-observed-test-secondaries ()
  (should
   (equal
    (lit-parse--1-observed
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

(ert-deftest lit-parse--1-observed-test-dataflows ()
  (should
   (equal
    (lit-parse--1-observed
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

(ert-deftest lit-parse--1-observed-test-only-dataflows ()
  (should
   (equal
    (lit-parse--1-observed
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

(ert-deftest lit-parse--1-observed-test-with-edits ()
  (should
   (equal
    (lit-parse--1-observed
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

(ert-deftest lit-parse--1-observed-test-multiline-edits ()
  (should
   (equal
    (lit-parse--1-observed
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

(ert-deftest lit-parse-all-observed-skips-extra-newline ()
  (should
   (equal
    (lit-parse-all-observed
     "
  /p/to/f.cpp
    67:3 67:22 S946:Address of stack memory associated with temporary object of type 'B' returned to caller")
    '(( :file "/p/to/f.cpp"
        :rule-id "S946"
        :primary ( :begin (:line 67 :col 3) :end (:line 67 :col 22)
                          :message "Address of stack memory associated with temporary object of type 'B' returned to caller")
        :secondaries ()
        :dataflows ()
        :fixes ())))))
(provide 'lit-parse-test)

;;; lit-parse-test.el ends here
