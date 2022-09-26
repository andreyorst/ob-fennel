(require 'ob-fennel)

(ert-deftest oneline-test ()
  (let ((ob-fennel-collapse-code 'oneline))
    (should (string= "(fn foo [x] (let [a 1 c 3] \"\\n;kuku\\n\" \"; pes\" (+ a c x)))
"
                     (org-babel-expand-body:fennel "(fn foo [x]
  (let [a 1 ; vaiv
        ;; b 2
        c 3]
    \"
;kuku
\"
    \"; pes\"
    (+ a c x)))    ; daun" '())))))
