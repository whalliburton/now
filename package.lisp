
(defpackage now
  (:use common-lisp deck-client iterate)
  (:import-from sb-thread with-mutex)
  (:import-from cl-who with-html-output-to-string with-html-output htm str esc fmt)
  (:import-from alexandria with-input-from-file when-let if-let)
  (:import-from hunchentoot session-value session-db-lock session-db *session*
                *catch-errors-p* url-encode url-decode)
  (:import-from local-time now decode-timestamp encode-timestamp format-timestring)
  (:import-from split-sequence split-sequence)
  (:import-from helpers breakout symb princ-with-ellipses-to-string random-string
                parse-float breakout bugout)
  (:import-from drakma http-request)
  (:import-from helpers print-table princ-with-ellipses-to-string run-program-to-string)
  (:import-from deck-client field-value template-is-type-of template-id)
  (:import-from anaphora aand it)
  (:import-from fare-csv read-csv-line))

(defpackage now-js
  (:use common-lisp parenscript)
  (:export js-file))

(deck-client:connect-to-deck)