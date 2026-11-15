;;; sentry-test.scm --- packages/sentry.scm, the read-only client, offline.
;;;
;;; Every test replaces the transport seam. No test reads Doppler or
;;; reaches Sentry.
;;;
;;; The list and detail tests below run the commands the keys are bound to.

(domain! 'testing)
(effects! '(write))

;; The seams are globals. Every test puts back what it found: the next
;; test reads the same ones, and the live editor reaches the network
;; through them.
(define (t--sentry-with-transport fetch thunk)
  (with-stub '*sentry-transport* fetch thunk))

(deftest 'an-issue-search-encodes-its-filters-and-caps-its-limit
  "the query, the environment and the period ride the URL; the limit is ours"
  (lambda ()
    (let ((url ""))
      (t--sentry-with-transport
        (lambda (u) (set! url u) "[]\n200")
        (lambda () (sentry-list-issues "is:unresolved assigned:me" "prod" "7d" 500)))
      (check-contains! url "/api/0/projects/svs-recruiting/ats-ash/issues/" "the path")
      (check-contains! url "query=is%3Aunresolved%20assigned%3Ame" "the encoded query")
      (check-contains! url "environment=prod" "the environment")
      (check-contains! url "statsPeriod=7d" "the period")
      (check-contains! url "per_page=50" "the capped limit"))))

(deftest 'http-failures-have-one-safe-result-shape
  "the status is the message, and the body never reaches the reader"
  (lambda ()
    (let ((reply (t--sentry-with-transport
                   (lambda (u) "private response body\n403")
                   (lambda () (sentry-list-issues)))))
      (check-true! (plist-get reply 'errors) "the reply carries errors")
      (check-contains! (value->string reply) "HTTP 403" "the status")
      (check-false! (string-contains? (value->string reply) "private response body")
                    "and not the body"))))

(deftest 'the-client-enforces-its-row-limit-when-sentry-returns-more
  "a server that ignores per_page does not get to fill the buffer"
  (lambda ()
    (let* ((rows (t--sentry-with-transport
                   (lambda (u) "[{\"id\":\"1\"},{\"id\":\"2\"},{\"id\":\"3\"}]\n200")
                   (lambda () (sentry-list-issues "" "prod" "24h" 2))))
           (ids (map (lambda (r) (plist-get r 'id)) rows)))
      (check-equal! ids '("1" "2") "two rows, in order"))))

(deftest 'the-issue-detail-puts-the-exception-first-and-keeps-the-raw-json
  "the reader sees the error, and the whole record is still there to read"
  (lambda ()
    (let ((text (sentry--issue-text
                  (list 'shortId "ATS-1"
                        'title ""
                        'status "unresolved"
                        'metadata
                        (list 'type "RuntimeError"
                              'value "credits exhausted"
                              'secret "visible-in-raw")))))
      (check-contains! text "ATS-1  RuntimeError" "the heading")
      (check-contains! text "Exception\ncredits exhausted" "the exception first")
      (check-contains! text "Raw issue JSON" "the raw section")
      (check-contains! text "visible-in-raw" "which holds everything")
      (check-false! (string-contains? text "<!doctype html>") "and no HTML"))))

(deftest 'the-sentry-api-declares-its-domain-and-effects
  "a read that reaches the network says both"
  (lambda ()
    (let ((entry (catalog-entry 'function "sentry-list-issues")))
      (check-equal! (plist-get entry 'domain) "sentry" "the domain")
      (check-equal! (plist-get entry 'effects) '("read" "external") "the effects"))
    (check-equal! (plist-get (catalog-entry 'command "sentry") 'effects)
                  '("write" "external") "the command writes a buffer too")))

;;; --- the issue list and its details, by the commands --------------------------
;;;
;;; These were test/sentry_test.exs, sentry_list_cache_test.exs and
;;; sentry_list_verbs_test.exs. They ran the keys; here each step runs the
;;; command its key is bound to. y-or-n is stubbed: t--sentry-yes! answers.

(define t--sentry-seams
  '(*sentry-transport* *sentry-write-transport* *sentry-async-transport*
    *sentry-open-url* y-or-n assign-chat! group-chat-buffer-show!))

(define t--sentry-buffers
  '("*Sentry issues*" "*Sentry issue: 42*" "*Sentry issue: 43*" "*Sentry issue: 99*"
    "*chat:sentry*"))

(define t--sentry-fetches 0)
(define t--sentry-asked #f)
(define t--sentry-yes #f)
(define t--sentry-writes '())
(define t--sentry-opened '())
(define t--sentry-prompt "")
(define t--sentry-chat-shown #f)

(define (t--sentry-issue id short title)
  (string-append "{\"id\":\"" id "\",\"shortId\":\"" short "\",\"title\":\"" title
                 "\",\"status\":\"unresolved\",\"permalink\":\"https://sentry.io/i/" id "\"}\n200"))

(define (t--sentry-row id short title count seen)
  (string-append "{\"id\":\"" id "\",\"shortId\":\"" short "\",\"title\":\"" title
                 "\",\"level\":\"error\",\"count\":\"" count "\",\"lastSeen\":\"" seen
                 "\",\"permalink\":\"https://sentry.io/i/" id "\"}"))

;; two issues, 42 boom and 43 crash, each with a detail and a permalink
(define (t--sentry-two-issues url)
  (set! t--sentry-fetches (+ t--sentry-fetches 1))
  (cond
    ((string-contains? url "/issues/42/") (t--sentry-issue "42" "ATS-42" "boom"))
    ((string-contains? url "/issues/43/") (t--sentry-issue "43" "ATS-43" "crash"))
    (else (string-append "[" (t--sentry-row "42" "ATS-42" "boom" "3" "2026-08-20T10:00:00Z")
                         "," (t--sentry-row "43" "ATS-43" "crash" "1" "2026-08-20T11:00:00Z")
                         "]\n200"))))

;; THUNK runs in a frame of its own with every seam replaced; nothing leaves
(define (t--sentry! thunk)
  (let ((saved (map (lambda (s) (cons s (symbol-value s))) t--sentry-seams)))
    (unwind-protect
      (lambda ()
        (set! t--sentry-fetches 0)
        (set! t--sentry-asked #f)
        (set! t--sentry-yes #f)
        (set! t--sentry-writes '())
        (set! t--sentry-opened '())
        (set! t--sentry-prompt "")
        (set! t--sentry-chat-shown #f)
        (set! *sentry-transport* t--sentry-two-issues)
        ;; the list fetches through the async seam; a synchronous delegate
        ;; keeps the per-URL transport authoritative
        (set! *sentry-async-transport* (lambda (url k) (k (*sentry-transport* url))))
        (set! *sentry-write-transport*
          (lambda (url body)
            (set! t--sentry-writes (cons (list url body) t--sentry-writes))
            "{\"status\":\"resolved\"}\n200"))
        ;; each issue starts its own chat; the stub keeps the prompts, starts none
        (set! assign-chat!
          (lambda (buf group prompt)
            (set! t--sentry-prompt (string-append t--sentry-prompt prompt "\n"))
            (list (string-append "*chat:" buf "*"))))
        (set! group-chat-buffer-show! (lambda (chat) (set! t--sentry-chat-shown chat)))
        (set! *sentry-open-url* (lambda (url) (set! t--sentry-opened (cons url t--sentry-opened))))
        (set! y-or-n (lambda (prompt yes . no) (set! t--sentry-asked prompt) (set! t--sentry-yes yes)))
        (with-frame (lambda (frame) (thunk))))
      (lambda ()
        (for-each (lambda (p) (set-symbol-value! (car p) (cdr p))) saved)
        (for-each (lambda (b) (when (buffer-exists? b) (buffer-kill! b))) t--sentry-buffers)))))

(define (t--sentry-yes!) (when t--sentry-yes (t--sentry-yes)))

(define (t--sentry-open-list!)
  (run-command "sentry")
  (switch-to-buffer! "*Sentry issues*")
  (list-goto-first-entry "*Sentry issues*"))

(define (t--sentry-in buf cmd)
  (switch-to-buffer! buf)
  (run-command cmd))

(define (t--sentry-shown) (value->string (map cadr (window-list))))

(deftest 'sentry-open-shows-grouped-details-and-serves-the-cache
  "sentry-open makes one detail per row in the sentry group; g refetches"
  (lambda ()
    (t--sentry!
      (lambda ()
        (set! *sentry-transport*
          (lambda (url)
            (if (string-contains? url "/issues/42/")
                "{\"id\":\"42\",\"shortId\":\"ATS-42\",\"title\":\"failure for jane@example.com\",\"status\":\"unresolved\",\"metadata\":{\"secret\":\"hidden\"}}\n200"
                (t--sentry-two-issues url))))
        (t--sentry-open-list!)
        (check-equal! (group-name (buffer-group "*Sentry issues*")) "sentry" "the list is in the group")
        (check-equal! (group-name (buffer-group (group-chat "sentry"))) "sentry" "and so is its chat")
        (t--sentry-in "*Sentry issues*" "sentry-open")
        (let ((text (buffer-text "*Sentry issue: 42*")))
          (check-contains! text "ATS-42" "the detail")
          (check-contains! text "[redacted-email]" "the email is redacted")
          (check-contains! text "Raw issue JSON" "the raw section")
          (check-contains! text "hidden" "which holds everything")
          (check-false! (string-contains? text "<!doctype html>") "no HTML"))
        (check-equal! (buffer-local-value 'render-mode "*Sentry issue: 42*") "blocks" "drawn as blocks")
        (check-equal! (group-name (buffer-group "*Sentry issue: 42*")) "sentry" "in the group")
        (t--sentry-in "*Sentry issues*" "list-next")
        (run-command "sentry-open")
        (check-contains! (buffer-text "*Sentry issue: 43*") "ATS-43" "a second detail")
        (check-equal! (group-name (buffer-group "*Sentry issue: 43*")) "sentry" "in the group")
        (set! *sentry-transport*
          (lambda (url) "{\"id\":\"42\",\"shortId\":\"ATS-42\",\"title\":\"downloaded twice\"}\n200"))
        (t--sentry-in "*Sentry issues*" "list-prev")
        (run-command "sentry-open")
        (check-false! (string-contains? (buffer-text "*Sentry issue: 42*") "downloaded twice")
                      "the same row inside the TTL serves the cache")
        (t--sentry-in "*Sentry issue: 42*" "sentry-detail-refresh")
        (check-contains! (buffer-text "*Sentry issue: 42*") "downloaded twice" "refresh fetches again")))))

(define (t--sentry-detail-99!)
  (set! *sentry-transport*
    (lambda (url)
      (if (string-contains? url "/issues/99/")
          "{\"id\":\"99\",\"shortId\":\"ATS-99\",\"title\":\"worker failed\",\"status\":\"unresolved\",\"priority\":\"high\",\"permalink\":\"https://example.sentry.io/issues/99/\",\"metadata\":{\"type\":\"RuntimeError\",\"value\":\"boom\"}}\n200"
          "[]\n200")))
  (buffer-create "*Sentry issue: 99*")
  (buffer-set-local! "*Sentry issue: 99*" 'sentry-issue-id "99")
  (switch-to-buffer! "*Sentry issue: 99*")
  (set-mode! "sentry-detail-mode"))

(deftest 'sentry-detail-sends-the-issue-to-its-group-agent
  "the detail's ask-agent starts the issue's chat with its id and buffer, and shows it"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-detail-99!)
        (run-command "sentry-detail-refresh")
        (let ((actions (value->string sentry--detail-actions)))
          (check-contains! actions "Ask agent" "ask")
          (check-contains! actions "Open in Sentry" "open")
          (check-contains! actions "Resolve" "resolve"))
        (check-equal! (buffer-local-value 'render-mode "*Sentry issue: 99*") "blocks" "drawn as blocks")
        (run-command "sentry-ask-agent")
        (check-contains! t--sentry-prompt "ATS-99" "the issue")
        (check-contains! t--sentry-prompt "*Sentry issue: 99*" "its buffer")
        (check-equal! t--sentry-chat-shown "*chat:*Sentry issue: 99**" "the issue's chat shows")))))

(deftest 'sentry-resolve-asks-first
  "sentry-resolve sends nothing until the answer is yes"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-detail-99!)
        (run-command "sentry-resolve")
        (check-equal! t--sentry-writes '() "nothing yet")
        (check-contains! t--sentry-asked "in Sentry?" "it asks")
        (t--sentry-yes!)
        (check-contains! (car (car t--sentry-writes)) "/api/0/organizations/svs-recruiting/issues/99/"
                         "the update goes to the issue")
        (check-contains! (cadr (car t--sentry-writes)) "resolved" "and resolves it")))))

(deftest 'sentry-one-open-is-one-fetch
  "marks and width changes reuse the rows; refresh fetches; a second open serves the cache"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-open-list!)
        (check-equal! t--sentry-fetches 1 "one fetch")
        (check-contains! (buffer-text "*Sentry issues*") "ATS-42" "the row")
        (run-command "list-mark")
        (check-equal! t--sentry-fetches 1 "a mark redraws the rows")
        (buffer-set-local! "*Sentry issues*" 'list-width 999)
        (list-post-command! "*Sentry issues*")
        (check-equal! t--sentry-fetches 1 "a width change redraws the rows")
        (check-contains! (buffer-text "*Sentry issues*") "ATS-42" "the row stays")
        (t--sentry-in "*Sentry issues*" "sentry-refresh")
        (check-equal! t--sentry-fetches 2 "refresh fetches")
        ;; the mark moved point to the next row
        (list-goto-first-entry "*Sentry issues*")
        (t--sentry-in "*Sentry issues*" "sentry-open")
        (check-equal! t--sentry-fetches 3 "the detail fetches once")
        (check-contains! (buffer-text "*Sentry issue: 42*") "ATS-42" "the detail")
        (t--sentry-in "*Sentry issues*" "sentry-open")
        (check-equal! t--sentry-fetches 3 "and then serves the cache")))))

(deftest 'sentry-first-detail-draw-comes-from-the-row
  "before the fetch answers, the detail shows what the row knew"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-open-list!)
        (set! *sentry-async-transport* (lambda (url k) #f))
        (t--sentry-in "*Sentry issues*" "sentry-open")
        (check-contains! (buffer-text "*Sentry issue: 42*") "ATS-42" "the id")
        (check-contains! (buffer-text "*Sentry issue: 42*") "boom" "the title")))))

(deftest 'sentry-window-change-relays-only-shown-lists
  "a hidden list keeps its width when the windows change"
  (lambda ()
    (t--sentry!
      (lambda ()
        (run-command "sentry")
        (for-each (lambda (w) (window-set-buffer! w "*scratch*")) (map car (window-list)))
        (buffer-set-local! "*Sentry issues*" 'list-width 777)
        (window-config-changed!)
        (check-equal! (buffer-local-value 'list-width "*Sentry issues*") 777 "nothing visited it")
        (check-equal! t--sentry-fetches 1 "and nothing fetched")))))

(define (t--sentry-mark-both!)
  (t--sentry-open-list!)
  (run-command "list-mark")
  (run-command "list-mark"))

(deftest 'sentry-list-resolve-asks-once-for-the-marked-rows
  "one question resolves every marked row"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-mark-both!)
        (t--sentry-in "*Sentry issues*" "sentry-list-resolve")
        (check-contains! t--sentry-asked "Resolve ATS-42, ATS-43 in Sentry?" "one question for both")
        (t--sentry-yes!)
        (let ((urls (value->string (map car t--sentry-writes))))
          (check-contains! urls "/issues/42/" "the first")
          (check-contains! urls "/issues/43/" "the second"))))))

(deftest 'sentry-list-open-web-opens-the-marked-rows
  "every marked row's permalink opens"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-mark-both!)
        (t--sentry-in "*Sentry issues*" "sentry-list-open-web")
        (check-true! (member "https://sentry.io/i/42" t--sentry-opened) "the first")
        (check-true! (member "https://sentry.io/i/43" t--sentry-opened) "the second")))))

(deftest 'sentry-list-ask-agent-names-the-marked-issues
  "one prompt names every marked issue and its detail buffer"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-mark-both!)
        (t--sentry-in "*Sentry issues*" "sentry-list-ask-agent")
        (check-contains! t--sentry-prompt "ATS-42" "the first issue")
        (check-contains! t--sentry-prompt "ATS-43" "the second issue")
        (check-contains! t--sentry-prompt "*Sentry issue: 42*" "the first buffer")
        (check-contains! t--sentry-prompt "*Sentry issue: 43*" "the second buffer")
        (check-contains! (buffer-text "*Sentry issue: 42*") "ATS-42" "the first detail is fetched")
        (check-contains! (buffer-text "*Sentry issue: 43*") "ATS-43" "the second detail is fetched")))))

(deftest 'sentry-resolving-the-shown-row-advances-its-detail
  "the detail window moves to the next row and the resolved detail goes"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-open-list!)
        (t--sentry-in "*Sentry issues*" "sentry-open")
        (check-contains! (t--sentry-shown) "*Sentry issue: 42*" "the detail shows")
        (t--sentry-in "*Sentry issues*" "sentry-list-resolve")
        (t--sentry-yes!)
        (check-contains! (t--sentry-shown) "*Sentry issue: 43*" "the next row's detail")
        (check-false! (buffer-exists? "*Sentry issue: 42*") "the resolved detail goes")))))

(deftest 'sentry-verbs-act-on-the-row-at-point
  "with nothing marked, the row at point is the target"
  (lambda ()
    (t--sentry!
      (lambda ()
        (t--sentry-open-list!)
        (t--sentry-in "*Sentry issues*" "sentry-list-open-web")
        (check-equal! t--sentry-opened '("https://sentry.io/i/42") "only the row at point")))))
