(require 'ert)

(ert-deftest w-db--sanitize-entry-test ()
  ";TODO:"
  (let ((entry '((reading_time . 12)
                 (created_at . "2023-08-25T05:01:37+0000")
                 (updated_at . "2023-08-25T05:03:02+0000")
                 (published_at . "2023-08-23T14:27:54+0000")
                 (published_by . ["SK Ventures"])
                 (starred_at)
                 (tags . [((id . 2)
                           (label . "tag1")
                           (slug . "tag1"))
                          ((id . 3)
                           (label . "tag2")
                           (slug . "tag2"))])
                 (annotations . [])
                 (mimetype . "text/html; charset=utf-8")
                 (language . "en")
                 (is_public . :json-false)))
        (entry-sanitized '((tag . "tag1,tag2")
                           (reading_time . 12)
                           (created_at . "2023-08-25T05:01:37")
                           (updated_at . "2023-08-25T05:03:02")
                           (published_at . "2023-08-23T14:27:54")
                           (published_by . ["SK Ventures"])
                           (starred_at)
                           (tags . [((id . 2)
                                     (label . "tag1")
                                     (slug . "tag1"))
                                    ((id . 3)
                                     (label . "tag2")
                                     (slug . "tag2"))])
                           (annotations . [])
                           (mimetype . "text/html; charset=utf-8")
                           (language . "en")
                           (is_public . 0))))
    (should (equal entry-sanitized
                   (w-db--sanitize-entry entry)))))




;; Local Variables:
;; read-symbol-shorthands: (("w-" . "wombag-"))
;; End:
