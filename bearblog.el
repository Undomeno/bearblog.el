;;; bearblog.el Use bearblog within emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Undomeno
;;
;; SPDX-License-Identifier: MIT
;;
;; Author: Undomeno <yucaleedil@proton.me>
;; Maintainer: Undomeno <yucaleedil@proton.me>
;; Created: November 16, 2026
;; Modified: November 16, 2026
;; Version: 0.0.1
;; Keywords: blog
;; Homepage: https://github.com/Undomeno/bearblog.el
;; Package-Requires: ((emacs "27.2") (plz "0.9"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defgroup bearblog nil
  "Post to Bear blog from Emacs."
  :group 'applications
  :prefix "bearblog-")

(defcustom bearblog-subdomain nil
  "Your Bearblog subdomain."
  :type 'string
  :group 'bearblog)
(defcustom bearblog-api-key nil
  "Your Bearblog API key."
  :type '(choice (const nil) string)
  :group 'bearblog)


(require 'plz)
(require 'url)
(require 'auth-source)
(require 'org)
(require 'ox-md)

(defun bearblog--api-key ()
  (or bearblog-api-key
      (auth-source-pick-first-password :host "bearblog.dev" :user bearblog-subdomain)
      (user-error "No bearblog-api-key set or found in auth source")))

(defun bearblog--parse-json ()
  "Parse the JSON body in the current buffer, or return nil if it is empty."
  (unless (string-blank-p (buffer-substring (point) (point-max)))
    (json-parse-buffer :object-type 'alist
                       :null-object nil
                       :false-object :false)))

(defun bearblog--request (method path &optional data)
  "Send an HTTP METHOD request to the Bear API at PATH.
METHOD is a symbol such as `get' or `patch'.  DATA, if non-nil, is an
alist sent as the JSON body.  Return the parsed JSON response."

  (unless bearblog-subdomain (user-error "Set `bearblog-subdomain' first"))
  (condition-case err
      (plz method
        (format "https://bearblog.dev/%s/api/v1/%s" bearblog-subdomain path)
        :headers `(("Authorization" . ,(concat "Bearer " (bearblog--api-key)))
                   ("Content-Type" . "application/json"))
        :body (and data (encode-coding-string (json-serialize data) 'utf-8))
        :body-type 'binary
        :as #'bearblog--parse-json
        :timeout 30)
    (plz-error
     (let* ((info (nth 2 err))
            (response (plz-error-response info)))
       (if response
           (error "bearblog %s: %s"
                  (plz-response-status response)
                  (or (alist-get 'error
                                 (ignore-errors
                                   (json-parse-string (plz-response-body response)
                                                      :object-type 'alist)))
                      "request failed"))
         (error "bearblog: %s" (or (cdr (plz-error-curl-error info))
                                   (plz-error-message info))))))))

(defun bearblog--create-post (fields)
  (bearblog--request 'post "post/" fields))

(defun bearblog--update-post (uid fields)
  (bearblog--request 'patch (format "post/%s/" uid) fields))

(defun bearblog--delete-post (uid)
  (bearblog--request 'delete (format "post/%s/" uid)))


;; --------------------- org mode post ---------------------
(defconst bearblog--org-fields
  '(("BEAR_SLUG"             slug             text)
    ("BEAR_PUBLISH"          publish          bool)
    ("BEAR_PUBLISH_DATE"             published_date   date)
    ("BEAR_TAGS"             tags             tags)
    ("BEAR_IS_PAGE"          is_page          bool)
    ("BEAR_DISCOVERABLE"     discoverable     bool)
    ("BEAR_ALIAS"            alias            text)
    ("BEAR_CANONICAL_URL"    canonical_url    text)
    ("BEAR_META_DESCRIPTION" meta_description text)
    ("BEAR_META_IMAGE"       meta_image       text)
    ("BEAR_CLASS_NAME"       class_name       text)
    ("BEAR_LANG"             lang             text))
  "Bear blog fields with corresponding Org fields. Title is #+title")

(defun bearblog--org-convert (type value)
  "Convert the keyword string VALUE to the JSON value for TYPE."
  (pcase type
    ('text value)
    ('bool (pcase value
             ("t" t)
             (_ :false)))
    ('tags (vconcat (split-string value "," t "[ \t]+")))
    ('date (format-time-string "%FT%T%:z" (org-time-string-to-time value)))))

(defun bearblog--org-keyword (name)
  "Return the value of #+NAME in the current buffer, or nil if missing or empty."
  (let ((value (cadr (assoc name (org-collect-keywords (list name))))))
    (and value (not (string-empty-p value)) value)))

(defun bearblog--org-body ()
  "Export the current Org buffer's body to Markdown."
  (org-export-as 'md nil nil t '(:with-toc nil :md-toplevel-hlevel 2)))

(defun bearblog--org-post-fields()
  "Build the bear blog post fields"
  (let ((title (or (bearblog--org-keyword "TITLE")
                   (user-error "This file has no #+TITLE")))
        (fields nil))
    (dolist (entry bearblog--org-fields)
      (let ((value (bearblog--org-keyword (nth 0 entry))))
        (when value
          (push (cons (nth 1 entry) (bearblog--org-convert (nth 2 entry) value))
                fields))))
    (append `((title . ,title)
              (content . ,(bearblog--org-body)))
            fields)))

(defun bearblog--org-insert-uid (uid)
  "Add #+BEAR_UID below the #+TITLE line after posting."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:.*$" nil t)
        (insert "\n")
      (goto-char (point-min))
      (insert "\n")
      (forward-line -1))
    (insert "#+BEAR_UID: " uid))
  (when buffer-file-name
    (save-buffer)))

;;;###autoload
(defun bearblog-post-org ()
  "Create or update a Bear post from the current Org buffer.
If #+BEAR_UID is there, update.
Otherwise, create a new post and write its uid."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org buffer"))
  (let* ((uid (bearblog--org-keyword "BEAR_UID"))
         (fields (bearblog--org-post-fields))
         (post (if uid
                   (bearblog--update-post uid fields)
                 (bearblog--create-post fields))))
    (unless uid
      (bearblog--org-insert-uid (alist-get 'uid post)))
    (message "%s: %s" (if uid "Updated" "Created") (alist-get 'url post))))


(defun bearblog--org-remove-uid ()
  "Remove the #+BEAR_UID line from the current buffer, then save the file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+bear_uid:.*\n?" nil t)
      (replace-match "")))
  (when buffer-file-name
    (save-buffer)))

;;;###autoload
(defun bearblog-delete-org-post ()
  "Delete the Bear post linked to the current Org buffer.
After deleting, remove #+BEAR_UID."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org buffer"))
  (let ((uid (or (bearblog--org-keyword "BEAR_UID")
                 (user-error "No #+BEAR_UID: this file isn't linked to a post"))))
    (when (yes-or-no-p "Permanently remove this file from Bear? ")
      (bearblog--delete-post uid)
      (bearblog--org-remove-uid)
      (message "Deleted this post from Bear"))))

(provide 'bearblog)
;;; bearblog.el ends here
