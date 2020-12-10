;;; helm-jira.el --- Helm bindings for JIRA/Bitbucket/stash -*- lexical-binding: t -*-

;; Author: Roman Decker <roman dot decker at gmail dot com>
;; URL: https://github.com/DeX3/helm-jira
;; Package-Version: 20180802.815
;; Package-Commit: 75d6ed5bd7a041fa8c1adb21cbbbe57b5a7c7cc7
;; Created: July 19, 2018
;; Keywords: tools, helm, jira, bitbucket, stash
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (helm "1.9.9"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; For more information see the README in the online repository.

;;; Code:
(require 'cl)
(require 'request)

(defgroup helm-jira nil
  "helm-jira customization group."
  :group 'applications)

(defcustom helm-jira-url nil
  "JIRA url to use, should include protocol and not end in a slash."
  :type 'string)

(defcustom helm-jira-stash-url nil
  "Stash url to use (for bitbucket API, should include protocol and not end in a slash)."
  :type 'string)

(defcustom helm-jira-board-id nil
  "ID of the JIRA-board you want to work with."
  :type 'integer)

(defcustom helm-jira-username nil
  "Username to use when logging in to JIRA."
  :type 'integer)

(defcustom helm-jira-password nil
  "Password to use when logging in to JIRA.  Not recommended to set this (helm-jira will save password per session)."
  :type 'string)

(defcustom helm-jira-project nil
  "The JIRA project to use for bitbucket API requests."
  :type 'string)

(defcustom helm-jira-repo nil
  "The BitBucket repo to use for bitbucket API requests."
  :type 'string)

(defun helm-jira-build-basic-auth-token ()
  "Build the base64-encoded auth token from `helm-jira-username' and `helm-jira-password'."
  (base64-encode-string (format "%s:%s" helm-jira-username helm-jira-password)))

(defun helm-jira-build-auth-header ()
  "Build the Authorization-Header for JIRA requests."
  (format "Basic %s" (helm-jira-build-basic-auth-token)))

(defun helm-jira-ensure-password ()
  "Ensures that `helm-jira-password' is set."
  (when (not helm-jira-password)
    (helm-jira-read-password)))

(defun helm-jira-read-password ()
  "Read a new value for `helm-jira-password'."
  (setq helm-jira-password (read-passwd (format "JIRA-Password for %s: " helm-jira-username)))
  nil)

(defun helm-jira-logout ()
  "Unset `helm-jira-password'."
  (interactive)
  (setq helm-jira-password nil)
  (message "Cleared JIRA password"))

(defun helm-jira-request (&rest args)
  "Call `request' with the supplied `ARGS', but ensure that a password is set and credentials are supplied."
  (helm-jira-ensure-password)
  (apply 'request (append args
                          `(:headers (("Authorization" . ,(helm-jira-build-auth-header))))
			  '(:sync t))))

;; original
;; (defun helm-jira-fetch-issues (callback)
;;   "Fetch all open issues for the configured board and call `CALLBACK' with the resulting list."
;;   (helm-jira-request
;;    (format "%s/rest/agile/1.0/board/%s/issue" helm-jira-url helm-jira-board-id)
;;    :params '(("fields" . "summary")
;;              ("maxResults" . "200")
;;              ("jql" . "sprint in openSprints()"))
;;    :parser 'json-read
;;    :success (function*
;;              (lambda (&key data &allow-other-keys)
;;                (funcall callback (alist-get 'issues data))))))

(defun helm-jira-fetch-pull-requests (callback)
  "Fetch all open pull requests for the configured project and repo and call `CALLBACK' with the resulting list."
  (helm-jira-request
   (format "%s/rest/api/1.0/projects/%s/repos/%s/pull-requests" helm-jira-stash-url helm-jira-project helm-jira-repo)
   :parser 'json-read
   :success (function* (lambda (&key data &allow-other-keys)
                         (funcall callback (alist-get 'values data))))))

(defun helm-jira-fetch-issue-details (issue-id callback)
  "Fetch the details for a single issue by its `ISSUE-ID' (=purely numeric, not its key), and call `CALLBACK' with the resulting list of issues."
  (helm-jira-request
   (format "%s/rest/dev-status/latest/issue/detail" helm-jira-url)
   :params `(("issueId" . ,issue-id)
             ("applicationType" . "stash")
             ("dataType" . "pullrequest"))
   :parser 'json-read
   :success (function* (lambda (&key data &allow-other-keys)
                         (funcall callback (elt (alist-get 'detail data) 0))))))

(defun helm-jira-build-candidate-list-from-issues (issues)
  "Take `ISSUES' as returned by ‘helm-jira-fetch-issues’ and build a suitable candidate list for helm with it."
  (mapcar
   (lambda (issue)
     (let* ((key     (let-alist issue .key))
	    (status  (let-alist issue .fields.status.name))
            (summary (let-alist issue .fields.summary)))
       `(,(format "%-15s: %-12s %s" key status summary) . ,issue))) ; need to modify project-name length '-' 4digit key id
   issues))

(defun helm-jira-build-candidate-list-from-pull-requests (pull-requests)
  "Take `PULL-REQUESTS' as returned by ‘helm-jira-fetch-pull-requests’ and build a suitable candidate list for helm with it."
  (mapcar
   (lambda (pr)
     (let* ((title (alist-get 'title pr))
            (id (alist-get 'id pr))
            (author (alist-get 'user (alist-get 'author pr)))
            (author-name (alist-get 'displayName author)))
       `(,(format "%s: %s\t%s"
                  (propertize (format "#%s" id) 'font-lock-face 'font-lock-constant-face)
                  title
                  (propertize (concat "@" author-name) 'font-lock-face 'font-lock-comment-face)) . ,pr)))
   pull-requests))

;; (defun helm-jira-helm-issues ()
;;   "Fetch a list of issues from JIRA and prompt for selection of one."
;;   (interactive)
;;   (helm-jira-fetch-issues
;;    (lambda (issues)
;;      (let* ((helm-src
;;              (helm-build-sync-source "jira-issues-source"
;;                :candidates (helm-jira-build-candidate-list-from-issues issues)
;;                :action (helm-make-actions
;;                         "Check-out" #'helm-jira-helm-action-checkout-issue
;;                         "Open in browser" #'helm-jira-helm-action-open-issue-in-browser))))
;;        (helm :sources helm-src)))))

(defun helm-jira-search-for-issues-using-jql (&optional project-key)
  "Fetch a list of issues from JIRA and prompt for selection of one."
  (interactive)
  (if project-key
      (helm-jira--search-for-issues-using-jql (format "project=%s" project-key))
    (if helm-jira-project
	(helm-jira--search-for-issues-using-jql (format "project=%s" helm-jira-project))
      (message "[error] %s: specify helm-jira-project!" this-command))))

(defun helm-jira--build-candidate-search-for-issues-using-jql (issues)
  "Take `ISSUES' as returned by ‘helm-jira-fetch-issues’ and build a suitable candidate list for helm with it."
  (mapcar
   (lambda (issue)
     (let ((key         (let-alist issue .key))
	   (defect_rank (let-alist issue .fields.customfield_10013.value))
	   (issuetype   (let-alist issue .fields.issuetype.name))
	   (status      (let-alist issue .fields.status.name))
           (summary     (let-alist issue .fields.summary)))
	   `(,(format "%-15s: %s %-11s %-11s %s" key (if defect_rank defect_rank " ")  issuetype status summary) . ,issue))) ; need to modify key project-name + length '-' 4digit key id
   issues))

(defun helm-jira--action-open-issue-in-buffer (issue)
  "Open the given `ISSUE' in the buffer."
  (let ((buffer-name (alist-get 'key issue)))
    (with-output-to-temp-buffer buffer-name
      (princ
       (mapconcat
	(lambda (field) (format "%s: %s" (car field) (cdr field)))
	(let-alist issue
	  (list
	   (cons "key"         .key)
	   (cons "issuetype"   .fields.issuetype.name)
	   (cons "summary"     .fields.summary)
	   (cons "Defect_Rank" .fields.customfield_10013.value)
	   (cons "Frequency"   .fields.customfield_10023.value)
	   (cons "FixType"     .fields.customfield_10030.value)
	   (cons "Assignee"    .fields.assignee.displayName)))
	"\n")))))

(defun helm-jira--action-open-issue-in-browser (issue)
  "Open the given `ISSUE' in the browser."
  (let ((key (alist-get 'key issue)))
    (browse-url-default-browser (format "%s/browse/%s" helm-jira-url key))))

(defun helm-jira--search-for-issues-using-jql (jql)
  "Fetch a list of issues from JIRA with jql."
  (helm-jira--fetch-search-for-issues-using-jql jql
   (lambda (issues)
     (let* ((helm-src
             (helm-build-sync-source "jira-issues-source"
               :candidates (helm-jira--build-candidate-search-for-issues-using-jql issues)
               :action (helm-make-actions
			"Open in browser" #'helm-jira--action-open-issue-in-browser
			"Show issue"      #'helm-jira--action-open-issue-in-buffer)
	       :migemo t)))
       (helm :sources helm-src
	     :candidate-number-limit 10000)))))

(defun helm-jira--fetch-search-for-issues-using-jql (jql callback)
  "Fetch issues of specified project-key and call `CALLBACK' with the resulting list."
  (let (issues          ; for all issues are stacked
	nissues         ; the number of issues on each request
	(startAt 0)     ; startAt=0 if you want to show id from 1, in case startAt=1, shows id from 2
	(maxResults 100)) ; fixed
    (cl-loop do
	     (message "[info] %s startAt: %d" this-command startAt)
	     (helm-jira-request
	      ;; (format "%s/rest/api/latest/search?startAt=%s&maxResults=%s&fields=summary&jql=%s+ORDER+BY+key+ASC" helm-jira-url startAt maxResults jql)
	      (format "%s/rest/api/latest/search?startAt=%s&maxResults=%s&jql=%s+ORDER+BY+key+ASC" helm-jira-url startAt maxResults jql)
	      :parser 'json-read
	      :success (function*
			(lambda (&key data &allow-other-keys)
			  (let ((tissues (alist-get 'issues data))) ; tissues: tmp issues
			    (setq nissues (length tissues))
			    (setq startAt (+ startAt nissues))
			    (setq issues (vconcat issues tissues))))))
	     while (>= nissues maxResults))
    (funcall callback issues)))

(defun helm-jira-helm-pull-requests ()
  "Fetch a list of pull-requests from Bitbucket and prompt for selection of one to open in the browser."
  (interactive)
  (helm-jira-fetch-pull-requests
   (lambda (pull-requests)
     (let* ((helm-src
             (helm-build-sync-source "jira-pull-requests-source"
               :candidates (helm-jira-build-candidate-list-from-pull-requests pull-requests)
               :action (helm-make-actions
                        "Check-out" #'helm-jira-helm-action-checkout-pull-request
                        "Open in browser" #'helm-jira-helm-action-open-pull-request-in-browser))))
       (helm :sources helm-src)))))

(defun helm-jira-magit-checkout-pull-request ()
  "Fetch a list of pull-requests from Bitbucket and prompt for selection of one to open in the browser."
  (interactive)
  (helm-jira-fetch-pull-requests
   (lambda (pull-requests)
     (let* ((helm-src
             (helm-build-sync-source "jira-pull-requests-source"
               :candidates (helm-jira-build-candidate-list-from-pull-requests pull-requests)
               :action (helm-make-actions "Check-out" #'helm-jira-helm-action-checkout-pull-request))))
       (helm :sources helm-src)))))

(defun helm-jira-helm-action-open-issue-in-browser (issue)
  "Open the given `ISSUE' in the browser."
  (let ((key (alist-get 'key issue)))
    (browse-url-default-browser (format "%s/browse/%s" helm-jira-url key))))

(defun helm-jira-helm-action-open-pull-request-in-browser (pull-request)
  "Open the given `PULL-REQUEST' in the browser."
  (let* ((links (alist-get 'links pull-request))
         (self (elt (alist-get 'self links) 0))
         (href (alist-get 'href self)))
    (browse-url href)))

(defun helm-jira-helm-action-checkout-pull-request (pull-request)
  "Check-out the given `PULL-REQUEST' using magit (branch has to already exist currently)."
  (let* ((from-ref (alist-get 'fromRef pull-request))
         (display-id (alist-get 'displayId from-ref)))
    (magit-checkout display-id)))

(defun helm-jira-helm-action-checkout-issue (issue)
  "Check-out a branch for the given `ISSUE'."
  (let* ((id (alist-get 'id issue)))
    (helm-jira-fetch-issue-details id #'helm-jira-checkout-branch-for-issue-details)))

(defun helm-jira-checkout-branch-for-issue-details (issue-details)
  "Check-out the branch contained in the given `ISSUE-DETAILS' response."
  (let* ((branches (alist-get 'branches issue-details))
         (branch-count (length branches))
         (branch (if (= branch-count 1)
                     (elt branches 0)
                   (message "There are multiple branches for this issue, not yet implemented!")
                   (elt branches 0)))
         (branch-name (alist-get 'name branch)))
    (magit-checkout branch-name)))

;; added, the followings
;; Get all projects
(defun helm-jira--action-get-all-projects-list-issues (project)
  "list `ISSUES'. of project"
  (let ((jql (format "project=%s" (alist-get 'key project))))
    (helm-jira--search-for-issues-using-jql jql)))

(defun helm-jira--fetch-get-all-projects (callback)
  "Fetch all projects"
  (helm-jira-request
   (format "%s/rest/api/latest/project" helm-jira-url)
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (funcall callback data)))))

(defun helm-jira--build-candidate-get-all-projects (projects)
  "Take `PROJECTS' as returned by‘helm-jira-fetch-projects’ and build a suitable candidate list for helm with it."
  (mapcar
   (lambda (project)
     (let* ((key  (alist-get 'key  project))
	    (name (alist-get 'name project)))
       `(,(format "%-17s: %s" key name) . ,project)))
   projects))

(defun helm-jira--action-get-all-projects-browser-issues (project)
  "Open the given `PROJECT' in the browser."
  (let ((key (alist-get 'key project)))
    (browse-url-default-browser (format "%s/browse/%s" helm-jira-url key))))

(defun helm-jira-get-all-projects ()
  "Fetch project list from JIRA and prompt for selection of one."
  (interactive)
  (helm-jira--fetch-get-all-projects
   (lambda (projects)
     (let* ((helm-src
             (helm-build-sync-source "jira-projects-source"
               :candidates (helm-jira--build-candidate-get-all-projects projects)
               :action (helm-make-actions
			"List Issues"     #'helm-jira--action-get-all-projects-list-issues
                        "Open in browser" #'helm-jira--action-get-all-projects-browser-issues)
	       :migemo t)))
       (helm :sources helm-src)))))

;; Get favourite filters
(defun helm-jira--fetch-get-favourite-filters (callback)
  (helm-jira-request
   (format "%s/rest/api/latest/filter/favourite" helm-jira-url)
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (funcall callback data)))))

(defun helm-jira--build-candidate-get-favourite-filters (filters)
  "Take `FILTERS' as returned by‘helm-jira-fetch-filters’ and build a suitable candidate list for helm with it."
  (mapcar
   (lambda (filter)
     (let* ((id      (alist-get 'id   filter))
	    (name    (alist-get 'name filter)))
       `(,(format "%+6s: %s" id name) . ,filter)))
   filters))

(defun helm-jira--action-get-favourite-filters-list-issues (filter)
  (let ((jql (format "filter=%s" (alist-get 'id filter))))
    (helm-jira--search-for-issues-using-jql jql)))

(defun helm-jira--action-get-favourite-filters-browser-issues (filter)
  "Open the given `FILTER' in the browser."
  (let ((viewUrl (alist-get 'viewUrl filter)))
    (browse-url-default-browser viewUrl)))

(defun helm-jira-get-favourite-filters ()
  (interactive)
  (helm-jira--fetch-get-favourite-filters
   (lambda (filters)
     (let* ((helm-src
             (helm-build-sync-source "favourite-filters"
               :candidates (helm-jira--build-candidate-get-favourite-filters filters)
               :action (helm-make-actions
                        "List issues"     #'helm-jira--action-get-favourite-filters-list-issues
                        "Open in browser" #'helm-jira--action-get-favourite-filters-browser-issues)
	       :migemo t)))
       (helm :sources helm-src)))))

;; Get current user
(defun helm-jira--get-current-user ()
  (helm-jira-request
   ;; https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-myself/#api-rest-api-3-myself-get
   (format "%s/rest/api/latest/myself" helm-jira-url)
   :success (function*
             (lambda (&key data &allow-other-keys)
	       (message "helm-jira--get-current-user: %s" data)))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
			 (message "[error] helm-jira--get-current-user: %S" error-thrown)))))

;; Add attachment
(defun helm-jira--add-ataachment (issueIdOrKey files callback)
  "add attachment files in JIRA issueIdOrKey
files should be in list with absolute path
callback specified nil"

  ;; Eventhoug helm-jira-build-auth-header is available,
  ;; add-attachment sometimes fails 401 {"errorMessages":["You do not have the permission to see the specified issue.","Login Required"],"errors":{}}
  ;; so, dummy call for uploading
  (helm-jira--get-current-user)

  (message "helm-jira--add-ataachment: uploading....")
  (helm-jira-request
   ;; https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-attachments/#api-rest-api-3-issue-issueidorkey-attachments-post
   (format "%s/rest/api/latest/issue/%s/attachments" helm-jira-url issueIdOrKey)
   :type "POST"
   :headers '(("X-Atlassian-Token" . "no-check"))
   :files (mapcar (lambda (file) `("file" . ,file)) files)
   :success (function*
             (lambda (&key data &allow-other-keys)
               ;;(funcall callback data)
	       (message "[debug] helm-jira--add-attachment success: %s" data)
	       (message "helm-jira--add-ataachment %s uploaded!" issueIdOrKey)))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
			 (message "[error] helm-jira--add-ataachment: %S" error-thrown)))))

(defun helm-jira-add-ataachment ()
  "Add attachments with the files marked in Dired in JIRA key specified by arg"
  (interactive)
  (let ((marked-files (when (> (string-to-number (dired-number-of-marked-files)) 0) (dired-get-marked-files)))
	(limitsize 40000000) ; upload limit size
	uploadfiles
	jira-key)
    (if marked-files
	(progn
	  (setq jira-key (read-string "Jira Key: "))
	  (setq uploadfiles
		(delq nil (mapcar (lambda (file)
				    (if (> (file-attribute-size (file-attributes file)) limitsize)
					(progn
					  (message "[warn] %s: %s > %s" this-command file limitsize)
					  nil)
				      file)) marked-files)))
	  (if uploadfiles
	      (helm-jira--add-ataachment jira-key uploadfiles nil)))
      (message "helm-jira-add-attachment %s: no marked files!"))))

(provide 'helm-jira)
;;; helm-jira.el ends here
