;;; org-recipes.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: February 24, 2024
;; Modified: February 24, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashweaver/org-recipes
;; Package-Requires: ((emacs "24.3") (org)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Recipe meta-data properties and getters.
;;
;;; Code:

(defgroup org-recipe nil
  "Options related to recipes in `org-mode'."
  :group 'org
  :tag "Recipes in org-roam."
  :link '(url-link :tag "Github" "https://github.com/cashpw/org-recipe"))

(defcustom org-recipes--property-prefix "RECIPE"
  "Prefix for all `org-mode' headline properties used by org-recipe."
  :type 'string
  :group 'org-recipe)

(defun org-recipes--get-prop (property pom &optional default)
  "Return value for PROPERTY at POM or (optional) DEFAULT if value is nil."
  (let ((property-value (org-entry-get pom property)))
    (if default
        (or property-value
            default)
      property-value)))

(defun org-recipes--get-int-property (property &optional point-or-marker)
  "Return the integer value of PROPERTY (at POINT-OR-MARKER).

Return nil if the property doesn't exist."
  (let* ((point-or-marker (or point-or-marker
                              (point)))
         (value (org-recipes--get-prop property
                                       point-or-marker)))
    (if value
        (string-to-number value)
      nil)))

(defun org-recipes--format-duration (minutes)
  "Return formatted time string of duration MINUTES."
  (org-duration-from-minutes minutes))

;; Preparation time

(defun org-recipes--prop-prep-minutes ()
  "Property for preparation minutes."
  (concat org-recipes--property-prefix "_PREP_MINUTES"))

(defun org-recipes-get-prep-minutes (&optional point-or-marker)
  "Return prep time in minutes at POINT-OR-MARKER."
  (org-recipes--get-int-property (org-recipes--prop-prep-minutes)
                                 point-or-marker))

(defun org-recipes-get-prep-duration (&optional point-or-marker)
  "Return formatted preparation duration at POINT-OR-MARKER."
  (let ((prep-minutes (org-recipes-get-prep-minutes point-or-marker)))
    (if (not prep-minutes)
        nil
      (org-recipes--format-duration prep-minutes))))

;; Cook time

(defun org-recipes--prop-cook-minutes ()
  "Property for cooking minutes."
  (concat org-recipes--property-prefix "_COOK_MINUTES"))

(defun org-recipes-get-cook-minutes (&optional point-or-marker)
  "Return cook time in minutes at POINT-OR-MARKER."
  (org-recipes--get-int-property (org-recipes--prop-cook-minutes)
                                 point-or-marker))

(defun org-recipes-get-cook-duration (&optional point-or-marker)
  "Return formatted cooking duration at POINT-OR-MARKER."
  (let ((cook-minutes (org-recipes-get-cook-minutes point-or-marker)))
    (if (not cook-minutes)
        nil
      (org-recipes--format-duration cook-minutes))))

;; Ready-in time

(defun org-recipes-get-total-duration (&optional point-or-marker)
  "Return formatted total duration at POINT-OR-MARKER."
  (let* ((cook-minutes (org-recipes-get-cook-minutes point-or-marker))
         (prep-minutes (org-recipes-get-prep-minutes point-or-marker))
         (total-minutes (apply #'+
                               (remove nil
                                       `(,cook-minutes
                                         ,prep-minutes)))))
    (if (= 0 total-minutes)
        nil
      (org-recipes--format-duration total-minutes))))

;; Servings

(defun org-recipes--prop-servings ()
  "Property for servings."
  (concat org-recipes--property-prefix "_SERVINGS"))

(defun org-recipes-get-servings (&optional point-or-marker)
  "Return servings at POINT-OR-MARKER (e.g. \"8\")."
  (org-recipes--get-int-property (org-recipes--prop-servings)
                                 point-or-marker))

;; Yield

(defun org-recipes--prop-yield ()
  "Property for yield."
  (concat org-recipes--property-prefix "_YIELD"))

(defun org-recipes-get-yield (&optional point-or-marker)
  "Return yield at POINT-OR-MARKER (e.g. \"7 liters\")."
  (org-recipes--get-prop (org-recipes--prop-yield)
                         point-or-marker))

(defcustom org-recipes--properties `(,(org-recipes--prop-prep-minutes)
                                     ,(org-recipes--prop-cook-minutes)
                                     ,(org-recipes--prop-servings)
                                     ,(org-recipes--prop-yield))
  "`org-mode' properties used by `org-recipe'."
  :type '(repeat string)
  :group 'org-recipe)

(provide 'org-recipes)
;;; org-recipes.el ends here
