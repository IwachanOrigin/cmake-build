;;; cmake-utils.el --- Define functions to assist cmake-build.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yuji Iwanaga

;; Author: Yuji Iwanaga <nm7.ty.nt.abc@gmail.com>
;; URL: https://github.com/IwachanOrigin/cmake-build
;; Version: 1.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; How to Use
;; Called cmake-utils-find-topmost-cmake-file,
;; the following processing is performed starting from the file in the current buffer.
;;   1. The directory where `git` is located is considered the "project root".
;;   2. Traverses the hierarchy from the root to the current file.
;;   3. Return the top-level (i.e., closest to the root) CMakeLists.txt full-path.
;;   4. Return nil if not found

;;; Code:

(require 'cl-lib)

(defun cmake-utils-find-topmost-cmake-file (&optional buffer-file)
  "Return the top-level (i.e., closest to the root) CMakeLists.txt full-path."
  (let* ((file (or buffer-file (buffer-file-name)))
         ;; 1) Determine Git Root
         (git-root (and file (locate-dominating-file file ".git")))
         (result   nil))
    (when git-root
      ;; 2) Root -> Enumerate directory hierarchy of files
      (cl-loop
       with dir = (file-name-directory file)
       while (and dir (file-in-directory-p dir git-root))
       do (when (file-exists-p (expand-file-name "CMakeLists.txt" dir))
            ;; 3) The closer to the root, the later it comes, so overwrite each time
            (setq result (expand-file-name "CMakeLists.txt" dir)))
       ;; Go to parent direcoty
       do (setq dir (if (equal dir git-root)
                        nil
                      (file-name-directory (directory-file-name dir))))))
    result))

(defun cmake-utils-find-topmost-cmake-file-dir (&optional buffer-file)
  "Return the top-level (i.e., closest to the root) CMakeLists.txt directory path."
  (let ((cmake-file (cmake-utils-find-topmost-cmake-file buffer-file)))
    (when cmake-file
      (file-name-directory cmake-file))))

(provide 'cmake-utils)
;;; cmake-utils.el ends here
