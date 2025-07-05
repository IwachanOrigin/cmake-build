;;; cmake-build.el --- “最上位 CMakeLists.txt” 上で固定 cmake を実行 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yuji Iwanaga

;; Author: Yuji Iwanaga <nm7.ty.nt.abc@gmail.com>
;; URL: https://github.com/IwachanOrigin/cmake-build
;; Version: 1.0.0
;; Package-Requires: ((cmake-utils "1.0.0"))

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

;; PRE-CONDITION
;;   CMake, ninja, clang and clang++ must be registered in the PATH.

;;; Code

(require 'cl-lib)
(require 'cmake-utils)

(defgroup cmake-build nil
  "A utility to build the entire project from Emacs."
  :group 'tools)

(defcustom cmake-build-directory "build"
  "The name of the build directory specified by cmake -B."
  :type 'string
  :group 'cmake-build)

(defcustom cmake-build-generator "Ninja Multi-Config"
  "Generator specified by cmake -G."
  :type 'string
  :group 'cmake-build)

(defcustom cmake-build-qt-dir "C:/software/Qt/6.8.3/msvc2022_64"
  "Qt installation path specified by cmake -D QTDIR"
  :type 'string
  :group 'cmake-build)

(defun cmake--compile (command dir)
  "Set DIR to `default-directory' and COMMAND to `compile'."
  (let ((default-directory dir))
    (compile command)))

(defun cmake-build--internal (build-type &optional buffer-file)
  "Internal function to execute build with BUILD-TYPE set to \“Debug\” or \“Release\”.
If BUFFER-FILE is non-nil, then the top level CMakeLists.txt is searched based on that file."
  (let* ((root (cmake-utils-find-topmost-cmake-file-dir buffer-file)))
    (unless root
      (user-error "Top level CMakeLists.txt not found."))
    (cmake--compile
     (format "cmake --build %s --config %s"
             (shell-quote-argument
              (expand-file-name cmake-build-directory root))
             build-type)
     root)))

;;;###autoload
(defun cmake-configure (&optional buffer-file)
  "Run cmake initialization in the directory containing the top-level CMakeLists.txt file.
If BUFFER-FILE is not specified, the search is based on the current buffer."
  (interactive)
  (let* ((root (cmake-utils-find-topmost-cmake-file-dir buffer-file)))
    (unless root
      (user-error "Top level CMakeLists.txt not found."))
    (cmake--compile
     (format "cmake -S . -B %s -G \"%s\" -D CMAKE_C_COMPILER=clang -D CMAKE_CXX_COMPILER=clang++ -D QTDIR=%s"
             cmake-build-directory
             cmake-build-generator
             cmake-build-qt-dir)
     root)))

;;;###autoload
(defun cmake-build-debug (&optional buffer-file)
  "Run `cmake --build` in the Debug configuration; you can specify a reference file with C-u."
  (interactive
   (list (when current-prefix-arg
           (read-file-name "Reference C/C++ files: "
                           nil (buffer-file-name) t))))
  (cmake-build--internal "Debug" buffer-file))

;;;###autoload
(defun cmake-build-release (&optional buffer-file)
  "Run `cmake --build` in the Release configuration; you can specify a reference file with C-u."
  (interactive
   (list (when current-prefix-arg
           (read-file-name "Reference C/C++ files: "
                           nil (buffer-file-name) t))))
  (cmake-build--internal "Release" buffer-file))

(provide 'cmake-build)
;;; cmake-build.el ends here
