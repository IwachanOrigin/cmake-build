;;; cmake-utils.el --- CMake 補助関数 -- lexical-binding t; --

;; 使い方
;; (myfind-topmost-cmake) を呼び出すと，現在バッファのファイルを起点に
;;   1. `.git` が存在するディレクトリを“プロジェクト・ルート”とみなす
;;   2. ルートから現在ファイルまでの階層を走査し
;;   3. 最上位（＝ルートに最も近い）CMakeLists.txt を返す
;; 見つからなければ nil を返す

(require 'cl-lib)

(defun myfind-topmost-cmake (&optional buffer-file)
  ""
  (let* ((file (or buffer-file (buffer-file-name)))
         ;; 1) Git ルートを決定
         (git-root (and file (locate-dominating-file file ".git")))
         (result   nil))
    (when git-root
      ;; 2) ルート→ファイルのディレクトリ階層を列挙
      (cl-loop
       with dir = (file-name-directory file)
       while (and dir (file-in-directory-p dir git-root))
       do (when (file-exists-p (expand-file-name "CMakeLists.txt" dir))
            ;; 3) ルートに近いものほど後に来るため、毎回上書き
            (setq result (expand-file-name "CMakeLists.txt" dir)))
       ;; 親ディレクトリへ
       do (setq dir (if (equal dir git-root)
                        nil
                      (file-name-directory (directory-file-name dir))))))
    result))

(defun myfind-topmost-cmake-dir (&optional buffer-file)
  ""
  (let ((cmake-file (myfind-topmost-cmake buffer-file)))
    (when cmake-file
      (file-name-directory cmake-file))))

;;; 簡易テスト (ert)
(eval-when-compile (require 'ert))

(ert-deftest myfind-topmost-cmake-test ()
  (let ((tmp (make-temp-file cmake-test- t))
        (root (expand-file-name repo tmp))
        (src  (expand-file-name src  root))
        (deep (expand-file-name deep src))
        (file (expand-file-name main.cpp deep)))
    (dolist (d (list root src deep)) (make-directory d t))
    ;; ルートに .git, src と deep に CMakeLists.txt を作成
    (make-directory (expand-file-name .git root) t)
    (dolist (d (list src deep))
      (with-temp-file (expand-file-name CMakeLists.txt d)))
    (with-temp-file file) ; ダミー cpp

    (should (equal (myfind-topmost-cmake file)
                   (expand-file-name CMakeLists.txt src)))))

(provide 'cmake-utils)
;;; cmake-utils.el ends here
