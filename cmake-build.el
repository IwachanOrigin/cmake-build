;;; cmake-build.el --- “最上位 CMakeLists.txt” 上で固定 cmake を実行 -*- lexical-binding: t; -*-

;; 事前条件
;;   1. myfind-topmost-cmake-dir が定義済み（前回までに作成）
;;   2. cmake, ninja, clang, clang++ が PATH で引ける
;;   3. 環境変数 QT_DIR が設定済み（なければ書き換えてください）

(require 'cl-lib)

(defgroup cmake-build nil
  "プロジェクト全体を Emacs からビルドするユーティリティ。"
  :group 'tools)

(defcustom cmake-build-directory "build"
  "cmake -B で指定するビルドディレクトリ名。"
  :type 'string
  :group 'cmake-build)

(defcustom cmake-build-generator "Ninja Multi-Config"
  "cmake -G で指定するジェネレータ。"
  :type 'string
  :group 'cmake-build)

(defcustom cmake-build-config "Debug"
  "cmake --build の --config 引数。マルチコンフィグ・ジェネレータ使用時のみ有効。"
  :type 'string
  :group 'cmake-build)

(defcustom cmake-build-qt-dir "C:/software/Qt/6.8.3/msvc2022_64"
  "cmake -D QTDIR で指定するQtのインストールパス"
  :type 'string
  :group 'cmake-build)

(defun cmake--compile (command dir)
  "DIR を `default-directory' にして COMMAND を `compile' で実行する。"
  (let ((default-directory dir))
    (compile command)))

;;;###autoload
(defun cmake-configure (&optional buffer-file)
  "最上位 CMakeLists.txt があるディレクトリで cmake 初期設定を実行する。
BUFFER-FILE を指定しない場合は現在のバッファを基準に探索する。"
  (interactive)
  (let* ((root (myfind-topmost-cmake-dir buffer-file)))
    (unless root
      (user-error "最上位 CMakeLists.txt が見つかりません"))
    (cmake--compile
     (format "cmake -S . -B %s -G \"%s\" -D CMAKE_C_COMPILER=clang -D CMAKE_CXX_COMPILER=clang++ -D QTDIR=%s"
             cmake-build-directory
             cmake-build-generator
             cmake-build-qt-dir)
     root)))

;;;###autoload
(defun cmake-build (&optional buffer-file)
  "cmake-configure 済みのフォルダで cmake --build を実行する。
BUFFER-FILE を指定しない場合は現在のバッファを基準に探索する。"
  (interactive)
  (let* ((root (myfind-topmost-cmake-dir buffer-file)))
    (unless root
      (user-error "最上位 CMakeLists.txt が見つかりません"))
    (cmake--compile
     (format "cmake --build %s --config %s"
             (shell-quote-argument
              (expand-file-name cmake-build-directory root))
             cmake-build-config)
     root)))

(provide 'cmake-build)
;;; cmake-build.el ends here
