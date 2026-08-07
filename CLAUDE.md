# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

jpops は日本国内の人口統計（国勢調査、住民基本台帳）データを tidy な形で提供する R パッケージ。データの一部は e-Stat API（estatapi パッケージ経由）から取得する。CRAN 未登録、lifecycle: experimental。

README.md は README.Rmd から生成される（`devtools::build_readme()`）。README.Rmd を編集し、README.md を直接編集しない。

## 開発コマンド

テストは testthat 3e（`tests/`）、CI は GitHub Actions（`.github/workflows/`）にある。renv はまだ導入していない。

```r
devtools::load_all()      # 読み込み
devtools::document()      # roxygen2 で man/ と NAMESPACE を再生成
devtools::test()          # 現状 32 passed / 1 skipped（API を叩くテスト）
devtools::check()         # 現状 0 errors / 0 warnings / 0 notes
devtools::build_readme()
```

R ファイルの整形は air（`air.toml`: line-width 80 / indent 2）で行う。編集後に `air format R tests` を実行する。CI でも `air format R tests --check` が走る。

API を叩く関数の手動確認には application id が必要:

```r
get_jinkou(2020, appid = Sys.getenv("ESTAT_TOKEN"), cache = FALSE)
reset_cache()  # キャッシュを全削除してからの再取得確認
```

API を叩くテストは `ESTAT_TOKEN` と `JPOPS_RUN_API_TESTS` の両方が設定されているときだけ実行される。CI には token が無いので常にスキップされる。

## アーキテクチャ

### 公開 API の構造

`get_jinkou()`（男女別総数）と `get_jinkou_age()`（男女・年齢別）が入口。どちらも同じ 3 層構造をとる:

1. **キャッシュ層** — `rappdirs::user_cache_dir("jpops")` 配下の `.rds` を読む／書く。処理済みキャッシュのパスは `jpops_processed_cache_file()` で組み立てる
2. **取得層** — `collect_jinkou_raw()` / `collect_jinkou_age_raw()` が内部アダプター `fetch_estat_table()`（[R/estat.R](R/estat.R)）経由で `estatapi::estat_getStatsData()` を呼ぶ
3. **整形層** — 年ごとの分岐で列を選択・改名・値を正規化

新しい調査を追加する場合はこの 3 層と、後述の年別 ID テーブル・列位置分岐の両方を触ることになる。

出力の形を変えたら、処理済みキャッシュのバージョン（`jpops_processed_cache_file()` の `cache_version`）を上げること。上げないと古い形のキャッシュが黙って再利用される。

### 年 → statsDataId のマッピング

[R/jinkou.R](R/jinkou.R) の `survey_year_dataid` が、調査種別（`total` / `age`）× 調査年 → e-Stat の `statsDataId` を保持する唯一の場所。年を追加するときはここに ID を足す。コメントで郡計を含む別 ID を併記してあるので消さない。許容年は `match_survey_year()` がこのテーブルから導出するので、年の一覧を二重管理しない。

### 年ごとに違う元データを吸収するのが本体

e-Stat の返す表は調査年ごとに列の並び・`cat0N_code` の意味・単位列の有無が異なる。そのため整形層は「年で `if`/`else if` 分岐 → `dplyr::filter(cat01_code == ...)` で絞る → **位置ベース**の `dplyr::select(5:10, 14)` / `dplyr::rename(gender = 2, ...)` で列を取る」という書き方に統一されている。列名ベースにできないのは元データの列名が年をまたいで揺れるため。分岐の末尾には `else` 節を置き、想定外の年で黙って `NULL` を返さないようにしてある。

年をまたいだ結合を可能にするため、`get_jinkou()` の出力列は `gender` / `area_code` / `area` / `value`、`get_jinkou_age()` は `gender` / `area_code` / `area` / `age` / `value` の順に揃え、元データの `cat0N_code` は公開出力に残さない。`conv_gender_vars()` が「総数／男／女」、`conv_age_vars()` が「総数（年齢）→ 総数」「年齢「不詳」→ 不詳」の表記揺れを正規化する（どちらも [R/converter.R](R/converter.R)）。2010 年の袖ケ浦市（`area_code == "12229"`）では、元データの片仮名小書き「ヶ」を通常の「ケ」に補正している。

年齢ラベルの正規化は**完全一致**で行う。かつて部分一致にしていたため 2015 年で「年齢「不詳」」まで「総数」に潰し、不詳の人口が総数として二重計上されていた。

一方、性別ラベルの正規化は**部分一致でなければならない**。元データが 2015・2010 で「総数（男女別）」、2005 で全角スペース付きの「　男」「　女」を返すため、完全一致では取りこぼす。ただし「総数（男女別）」は「男」も含むので、`conv_gender_vars()` の `case_when()` が「総数」を先に評価していることが正しさの前提になっている。**分岐の順序を入れ替えてはならない。**

### 非 ASCII 文字をソースに直接書かない

`R/` と `tests/` 配下の日本語リテラルは `"\u7dcf\u6570"` のような Unicode エスケープで書き、ソースを ASCII に保つ（R CMD check の非 ASCII 警告回避）。`intToUtf8()` は可読性が低く、引数の取り違えで「年齢」が「年」になるバグを生んだ実績があるため使わない。実装側で繰り返す語は [R/constants.R](R/constants.R) の名前付き定数に切り出す。テスト側は実装と同じ誤りを共有しないよう、定数を参照せず期待値を Unicode エスケープで直接書く。`data-raw/` はビルドから除外されるため日本語リテラルを直接書いてよい。

### R CMD check 対策の NULL 代入

各関数の冒頭にある `area_code <- gender <- NULL` は、tidyselect/data-masking による "no visible binding" NOTE を消すためのもの。新しい列名を data-masking で参照したら、同様に NULL 代入を追加する。

### コーディング規約

- パイプはネイティブ `|>`（magrittr は依存から外し、`%>%` の再エクスポートも廃止済み）。`Depends: R (>= 4.1)`。
- `dplyr::select()` / `rename()` の**位置指定は意図的**なので、列名ベースに「直さない」（前節の理由）。
- 依存は最小限に保つ。かつて purrr shim（`R/compat-purrr.R`）を抱えていたが未使用のため削除した。map 系が必要になったら shim を復活させず、purrr を Imports に入れるか base で書く。

### data-raw/

`data-raw/` は都道府県ごとのオープンデータ収集を試した**探索的スクリプト**で、パッケージから source されておらず、`data/` オブジェクトも生成していない（同梱データセットが無いため `LazyData` は削除済み。`data/` を追加する際に戻す）。ダウンロード行はコメントアウトされている。ここのコードは実装の参考であって動作保証されたものではない。air の整形対象からも外してある。

## 既知の不整合（未修正）

いずれも実物を確認済みで、意図的に未修正のまま残してある。

1. **`area_filter()`** — 都道府県コードの判定が `%in%` ではなく非アンカーの `stringr::str_detect()` による部分一致。`area` が `"prefecture"` / `"city"` 以外だと未定義の `out` を返す。加えて `.area` の分岐が呼び出し側と `area_filter()` 内で二重になっている（`get_jinkou()` と `get_jinkou_age()` はほぼ同一で、まだ共通ヘルパへの抽出余地がある）。現状の挙動は `tests/testthat/test-filter.R` で固定してあるので、直すときに差分が見える。

## 保留中の判断

- **jpstat への移行** — `estatapi` は 2020 年 4 月が最終リリースで保守が低調、`jpstat` は現行。ただし jpstat は列を分類メタデータから動的に構成するため、年別の位置ベース `select()` が全 8 経路で破綻する。appId をプロセス全体の環境変数 `ESTAT_API_KEY` から読む点も並列取得で競合しうる。取得層は `fetch_estat_table()` に切り出してあるので、差し替えるならこの内部だけで済む。判断には 8 統計表の実レスポンス比較が必要。
- **住民基本台帳人口** — [notes/juki-population-design.md](notes/juki-population-design.md) に設計を保留してある。当該調査は e-Stat のデータベース提供が無く（DB 0 件 / ファイル 270 件）`statsDataId` が存在しないため、実装経路はファイル取得 + Excel 解析になる。着手前に社会・人口統計体系（政府統計コード `00200502`）で必要な粒度が取れるかを確認すること。
- **CI が未実行** — ワークフローのトリガーが `main` 限定のため、フィーチャーブランチへの push では起動しない。main への PR を開いた時点で走る。最初の実行では `ubuntu-22.04` + R 4.1 のジョブ（`Depends: R (>= 4.1)` の下限を直接検証する）の結果を確認すること。

## コミット

Conventional Commits に従う（`/commit-msg` スキル参照）。`Co-Authored-By:` フッタは付けない。
