# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

jpops は日本国内の人口統計（国勢調査、住民基本台帳）データを tidy な形で提供する R パッケージ。データの一部は e-Stat API（estatapi パッケージ経由）から取得する。CRAN 未登録、lifecycle: experimental。

README.md は README.Rmd から生成される（`devtools::build_readme()`）。README.Rmd を編集し、README.md を直接編集しない。

## 開発コマンド

テストスイート・CI・renv はまだ存在しない（`tests/` なし、`.github/workflows/` なし）。

```r
devtools::load_all()      # 読み込み
devtools::document()      # roxygen2 で man/ と NAMESPACE を再生成
devtools::check()         # 現状 0 errors / 0 warnings / 0 notes
devtools::build_readme()
```

R ファイルの整形は air（`air.toml`: line-width 80 / indent 2）で行う。編集後に `air format R/` を実行する。

API を叩く関数の手動確認には application id が必要:

```r
get_jinkou(2020, appid = Sys.getenv("ESTAT_TOKEN"), cache = FALSE)
reset_cache()  # キャッシュを全削除してからの再取得確認
```

## アーキテクチャ

### 公開 API の構造

`get_jinkou()`（男女別総数）と `get_jinkou_age()`（男女・年齢別）が入口。どちらも同じ 3 層構造をとる:

1. **キャッシュ層** — `rappdirs::user_cache_dir("jpops")` 配下の `.rds` を読む／書く
2. **取得層** — `collect_jinkou_raw()` / `collect_jinkou_age_raw()` が `estatapi::estat_getStatsData()` を呼ぶ
3. **整形層** — 年ごとの分岐で列を選択・改名・値を正規化

新しい調査を追加する場合はこの 3 層と、後述の年別 ID テーブル・列位置分岐の両方を触ることになる。

### 年 → statsDataId のマッピング

[R/jinkou.R](R/jinkou.R) の `survey_year_dataid` が、調査種別（`total` / `age`）× 調査年 → e-Stat の `statsDataId` を保持する唯一の場所。年を追加するときはここに ID を足す。コメントで郡計を含む別 ID を併記してあるので消さない。

### 年ごとに違う元データを吸収するのが本体

e-Stat の返す表は調査年ごとに列の並び・`cat0N_code` の意味・単位列の有無が異なる。そのため整形層は「年で `if`/`else if` 分岐 → `dplyr::filter(cat01_code == ...)` で絞る → **位置ベース**の `dplyr::select(5:10, 14)` / `dplyr::rename(gender = 2, ...)` で列を取る」という書き方に統一されている。列名ベースにできないのは元データの列名が年をまたいで揺れるため。

年をまたいだ結合を可能にするため、出力列名は `gender` / `area` / `area_code` / `age` / `value` に揃える。`conv_gender_vars()`（[R/converter.R](R/converter.R)）が「総数／男／女」の表記揺れを正規化する。2010 年の袋井市の異体字補正のように、元データ側の表記ミスを整形層で潰している箇所がある。

### 非 ASCII 文字をソースに直接書かない

`R/` 配下の日本語リテラルはすべて `intToUtf8(c(32207, 25968))` のようなコードポイント指定で書かれている（R CMD check の非 ASCII 警告回避）。ソースを ASCII に保つ方針は維持するが、**`intToUtf8()` 自体は推奨しない**。可読性が低く、実際に引数の取り違えによるバグを生んでいる（「既知の不整合」1）。新規に書くときは Unicode エスケープ（`\uXXXX` 形式。総数なら `"\u7dcf\u6570"`）を使い、繰り返す語は名前付き定数に切り出すこと。`data-raw/` はビルドから除外されるため日本語リテラルを直接書いてよい。

### R CMD check 対策の NULL 代入

各関数の冒頭にある `area_code <- gender <- NULL` は、tidyselect/data-masking による "no visible binding" NOTE を消すためのもの。新しい列名を data-masking で参照したら、同様に NULL 代入を追加する。

### コーディング規約

- パイプはネイティブ `|>`（magrittr は依存から外し、`%>%` の再エクスポートも廃止済み）。`Depends: R (>= 4.1)`。
- `dplyr::select()` / `rename()` の**位置指定は意図的**なので、列名ベースに「直さない」（前節の理由）。
- 依存は最小限に保つ。かつて purrr shim（`R/compat-purrr.R`）を抱えていたが未使用のため削除した。map 系が必要になったら shim を復活させず、purrr を Imports に入れるか base で書く。

### data-raw/

`data-raw/` は都道府県ごとのオープンデータ収集を試した**探索的スクリプト**で、パッケージから source されておらず、`data/` オブジェクトも生成していない（同梱データセットが無いため `LazyData` は削除済み。`data/` を追加する際に戻す）。ダウンロード行はコメントアウトされている。ここのコードは実装の参考であって動作保証されたものではない。

## 既知の不整合（未修正・着手時にまとめて直す）

いずれも実物を確認済みで、意図的に未修正のまま残してある。

1. **年齢ラベルの正規化が過剰マッチ** — `intToUtf8(24180, 40802)` は「年齢」ではなく **「年」** を返す（第 2 引数が `intToUtf8()` の `multiple` に食われている）。2015・2010 の年齢分岐にあり、「年齢不詳」まで「総数」に潰す。修正時は `"\u5e74\u9f62"` のような Unicode エスケープ（ソースは ASCII のまま・実行時計算なし・引数の取り違えが起きない）へ寄せるのが望ましい。
2. **`get_jinkou_age()` がキャッシュディレクトリを作らない** — `dir.create()` は `get_jinkou()` にしか無いので、`get_jinkou()` 未実行の環境では初回の `saveRDS()` が失敗する。
3. **`cache = FALSE` が効かない** — `collect_jinkou_age_raw()` は `cache` を受け取らず、常に `*_raw.rds` を読み書きする。README の「キャッシュを利用せず API 経由で再取得」と矛盾。
4. **`area_filter()`** — 都道府県コードの判定が `%in%` ではなく非アンカーの `stringr::str_detect()` による部分一致。`area` が `"prefecture"` / `"city"` 以外だと未定義の `out` を返す。加えて `.area` の分岐が呼び出し側と `area_filter()` 内で二重になっている（キャッシュ層の重複も含め `get_jinkou()` と `get_jinkou_age()` はほぼ同一で、共通ヘルパへの抽出余地がある）。
5. **年の検証範囲のずれ** — `rlang::arg_match()` は 2000〜2020 の 5 年刻みを許容するが、`survey_year_dataid` は 2005 以降しか持たない。2000 を渡すと ID が `NA` になり不親切なエラーになる。候補は `names(survey_year_dataid$total)` から導くべき。
6. **テストが無い** — `tests/` と CI が未整備。API を叩かずに検証できる `conv_gender_vars()` / `area_filter()` / キャッシュ経路から着手できる（`withr` でキャッシュディレクトリを隔離する。掃除の際に Suggests から外したので再追加が必要）。

## コミット

Conventional Commits に従う（`/commit-msg` スキル参照）。`Co-Authored-By:` フッタは付けない。
