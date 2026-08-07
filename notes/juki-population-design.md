# 住民基本台帳人口の取り込み設計

> [!CAUTION]
> 保留中の設計。実装前に `statsDataId` の非存在を認証付き e-Stat API で最終確認すること。

## (a) 調査で判明した事実（根拠 URL 付き）

### statsDataId の有無

対象調査について採用可能な `statsDataId` は確認できなかった。e-Stat の公開一覧では「住民基本台帳に基づく人口、人口動態及び世帯数調査」は 270 件すべてが「ファイル」で、「データベース 0」と表示される。このため、公開ページ上の `statInfId` を `estatapi::estat_getStatsData()` 用の `statsDataId` として扱うことはできない。[e-Stat 対象調査一覧](https://www.e-stat.go.jp/stat-search/files?cycle=7&toukei=00200241)

`00200241` は政府統計コード、`000001039591` は e-Stat 内の統計系列識別子、`000001039601` は提供分類、各ページの `statInfId` は Excel/PDF ファイルの識別子である。これらはいずれも `statsDataId` ではない。個別ファイルページに表示される「API リクエスト URL」からも `getStatsData` 対応とは判断できない。

したがって、取得可能な全調査年について `statsDataId` は未確認であり、現時点で `survey_year_dataid` に追加できる候補はない。

### 代替となり得るデータベース経路

社会・人口統計体系（政府統計コード `00200502`）には e-Stat のデータベース提供があり、追加調査時点で DB 59 件、うち市区町村データ 33 件が確認された。住民基本台帳由来の市区町村人口指標を API 経由で取得できる可能性があるが、jpops が必要とする性別 × 5歳階級 × 市区町村の粒度を満たすかは未確認である。住基対応を実装する前にこの経路を先に確認し、必要粒度を満たすなら Excel 解析サブシステムと `readxl` 依存を回避する。[e-Stat 社会・人口統計体系](https://www.e-stat.go.jp/stat-search/database?layout=datalist&toukei=00200502)

e-Stat のデータベースを「住民基本台帳 年齢階級別人口」で検索すると 5 件がヒットするが、いずれも人口推計の参考表で、1991～1995年の全国・都道府県データに限られる。現在の jpops が必要とする年次・市区町村粒度には利用できない。[e-Stat データベース検索](https://www.e-stat.go.jp/stat-search/database?layout=datalist&query=%E4%BD%8F%E6%B0%91%E5%9F%BA%E6%9C%AC%E5%8F%B0%E5%B8%B3%20%E5%B9%B4%E9%BD%A2%E9%9A%8E%E7%B4%9A%E5%88%A5%E4%BA%BA%E5%8F%A3)

### 年次範囲と調査仕様

調査概要によれば、人口・世帯数は住民基本台帳法施行後の1968年から、年齢別人口は1994年から毎年実施されている。2013年調査から外国人住民区分が追加され、日本人住民と外国人住民の合計が「総計」とされる。2014年調査から調査期日は3月31日から1月1日に変更された。年齢は調査期日現在の満年齢を5歳階級に区分する。[e-Stat 調査概要](https://www.e-stat.go.jp/stat-search/file-download?fileKind=2&statInfId=000008291746)

e-Stat の年次一覧には1968～2026年のデータが掲載されている。2013年以降は原則12表、2014年のみ13表で、2026年分は2026年7月29日に公開されている。2012年以前は表数と構成が異なるため、同じパーサーで扱えるとは判断できない。[e-Stat 年次一覧](https://www.e-stat.go.jp/stat-search/files?cycle=7&toukei=00200241)

| 調査年 | 公開時期 | 住民区分 | 基準日 | statsDataId |
|---|---|---|---|---|
| 1968～1993 | 年次。各年の正確な公開日は未確認 | 日本人住民中心と考えられるが、表ごとの定義は未確認 | 3月31日 | 未確認。公開一覧はデータベース0件 |
| 1994～2012 | 年次。各年の正確な公開日は未確認 | 日本人住民。2012年以前の外国人の扱いは表ごとには未確認 | 3月31日 | 未確認。公開一覧はデータベース0件 |
| 2013 | 2013年8月。日付は未確認 | 総計／日本人住民／外国人住民 | 3月31日 | 未確認。公開一覧はデータベース0件 |
| 2014 | 2014-06-25 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。[年齢表ページ](https://www.e-stat.go.jp/stat-search/files?cycle=7&layout=datalist&month=0&page=1&result_back=1&stat_infid=000025711035&tclass1=000001039601&tclass2val=0&toukei=00200241&tstat=000001039591&year=20140) |
| 2015～2019 | 各年6～7月。正確な日付は未確認 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。公開一覧はデータベース0件 |
| 2020 | 2020-08-05 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。[年齢表ページ](https://www.e-stat.go.jp/stat-search/files?cycle=7&layout=datalist&month=0&page=1&result_back=1&stat_infid=000031971230&tclass1=000001039601&tclass2val=0&toukei=00200241&tstat=000001039591&year=20200) |
| 2021 | 2021-08-04 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認 |
| 2022 | 2022-08-09 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。[年齢表ページ](https://www.e-stat.go.jp/stat-search/files?layout=dataset&page=1&query=%E5%B8%82%E5%8C%BA%E7%94%BA%E6%9D%91%E5%88%A5%E5%B9%B4%E9%BD%A2%E9%9A%8E%E7%B4%9A%E5%88%A5%E4%BA%BA%E5%8F%A3&stat_infid=000032224637) |
| 2023 | 2023-07-26 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。[年齢表ページ](https://www.e-stat.go.jp/stat-search/files?cycle=7&layout=datalist&month=0&page=1&result_back=1&stat_infid=000040306648&tclass1=000001039601&tclass2val=0&toukei=00200241&tstat=000001039591&year=20230) |
| 2024 | 2024-07-24 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。[年齢表ページ](https://www.e-stat.go.jp/stat-search/files?layout=dataset&page=1&stat_infid=000040306674&toukei=00200241) |
| 2025 | 2025-08-06 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。下表の `statInfId` のみ確認 |
| 2026 | 2026-07-29 | 総計／日本人住民／外国人住民 | 1月1日 | 未確認。個別ファイル識別子は未確認 |

### 2025年について確認できたファイル識別子

2025年については、jpops の総人口・年齢別人口に対応し得る市区町村表と `statInfId` を公開ページから確認できた。これらは実装候補となるファイルだが、`statsDataId` の代替ではない。

| 区分 | 表番号・statInfId | 統計表名 | 収録単位・分類軸 |
|---|---|---|---|
| 総計・総人口 | [25-03 / 000040306653](https://www.e-stat.go.jp/stat-search/files?stat_infid=000040306653&toukei=00200241) | 〖総計〗市区町村別人口、人口動態及び世帯数 | 全国を収録範囲とする市区町村表。性別人口、世帯数、自然・社会動態等。年齢なし |
| 総計・年齢 | [25-04 / 000040306654](https://www.e-stat.go.jp/stat-search/files?stat_infid=000040306654&toukei=00200241) | 〖総計〗市区町村別年齢階級別人口 | 市区町村、性別、5歳年齢階級。世帯数なし |
| 日本人・総人口 | [25-07 / 000040306660](https://www.e-stat.go.jp/stat-search/files?stat_infid=000040306660&toukei=00200241) | 〖日本人住民〗市区町村別人口、人口動態及び世帯数 | 市区町村、性別人口、世帯数、人口動態。日本人住民のみ |
| 日本人・年齢 | [25-08 / 000040306662](https://www.e-stat.go.jp/stat-search/files?stat_infid=000040306662&toukei=00200241) | 〖日本人住民〗市区町村別年齢階級別人口 | 市区町村、性別、5歳年齢階級。日本人住民のみ |
| 外国人・総人口 | [25-11 / 000040306688](https://www.e-stat.go.jp/stat-search/files?stat_infid=000040306688&toukei=00200241) | 〖外国人住民〗市区町村別人口、人口動態及び世帯数 | 市区町村、性別人口、世帯数、人口動態。外国人住民のみ |
| 外国人・年齢 | [25-12 / 000040306690](https://www.e-stat.go.jp/stat-search/files?stat_infid=000040306690&toukei=00200241) | 〖外国人住民〗市区町村別年齢階級別人口 | 市区町村、性別、5歳年齢階級。外国人住民のみ |

代替表として、同じ住民区分ごとに都道府県別人口・世帯数表（末尾01・05・09）と都道府県別年齢階級表（末尾02・06・10）がある。ただし2025年のこれらの `statInfId` は未確認である。市区町村表に全国・都道府県の集計行が含まれるかを含め、各シートの行構造は未確認である。

外国人住民の年齢表には秘匿がある。男性総数1～9人、女性総数1～9人、男女計総数49人以下のいずれかに該当する市区町村では5歳階級の内訳が非公表になり、都道府県集計でも階級別合計と総数が一致しない場合がある。[年齢表検索結果](https://www.e-stat.go.jp/stat-search/files?layout=dataset&page=1&query=%E5%B8%82%E5%8C%BA%E7%94%BA%E6%9D%91%E5%88%A5%E5%B9%B4%E9%BD%A2%E9%9A%8E%E7%B4%9A%E5%88%A5%E4%BA%BA%E5%8F%A3)

## (b) 推奨案と理由

### ID 管理

`survey_year_dataid` には追加せず、別の内部カタログ `juki_table_catalog` に分離する。現行オブジェクトは「国勢調査の調査年 × total/age → statsDataId」という単純な対応だが、住基表は「年 × total/age × nationality_scope × geography」に分かれ、現時点の識別子も `statsDataId` ではなく `statInfId` である。

内部カタログには少なくとも `year`, `reference_date`, `table_kind`, `nationality_scope`, `geography`, `resource_type`, `resource_id`, `table_number`, `publication_date`, `source_url` を持たせる。`resource_type` は現状 `"file"`、`resource_id` は `statInfId` とし、将来データベース提供が確認された場合だけ `"stats_data"` と `statsDataId` を登録する。

2014年以降を初期対応範囲とする。2013年は外国人区分を持つが基準日が3月31日であり、2014年以降の1月1日系列とは明示的に分ける。1994～2012年は表構造と対象母集団の追加調査が必要である。

### 出力スキーマ

住基と国勢調査を比較・結合可能にする場合は、次の共通スキーマとする。

| 列名 | R型 | 値・定義 |
|---|---|---|
| `source` | character | `"population_census"` または `"basic_resident_register"` |
| `population_basis` | character | `"usual_residents"` または `"registered_residents"` |
| `reference_date` | Date | 国勢調査は各年10月1日、住基は2014年以降1月1日。2013年以前は確認した実日付 |
| `nationality_scope` | character | `"all_residents"`, `"japanese"`, `"foreign"` |
| `area_code` | character | 先頭ゼロを保持した自治体コード。検査数字を含むかは原ファイル確認後に固定 |
| `area` | character | 地域名 |
| `gender` | character | 当面は既存規約の「総数／男／女」 |
| `age` | character | 原表を正規化した年齢または年齢階級ラベル。総人口表では `NA_character_` |
| `age_lower` | integer | 階級下限。総数・不詳では `NA_integer_` |
| `age_upper` | integer | 階級上限。100歳以上などの開区間では `NA_integer_` |
| `age_granularity` | character | `"none"`, `"single_year"`, `"five_year"`, `"open_ended"`, `"unknown"` |
| `value` | double | 人口。整数値であることを検証するが、欠測・秘匿値のため格納型は double |
| `value_status` | character | `"observed"`, `"suppressed"`, `"not_available"` |
| `survey_year` | integer | e-Stat の調査年月に対応する年 |

現在の実装では `get_jinkou()` の実出力に `age` はなく、`get_jinkou_age()` だけが `age` を持つ。共通スキーマへ移行するなら総人口表にも `age = NA_character_` と `age_granularity = "none"` を追加する。

既存2関数にもメタデータ列を追加する。追加列は列数・列順・`names()`・位置指定・スナップショットに依存する利用コードを壊すが、jpops は experimental かつ CRAN 未登録なので、住基 API 公開時にスキーマを一度明示的に更新する方が安全である。NEWSとREADMEで破壊的変更を告知し、処理済みキャッシュのスキーマ世代も同時に上げる。

### 公開 API

`get_juki()` と `get_juki_age()` を新設する。例として `get_juki(year, nationality_scope = "all_residents", ...)` と `get_juki_age(year, nationality_scope = "all_residents", ...)` とする。関数名だけで国勢調査と住基を区別でき、調査年の許容範囲、基準日、年齢粒度、取得経路、年別パーサーを独立して管理できる。

国勢調査の各歳と住基の5歳階級は同じ粒度へ変換しない。`age` は表示ラベルとして維持し、`age_lower`, `age_upper`, `age_granularity` で機械判定可能にする。国勢調査を住基の5歳階級へ集約する処理は、取得関数ではなく明示的な集約関数に分離する。

総人口表には世帯数や人口動態も含まれるが、初期の `get_juki()` は人口だけを返す。将来扱う場合は `get_juki_households()` 等を分けるか、`measure` と `unit` を導入した別スキーマにする。

### キャッシュ規約

既存の `jinkou_<year>.rds` と `jinkou_age_<year>.rds` は変更せず、新規キャッシュはソース、表種別、住民区分、スキーマ世代を含める。例えば処理済みデータを `v2/processed/juki_total_2025_all_residents.rds`、生ファイルを `v2/raw/juki_age_2025_all_residents_000040306654.xlsx` またはその RDS 表現とする。

キャッシュには `resource_type`, `resource_id`, `source_url`, `downloaded_at`, `sha256`, `parser_version`, `schema_version` を保存したマニフェストを持たせる。処理済みキャッシュは `schema_version` または `parser_version` が一致しなければ無効化し、生ファイルは同じ `statInfId` と SHA-256 が一致する限り再利用する。

新しい住基取得経路では `cache = FALSE` を生ファイルと処理済みデータの両方に一貫して適用する。

### data-raw

`data-raw/age.R` と `data-raw/prefecture.R` は削除せず、探索的・非実行保証・パッケージ機能とは無関係であることを README.Rmd または `data-raw/README.md` に明記する。都道府県別スクリプトは採用しなかった経路の記録として残すが、サポート対象と誤認されない表示が必要である。

`data-raw` は `.Rbuildignore` の `^data-raw$` で除外済みである。

## (c) 却下した代替案と理由

`survey_year_dataid$juki` のような第3キーを追加する案は却下する。国勢調査の `statsDataId` と住基の `statInfId` を同じ名前・構造に格納すると、取得関数へ誤った識別子を渡す事故を誘発し、住民区分や地理粒度も表現できない。

公開ページの `statInfId` を `statsDataId` とみなす案と、未確認のIDを推測して登録する案は却下する。

`get_jinkou(source = "census" | "juki")` のように既存関数へ `source` 引数を追加する案は初期実装では却下する。同じ `year` でも基準日、対象母集団、年齢粒度、取得経路、許容年が異なる。

住基の5歳階級を各歳へ展開する案は却下する。階級内分布を仮定しなければ復元できない。国勢調査を自動的に5歳階級へ落とすことも取得関数の責務にはしない。

各自治体・都道府県サイトから月次データを収集する案は却下する。対象時点、ライセンス、ファイル形式、自治体再編への対応が統一できず、全国統一年次調査という要件から外れる。

既存キャッシュ名に住基データを保存する案は却下する。同じ年の国勢調査キャッシュと衝突し、基準日や母集団の異なるデータを静かに読み出す危険がある。

## (d) 段階的な実装計画と各段階の検証方法

1. **識別子を最初に確定する。** `ESTAT_TOKEN` が利用可能になったら、e-Stat の統計表情報取得 API で政府統計コード `00200241` を検索し、各年について `statsDataId` が本当に存在しないか確認する。同時に社会・人口統計体系 `00200502` の市区町村DBを調査し、性別 × 5歳階級 × 市区町村を満たす表があればこちらを優先する。

2. **2025年の6表を基準年として検証する。** ファイル経路が必要な場合は25-03/04/07/08/11/12を取得し、シート名、ヘッダー行、団体コード、性別、年齢階級、人口・世帯列、秘匿記号、集計行を確認する。総計と日本人住民＋外国人住民、性別計と男女計を検証する。外国人年齢表は秘匿による不一致を許容する。

3. **年次カタログを確定する。** 2014年から最新年までの `statInfId`、表番号、公開日、基準日、URLを公開ページと取得ファイルの両方で確認する。確認済みの行だけ登録し、欠けた年はエラーで停止させる。

4. **取得アダプターを実装する。** 社会・人口統計体系で必要粒度を取得できる場合は既存の e-Stat API アダプターを利用する。ファイル経路が必要な場合は、既存の `fetch_estat_table()` は `statsDataId` 用のまま維持し、住基には `fetch_estat_file(stat_inf_id, ...)` のような別アダプターを設ける。直接ファイルダウンロードに appId が不要かもこの段階で確認する。

5. **年別パーサーを実装する。** まず2025年を実装し、同一スキーマと確認できた年を広げる。形式が変わる年は年別分岐を残し、表番号、列数、必須セル、年、基準日を事前条件として検証する。

6. **共通スキーマと公開APIを実装する。** `get_juki()`、`get_juki_age()` とメタデータ列を追加し、同じリリースで既存国勢調査出力も共通スキーマへ移行する。

7. **キャッシュを実装・検証する。** 一時キャッシュディレクトリを使い、初回取得、再利用、`cache = FALSE`、ファイル破損、ハッシュ変更、スキーマ世代変更、生ファイルからの再構築を検証する。

8. **回帰確認と文書化を行う。** `air format --check R`、`devtools::document()`、`devtools::check()` を実行し、README.Rmd に国勢調査と住基の定義差、基準日、年齢粒度、外国人秘匿、利用可能年を記載して README.md を再生成する。通常CIでは確認済みファイルの不変URLまたは `statInfId` と SHA-256 に基づく固定fixtureを使い、ライブ確認は認証情報を持つ手動・定期ジョブに分離する。

## (e) 残るリスク・不確実性

対象調査に `statsDataId` が存在しないことは公開一覧と独立照会から強く確認されているが、e-Stat APIを認証付きで検索していないため最終的には未確認である。

社会・人口統計体系の市区町村DBが必要な性別 × 5歳階級 × 市区町村の粒度を持つかは未確認である。この確認結果により、API経路とExcel経路のどちらを採るかが変わる。

直接のExcelダウンロードが appId なしで利用できる可能性があるが、バイナリ取得は未確認である。取得可能なら住基実装は estatapi ではなくファイル取得・Excel解析が中心となり、`readxl` 等の新しい依存が必要になる。

2014～2026年の全表について、表番号体系、シート構成、ヘッダー位置、自治体コード形式が不変であることは未確認である。2014年だけ掲載表が13件である理由も未確認である。

2013年以前は基準日変更、外国人区分、表数、オンラインファイル形式が異なる。初期対応を2014年以降に限定する場合も、その理由を明記する必要がある。

外国人住民の小地域年齢値には秘匿があり、総計表と年齢表の単純な整合性検査が成立しない。秘匿を欠測と区別する `value_status` が必要である。

e-Stat が同じ `statInfId` のファイルを訂正・差し替えする可能性がある。取得時のSHA-256と公開日を保存し、変更時は自動上書きではなく明示的な更新処理にする必要がある。
