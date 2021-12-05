##############################
# 都道府県
# 人口数（総数、男女、国籍）、自然増減率、社会増減率
##############################
library(rvest)
library(readr)
library(dplyr)
library(assertr)
library(readxl)

# 01. 北海道 (CC-BY) -----------------------------------------------------------------
# 北海道オープンデータ (http://www.pref.hokkaido.lg.jp/ss/jsk/opendata.kiyaku.pdf)
# 住民基本台帳人口・世帯数
domain <- "http://www.pref.hokkaido.lg.jp"
# http://www.pref.hokkaido.lg.jp/ss/tuk/900brr/index2.htm
# download.file(
#   "http://www.pref.hokkaido.lg.jp/file.jsp?id=1218305",
#   destfile = "19_t01.xlsx",
# )
# 住基ネットにおける人口
# pref-01_202002.xlsx (2002.xlsx) は 2020年2月の意味

df_pref01_202002 <-
  read_excel("data-raw/pref-01_202002.xlsx", skip = 8)
c(df_pref01_202002[1, ]) %>% dput()

df_pref01_202002 <-
  df_pref01_202002 %>%
  slice(-1L) %>%
  purrr::set_names(c("市区町村",
                     paste0("総数_",
                            c("日本人", "外国人", "計")),
                     paste0("男_",
                            c("日本人", "外国人", "計")),
                     paste0("女_",
                            c("日本人", "外国人", "計"))))

df_pref01_202002 <-
  df_pref01_202002 %>%
  tidyr::pack(total = starts_with("総数"),
              man = starts_with("男"),
              woman = starts_with("女"))

df_pref01_202002 %>%
  select(`市区町村`, man) %>%
  tidyr::unpack(man)


# 02. 青森県 (CC-BY) -----------------------------------------------------------------
domain <- "https://opendata.pref.aomori.lg.jp"

# 青森県の推計人口 令和元年（平成31年）
x <-
  read_html(glue::glue("{domain}/dataset/1372.html"))

df_dlfile <-
  tibble::tibble(
  name = x %>%
    html_nodes(css = "div.name.format-xlsx") %>%
    html_text(trim = TRUE),
  url = glue::glue("{domain}{file}",
                   file = x %>%
                     html_nodes(css = "div.clipboard-wrap > a") %>%
                     html_attr("href")))
dl_file <-
  df_dlfile %>%
  filter(stringr::str_detect(name, "令和元年12月")) %>%
  pull(url)
dest_file <-
  glue::glue("data-raw/pref-02_201912.{type}",
             type = stringr::str_remove(basename(dl_file), ".+\\."))

download.file(
  dl_file,
  destfile = dest_file)

# 市町村
df_pref02_20212 <-
  read_excel(dest_file,
           sheet = 3,
           range = "A3:N34")

df_pref02_20212
# 人口推移


# 03. 岩手県 -----------------------------------------------------------------
# http://www3.pref.iwate.jp/webdb/view/outside/s14Tokei/bnyaBtKekka.html?C=B0203&R=I002
# pref-03_202003.xls (令和2年3月.xls)

# 市町村別人口
df_pref03_202003 <-
  read_excel("data-raw/pref-03_202003.xls",
           sheet = 4+1,
           range = "A3:I37")

df_pref03_202003 %>%
  select(1, 2)
df_pref03_202003 %>%
  select(1, 3, 4, 5)


# 04. 宮城県 -----------------------------------------------------------------
# 月単位での情報は見つからず
# https://www.pref.miyagi.jp/soshiki/toukei/juki-nen.html


# 05. 秋田県 -----------------------------------------------------------------
# https://www.pref.akita.lg.jp/pages/archive/9910


# 06. 山形県 ----------------------------------------------------------------
# https://www.pref.yamagata.jp/020052/kensei/shoukai/toukeijouhou/jinkou/jinkm.html


# 07. 福島県 -----------------------------------------------------------------
# https://www.pref.fukushima.lg.jp/sec/11045b/15847.html


# 08. 茨城県 ------------------------------------------------------------------
# 毎月1日現在の記録
# 平成11年4月から
pref_08_url <- function(year, month) {
  domain <- "https://www.pref.ibaraki.jp"
  glue::glue("{domain}/kikaku/tokei/fukyu/tokei/betsu/jinko/getsu/jinko{yy}{mm}.html",
             yy = stringr::str_sub(year, 1, 2),
             mm = stringr::str_pad(month, width = 2, pad = "0"))
}

tweak_pref08 <- function(obj, index) {
  obj %>%
    purrr::pluck(index) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    select(-2) %>%
    purrr::set_names(c("市町村", # city
                       "世帯", # household
                       paste0("人口_",
                              c("総数", "男", "女")),
                       paste0("前月中の人口移動_",
                              c("増減",
                                paste0("自然動態_",
                                       c("自然増減", "出生", "死亡")),
                                paste0("社会動態",
                                       c("社会増減", "転入", "転出")))))) %>%
    filter(stringr::str_detect(`市町村`, "^市町村$", negate = TRUE)) %>%
    type_convert()
}

jpop_pref08 <- function(year, month, ...) {
  res <-
    pref_08_url(year, month) %>%
    read_html()
  res_tbl <-
    res %>%
    html_nodes(css = "#tmp_contents > table") %>%
    html_table(fill = TRUE)
  c(4, 5) %>%
    purrr::map_dfr(
     ~ res_tbl %>%
       tweak_pref08(index = .x))
}
if (!file.exists("data-raw/pref-08_202003.rds")) {
  df_pref08_202003 <-
    jpop_pref08(2020, 3) %>%
    verify(dim(.) == c(51, 12))
  df_pref08_202003 %>%
    saveRDS("data-raw/pref-08_202003.rds")
} else {
  df_pref08_202003 <-
    readRDS("data-raw/pref-08_202003.rds")
}

# 人口数
df_pref08_202003 %>%
  select(seq.int(5))
# 人口移動
df_pref08_202003 %>%
  select(seq.int(6, ncol(.)))

name_vars <-
  list(
  city = "市町村",
  household = "世帯",
  population = list("人口" = c("総計", "男", "女")),
  dynamics = list("前月中の人口移動",
                  c("増減",
                    list("自然動態" = c("自然増減", "出生", "死亡")),
                    list("社会動態" = c("社会増減", "転入", "転出")))))


# 09. 栃木県 -----------------------------------------------------------------
# https://www.pref.tochigi.lg.jp/c04/pref/toukei/toukei/popu1.html


# 10. 群馬県 -----------------------------------------------------------------
# https://toukei.pref.gunma.jp/idj/


# 11. 埼玉県 -----------------------------------------------------------------
# https://www.pref.saitama.lg.jp/a0206/03suikei/geppou01.html


# 12. 千葉県 -----------------------------------------------------------------
# https://www.pref.chiba.lg.jp/toukei/toukeidata/joujuu/geppou/saishin/setai.html


# 13. 東京都 -----------------------------------------------------------------
# https://www.toukei.metro.tokyo.lg.jp/jsuikei/js-index.htm


# 14. 神奈川県 ----------------------------------------------------------------
# https://www.pref.kanagawa.jp/docs/x6z/tc30/jinko/kohyosiryo.html


# 15. 新潟県 -----------------------------------------------------------------
# https://www.pref.niigata.lg.jp/site/tokei/1356842262748.html


# 16. 富山県 -----------------------------------------------------------------
# https://www.pref.toyama.jp/sections/1015/lib/jinko/index.html


# 33. 岡山県 -----------------------------------------------------------------
# 岡山県毎月流動人口調査 (月報)

# Find a file -------------------------------------------------------------
domain <- "https://www.pref.okayama.jp"
dl_file <-
  read_html(glue::glue("{domain}/page/268956.html")) %>%
  html_nodes(css = "a") %>%
  html_attr("href") %>%
  stringr::str_subset(".xlsx") %>%
  purrr::pluck(1)
dest_file <-
  glue::glue("data-raw/pref-33.{type}",
             type = stringr::str_remove(basename(dl_file), ".+\\."))
download.file(
  glue::glue("{domain}{dl_file}"),
  destfile = dest_file)

# 1/2 人口
df_pref33_raw <-
  read_excel(dest_file, sheet = 1, range = "A3:G7")

df_pref33_raw %>%
  select(1, 2, 3) %>%
  head(2) %>%
  pull(3) %>%
  sum()
# 2/2 年齢階級別移動状況：社会動態
df_pref33_raw2 <-
  read_excel(dest_file, sheet = 1, range = "A12:S26")


# 36.  徳島県 ----------------------------------------------------------------
# 徳島県推計人口
# 手動ダウンロード
# domain <- "https://www.pref.tokushima.lg.jp"
# dl_file <-
#   read_html(glue::glue("/{domain}/statistics/month/jinkou/"))
# dl_file <-
#   dl_file %>%
#   html_nodes(css = "body > div.cms-public > div > div > div.area-group-2-3-4 > div > div > div > div:nth-child(9) > div > ul > li > a") %>%
#   html_attr("href") %>%
#   stringr::str_subset(".xlsx")
# dest_file <-
#   glue::glue("data-raw/pref-38_{file}",
#              file = basename(dl_file))
# download.file(
#   glue::glue("{domain}{dl_file}")[1],
#   destfile = dest_file[1])
jpop_pref36 <- function(path) {
  start_index <-
    readxl::read_excel(path,
                       n_max = 10,
                       col_names = FALSE) %>%
    pull(1) %>%
    stringr::str_which("計$") %>%
    min() -1
  readxl::read_excel(path,
                      skip = start_index,
                      col_names = FALSE) %>%
    filter(!is.na(`...1`)) %>%
    purrr::set_names(c("市町村",
                       paste0("推計人口_",
                              c("総数", "男", "女")),
                       "増減",
                       paste("自然動態",
                             c(rep("出生", 3),
                               rep("死亡", 3)),
                             rep(c("総数", "男", "女"), times = 2),
                             sep = "_"),
                       "自然動態_増減",
                       paste("社会動態",
                             c(rep("転入", 3),
                               rep("転出", 3)),
                             rep(c("総数", "男", "女"), times = 2),
                             sep = "_"),
                       "社会動態_増減",
                       "世帯推計数",
                       paste("世帯の移動",
                             c("増加", "減少", "増減計"),
                             sep = "_"),
                       "1世帯あたりの人員")) %>%
    mutate(date = readxl::read_excel(path,
                                    n_max = 1,
                                    col_names = FALSE) %>%
             pull(1) %>%
             stringr::str_remove_all("[[:space:]]") %>%
             zipangu::convert_jdate()) %>%
    relocate(date, .before = 1)
}

# jpop_pref36("data-raw/pref36_20210101.xlsx")
# jpop_pref36("data-raw/pref36_20210201.xlsx")
# jpop_pref36("data-raw/pref36_20210301.xlsx")
# jpop_pref36("data-raw/pref36_20200101.xls")
# jpop_pref36("data-raw/pref36_20010101.xls")

