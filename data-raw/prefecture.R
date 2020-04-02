##############################
# 都道府県
##############################
library(rvest)
library(readr)
library(dplyr)
library(assertr)

# 01. 北海道 -----------------------------------------------------------------
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
    purrr::set_names(c("city",
                       "household",
                       paste0("population_",
                              c("total", "male", "female")),
                       paste0("前月中の人口移動_",
                              c("増減",
                                paste0("自然動態_",
                                       c("自然増減", "出生", "死亡")),
                                paste0("社会動態",
                                       c("社会増減", "転入", "転出")))))) %>%
    filter(stringr::str_detect(city, "^市町村$", negate = TRUE)) %>%
    type_convert()
}

jpop_pref08 <- function(year, month, ...) {
  url <-
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
# df_pref08 <- jpop_pref08(2020, 3)

name_vars <-
  list(
  city = "市町村",
  household = "世帯",
  population = list("人口" = c("総計", "男", "女")),
  dynamics = list("前月中の人口移動",
                  c("増減",
                    list("自然動態" = c("自然増減", "出生", "死亡")),
                    list("社会動態" = c("社会増減", "転入", "転出")))))


# 13. 東京都 -----------------------------------------------------------------


