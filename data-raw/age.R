##############################
# 年齢構成
# 国勢調査 都道府県・市区町村別の主な結果
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001049104&cycle=0&tclass1=000001049105
##############################

# download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032143614&fileKind=0",
#               destfile = "data-raw/age/major_results_2020.xlsx")
# download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031594311&fileKind=0",
#               destfile = "data-raw/age/major_results_2015.xls")
# download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000012777573&fileKind=0",
#               destfile = "data-raw/age/major_results_2010.xls")
# download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000012777574&fileKind=0",
#               destfile = "data-raw/age/major_results_2010.xls")
# download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000012777575&fileKind=0",
#               destfile = "data-raw/age/major_results_2010.xls")

# c("都道府県名", "都道府県・市区町村名", "都道府県・市区町村名_英語",
#   "市などの別",
#   paste0("総人口", c("総数", "男", "女")),
#   )
readxl::read_xlsx("data-raw/age/major_results_2020.xlsx",
                  skip = 7,
                  col_types = rep("text", 49))
