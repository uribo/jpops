fetch_estat_table <- function(stats_data_id, appid, ...) {
  estatapi::estat_getStatsData(
    appId = appid,
    statsDataId = stats_data_id,
    ...
  )
}
