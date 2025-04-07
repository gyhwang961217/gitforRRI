# 필요한 라이브러리 로드
library(httr)
library(XML)
library(openxlsx)  # 엑셀 파일 저장을 위한 패키지

# API 호출에 필요한 변수 설정
serviceUrl <- "http://apis.data.go.kr/1390802/AgriWeather/WeatherObsrInfo/V2/GnrlWeather"
oper_serve <- "/getWeatherTermDayList"
my_key <- "6r60eSbapWVMrmNJMwktYu%2Bzu4yERJB%2BktSKkJIV9k%2FkmDp9aGEvNOrAGv0PVBCzrHm6%2FQpd%2FXegJXoXSG2Jqg%3D%3D"
obsr_Spot_Code <- "635821A001"
begin_date <- "2023-01-01"
end_date <- "2023-12-31"

pageNo <- 4  # 페이지 번호
rows <- 100   # 한 페이지 결과 수

# URL 생성
url <- paste0(serviceUrl, oper_serve,
              "?serviceKey=", I(my_key),
              "&Page_No=", pageNo,
              "&Page_Size=", rows,
              "&begin_Date=", begin_date,
              "&end_Date=", end_date,
              "&obsr_Spot_Code=", obsr_Spot_Code)

# API 요청
response <- GET(url)

if (status_code(response) == 200) {
  # XML 내용 추출
  xml_content <- content(response, "text", encoding = "UTF-8")
  
  # XML 파싱
  doc <- xmlParse(xml_content)
  rootNode <- xmlRoot(doc)
  
  # 'items' 노드를 추출하여 데이터프레임으로 변환
  items <- getNodeSet(rootNode, "//body/items/item")
  data_list <- lapply(items, function(x) {
    # 각 항목에서 필요한 데이터를 추출
    data.frame(
      no = xmlValue(xmlChildren(x)$no),
      stn_Code = xmlValue(xmlChildren(x)$stn_Code),
      stn_Name = xmlValue(xmlChildren(x)$stn_Name),
      date = xmlValue(xmlChildren(x)$date),
      temp = xmlValue(xmlChildren(x)$temp),
      max_Temp = xmlValue(xmlChildren(x)$max_Temp),
      min_Temp = xmlValue(xmlChildren(x)$min_Temp),
      hum = xmlValue(xmlChildren(x)$hum),
      wind = xmlValue(xmlChildren(x)$wind),
      rain = xmlValue(xmlChildren(x)$rain),
      soilwt = xmlValue(xmlChildren(x)$soil_Wt)
    )
  })
  
  # 리스트를 데이터프레임으로 변환
  weather_data <- do.call(rbind, data_list)
  
  # 엑셀로 저장
  write.xlsx(weather_data, "weather_data4.xlsx")
  
  cat("엑셀 파일로 저장되었습니다.\n")
} else {
  cat("API 요청 실패. 상태 코드:", status_code(response), "\n")
}
