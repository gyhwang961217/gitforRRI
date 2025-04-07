library(readxl)
library(openxlsx)

# 원본 엑셀 파일 경로
input_file <- file.choose()

# 엑셀 파일의 모든 시트 이름 가져오기
sheet_names <- excel_sheets(input_file)

# 저장할 폴더 지정
output_folder <- "C:/Users/EKR/Desktop/관측공 데이터 분류하기/충남"


# 폴더가 없으면 생성
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# 각 시트를 개별 파일로 저장
for (sheet in sheet_names) {
  # 시트 데이터 불러오기
  data <- read_excel(input_file, sheet = sheet)
  
  # 저장할 파일 경로 설정
  output_file <- file.path(output_folder, paste0(sheet, ".xls"))
  
  # 데이터 저장
  write.xlsx(data, output_file)
}

cat("모든 시트가 개별 파일로 저장되었습니다!")
