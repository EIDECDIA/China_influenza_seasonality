#install.packages("geofacet")
#library("geofacet")


#grid_preview("china_prov_grid2")
#grid_design(data = china_prov_grid2) #link to website 

CHN_GF <- data.frame(
  code = c("HLJ", "NM", "JL", "BJ", "LN", "XJ", "GS", "SHX", "HEB", "TJ", "XZ", "QH", "NX", "SAX", "HEN", "SD", "SC", "CQ", "HUB", "AH", "JS", "SH", "YN", "GZ", "HUN", "JX", "ZJ", "GX", "GD", "FJ", "HAN"),
  row = c(1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 10),
  col = c(7, 6, 7, 6, 7, 1, 2, 4, 5, 6, 1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 4, 5, 6, 5),
  name = c("Heilongjiang", "Neimenggu", "Jilin", "Beijing", "Liaoning", "Xinjiang", "Gansu", "Shanxi", "Hebei", "Tianjin", "	Xizang", "Qinghai", "Ningxia", "Shaanxi", "Henan", "Shandong", "Sichuan", "Chongqing", "Hubei", "Anhui", "Jiangsu", "Shanghai", "Yunnan", "Guizhou", "Hunan", "Jiangxi", "Zhejiang", "Guangxi", "Guangdong", "Fujian", "Hainan"),
  stringsAsFactors = FALSE)



