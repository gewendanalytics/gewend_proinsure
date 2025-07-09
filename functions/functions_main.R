# 
# PACKAGES ----
packages <- c(
  "quarto", "shiny", "shinyWidgets", "shinyjs", "tidyverse", "tidyquant", "htmltools",
  "shinythemes", "shinydashboard", "shinyBS", "reactable", "formattable",
  "plotly", "ggplot2", "ggpubr", "viridis", "hrbrthemes", "scales",
  "shinyalert", "reactable", "formattable", "DT", "kableExtra",
  "writexl", "readxl", "collapsibleTree", "highcharter", "lubridate",
  "bslib", "bsicons", "sf", "mapview", "leaflet", "modelr", "caret",
  "billboarder", "collapsibleTree", "echarts4r", "shinyMatrix", "glue", "gemini.R",
  "DT", "rhandsontable", "bslib", "visNetwork", "lpSolve", "geosphere", "osrm", "leaflet.extras", "colorspace","RColorBrewer",
  "modelr", "caret", "billboarder", "collapsibleTree", "echarts4r", "shinyMatrix", "shinycssloaders", "shinyauthr"
)

# Load packages
lapply(packages, library, character.only = TRUE)
library(collapsibleTree)

Sys.setlocale(locale = "Turkish")




# FONKSİYONLAR ---- 
## ORTAK VERİ TABLOSU FUNC.----

ortak_veri_tablosu <- function(data, yasam_tablosu) {
  
  Asgari_Tablo <- read_excel("data/Asgari_Ucret_Tablosu_rvz.xlsx", sheet = "Program")
  # Teminat_Limit_Tablosu <- read_excel("../gewend_proinsure/data/Teminat_Limit_Tablosu_rvz.xlsx", sheet = "Teminat")
  
  Teminat_Limit_Tablosu <- tibble::tribble(
    ~Donem, ~Teminat_Limiti,
    
    "2010-01-01/2010-12-31", 150000L,
    "2011-01-01/2011-06-30", 200000L,
    "2011-07-01/2011-12-31", 200000L,
    "2012-01-01/2012-06-30", 225000L,
    "2012-07-01/2012-12-31", 225000L,
    "2013-01-01/2013-06-30", 250000L,
    "2013-07-01/2013-12-31", 250000L,
    "2014-01-01/2014-06-30", 268000L,
    "2014-07-01/2014-12-31", 268000L,
    "2015-01-01/2015-06-30", 290000L,
    "2015-07-01/2015-12-31", 290000L,
    "2016-01-01/2016-12-31", 310000L,
    "2017-01-01/2017-12-31", 330000L,
    "2018-01-01/2018-12-31", 360000L,
    "2019-01-01/2019-06-30", 360000L,
    "2019-07-01/2019-12-31", 390000L,
    "2020-01-01/2020-12-31", 410000L,
    "2021-01-01/2021-12-31", 430000L,
    "2022-01-01/2022-06-30", 500000L,
    "2022-07-01/2022-12-31", 1000000L,
    "2023-01-01/2023-12-31", 1200000L,
    "2024-01-01/2024-12-31", 5000000L,
    "2025-01-01/2025-12-31", 9000000L
  )
  
  
  dosya_bilgileri <- data 

  
  # Genel Bilgiler 
  Teknik_Faiz <- 0
  pasif_donem_yas <- 60
  faiz_oranı <- 9
  faiz_oranı2 <- 24
  
  ## Manuel Gelir Tablosu
  
  gelir_tablosu <- 
    
    tibble::tribble(
      ~Donem, ~Gelir,
      
      "2005-01-01/2005-12-31", 100L,
      "2006-01-01/2006-12-31", 100L,
      "2007-01-01/2007-06-30", 100L,
      "2007-07-01/2007-12-31", 100L,
      "2008-01-01/2008-06-30", 100L,
      "2008-07-01/2008-12-31", 100L,
      "2009-01-01/2009-06-30", 100L,
      "2009-07-01/2009-12-31", 100L,
      "2010-01-01/2010-06-30", 100L,
      "2010-07-01/2010-12-31", 100L,
      "2011-01-01/2011-06-30", 100L,
      "2011-07-01/2011-12-31", 100L,
      "2012-01-01/2012-06-30", 100L,
      "2012-07-01/2012-12-31", 100L,
      "2013-01-01/2013-06-30", 100L,
      "2013-07-01/2013-12-31", 100L,
      "2014-01-01/2014-06-30", 100L,
      "2014-07-01/2014-12-31", 100L,
      "2015-01-01/2015-06-30", 100L,
      "2015-07-01/2015-12-31", 100L,
      "2016-01-01/2016-12-31", 100L,
      "2017-01-01/2017-12-31", 100L,
      "2018-01-01/2018-12-31", 100L,
      "2019-01-01/2019-12-31", 100L,
      "2020-01-01/2020-12-31", 100L,
      "2021-01-01/2021-12-31", 100L,
      "2022-01-01/2022-06-30", 100L,
      "2022-07-01/2022-12-31", 100L,
      "2023-01-01/2023-06-30", 100L,
      "2023-07-01/2023-12-31", 100L,
      "2024-01-01/2024-12-31", 100L,
      "2025-01-01/2025-12-31", 100L
      
    )
  
  gelir_tablosu <- gelir_tablosu %>% 
    mutate(Gelir = as.numeric(Gelir))
  
  ## Kişisel Bilgiler
  
  dosya_no <- as.character(dosya_bilgileri$Dosya_No)
  rapor_turu <- dosya_bilgileri$Rapor_Turu
  Ad_Soyad <- dosya_bilgileri$Ad_Soyad
  Cinsiyet <- dosya_bilgileri$Cinsiyet
  Dogum_Tarihi <- as.Date(dosya_bilgileri$Dogum_Tarihi)
  Gelir_Durumu <- dosya_bilgileri$Gelir
  Kaza_Tarihi <- as.Date(dosya_bilgileri$Kaza_Tarihi)
  Maluliyet_Oranı <- dosya_bilgileri$Maluliyet_Orani
  Kusur_Oranı <- dosya_bilgileri$Kusur_Orani
  Gecici_Maluliyet_sure <- dosya_bilgileri$Gecici_Maluliyet_sure
  Kısmi_Odeme_Sayısı <- dosya_bilgileri$Kısmi_Odeme_Sayısı
  Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_1)
  Kısmi_Odeme_Tutarı_1 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_1
  Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_2)
  Kısmi_Odeme_Tutarı_2 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_2
  Yasam_Tablosu <- yasam_tablosu
  Bakici_Gideri_Suresi_gun <- dosya_bilgileri$`Bakıcı_Gideri_Süresi_(gun)`
  
  # Genel Gelir Tablosu
  
  Gelir_tablo <- suppressMessages({
    Asgari_Tablo %>% 
      left_join(gelir_tablosu) %>%
      separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
      mutate(D_B = as.Date(Donem_Baslangic), 
             D_S = as.Date(Donem_Son))
  })
  
  # Kaza Tarihi Teminat Limiti
  
  Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic), 
           D_S = as.Date(Donem_Son))
  
  kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
    filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)
  
  kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE)) 
  
  # YAS HESAPLAMALARI ----
  
  Hesap_Tarihi <- Sys.Date()
  Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1
  
  kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
  hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 
  
  sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 
  
  
  # HESAPLAMADA KULLANILACAK TRH TABLOSU ----
  
  ## Hesaplama Tarihine göre beklenen ömür ----
  
  PR_TRH_2010 <- if (Yasam_Tablosu == "TRH-2010") {
    read_excel("data/All_Tables.xlsx", sheet = "TRH-2010")
  } else if  (Yasam_Tablosu == "TUIK_20-22") {
    read_excel("data/All_Tables.xlsx", sheet = "TUIK_20-22")
  } else if (Yasam_Tablosu == "TUIK_19-21") {
    read_excel("data/All_Tables.xlsx", sheet = "TUIK_19-21")
  } else if (Yasam_Tablosu == "TUIK_18-20") {
    read_excel("data/All_Tables.xlsx", sheet = "TUIK_18-20")
  } else {
    read_excel("data/All_Tables.xlsx", sheet = "PMF-1931")
  }
  
  
  erkek_table <- PR_TRH_2010 %>% 
    select(Yas, Erkek)
  
  kadın_table <- PR_TRH_2010 %>% 
    select(Yas, Kadın) 
  
  
  hesaplama_tarihi_PR_TRH <- if (Cinsiyet == "Erkek") {
    filtered_PR_TRH <- erkek_table %>% 
      filter(Yas == hesap_tarihi_yas) %>% 
      select(Erkek)
  } else {
    filtered_PR_TRH <- kadın_table %>% 
      filter(Yas == hesap_tarihi_yas) %>% 
      select(Kadın)
  }
  
  
  hesaplama_tarihi_beklenen_omur <- ifelse(Cinsiyet == "Erkek", hesaplama_tarihi_PR_TRH$Erkek, hesaplama_tarihi_PR_TRH$Kadın)
  hesaplama_tarihi_beklenen_omur <- round(hesaplama_tarihi_beklenen_omur,digits = 2)
  
  
  ## Şirket Ödeme Tarihine göre beklenen ömür ----
  
  sirket_odeme_tarihi_PR_TRH <- if (Cinsiyet == "Erkek") {
    filtered_PR_TRH <- erkek_table %>% 
      filter(Yas == sirket_odeme_tarihi_yas) %>% 
      select(Erkek)
  } else {
    filtered_PR_TRH <- kadın_table %>% 
      filter(Yas == sirket_odeme_tarihi_yas) %>% 
      select(Kadın)
  }
  
  
  sirket_odeme_beklenen_omur <- ifelse(Cinsiyet == "Erkek", sirket_odeme_tarihi_PR_TRH$Erkek, sirket_odeme_tarihi_PR_TRH$Kadın)
  sirket_odeme_beklenen_omur <- round(sirket_odeme_beklenen_omur,digits = 2)
  
  
  
  ## Pasif Dönem Yas- TRH Tablosu ----
  
  hesaplama_tarihi_PR_TRH_pasif <- 
    if (Cinsiyet == "Erkek") {
      erkek_table %>% 
        filter(Yas == pasif_donem_yas)
    } else {
      kadın_table %>% 
        filter(Yas == pasif_donem_yas)
    }
  
  
  hesap_tarihi_pasif_beklenen_omur <- ifelse(Cinsiyet == "Erkek", hesaplama_tarihi_PR_TRH_pasif$Erkek, hesaplama_tarihi_PR_TRH_pasif$Kadın)
  hesap_tarihi_pasif_beklenen_omur <- round(hesap_tarihi_pasif_beklenen_omur,digits = 2)
  
  
  # PARAMATRE TABLOSU ----
  
  
  parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Rapor_Turu", "Ad-Soyad", "Kaza Tarihi Yas", "Hesap Tarihi Yas", "Cinsiyet",  "Gelir_Durumu",  "Kaza_Tarihi", "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)", "Kısmi_Odeme_Sayısı", "Ödeme Tarihi-1", "Ödeme Tutarı-1","Ödeme Tarihi-2", "Ödeme Tutarı-2", "Bakıcı Gideri Süresi (gün)"),
                                  DEĞER = c(dosya_no, rapor_turu, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet, Gelir_Durumu, as.character(Kaza_Tarihi), 
                                            Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure, Kısmi_Odeme_Sayısı, Kısmi_Odeme_Tarihi_1, Kısmi_Odeme_Tutarı_1, Kısmi_Odeme_Tarihi_2, Kısmi_Odeme_Tutarı_2, Bakici_Gideri_Suresi_gun)
  )
  
  parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")
  
  parametre_tablosu2 <- tibble(
    "Dosya_No" = dosya_no,
    "Rapor_Turu" = rapor_turu,
    "Ad-Soyad" = Ad_Soyad,
    "Yas" = kaza_tarihi_yas,
    "Hesap Tarihi Yas" = hesap_tarihi_yas,
    "Cinsiyet" = Cinsiyet, 
    "Gelir_Durumu" =  Gelir_Durumu,
    "Kaza Tarihi" = as.character(Kaza_Tarihi),
    "Maluliyet Oranı" = Maluliyet_Oranı,
    "Teknik Faiz" = Teknik_Faiz,
    "Yaşam Tablosu" = Yasam_Tablosu,
    "Kusur Oranı" = Kusur_Oranı,
    "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure,
    "Kısmi_Odeme_Sayısı" = Kısmi_Odeme_Sayısı,
    "Ödeme Tarihi-1" = Kısmi_Odeme_Tarihi_1,
    "Ödeme Tutarı-1" = Kısmi_Odeme_Tutarı_1,
    "Ödeme Tarihi-2" = Kısmi_Odeme_Tarihi_2,
    "Ödeme Tutarı-2" = Kısmi_Odeme_Tutarı_2,
    "Bakıcı Gideri Süresi (gün)" <- Bakici_Gideri_Suresi_gun
  ) |>
    select(where(~ all(!is.na(.))))
  
  
  
  return(list(Dosya_No = dosya_no,
              rapor_turu = rapor_turu,
              yasam_tablosu = Yasam_Tablosu,
              parametre_tablosu = parametre_tablosu, 
              parametre_tablosu2 = parametre_tablosu2, 
              Hesap_Tarihi= Hesap_Tarihi, 
              Kaza_Tarihi= Kaza_Tarihi, 
              Maluliyet_Oranı= Maluliyet_Oranı, 
              Kusur_Oranı = Kusur_Oranı,
              Hesap_Tarihi_Sirket = Hesap_Tarihi_Sirket,
              Gelir_Durumu = Gelir_Durumu,
              Gelir_tablo = Gelir_tablo, 
              Gecici_Maluliyet_sure = Gecici_Maluliyet_sure,
              kaza_tarihi_yas = kaza_tarihi_yas, 
              hesap_tarihi_yas= hesap_tarihi_yas, 
              Hesap_Tarihi_Sirket=Hesap_Tarihi_Sirket,
              sirket_odeme_tarihi_yas = sirket_odeme_tarihi_yas, 
              kaza_tarihi_teminat_limiti = kaza_tarihi_teminat_limiti, 
              hesaplama_tarihi_beklenen_omur = hesaplama_tarihi_beklenen_omur, 
              hesap_tarihi_pasif_beklenen_omur= hesap_tarihi_pasif_beklenen_omur,
              sirket_odeme_beklenen_omur= sirket_odeme_beklenen_omur,
              pasif_donem_yas = pasif_donem_yas,
              Bakici_Gideri_Suresi_gun = Bakici_Gideri_Suresi_gun,
              Odeme_Tarihi_1 = Kısmi_Odeme_Tarihi_1,
              Odeme_Tutarı_1 = Kısmi_Odeme_Tutarı_1,
              Odeme_Tarihi_2 = Kısmi_Odeme_Tarihi_2,
              Odeme_Tutarı_2 = Kısmi_Odeme_Tutarı_2,
              faiz_oranı = faiz_oranı,
              faiz_oranı2 = faiz_oranı2
  )
  )
  
}



# ////////////////////////////////////////////////////////////////////////////////


# HESAPLAMALAR ----

## HESAP TARİHİ TAM MALULİYET DONEMİ TABLOSU ----

HT_Tam_Maluliyet_Donemi_Tablosu <- function(ortak_tablo = ortak_veri_tbl){
  
  maluliyet_tarihi_baslangıc <- ortak_tablo$Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (ortak_tablo$Gecici_Maluliyet_sure * 30)
  
  HT_Maluliyet_Donemi_tablosu <- ortak_tablo$Gelir_tablo %>% 
    filter(D_S >= maluliyet_tarihi_baslangıc & maluliyet_tarihi_bitis > D_B)
  
  HT_Maluliyet_Donemi_tablosu$Donem_Baslangic[1] <- as.character(maluliyet_tarihi_baslangıc)
  HT_Maluliyet_Donemi_tablosu$Donem_Son[nrow(HT_Maluliyet_Donemi_tablosu)] <- as.character(maluliyet_tarihi_bitis)
  
  HT_Maluliyet_Donemi_tazminat_hesaplama <- HT_Maluliyet_Donemi_tablosu %>% 
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>% 
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = 100) %>% 
    mutate(Kusur_Oranı = ortak_tablo$Kusur_Oranı) %>% 
    select(ortak_tablo$Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>%
    rename("Gelir" = ortak_tablo$Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  HT_Tam_Maluliyet_Donemi_Tablosu
  
}




## HESAP TARİHİ TAM MALULİYET SONRASI DONEM TABLOSU ----

HT_Maluliyet_Sonrasi_Donem_Tablosu <- function(ortak_tablo = ortak_veri_tbl){
  
  maluliyet_tarihi_baslangıc <- ortak_tablo$Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (ortak_tablo$Gecici_Maluliyet_sure * 30)
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu <- ortak_tablo$Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= ortak_tablo$Hesap_Tarihi + 365)
  
  hesaplama_tarihi_chr <- as.character(ortak_tablo$Hesap_Tarihi)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(HT_Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_chr
  
  
  HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- HT_Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    # 
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = ortak_tablo$Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = ortak_tablo$Kusur_Oranı) %>% 
    select(ortak_tablo$Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>%
    rename("Gelir" = ortak_tablo$Gelir_Durumu) %>%
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu
  
}




## HESAP TARİHİ BİLİNEN DÖNEM TABLOSU ----

HT_Bilinen_Donem_Tablosu <- function(data = ortak_veri_tbl){
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu(data)
  
}



## HESAP TARİHİ BİLİNMEYEN DÖNEM TABLOSU ----

HT_Bilinmeyen_Donem_Tablosu <- function(ortak_tablo = ortak_veri_tbl){
  
  ##### Hesap Tarihi Aktif Donem ----          
  
  ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu()$Gelir[nrow(HT_Bilinen_Donem_Tablosu())]
  
  ht_bilinmeyen_aktif_donem_tazminat <- (ortak_tablo$pasif_donem_yas - ortak_tablo$hesap_tarihi_yas) * ht_bilinen_son_donem_gelir * 12 * ortak_tablo$Maluliyet_Oranı/100 * ortak_tablo$Kusur_Oranı/100
  
  ht_bilinmeyen_aktif_donem_tazminat <- round(ht_bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                     'Toplam Yıl' = (ortak_tablo$pasif_donem_yas - ortak_tablo$hesap_tarihi_yas),
                                                                     'Toplam Ay' = (ortak_tablo$pasif_donem_yas - ortak_tablo$hesap_tarihi_yas) * 12,
                                                                     Gelir = ht_bilinen_son_donem_gelir,
                                                                     'Maluliyet Oranı' = ortak_tablo$Maluliyet_Oranı,
                                                                     'Kusur Oranı' = ortak_tablo$Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_aktif_donem_tazminat)
  
  
  ##### Hesap Tarihi Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_ht <- ortak_tablo$Gelir_tablo %>% filter (D_S >= ortak_tablo$Hesap_Tarihi & ortak_tablo$Hesap_Tarihi > D_B)
  ht_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_ht$Bekar
  
  ht_pasif_donem_beklenen_omur <- 
    
    if (ortak_tablo$hesap_tarihi_yas <= ortak_tablo$pasif_donem_yas) {
      
      (ortak_tablo$hesap_tarihi_yas + ortak_tablo$hesaplama_tarihi_beklenen_omur - ortak_tablo$pasif_donem_yas) 
      
    } else {
      
      ortak_tablo$hesaplama_tarihi_beklenen_omur
    }
  
  
  ht_bilinmeyen_pasif_donem_tazminat <- ht_pasif_donem_beklenen_omur * ht_pasif_donem_geliri * 12 * ortak_tablo$Maluliyet_Oranı/100 * ortak_tablo$Kusur_Oranı/100
  
  
  hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                     'Toplam Yıl' = ht_pasif_donem_beklenen_omur,
                                                                     'Toplam Ay' = ht_pasif_donem_beklenen_omur*12,
                                                                     Gelir = ht_pasif_donem_geliri,
                                                                     'Maluliyet Oranı' = ortak_tablo$Maluliyet_Oranı,
                                                                     'Kusur Oranı' = ortak_tablo$Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_pasif_donem_tazminat)
  
  
  ##### Hesap Tarihi Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                              hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(ortak_tablo$hesap_tarihi_yas > ortak_tablo$pasif_donem_yas) {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
  
}



## TOPLAM TAZMİNAT ----

toplam_tazminat_func <- function(data = ortak_veri_tbl){
  
  ### Toplam Tazminat Tablosu ----
  
  HT_Toplam_Tazminat_Tablosu <- function(data_tbl = data){
    
    hesap_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", 
                                                                 "Bilinmeyen Dönem Tazminatı", 
                                                                 "Toplam"
    ),
    Tutar = c(sum(HT_Bilinen_Donem_Tablosu(data)$Donem_Tazminat), 
              sum(HT_Bilinmeyen_Donem_Tablosu(data)$Toplam), 
              (sum(HT_Bilinen_Donem_Tablosu(data)$Donem_Tazminat) + sum(HT_Bilinmeyen_Donem_Tablosu(data)$Toplam))
    )
    )
    
    hesap_tarihi_toplam_tazminat_tablosu
  }
  
  
  ### Toplam Tazminat Tutarı ----
  
  Toplam_Tazminat_Tutarı <- function(data_tbl = data){
    HT_Toplam_Tazminat_Tablosu(data)[HT_Toplam_Tazminat_Tablosu(data)$Dönem == "Toplam", "Tutar"]
  }
  
  
  
  return(list(tablo =  HT_Toplam_Tazminat_Tablosu(ortak_veri_tbl),
              tutar =   Toplam_Tazminat_Tutarı(ortak_veri_tbl)
  )
  )
  
}



## BAKİYE TAZMİNAT ----

Bakiye_Tazminat <- function(data = ortak_veri_tbl){
  
  bakiye_tazminat_tutarı <- if(toplam_tazminat_func(data)$tutar >= as.numeric(data$kaza_tarihi_teminat_limiti)) {
    as.numeric(data$kaza_tarihi_teminat_limiti)
  } else {
    toplam_tazminat_func(data)$tutar
  }
  
  bakiye_tazminat_tutarı
  
}



## BAKICI GİDERİ ----

bakici_gideri_func <- function(data = ortak_veri_tbl){
  
  
  ### Bakıcı Gİderi Tablosu ----
  
  bakici_gideri_tablosu <- function(ortak_tablo = data){
    
    Bakici_Gideri_Baslangic_Tarihi <- data$Kaza_Tarihi
    Bakici_Gideri_Bitis_Tarihi <- data$Kaza_Tarihi + data$Bakici_Gideri_Suresi_gun
    
    
    Bakici_Gideri_Donemi_filtered <- data$Gelir_tablo %>%
      filter(D_S >= Bakici_Gideri_Baslangic_Tarihi & Bakici_Gideri_Bitis_Tarihi > D_B)
    
    bakici_gideri_baslangic_tarihi_chr <- as.character(Bakici_Gideri_Baslangic_Tarihi)
    bakici_gideri_bitis_tarihi_chr <- as.character(Bakici_Gideri_Bitis_Tarihi)
    
    Bakici_Gideri_Donemi_filtered$Donem_Baslangic[1] <- bakici_gideri_baslangic_tarihi_chr
    Bakici_Gideri_Donemi_filtered$Donem_Son[nrow(Bakici_Gideri_Donemi_filtered)] <- bakici_gideri_bitis_tarihi_chr
    
    Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu <- Bakici_Gideri_Donemi_filtered %>% 
      
      mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
             Donem_Sonu = as.Date(Donem_Son)) %>%
      mutate(Kazanilan_Gun = signif(as.numeric(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Baslangici), "day")) , digits = 2)) %>%
      
      mutate(Kusur_Oranı = data$Kusur_Oranı) %>% 
      select(Donem_Baslangici, Donem_Son, Kazanilan_Gun, Brut, Kusur_Oranı) %>% 
      rename("Aylık_Bakıcı_Gideri" = Brut) %>% 
      mutate(Donem_Bakıcı_Gideri = round(Kazanilan_Gun * Aylık_Bakıcı_Gideri/30 * Kusur_Oranı/100, digits = 2))  
    
    Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu
    
  }
  
  
  ### Bakıcı Gİderi Tutarı ----
  
  bakici_gideri_tutarı <- function(data = data){
    
    bakici_gideri_tablosu(data)$Donem_Bakıcı_Gideri
    
  }
  
  
  
  return(list(tablo =  bakici_gideri_tablosu(data),
              tutar =   bakici_gideri_tutarı(data)
  )
  )
  
  
}



## ŞİRKET ÖDEME TARİHİ İLE HESAPLAMA ----
### Bilinen Dönem Hesaplaması ----

#### Tam Maluliyet Donemi Tablosu ----

SOT_Tam_Maluliyet_Donemi_Tablosu <- function(ortak_tablo = ortak_veri_tbl){
  
  maluliyet_tarihi_baslangıc <- ortak_tablo$Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (ortak_tablo$Gecici_Maluliyet_sure * 30)
  
  SOT_Maluliyet_Donemi_tablosu <- ortak_tablo$Gelir_tablo %>% 
    filter(D_S >= maluliyet_tarihi_baslangıc & maluliyet_tarihi_bitis > D_B)
  
  SOT_Maluliyet_Donemi_tablosu$Donem_Baslangic[1] <- as.character(maluliyet_tarihi_baslangıc)
  SOT_Maluliyet_Donemi_tablosu$Donem_Son[nrow(SOT_Maluliyet_Donemi_tablosu)] <- as.character(maluliyet_tarihi_bitis)
  
  SOT_Maluliyet_Donemi_tazminat_hesaplama <- SOT_Maluliyet_Donemi_tablosu %>% 
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>% 
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = 100) %>% 
    mutate(Kusur_Oranı = ortak_tablo$Kusur_Oranı) %>% 
    select(ortak_tablo$Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>%
    rename("Gelir" = ortak_tablo$Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  SOT_Tam_Maluliyet_Donemi_Tablosu <- SOT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  SOT_Tam_Maluliyet_Donemi_Tablosu
  
}




#### Tam Maluliyet Sonrası Dönem Tablosu ----

SOT_Maluliyet_Sonrasi_Donem_Tablosu <- function(ortak_tablo = ortak_veri_tbl){
  
  maluliyet_tarihi_baslangıc <- ortak_tablo$Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (ortak_tablo$Gecici_Maluliyet_sure * 30)
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  
  SOT_Maluliyet_Sonrasi_Donem_tablosu <- ortak_tablo$Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= ortak_tablo$Hesap_Tarihi_Sirket + 180)
  
  hesaplama_tarihi_chr <- as.character(ortak_tablo$Hesap_Tarihi_Sirket)
  
  
  SOT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  SOT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(SOT_Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_chr
  
  
  SOT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- SOT_Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    # 
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = ortak_tablo$Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = ortak_tablo$Kusur_Oranı) %>% 
    select(ortak_tablo$Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>%
    rename("Gelir" = ortak_tablo$Gelir_Durumu) %>%
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  SOT_Maluliyet_Sonrasi_Donem_Tablosu <- SOT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  SOT_Maluliyet_Sonrasi_Donem_Tablosu
  
}





#### Bilinen Dönem Tablosu ----

SOT_Bilinen_Donem_Tablosu <- function(data = ortak_veri_tbl){
  
  SOT_Maluliyet_Sonrasi_Donem_Tablosu(data)
  
}



### Bilinmeyen Dönem Hesaplaması ----

SOT_Bilinmeyen_Donem_Tablosu <- function(data = ortak_veri_tbl){
  
  ##### Şirket Ödeme Tarihi Aktif Donem ----          
  
  sot_bilinen_son_donem_gelir <- SOT_Bilinen_Donem_Tablosu(data)$Gelir[nrow(SOT_Bilinen_Donem_Tablosu())]
  
  sot_bilinmeyen_aktif_donem_tazminat <- (data$pasif_donem_yas - data$sirket_odeme_tarihi_yas) * sot_bilinen_son_donem_gelir * 12 * data$Maluliyet_Oranı/100 * data$Kusur_Oranı/100
  
  sot_bilinmeyen_aktif_donem_tazminat <- round(sot_bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                            'Toplam Yıl' = (data$pasif_donem_yas - data$sirket_odeme_tarihi_yas),
                                                                            'Toplam Ay' = (data$pasif_donem_yas - data$sirket_odeme_tarihi_yas) * 12,
                                                                            Gelir = sot_bilinen_son_donem_gelir,
                                                                            'Maluliyet Oranı' = data$Maluliyet_Oranı,
                                                                            'Kusur Oranı' = data$Kusur_Oranı,
                                                                            Toplam = sot_bilinmeyen_aktif_donem_tazminat)
  
  
  ##### Şirket Ödeme Tarihi Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_sot <- data$Gelir_tablo %>% filter (D_S >= data$Hesap_Tarihi_Sirket & data$Hesap_Tarihi_Sirket > D_B)
  
  sot_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_sot$Bekar
  
  sot_pasif_donem_beklenen_omur <- 
    
    if (data$sirket_odeme_tarihi_yas <= data$pasif_donem_yas) {
      
      (data$hesap_tarihi_yas + data$sirket_odeme_beklenen_omur - data$pasif_donem_yas) 
      
    } else {
      
      data$hesaplama_tarihi_beklenen_omur
    }
  
  
  sot_bilinmeyen_pasif_donem_tazminat <- sot_pasif_donem_beklenen_omur * sot_pasif_donem_geliri * 12 * data$Maluliyet_Oranı/100 * data$Kusur_Oranı/100
  
  
  sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                            'Toplam Yıl' = sot_pasif_donem_beklenen_omur,
                                                                            'Toplam Ay' = sot_pasif_donem_beklenen_omur*12,
                                                                            Gelir = sot_pasif_donem_geliri,
                                                                            'Maluliyet Oranı' = data$Maluliyet_Oranı,
                                                                            'Kusur Oranı' = data$Kusur_Oranı,
                                                                            Toplam = sot_bilinmeyen_pasif_donem_tazminat)
  
  
  ##### Şirket Ödeme Tarihi Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu  <- bind_rows(sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                                      sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu  <- if(data$hesap_tarihi_yas > data$pasif_donem_yas) {
    sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu
  
}





### KISMİ ÖDEME ----

sirket_odeme_hesap_tarih_degeri_hesaplama <- function(ortak_tablo = ortak_veri_tbl){
  
  # Şirket Ödemesinin Hesap Tarihli Değeri 
  
  odeme1_gecen_gun_9faiz <- as.numeric(as.Date(dmy("01.06.2024")) - ortak_tablo$Odeme_Tarihi_1)
  
  odeme1_gecen_gun_24faiz <- as.numeric(as.Date(ortak_tablo$Hesap_Tarihi) - as.Date(dmy("01.06.2024")))
  
  #
  
  donemde_elde_edilen_faiz9 <- (ortak_tablo$faiz_oranı/100) * (odeme1_gecen_gun_9faiz/365)
  donemde_elde_edilen_faiz24 <- (ortak_tablo$faiz_oranı2/100) * (odeme1_gecen_gun_24faiz/365)
  
  Sirket_Odeme_Tablosu_9faiz <- tibble(
    "Ödeme Tarihi" = as.character(format(ymd(ortak_tablo$Odeme_Tarihi_1),"%d/%m/%Y")), 
    "Hesaplama Tarihi" = as.character(format(dmy("30.05.2024"),"%d/%m/%Y")),
    "Geçen Gün Sayısı" = odeme1_gecen_gun_9faiz,
    "Faiz Oranı" = scales::percent(ortak_tablo$faiz_oranı, scale = 1),
    "Donemde Elde Edilen Faiz" = scales::percent(donemde_elde_edilen_faiz9, accuracy = 0.0001)
  )
  
  Sirket_Odeme_Tablosu_24faiz <- tibble(
    "Ödeme Tarihi" = as.character(format(dmy("01.06.2024"),"%d/%m/%Y")), 
    "Hesaplama Tarihi" = as.character(format(ymd(ortak_tablo$Hesap_Tarihi),"%d/%m/%Y")),
    "Geçen Gün Sayısı" = odeme1_gecen_gun_24faiz,
    "Faiz Oranı" = scales::percent(ortak_tablo$faiz_oranı2, scale = 1),
    "Donemde Elde Edilen Faiz" = scales::percent(donemde_elde_edilen_faiz24, accuracy = 0.0001)
  )
  
  
  Sirket_Odeme_Tablosu <- bind_rows(Sirket_Odeme_Tablosu_9faiz, Sirket_Odeme_Tablosu_24faiz)
  
  toplam_faiz <- donemde_elde_edilen_faiz9 + donemde_elde_edilen_faiz24
  
  kismi_odeme_ht <- (1+toplam_faiz) * ortak_tablo$Odeme_Tutarı_1
  
  return(list(Sirket_Odeme_Tablosu = Sirket_Odeme_Tablosu,
              toplam_faiz = toplam_faiz, 
              kısmi_odeme = ortak_tablo$Odeme_Tutarı_1,
              kismi_odeme_ht = kismi_odeme_ht
  )
  )
  
}




### Şirket Ödeme Faiz Tablosu ----

sirket_odeme_faiz_tablosu <- function(data = ortak_veri_tbl){
  
  sirket_odeme_hesap_tarih_degeri_hesaplama(data)$Sirket_Odeme_Tablosu
  
}



### Şirket Ödeme Hesap Tarihi Tutarı ----

sirket_odeme_ht_tutarı <- function(data = ortak_veri_tbl){
  
  sirket_odeme_hesap_tarih_degeri_hesaplama()$kismi_odeme_ht
  
}





# ////////////////////////////////////////////////////////////////////////////////



## TEKNİK FAİZ HESAPLAMASI ----


teknik_faizli_surekli_tazminat_func <- function(data = ortak_veri_tbl, teknik_faiz = 0.1){
  
  
  ### Bilinmeyen Dönem Hesaplaması ----
  
  teknik_faiz_tablo <- read_excel("../KURUMSAL_PROINSURE/data/teknik_faiz_tablolar.xlsx", sheet = "Sayfa1")
  
  # Calculate mortality table columns
  
  teknik_faiz_tablo <- teknik_faiz_tablo %>%
    mutate(
      dx = c(-diff(lx), 0),
      qx = round(dx / lx, 6),
      Cx = round(dx / (1 + teknik_faiz)^(Yaş + 1), 0),
      Dx = round(lx / (1 + teknik_faiz)^Yaş, 0),
      Nx = rev(cumsum(rev(Dx))),
      Mx = rev(cumsum(rev(Cx))),
      ax = Nx / Dx,
      ex = rev(cumsum(rev(lx))) / lx - teknik_faiz
    )
  
  teknik_faiz_tablo
  
  # 
  
  
  # Calculate Gelir
  
  HT_Bilinen_Donem_Tablosu <- function(data){
    
    HT_Maluliyet_Sonrasi_Donem_Tablosu(data)
    
  }
  
  
  ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu(data)$Gelir[nrow(HT_Bilinen_Donem_Tablosu(data))]
  
  
  # Calculate claims using vectorized approach
  
  if (data$kaza_tarihi_yas < 90) {
    
    N_value <- teknik_faiz_tablo %>%
      filter(Yaş == data$kaza_tarihi_yas) %>%
      pull(Nx)
    
    N_value_pasif <- teknik_faiz_tablo %>%
      filter(Yaş == 60) %>% # Example of a passive age like retirement age
      pull(Nx)
    
    D_value <- teknik_faiz_tablo %>%
      filter(Yaş == data$kaza_tarihi_yas) %>%
      pull(Dx)
    
    if (data$kaza_tarihi_yas > 59){
      annuity_factor_active <-  0 
      annuity_factor_pasif <- N_value/ D_value
    }
    else{
      annuity_factor_active <-  (N_value - N_value_pasif) / D_value 
      annuity_factor_pasif <- N_value_pasif/ D_value
    }
    
    
    # Calculate claims
    active_claim <- ht_bilinen_son_donem_gelir * 12 * data$Maluliyet_Oranı/100 * data$Kusur_Oranı/100 * annuity_factor_active 
    passive_claim <- ht_bilinen_son_donem_gelir * 12 * data$Maluliyet_Oranı/100 * data$Kusur_Oranı/100 * annuity_factor_pasif
    total_claim <- active_claim + passive_claim
    if(total_claim > data$kaza_tarihi_teminat_limiti) {total_claim = data$kaza_tarihi_teminat_limiti} else {total_claim = total_claim}
    
    # Store results
    claim_results <- data.frame(
      Age = data$kaza_tarihi_yas,
      Gelir = ht_bilinen_son_donem_gelir,
      Disability_Rate = data$Maluliyet_Oranı/100,
      Kusur_Rate = data$Kusur_Oranı/100,
      Active_Claim = formattable::currency(active_claim, digits = 2, format = "f", big.mark = ".",decimal.mark = ",",  symbol = ""),
      Passive_Claim = formattable::currency(passive_claim, digits = 2, format = "f", big.mark = ".",decimal.mark = ",",  symbol = ""),
      Total_Claim = total_claim,
      Annuity_Active = annuity_factor_active,
      Annuity_Passive = annuity_factor_pasif
    )
  }
  claim_results
  
  
}





# SON TABLO 


# YAŞAM TABLOSU / RAPOR TÜRÜ HESAPLAMALARI ----
## 1. GEÇİCİ TAZMİNAT ----

gecici_tazminat_func <- function(data = ortak_veri_tbl){
  
  ### 1.1 Geçici Tazminat Tablosu ----
  
  gecici_tazminat_tablosu <- function(data){
    
    HT_Tam_Maluliyet_Donemi_Tablosu(data)
    
  }
  
  ### 1.2 Geçici Tazminat Tutarı ----
  
  gecici_tazminat_tutarı <- function(data) {
    
    sum(gecici_tazminat_tablosu(data)$Donem_Tazminat)
    
  }
  
  
  # 1.3 Sonuç Tablosu ----
  
  sonuc_tablosu <- function(data){
    
    sonuc_tablosu <- tibble(
      "Dosya_No" =  data$dosya_no,
      "Rapor_Turu" =  data$rapor_turu,
      "Gecici" = sum(gecici_tazminat_tablosu(data)$Donem_Tazminat),
      "Surekli" = 0,
      "Bakici" = 0,
      "Toplam" = sum(gecici_tazminat_tablosu(data)$Donem_Tazminat),
      "Toplam_Sonuc" = sum(gecici_tazminat_tablosu(data)$Donem_Tazminat),
      "Odeme_HT_Degeri" = 0
      
    )
    sonuc_tablosu 
    
  }
  
  return(list(tablo =  gecici_tazminat_tablosu(data),
              tutar =   gecici_tazminat_tutarı(data),
              sonuc_tablosu = sonuc_tablosu(data)
  )
  )
  
}



## 2. SÜREKLİ TAZMİNAT ----

surekli_tazminat_func <- function(data = ortak_veri_tbl){
  
  
  ### 2.1 Sürekli Tazminat Tablosu ----
  
  surekli_tazminat_tablosu <- function(data){
    
    
    son_hesaplama_tablosu <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat", "Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                        Tutar = c(sum(HT_Bilinen_Donem_Tablosu(data)$Donem_Tazminat), sum(HT_Bilinmeyen_Donem_Tablosu(data)$Toplam), toplam_tazminat_func(data)$tutar, data$kaza_tarihi_teminat_limiti,  Bakiye_Tazminat(data))
                                        
    )
    
    son_hesaplama_tablosu
    
  }
  
  
  ### 2.2 Sürekli Tazminat Tutarı ----
  
  surekli_tazminat_tutarı <- function(data){
    
    surekli_tazminat_tablosu(data)[surekli_tazminat_tablosu(data)$Tazminat == "Bakiye Tazminat Tutarı", "Tutar"]
    
  }
  
  
  # 2.3 Sonuç Tablosu ----
  
  sonuc_tablosu <- function(data){
    
    sonuc_tablosu <- tibble(
      "Dosya_No" =  data$dosya_no,
      "Rapor_Turu" =  data$rapor_turu,
      "Gecici" = 0,
      "Surekli" = surekli_tazminat_tablosu(data)[surekli_tazminat_tablosu(data)$Tazminat == "Bakiye Tazminat Tutarı", "Tutar"],
      "Bakici" = 0,
      "Toplam" = surekli_tazminat_tablosu(data)[surekli_tazminat_tablosu(data)$Tazminat == "Toplam Tazminat", "Tutar"],
      "Toplam_Sonuc" = surekli_tazminat_tablosu(data)[surekli_tazminat_tablosu(data)$Tazminat == "Bakiye Tazminat Tutarı", "Tutar"],
      "Odeme_HT_Degeri" = 0
      
    )
    sonuc_tablosu 
    
  }
  
  
  
  return(list(tablo =  surekli_tazminat_tablosu(data),
              tutar =  surekli_tazminat_tutarı(data),
              sonuc_tablosu = sonuc_tablosu(data)
  )
  )
  
}




## 3. SÜREKLİ & GEÇİCİ TAZMİNAT ----

surekli_gecici_tazminat_func <- function(data = ortak_veri_tbl){
  
  ### 3.1 Sürekli & Geçici Tazminat Tablosu ----
  
  surekli_gecici_tazminat_tablosu <- function(data){
    
    son_hesaplama_tablosu <- data.frame(Tazminat = c("Geçici Dönem Tazminatı", "Sürekli Dönem Tazminatı", "Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                        Tutar = c(gecici_tazminat_func(data)$tutar, toplam_tazminat_func(data)$tutar, data$kaza_tarihi_teminat_limiti, Bakiye_Tazminat(data))
    )
    
    son_hesaplama_tablosu
    
  }
  
  surekli_gecici_tazminat_tablosu(ortak_veri_tbl)
  
  ### 3.2 Sürekli & Geçici Tazminat Tutarı ----
  
  surekli_gecici_tazminat_tutarı <- function(data){
    Bakiye_Tazminat(data)
  }
  
  
  # 3.3 Sonuç Tablosu ----
  
  sonuc_tablosu <- function(data){
    
    sonuc_tablosu <- tibble(
      "Dosya_No" =  data$dosya_no,
      "Rapor_Turu" =  data$rapor_turu,
      "Gecici" = gecici_tazminat_func(data)$tutar,
      "Surekli" = Bakiye_Tazminat(data),
      "Bakici" = 0,
      "Toplam" = surekli_gecici_tazminat_tablosu(data)[surekli_gecici_tazminat_tablosu(data)$Tazminat == "Sürekli Dönem Tazminatı", "Tutar"],
      "Toplam_Sonuc" = Bakiye_Tazminat(data),
      "Odeme_HT_Degeri" = 0
      
    )
    sonuc_tablosu 
    
  }
  
  
  return(list(tablo =  surekli_gecici_tazminat_tablosu(data),
              tutar =  surekli_gecici_tazminat_tutarı(data),
              sonuc_tablosu = sonuc_tablosu(data)
  )
  )
  
}




## 4. TÜM RAPOR - ŞİRKET ÖDEMESİZ ----

tum_rapor_sirket_odemesiz_func <- function(data = ortak_veri_tbl){
  
  
  ### 4.1 Tüm Rapor Tazminat Tablosu ----
  
  tum_rapor_tazminat_tablosu <- function(data){
    
    son_hesaplama_tablosu <- data.frame(Tazminat = c("Geçici Dönem Tazminatı", "Sürekli Dönem Tazminatı", "Teminat Limiti", "Bakiye Tazminat Tutarı", "Bakıcı Gideri Tutarı"),
                                        Tutar = c(gecici_tazminat_func(data)$tutar, toplam_tazminat_func(data)$tutar, data$kaza_tarihi_teminat_limiti,  Bakiye_Tazminat(data), bakici_gideri_func(data)$tutar)
                                        
    )
    
    son_hesaplama_tablosu
    
  }
  
  ### 4.2 Tüm Rapor Tazminat Tutarı ----
  
  tum_rapor_tazminat_tutarı <- function(data){
    Bakiye_Tazminat(data)
  }
  
  # 4.3 Sonuç Tablosu ----
  
  sonuc_tablosu <- function(data){
    
    sonuc_tablosu <- tibble(
      "Dosya_No" =  data$dosya_no,
      "Rapor_Turu" =  data$rapor_turu,
      "Gecici" = gecici_tazminat_func(data)$tutar,
      "Surekli" = Bakiye_Tazminat(data),
      "Bakici" = bakici_gideri_func(data)$tutar,
      "Toplam" = tum_rapor_tazminat_tablosu(data)[tum_rapor_tazminat_tablosu(data)$Tazminat == "Sürekli Dönem Tazminatı", "Tutar"],
      "Toplam_Sonuc" = Bakiye_Tazminat(data),
      "Odeme_HT_Degeri" = 0
      
    )
    sonuc_tablosu 
    
  }
  
  
  return(list(tablo =  tum_rapor_tazminat_tablosu(data),
              tutar =  tum_rapor_tazminat_tutarı(data),
              sonuc_tablosu = sonuc_tablosu(data)
  )
  )
  
}







## 5. TÜM RAPOR - 1 ÖDEME ----

### 4.1 Tüm Rapor-1 Ödeme Tablosu ----

tum_rapor_1odeme <- function(){
  
  
}

### 4.1 Tüm Rapor-1 Ödeme Tutarı ----













# TEKNİK FAİZ / RAPOR TÜRÜ HESAPLAMALARI ----


## 1. TEKNİK FAİZ / SÜREKLİ TAZMİNAT ----

teknik_faiz_surekli_tazminat_func <- function(data = ortak_veri_tbl, teknik_faiz_orani = 0.1){
  
  
  ### 2.1 Sürekli Tazminat Tablosu ----
  
  teknik_faiz_surekli_tazminat_tablosu <- function(data){
    
    total_claim = sum(teknik_faizli_surekli_tazminat_func(data, teknik_faiz_orani)$Active_Claim) + sum(teknik_faizli_surekli_tazminat_func(data, teknik_faiz_orani)$Passive_Claim)
    toplam_tazminat = sum(HT_Bilinen_Donem_Tablosu(data)$Donem_Tazminat) + total_claim
    
    son_hesaplama_tablosu <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat", "Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                        Tutar = c(sum(HT_Bilinen_Donem_Tablosu(data)$Donem_Tazminat), total_claim, toplam_tazminat, data$kaza_tarihi_teminat_limiti,  sum(teknik_faizli_surekli_tazminat_func(data, teknik_faiz_orani)$Total_Claim))
                                        
    )
    
    son_hesaplama_tablosu
    
  }
  
  
  ### 2.2 Sürekli Tazminat Tutarı ----
  
  teknik_faiz_surekli_tazminat_tutarı <- function(data){
    
    teknik_faiz_surekli_tazminat_tablosu(data)[teknik_faiz_surekli_tazminat_tablosu(data)$Tazminat == "Bakiye Tazminat Tutarı", "Tutar"]
    
  }
  
  
  # 2.3 Sonuç Tablosu ----
  
  teknik_faiz_sonuc_tablosu <- function(data){
    
    sonuc_tablosu <- tibble(
      "Dosya_No" =  data$dosya_no,
      "Rapor_Turu" =  data$rapor_turu,
      "Gecici" = 0,
      "Surekli" = teknik_faiz_surekli_tazminat_tablosu(data)[teknik_faiz_surekli_tazminat_tablosu(data)$Tazminat == "Bakiye Tazminat Tutarı", "Tutar"],
      "Bakici" = 0,
      "Toplam" = teknik_faiz_surekli_tazminat_tablosu(data)[teknik_faiz_surekli_tazminat_tablosu(data)$Tazminat == "Toplam Tazminat", "Tutar"],
      "Toplam_Sonuc" = teknik_faiz_surekli_tazminat_tablosu(data)[teknik_faiz_surekli_tazminat_tablosu(data)$Tazminat == "Bakiye Tazminat Tutarı", "Tutar"],
      "Odeme_HT_Degeri" = 0
      
    )
    sonuc_tablosu 
    
  }
  
  
  
  return(list(tablo = teknik_faiz_surekli_tazminat_tablosu(data),
              tutar =  teknik_faiz_surekli_tazminat_tutarı(data),
              sonuc_tablosu = teknik_faiz_sonuc_tablosu(data)
  )
  )
  
}





## 2. SÜREKLİ & GEÇİCİ TAZMİNAT ----

surekli_gecici_tazminat_func <- function(data = ortak_veri_tbl){
  
  ### 3.1 Sürekli & Geçici Tazminat Tablosu ----
  
  surekli_gecici_tazminat_tablosu <- function(data){
    
    son_hesaplama_tablosu <- data.frame(Tazminat = c("Geçici Dönem Tazminatı", "Sürekli Dönem Tazminatı", "Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                        Tutar = c(gecici_tazminat_func(data)$tutar, toplam_tazminat_func(data)$tutar, data$kaza_tarihi_teminat_limiti, Bakiye_Tazminat(data))
    )
    
    son_hesaplama_tablosu
    
  }
  

  
  ### 3.2 Sürekli & Geçici Tazminat Tutarı ----
  
  surekli_gecici_tazminat_tutarı <- function(data){
    Bakiye_Tazminat(data)
  }
  
  
  # 3.3 Sonuç Tablosu ----
  
  sonuc_tablosu <- function(data){
    
    sonuc_tablosu <- tibble(
      "Dosya_No" =  data$dosya_no,
      "Rapor_Turu" =  data$rapor_turu,
      "Gecici" = gecici_tazminat_func(data)$tutar,
      "Surekli" = Bakiye_Tazminat(data),
      "Bakici" = 0,
      "Toplam" = surekli_gecici_tazminat_tablosu(data)[surekli_gecici_tazminat_tablosu(data)$Tazminat == "Sürekli Dönem Tazminatı", "Tutar"],
      "Toplam_Sonuc" = Bakiye_Tazminat(data),
      "Odeme_HT_Degeri" = 0
      
    )
    sonuc_tablosu 
    
  }
  
  
  return(list(tablo =  surekli_gecici_tazminat_tablosu(data),
              tutar =  surekli_gecici_tazminat_tutarı(data),
              sonuc_tablosu = sonuc_tablosu(data)
  )
  )
  
}






