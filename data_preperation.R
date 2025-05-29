



# PACKAGES ----

packages <- c(
  "quarto", "shiny", "shinyWidgets", "shinyjs", "tidyverse", "tidyquant", "htmltools",
  "shinythemes", "shinydashboard", "shinyBS", "reactable", "formattable", 
  "plotly", "ggplot2", "ggpubr", "viridis", "hrbrthemes", "scales", 
  "shinyalert", "reactable", "formattable", "DT", "kableExtra", 
  "writexl", "readxl", "collapsibleTree", "highcharter", "lubridate", 
  "bslib", "bsicons", "sf", "mapview", "leaflet", "modelr", "caret", 
  "billboarder", "collapsibleTree", "echarts4r", "shinyMatrix", 
  "DT", "rhandsontable", "bslib", "visNetwork", "lpSolve", "geosphere", "osrm", "leaflet.extras", "colorspace","RColorBrewer",
  "modelr", "caret", "billboarder", "collapsibleTree", "echarts4r", "shinyMatrix", "shinycssloaders", "shinyauthr"
)

# Load packages 
lapply(packages, library, character.only = TRUE)
library(collapsibleTree)

Sys.setlocale(locale = "Turkish")

# LOAD FILES ----

# A. GECICI - SIRKET ODEMESIZ ----


gecici_sirket_odemesiz <- function(data, yasam_tablosu, teknik_faiz) {
  
  
  ## Dosya Bilgileri ----
  
  # dosya_bilgiler <- read_excel("../KURUMSAL_PROINSURE/data/dosya_bilgiler.xlsx", sheet = "Sayfa1")
  
  Asgari_Tablo <- read_excel("../KURUMSAL_PROINSURE/data/Asgari_Ucret_Tablosu_rvz.xlsx", sheet = "Program")
  
  Teminat_Limit_Tablosu <- read_excel("../KURUMSAL_PROINSURE/data/Teminat_Limit_Tablosu_rvz.xlsx", sheet = "Teminat")
  
  
  # PREPARE DATA ----
  
  # dosya_bilgileri <- dosya_bilgiler[6, ] # Use this to test the code
  
  dosya_bilgileri <- data 
  
  # Genel Bilgiler ----
  
  Teknik_Faiz <- teknik_faiz
  pasif_donem_yas <- 60
  faiz_oranı <- 9
  
  
  ## Manuel Gelir Tablosu ----
  
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
  
  
  ## Kişisel Bilgiler ----
  
  dosya_no <- as.character(dosya_bilgileri$Dosya_No)
  rapor_turu <- dosya_bilgileri$Rapor_Turu
  Ad_Soyad <- dosya_bilgileri$Ad_Soyad
  Cinsiyet <- dosya_bilgileri$Cinsiyet
  Dogum_Tarihi <- as.Date(dosya_bilgileri$Dogum_Tarihi)
  Gelir_Durumu <- dosya_bilgileri$Gelir
  Kaza_Tarihi <- as.Date(dosya_bilgileri$Kaza_Tarihi)
  Maluliyet_Oranı <-dosya_bilgileri$Maluliyet_Orani
  Kusur_Oranı <- dosya_bilgileri$Kusur_Orani
  Gecici_Maluliyet_sure <- dosya_bilgileri$Gecici_Maluliyet_sure
  Kısmi_Odeme_Sayısı <- dosya_bilgileri$Kısmi_Odeme_Sayısı
  Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_1)
  Kısmi_Odeme_Tutarı_1 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_1
  Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_2)
  Kısmi_Odeme_Tutarı_2 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_2
  Yasam_Tablosu <- yasam_tablosu
  
  
  ## Genel Gelir Tablosu ----
  
  Gelir_tablo <-  Asgari_Tablo %>%
    left_join(gelir_tablosu) %>%
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  
  ## Kaza Tarihi Teminat Limiti ----
  
  Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
    filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)
  
  kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE)) 
  
  
  # 1.0 YAS HESAPLAMALARI ----
  
  Hesap_Tarihi <- Sys.Date()
  Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1
  
  kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
  hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 
  
  sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 
  
  
  # 2.0 HESAPLAMADA KULLANILACAK TRH TABLOSU ----
  
  ## 2.1 Hesaplama Tarihine göre beklenen ömür ----
  
  PR_TRH_2010 <- if (Yasam_Tablosu == "TRH-2010") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TRH-2010")
  } else if  (Yasam_Tablosu == "TUIK_20-22") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_20-22")
  } else if (Yasam_Tablosu == "TUIK_19-21") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_19-21")
  } else if (Yasam_Tablosu == "TUIK_18-20") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_18-20")
  } else {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "PMF-1931")
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
  
  
  ## 2.2 Pasif Dönem Yas- TRH Tablosu ----
  
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
  
  
  # 3.0 PARAMATRE TABLOSU ----
  
  
  parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Ad-Soyad", "Kaza Tarihi Yas", "Hesap Tarihi Yas", "Cinsiyet", "Kaza Tarihi", "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)"),
                                  DEĞER = c(dosya_no, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet, as.character(Kaza_Tarihi), 
                                            Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure)
  )
  
  parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")
  
  parametre_tablosu2 <- tibble(
    "Dosya_No" = dosya_no,
    "Ad-Soyad" = Ad_Soyad,
    "Yas" = kaza_tarihi_yas,
    "Hesap Tarihi Yas" = hesap_tarihi_yas,
    "Cinsiyet" = Cinsiyet, 
    "Kaza Tarihi" = as.character(Kaza_Tarihi),
    "Maluliyet Oranı" = Maluliyet_Oranı,
    "Teknik Faiz" = Teknik_Faiz,
    "Yaşam Tablosu" = Yasam_Tablosu,
    "Kusur Oranı" = Kusur_Oranı,
    "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure
    
  )
  
  
  # 4.0 HESAP TARİHİ İLE HESAPLAMA (AKTÜERYAL HESAPLAMA) ----
  
  ## 4.1 Hesap tarihi Bilinen Dönem Hesaplaması ----
  
  ### 4.1.1 Hesap tarihi Tam Maluliyet Donemi Tablosu ----
  
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  HT_Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
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
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  
  
  # 5.0 AKTÜERYAL HESAPLAMA ----
  
  gecici_donem_tazminat <- sum(HT_Tam_Maluliyet_Donemi_Tablosu$Donem_Tazminat)
  
  # 6.0 SONUÇ ----
  
  sonuc_tablosu <- tibble(
    "Dosya_No" = dosya_no,
    "Rapor_Turu" = rapor_turu,
    "Gecici" = gecici_donem_tazminat,
    "Surekli" = 0,
    "Bakici" = 0,
    "Toplam" = sum(Gecici+Surekli+Bakici),
    "Odeme_HT_Degeri" = 0
    
  )
  
  sonuc_tablosu
  
}


gecici_sirket_odemesiz(dosya_bilgiler[1, ], "TRH-2010", 2)




# B. SÜREKLİ - ŞİRKET ÖDEMESİZ ----


surekli_sirket_odemesiz <- function(data, yasam_tablosu, teknik_faiz) {
  
  ## Dosya Bilgileri ----
  
  # dosya_bilgiler <- read_excel("../KURUMSAL_PROINSURE/data/dosya_bilgiler.xlsx", sheet = "Sayfa1")
  
  Asgari_Tablo <- read_excel("../KURUMSAL_PROINSURE/data/Asgari_Ucret_Tablosu_rvz.xlsx", sheet = "Program")
  
  Teminat_Limit_Tablosu <- read_excel("../KURUMSAL_PROINSURE/data/Teminat_Limit_Tablosu_rvz.xlsx", sheet = "Teminat")
  
  # PREPARE DATA ----
  
  # dosya_bilgileri <- dosya_bilgiler[5, ] # Use this to test the code
  
  dosya_bilgileri <- data 
  
  # Genel Bilgiler ----
  
  Teknik_Faiz <- teknik_faiz
  pasif_donem_yas <- 60
  faiz_oranı <- 9
  
  
  ## Manuel Gelir Tablosu ----
  
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
  
  
  ## Kişisel Bilgiler ----
  
  dosya_no <- as.character(dosya_bilgileri$Dosya_No)
  rapor_turu <- dosya_bilgileri$Rapor_Turu
  Ad_Soyad <- dosya_bilgileri$Ad_Soyad
  Cinsiyet <- dosya_bilgileri$Cinsiyet
  Dogum_Tarihi <- as.Date(dosya_bilgileri$Dogum_Tarihi)
  Gelir_Durumu <- dosya_bilgileri$Gelir
  Kaza_Tarihi <- as.Date(dosya_bilgileri$Kaza_Tarihi)
  Maluliyet_Oranı <-dosya_bilgileri$Maluliyet_Orani
  Kusur_Oranı <- dosya_bilgileri$Kusur_Orani
  Gecici_Maluliyet_sure <- dosya_bilgileri$Gecici_Maluliyet_sure
  Kısmi_Odeme_Sayısı <- dosya_bilgileri$Kısmi_Odeme_Sayısı
  Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_1)
  Kısmi_Odeme_Tutarı_1 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_1
  Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_2)
  Kısmi_Odeme_Tutarı_2 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_2
  Yasam_Tablosu <- yasam_tablosu
  
  
  ## Genel Gelir Tablosu ----
  
  Gelir_tablo <-  Asgari_Tablo %>%
    left_join(gelir_tablosu) %>%
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  
  ## Kaza Tarihi Teminat Limiti ----
  
  Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
    filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)
  
  kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE)) 
  
  
  
  # 1.0  YAS HESAPLAMALARI ----
  
  Hesap_Tarihi <- Sys.Date()
  Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1
  
  kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
  hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 
  
  sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 
  
  
  # 2.0 HESAPLAMADA KULLANILACAK TRH TABLOSU ----
  
  ## 2.1 Hesaplama Tarihine göre beklenen ömür ----
  
  
  PR_TRH_2010 <- if ( Yasam_Tablosu == "TRH-2010") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TRH-2010")
  } else if ( Yasam_Tablosu == "TUIK_20-22") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_20-22")
  } else if ( Yasam_Tablosu == "TUIK_19-21") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_19-21")
  } else if ( Yasam_Tablosu == "TUIK_18-20") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_18-20")
  } else {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "PMF-1931")
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
  
  
  ## 2.2 Pasif Dönem Yas- TRH Tablosu ----
  
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
  
  
  # 3.0 PARAMATRE TABLOSU ----
  
  
  parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Ad-Soyad", "Kaza Tarihi Yas","Hesap Tarihi Yas", "Cinsiyet", "Kaza Tarihi", 
                                                "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)"
  ),
  DEĞER = c(dosya_no, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet,as.character(Kaza_Tarihi), 
            Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure
  )
  )
  
  parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")
  
  parametre_tablosu2 <- tibble(
    "Dosya_No" = dosya_no,
    "Ad-Soyad" = Ad_Soyad,
    "Yas" = kaza_tarihi_yas,
    "Hesap Tarihi Yas" = hesap_tarihi_yas,
    "Cinsiyet" = Cinsiyet, 
    "Kaza Tarihi" = as.character(Kaza_Tarihi),
    "Maluliyet Oranı" = Maluliyet_Oranı,
    "Teknik Faiz" = Teknik_Faiz,
    "Yaşam Tablosu" = Yasam_Tablosu,
    "Kusur Oranı" = Kusur_Oranı,
    "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure
    
  )
  
  # 4.0 HESAP TARİHİ İLE HESAPLAMA (AKTÜERYAL HESAPLAMA) ----
  
  ## 4.1 Hesap tarihi Bilinen Dönem Hesaplaması ----
  
  ### 4.1.1 Hesap tarihi Tam Maluliyet Donemi Tablosu ----
  
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  HT_Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
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
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  ### 4.1.2 Hesap tarihi Tam Maluliyet Sonrası Dönem Tablosu ----
  
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi + 365)
  
  hesaplama_tarihi_chr <- as.character(Hesap_Tarihi)
  
  
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
    
    mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  
  ### 4.1.3 Hesap Tarihi Bilinen Dönem Tablosu ve Tazminatı ----
  
  
  HT_Bilinen_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_Tablosu
  
  HT_Bilinen_Donem_Tazminatı = sum(HT_Bilinen_Donem_Tablosu$Donem_Tazminat)
  
  
  
  ## 4.2 Hesap Tarihi Bilinmeyen Dönem Hesaplaması ----
  
  ### 4.2.1 Hesap Tarihi Aktif Donem ----          
  
  ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu$Gelir[nrow(HT_Bilinen_Donem_Tablosu)]
  
  ht_bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-hesap_tarihi_yas) * ht_bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  ht_bilinmeyen_aktif_donem_tazminat <- round(ht_bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                     'Toplam Yıl' = (pasif_donem_yas-hesap_tarihi_yas),
                                                                     'Toplam Ay' = (pasif_donem_yas-hesap_tarihi_yas)*12,
                                                                     Gelir = ht_bilinen_son_donem_gelir,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_aktif_donem_tazminat)
  
  
  ### 4.2.2 Hesap Tarihi Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_ht <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi & Hesap_Tarihi > D_B)
  ht_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_ht$Bekar
  
  ht_pasif_donem_beklenen_omur <- 
    
    if (hesap_tarihi_yas <= pasif_donem_yas) {
      
      (hesap_tarihi_yas + hesaplama_tarihi_beklenen_omur - pasif_donem_yas) 
      
    } else {
      
      hesaplama_tarihi_beklenen_omur
    }
  
  
  ht_bilinmeyen_pasif_donem_tazminat <- ht_pasif_donem_beklenen_omur * ht_pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  
  
  hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                     'Toplam Yıl' = ht_pasif_donem_beklenen_omur,
                                                                     'Toplam Ay' = ht_pasif_donem_beklenen_omur*12,
                                                                     Gelir = ht_pasif_donem_geliri,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 4.2.3 Hesap Tarihi Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                              hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(hesap_tarihi_yas > pasif_donem_yas) {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  
  
  ht_bilinmeyen_donem_toplam_tazminat <- ifelse(hesap_tarihi_yas <= pasif_donem_yas, ht_bilinmeyen_aktif_donem_tazminat + ht_bilinmeyen_pasif_donem_tazminat, ht_bilinmeyen_pasif_donem_tazminat)
  
  HT_Bilinmeyen_Donem_Tazminatı <- round(ht_bilinmeyen_donem_toplam_tazminat,digits = 2)
  
  
  # knitr::kable(hesap_tarihi_bilinmeyen_donem_tazminat_tablosu, caption = "Bilinmeyen Dönem Toplam Kazanç Tutarı")
  
  
  ## 4.3 Toplam Tazminat ----
  
  hesap_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                     Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, (HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı)))
  
  
  Toplam_Tazminat <- HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı
  Toplam_Tazminat <- round(Toplam_Tazminat,digits = 2)
  
  
  # 5.0 BAKİYE TAZMİNAT ve SON TABLO ----
  
  kaza_tarihi_teminat_limiti2 <- as.numeric(kaza_tarihi_teminat_limiti)
  
  
  bakiye_tazminat <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti) {
    kaza_tarihi_teminat_limiti
  } else {
    Toplam_Tazminat
  }
  
  
  
  son_hesaplama_tablosu1 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat","Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat, kaza_tarihi_teminat_limiti,  bakiye_tazminat)
  )
  
  
  son_hesaplama_tablosu2 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat",  "Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat,  kaza_tarihi_teminat_limiti, bakiye_tazminat)
  )
  
  
  
  # 6.0 SONUÇ ----
  
  
  sonuc_tablosu <- tibble(
    "Dosya_No" = dosya_no,
    "Rapor_Turu" = rapor_turu,
    "Gecici" = 0,
    "Surekli" = bakiye_tazminat,
    "Bakici" = 0,
    "Toplam" = sum(Gecici+Surekli+Bakici),
    "Odeme_HT_Degeri" = 0
    
  )
  
  sonuc_tablosu
  
  
}

surekli_sirket_odemesiz(dosya_bilgiler[4, ], "TRH-2010")


# C. SÜREKLİ & GEÇİCİ - ŞİRKET ÖDEMESİZ ----


surekli_gecici_sirket_odemesiz <- function(data, yasam_tablosu, teknik_faiz) {
  
  
  ## Dosya Bilgileri ----
  
  dosya_bilgiler <- read_excel("../KURUMSAL_PROINSURE/data/dosya_bilgiler.xlsx", sheet = "Sayfa1")
  
  Asgari_Tablo <- read_excel("../KURUMSAL_PROINSURE/data/Asgari_Ucret_Tablosu_rvz.xlsx", sheet = "Program")
  
  Teminat_Limit_Tablosu <- read_excel("../KURUMSAL_PROINSURE/data/Teminat_Limit_Tablosu_rvz.xlsx", sheet = "Teminat")
  
  # PREPARE DATA ----
  
  # dosya_bilgileri <- dosya_bilgiler[5, ] # Use this to test the code
  
  dosya_bilgileri <- data 
  
  # Genel Bilgiler ----
  
  Teknik_Faiz <- teknik_faiz
  pasif_donem_yas <- 60
  faiz_oranı <- 9
  
  
  ## Manuel Gelir Tablosu ----
  
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
  
  
  ## Kişisel Bilgiler ----
  
  dosya_no <- as.character(dosya_bilgileri$Dosya_No)
  rapor_turu <- dosya_bilgileri$Rapor_Turu
  Ad_Soyad <- dosya_bilgileri$Ad_Soyad
  Cinsiyet <- dosya_bilgileri$Cinsiyet
  Dogum_Tarihi <- as.Date(dosya_bilgileri$Dogum_Tarihi)
  Gelir_Durumu <- dosya_bilgileri$Gelir
  Kaza_Tarihi <- as.Date(dosya_bilgileri$Kaza_Tarihi)
  Maluliyet_Oranı <-dosya_bilgileri$Maluliyet_Orani
  Kusur_Oranı <- dosya_bilgileri$Kusur_Orani
  Gecici_Maluliyet_sure <- dosya_bilgileri$Gecici_Maluliyet_sure
  Kısmi_Odeme_Sayısı <- dosya_bilgileri$Kısmi_Odeme_Sayısı
  Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_1)
  Kısmi_Odeme_Tutarı_1 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_1
  Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_2)
  Kısmi_Odeme_Tutarı_2 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_2
  Yasam_Tablosu <- yasam_tablosu
  
  
  ## Genel Gelir Tablosu ----
  
  Gelir_tablo <-  Asgari_Tablo %>%
    left_join(gelir_tablosu) %>%
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  
  ## Kaza Tarihi Teminat Limiti ----
  
  Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
    filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)
  
  kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE)) 
  
  
  
  # 1.0  YAS HESAPLAMALARI ----
  
  Hesap_Tarihi <- Sys.Date()
  Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1
  
  kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
  hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 
  
  sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 
  
  
  # 2.0 HESAPLAMADA KULLANILACAK TRH TABLOSU ----
  
  ## 2.1 Hesaplama Tarihine göre beklenen ömür ----
  
  
  PR_TRH_2010 <- if ( Yasam_Tablosu == "TRH-2010") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TRH-2010")
  } else if ( Yasam_Tablosu == "TUIK_20-22") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_20-22")
  } else if ( Yasam_Tablosu == "TUIK_19-21") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_19-21")
  } else if ( Yasam_Tablosu == "TUIK_18-20") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_18-20")
  } else {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "PMF-1931")
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
  
  
  ## 2.2 Pasif Dönem Yas- TRH Tablosu ----
  
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
  
  
  # 3.0 PARAMATRE TABLOSU ----
  
  
  parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Ad-Soyad", "Kaza Tarihi Yas","Hesap Tarihi Yas", "Cinsiyet", "Kaza Tarihi", 
                                                "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)"
  ),
  DEĞER = c(dosya_no, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet,as.character(Kaza_Tarihi), 
            Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure
  )
  )
  
  parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")
  
  parametre_tablosu2 <- tibble(
    "Dosya_No" = dosya_no,
    "Ad-Soyad" = Ad_Soyad,
    "Yas" = kaza_tarihi_yas,
    "Hesap Tarihi Yas" = hesap_tarihi_yas,
    "Cinsiyet" = Cinsiyet, 
    "Kaza Tarihi" = as.character(Kaza_Tarihi),
    "Maluliyet Oranı" = Maluliyet_Oranı,
    "Teknik Faiz" = Teknik_Faiz,
    "Yaşam Tablosu" = Yasam_Tablosu,
    "Kusur Oranı" = Kusur_Oranı,
    "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure
    
  )
  
  
  # 4.0 HESAP TARİHİ İLE HESAPLAMA (AKTÜERYAL HESAPLAMA) ----
  
  ## 4.1 Hesap tarihi Bilinen Dönem Hesaplaması ----
  
  ### 4.1.1 Hesap tarihi Tam Maluliyet Donemi Tablosu ----
  
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  HT_Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
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
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  gecici_donem_tazminat <- sum(HT_Tam_Maluliyet_Donemi_Tablosu$Donem_Tazminat)
  
  
  ### 4.1.2 Hesap tarihi Tam Maluliyet Sonrası Dönem Tablosu ----
  
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi + 365)
  
  hesaplama_tarihi_chr <- as.character(Hesap_Tarihi)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(HT_Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_chr
  
  
  HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- HT_Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  
  ### 4.1.3 Hesap Tarihi Bilinen Dönem Tablosu ve Tazminatı ----
  
  HT_Bilinen_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_Tablosu
  
  HT_Bilinen_Donem_Tazminatı = sum(HT_Bilinen_Donem_Tablosu$Donem_Tazminat)
  
  
  
  ## 4.2 Hesap Tarihi Bilinmeyen Dönem Hesaplaması ----
  
  ### 4.2.1 Hesap Tarihi Aktif Donem ----          
  
  ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu$Gelir[nrow(HT_Bilinen_Donem_Tablosu)]
  
  ht_bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-hesap_tarihi_yas) * ht_bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  ht_bilinmeyen_aktif_donem_tazminat <- round(ht_bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                     'Toplam Yıl' = (pasif_donem_yas-hesap_tarihi_yas),
                                                                     'Toplam Ay' = (pasif_donem_yas-hesap_tarihi_yas)*12,
                                                                     Gelir = ht_bilinen_son_donem_gelir,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_aktif_donem_tazminat)
  
  
  
  ### 4.2.2 Hesap Tarihi Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_ht <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi & Hesap_Tarihi > D_B)
  ht_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_ht$Bekar
  
  ht_pasif_donem_beklenen_omur <- 
    
    if (hesap_tarihi_yas <= pasif_donem_yas) {
      
      (hesap_tarihi_yas + hesaplama_tarihi_beklenen_omur - pasif_donem_yas) 
      
    } else {
      
      hesaplama_tarihi_beklenen_omur
    }
  
  
  
  
  ht_bilinmeyen_pasif_donem_tazminat <- ht_pasif_donem_beklenen_omur * ht_pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  
  
  hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                     'Toplam Yıl' = ht_pasif_donem_beklenen_omur,
                                                                     'Toplam Ay' = ht_pasif_donem_beklenen_omur*12,
                                                                     Gelir = ht_pasif_donem_geliri,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 4.2.3 Hesap Tarihi Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                              hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(hesap_tarihi_yas > pasif_donem_yas) {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  
  
  ht_bilinmeyen_donem_toplam_tazminat <- ifelse(hesap_tarihi_yas <= pasif_donem_yas, ht_bilinmeyen_aktif_donem_tazminat + ht_bilinmeyen_pasif_donem_tazminat, ht_bilinmeyen_pasif_donem_tazminat)
  
  HT_Bilinmeyen_Donem_Tazminatı <- round(ht_bilinmeyen_donem_toplam_tazminat,digits = 2)
  
  
  
  
  ## 4.3 Toplam Tazminat ----
  
  hesap_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                     Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, (HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı)))
  
  
  Toplam_Tazminat <- HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı
  Toplam_Tazminat <- round(Toplam_Tazminat,digits = 2)
  
  
  
  # 5.0 BAKİYE TAZMİNAT ve SON TABLO ----
  
  kaza_tarihi_teminat_limiti2 <- as.numeric(kaza_tarihi_teminat_limiti)
  
  
  bakiye_tazminat <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti) {
    kaza_tarihi_teminat_limiti
  } else {
    Toplam_Tazminat
  }
  
  
  # 5.0 AKTÜERYAL HESAPLAMA ----
  
  gecici_donem_tazminat <- sum(HT_Tam_Maluliyet_Donemi_Tablosu$Donem_Tazminat)
  
  # 6.0 SONUÇ ----
  
  sonuc_tablosu <- tibble(
    "Dosya_No" = dosya_no,
    "Rapor_Turu" = rapor_turu,
    "Gecici" = gecici_donem_tazminat,
    "Surekli" = bakiye_tazminat,
    "Bakici" = 0,
    "Toplam" = sum(Gecici+Surekli+Bakici),
    "Odeme_HT_Degeri" = 0
    
  )
  
  sonuc_tablosu
  
  
}


surekli_gecici_sirket_odemesiz(dosya_bilgiler[3, ],"TRH-2010")



# D. TÜM RAPOR - ŞİRKET ÖDEMESİZ ----


tum_rapor_sirket_odemesiz <- function(data, yasam_tablosu, teknik_faiz) {
  
  
  ## Dosya Bilgileri ----
  
  # dosya_bilgiler <- read_excel("../KURUMSAL_PROINSURE/data/dosya_bilgiler.xlsx", sheet = "Sayfa1")
  
  Asgari_Tablo <- read_excel("../KURUMSAL_PROINSURE/data/Asgari_Ucret_Tablosu_rvz.xlsx", sheet = "Program")
  
  Teminat_Limit_Tablosu <- read_excel("../KURUMSAL_PROINSURE/data/Teminat_Limit_Tablosu_rvz.xlsx", sheet = "Teminat")
  
  # PREPARE DATA ----
  
  #dosya_bilgileri <- dosya_bilgiler[5, ] # Use this to test the code
  
  dosya_bilgileri <- data 
  
  # Genel Bilgiler ----
  
  Teknik_Faiz <- teknik_faiz
  pasif_donem_yas <- 60
  faiz_oranı <- 9
  
  
  ## Manuel Gelir Tablosu ----
  
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
  
  
  ## Kişisel Bilgiler ----
  
  dosya_no <- as.character(dosya_bilgileri$Dosya_No)
  rapor_turu <- dosya_bilgileri$Rapor_Turu
  Ad_Soyad <- dosya_bilgileri$Ad_Soyad
  Cinsiyet <- dosya_bilgileri$Cinsiyet
  Dogum_Tarihi <- as.Date(dosya_bilgileri$Dogum_Tarihi)
  Gelir_Durumu <- dosya_bilgileri$Gelir
  Kaza_Tarihi <- as.Date(dosya_bilgileri$Kaza_Tarihi)
  Maluliyet_Oranı <-dosya_bilgileri$Maluliyet_Orani
  Kusur_Oranı <- dosya_bilgileri$Kusur_Orani
  Gecici_Maluliyet_sure <- dosya_bilgileri$Gecici_Maluliyet_sure
  Kısmi_Odeme_Sayısı <- dosya_bilgileri$Kısmi_Odeme_Sayısı
  Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_1)
  Kısmi_Odeme_Tutarı_1 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_1
  Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_2)
  Kısmi_Odeme_Tutarı_2 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_2
  Yasam_Tablosu <- yasam_tablosu
  
  ## Bakıcı Gideri ----
  
  # Bakıcı_gideri <- "Var"
  # Bakıcı_gideri_suresi <- 12
  
  #
  Bakıcı_gideri <- dosya_bilgileri$Bakıcı_Gideri
  Bakıcı_gideri_suresi <- dosya_bilgileri$`Bakıcı_Gideri_Süresi_(gun)`
  
  
  ## Genel Gelir Tablosu ----
  
  Gelir_tablo <-  Asgari_Tablo %>%
    left_join(gelir_tablosu) %>%
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  
  ## Kaza Tarihi Teminat Limiti ----
  
  Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
    filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)
  
  kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE)) 
  
  
  
  # 1.0  YAS HESAPLAMALARI ----
  
  Hesap_Tarihi <- Sys.Date()
  Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1
  
  kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
  hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 
  
  sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 
  
  
  # 2.0 HESAPLAMADA KULLANILACAK TRH TABLOSU ----
  
  ## 2.1 Hesaplama Tarihine göre beklenen ömür ----
  
  
  PR_TRH_2010 <- if ( Yasam_Tablosu == "TRH-2010") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TRH-2010")
  } else if ( Yasam_Tablosu == "TUIK_20-22") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_20-22")
  } else if ( Yasam_Tablosu == "TUIK_19-21") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_19-21")
  } else if ( Yasam_Tablosu == "TUIK_18-20") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_18-20")
  } else {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "PMF-1931")
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
  
  
  ## 2.2 Pasif Dönem Yas- TRH Tablosu ----
  
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
  
  
  # 3.0 PARAMATRE TABLOSU ----
  
  
  parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Ad-Soyad", "Kaza Tarihi Yas","Hesap Tarihi Yas", "Cinsiyet", "Kaza Tarihi", 
                                                "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)"
  ),
  DEĞER = c(dosya_no, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet,as.character(Kaza_Tarihi), 
            Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure
  )
  )
  
  parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")
  
  parametre_tablosu2 <- tibble(
    "Dosya_No" = dosya_no,
    "Ad-Soyad" = Ad_Soyad,
    "Yas" = kaza_tarihi_yas,
    "Hesap Tarihi Yas" = hesap_tarihi_yas,
    "Cinsiyet" = Cinsiyet, 
    "Kaza Tarihi" = as.character(Kaza_Tarihi),
    "Maluliyet Oranı" = Maluliyet_Oranı,
    "Teknik Faiz" = Teknik_Faiz,
    "Yaşam Tablosu" = Yasam_Tablosu,
    "Kusur Oranı" = Kusur_Oranı,
    "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure
    
  )
  
  
  
  
  # 4.0 HESAP TARİHİ İLE HESAPLAMA (AKTÜERYAL HESAPLAMA) ----
  
  ## 4.1 Hesap tarihi Bilinen Dönem Hesaplaması ----
  
  ### 4.1.1 Hesap tarihi Sürekli Maluliyet Donemi Tablosu ----
  
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  HT_Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
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
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  gecici_donem_tazminat <- sum(HT_Tam_Maluliyet_Donemi_Tablosu$Donem_Tazminat)
  
  
  ### 4.1.2 Hesap tarihi Tam Maluliyet Sonrası Dönem Tablosu ----
  
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi + 365)
  
  
  hesaplama_tarihi_chr <- as.character(Hesap_Tarihi)
  
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(HT_Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_chr
  
  
  HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- HT_Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  
  ### 4.1.3 Hesap Tarihi Bilinen Dönem Tablosu ve Tazminatı ----
  
  HT_Bilinen_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_Tablosu
  
  HT_Bilinen_Donem_Tazminatı = sum(HT_Bilinen_Donem_Tablosu$Donem_Tazminat)
  
  
  
  ## 4.2 Hesap Tarihi Bilinmeyen Dönem Hesaplaması ----
  
  ### 4.2.1 Hesap Tarihi Aktif Donem ----          
  
  ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu$Gelir[nrow(HT_Bilinen_Donem_Tablosu)]
  
  ht_bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-hesap_tarihi_yas) * ht_bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  ht_bilinmeyen_aktif_donem_tazminat <- round(ht_bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                     'Toplam Yıl' = (pasif_donem_yas-hesap_tarihi_yas),
                                                                     'Toplam Ay' = (pasif_donem_yas-hesap_tarihi_yas)*12,
                                                                     Gelir = ht_bilinen_son_donem_gelir,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_aktif_donem_tazminat)
  
  
  ### 4.2.2 Hesap Tarihi Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_ht <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi & Hesap_Tarihi > D_B)
  ht_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_ht$Bekar
  
  ht_pasif_donem_beklenen_omur <- 
    
    if (hesap_tarihi_yas <= pasif_donem_yas) {
      
      (hesap_tarihi_yas + hesaplama_tarihi_beklenen_omur - pasif_donem_yas) 
      
    } else {
      
      hesaplama_tarihi_beklenen_omur
    }
  
  
  
  
  ht_bilinmeyen_pasif_donem_tazminat <- ht_pasif_donem_beklenen_omur * ht_pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  
  
  hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                     'Toplam Yıl' = ht_pasif_donem_beklenen_omur,
                                                                     'Toplam Ay' = ht_pasif_donem_beklenen_omur*12,
                                                                     Gelir = ht_pasif_donem_geliri,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 4.2.3 Hesap Tarihi Bilinmeyen Dönem Toplam Sürekli Maluliyet Tazminat Tablosu ----
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                              hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(hesap_tarihi_yas > pasif_donem_yas) {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  
  
  ht_bilinmeyen_donem_toplam_tazminat <- ifelse(hesap_tarihi_yas <= pasif_donem_yas, ht_bilinmeyen_aktif_donem_tazminat + ht_bilinmeyen_pasif_donem_tazminat, ht_bilinmeyen_pasif_donem_tazminat)
  
  HT_Bilinmeyen_Donem_Tazminatı <- round(ht_bilinmeyen_donem_toplam_tazminat,digits = 2)
  
  
  
  ## 4.3 Toplam Sürekli Maluliyet Tazminatı ----
  
  hesap_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                     Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, (HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı)))
  
  
  Toplam_Tazminat <- HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı
  Toplam_Tazminat <- round(Toplam_Tazminat,digits = 2)
  
  
  # 5.0 BAKICI GİDERİ HESAPLAMASI ----
  
  
  Bakici_Gideri_Baslangic_Tarihi <- Kaza_Tarihi
  Bakici_Gideri_Bitis_Tarihi <- Kaza_Tarihi + Bakıcı_gideri_suresi
  
  Bakici_Gideri_Donemi_filtered <- Gelir_tablo %>% 
    filter(D_S >= Bakici_Gideri_Baslangic_Tarihi & Bakici_Gideri_Bitis_Tarihi > D_B)
  
  bakici_gideri_baslangic_tarihi_chr <- as.character(Bakici_Gideri_Baslangic_Tarihi)
  bakici_gideri_bitis_tarihi_chr <- as.character(Bakici_Gideri_Bitis_Tarihi)
  
  
  Bakici_Gideri_Donemi_filtered$Donem_Baslangic[1] <- bakici_gideri_baslangic_tarihi_chr
  Bakici_Gideri_Donemi_filtered$Donem_Son[nrow(Bakici_Gideri_Donemi_filtered)] <- bakici_gideri_bitis_tarihi_chr
  
  
  Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu <- Bakici_Gideri_Donemi_filtered %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    mutate(Kazanilan_Gun = signif(as.numeric(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Baslangici), "day")) , digits = 2)) %>%
    
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Donem_Baslangici, Donem_Son, Kazanilan_Gun, Brut, Kusur_Oranı) %>% 
    rename("Aylık_Bakıcı_Gideri" = Brut) %>% 
    mutate(Donem_Bakıcı_Gideri = round(Kazanilan_Gun * Aylık_Bakıcı_Gideri/30 * Kusur_Oranı/100, digits = 2))  
  
  
  if (Bakıcı_gideri == "Var") {
    knitr::kable(Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu , caption = "Bakıcı Gideri Tablosu") } else {str_glue("Dosya kapsamında bakıcı gideri bulunmamaktadır.")}
  
  
  Toplam_Bakici_gideri_hesaplama = sum(Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu$Donem_Bakıcı_Gideri) 
  
  
  Toplam_Bakici_Gideri <- if (Bakıcı_gideri == "Var") { Toplam_Bakici_gideri_hesaplama } else { 0 }
  
  if (Bakıcı_gideri == "Var") {
    str_glue("Toplam Bakıcı Gideri: {Toplam_Bakici_Gideri} TL") } else {str_glue(" ")} 
  
  
  # 6.0 BAKİYE TAZMİNAT ve SON TABLO ----
  
  kaza_tarihi_teminat_limiti2 <- as.numeric(kaza_tarihi_teminat_limiti)
  
  
  bakiye_tazminat <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti) {
    kaza_tarihi_teminat_limiti
  } else {
    Toplam_Tazminat
  }
  
  
  
  son_hesaplama_tablosu1 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat","Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat, kaza_tarihi_teminat_limiti,  bakiye_tazminat)
  )
  
  
  son_hesaplama_tablosu2 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat",  "Teminat Limiti", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat,  kaza_tarihi_teminat_limiti, bakiye_tazminat)
  )
  
  
  
  
  # 7.0 SONUÇ ----
  
  sonuc_tablosu <- tibble(
    "Dosya_No" = dosya_no,
    "Rapor_Turu" = rapor_turu,
    "Gecici" = gecici_donem_tazminat,
    "Surekli" = bakiye_tazminat,
    "Bakici" = Toplam_Bakici_Gideri ,
    "Toplam" = sum(Gecici+Surekli+Bakici),
    "Odeme_HT_Degeri" = 0
    
  )
  
  sonuc_tablosu
  
  
}


tum_rapor_sirket_odemesiz(dosya_bilgiler[5, ], "TRH-2010")



# E. TÜM RAPOR - 1 ÖDEME ----


tum_rapor_1odeme <- function(data, yasam_tablosu, teknik_faiz) {
  
  
  ## Dosya Bilgileri ----
  
  # dosya_bilgiler <- read_excel("../KURUMSAL_PROINSURE/data/dosya_bilgiler.xlsx", sheet = "Sayfa1")
  
  Asgari_Tablo <- read_excel("../KURUMSAL_PROINSURE/data/Asgari_Ucret_Tablosu_rvz.xlsx", sheet = "Program")
  
  Teminat_Limit_Tablosu <- read_excel("../KURUMSAL_PROINSURE/data/Teminat_Limit_Tablosu_rvz.xlsx", sheet = "Teminat")
  
  # PREPARE DATA ----
  
  # dosya_bilgileri <- dosya_bilgiler[4, ] # Use this to test the code
  
  dosya_bilgileri <- data 
  
  # Genel Bilgiler ----
  
  Teknik_Faiz <- teknik_faiz
  pasif_donem_yas <- 60
  faiz_oranı <- 9
  faiz_oranı2 <- 24
  
  
  ## Manuel Gelir Tablosu ----
  
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
  
  
  ## Kişisel Bilgiler ----
  
  dosya_no <- as.character(dosya_bilgileri$Dosya_No)
  rapor_turu <- dosya_bilgileri$Rapor_Turu
  Ad_Soyad <- dosya_bilgileri$Ad_Soyad
  Cinsiyet <- dosya_bilgileri$Cinsiyet
  Dogum_Tarihi <- as.Date(dosya_bilgileri$Dogum_Tarihi)
  Gelir_Durumu <- dosya_bilgileri$Gelir
  Kaza_Tarihi <- as.Date(dosya_bilgileri$Kaza_Tarihi)
  Maluliyet_Oranı <-dosya_bilgileri$Maluliyet_Orani
  Kusur_Oranı <- dosya_bilgileri$Kusur_Orani
  Gecici_Maluliyet_sure <- dosya_bilgileri$Gecici_Maluliyet_sure
  Kısmi_Odeme_Sayısı <- dosya_bilgileri$Kısmi_Odeme_Sayısı
  Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_1)
  Kısmi_Odeme_Tutarı_1 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_1
  Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_2)
  Kısmi_Odeme_Tutarı_2 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_2
  Yasam_Tablosu <- yasam_tablosu
  
  ## Bakıcı Gideri ----
  
  # Bakıcı_gideri <- "Var"
  # Bakıcı_gideri_suresi <- 12
  
  #
  Bakıcı_gideri <- dosya_bilgileri$Bakıcı_Gideri
  Bakıcı_gideri_suresi <- dosya_bilgileri$`Bakıcı_Gideri_Süresi_(gun)`
  
  
  ## Genel Gelir Tablosu ----
  
  Gelir_tablo <-  Asgari_Tablo %>%
    left_join(gelir_tablosu) %>%
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  
  ## Kaza Tarihi Teminat Limiti ----
  
  Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
    filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)
  
  kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE)) 
  
  
  
  # 1.0  YAS HESAPLAMALARI ----
  
  Hesap_Tarihi <- Sys.Date()
  Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1
  
  kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
  hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 
  
  sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 
  
  
  # 2.0 HESAPLAMADA KULLANILACAK TRH TABLOSU ----
  
  ## 2.1 Hesaplama Tarihine göre beklenen ömür ----
  
  
  PR_TRH_2010 <- if ( Yasam_Tablosu == "TRH-2010") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TRH-2010")
  } else if ( Yasam_Tablosu == "TUIK_20-22") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_20-22")
  } else if ( Yasam_Tablosu == "TUIK_19-21") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_19-21")
  } else if ( Yasam_Tablosu == "TUIK_18-20") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_18-20")
  } else {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "PMF-1931")
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
  
  
  ## 2.2 Şirket Ödeme Tarihine göre beklenen ömür ----
  
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
  
  
  ## 2.3 Pasif Dönem Yas- TRH Tablosu ----
  
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
  
  
  ## 3.0 PARAMATRE TABLOSU ----
  
  
  parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Ad-Soyad", "Kaza Tarihi Yas","Hesap Tarihi Yas", "Cinsiyet", "Kaza Tarihi", 
                                                "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)", "Bakıcı Süresi (gün)",
                                                "Şirket Ödeme Sayısı", "Ödeme Tarihi-1", "Ödeme Tutarı-1", "Ödeme Tarihi-2", "Ödeme Tutarı-2"),
                                  DEĞER = c(dosya_no, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet,as.character(Kaza_Tarihi), 
                                            Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure,Bakıcı_gideri_suresi, 
                                            Kısmi_Odeme_Sayısı, as.character(Kısmi_Odeme_Tarihi_1), Kısmi_Odeme_Tutarı_1, as.character(Kısmi_Odeme_Tarihi_2), Kısmi_Odeme_Tutarı_2)
  )
  
  parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")
  
  parametre_tablosu2 <- tibble(
    "Dosya_No" = dosya_no,
    "Ad-Soyad" = Ad_Soyad,
    "Yas" = kaza_tarihi_yas,
    "Hesap Tarihi Yas" = hesap_tarihi_yas,
    "Cinsiyet" = Cinsiyet, 
    "Kaza Tarihi" = as.character(Kaza_Tarihi),
    "Maluliyet Oranı" = Maluliyet_Oranı,
    "Teknik Faiz" = Teknik_Faiz,
    "Yaşam Tablosu" = Yasam_Tablosu,
    "Kusur Oranı" = Kusur_Oranı,
    "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure,
    "Bakıcı Süresi (gün)"= Bakıcı_gideri_suresi,
    "Kısmi Ödeme Sayısı" = Kısmi_Odeme_Sayısı,
    "Ödeme Tarihi-1" = Kısmi_Odeme_Tarihi_1,
    "Ödeme Tutarı-1" = Kısmi_Odeme_Tutarı_1,
    "Ödeme Tarihi-2" = Kısmi_Odeme_Tarihi_2,
    "Ödeme Tutarı-2" = Kısmi_Odeme_Tutarı_2
  )
  
  
  # 4.0 ŞİRKET ÖDEME TARİHİ İLE HESAPLAMA ----
  
  ## 4.1 Bilinen Dönem Hesaplaması ----
  
  ### 4.1.1 Tam Maluliyet Donemi Tablosu ----
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_tarihi_baslangıc & maluliyet_tarihi_bitis > D_B)
  
  Maluliyet_Donemi_tablosu$Donem_Baslangic[1] <- as.character(maluliyet_tarihi_baslangıc)
  Maluliyet_Donemi_tablosu$Donem_Son[nrow(Maluliyet_Donemi_tablosu)] <- as.character(maluliyet_tarihi_bitis)
  
  Maluliyet_Donemi_tazminat_hesaplama <- Maluliyet_Donemi_tablosu %>% 
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
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  Tam_Maluliyet_Donemi_Tablosu <- Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  ### 4.1.2 Tam Maluliyet Sonrası Dönem Tablosu ----
  
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi_Sirket + 180)
  
  hesaplama_tarihi_sirket_chr <- as.character(Hesap_Tarihi_Sirket)
  
  
  Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_sirket_chr
  
  
  Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    
    
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    
    mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>%
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  Maluliyet_Sonrasi_Donem_Tablosu <- Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  ### 4.1.3 Bilinen Dönem Tablosu ve Tazminatı ----
  
  Bilinen_Donem_Tablosu <- Maluliyet_Sonrasi_Donem_Tablosu
  
  Bilinen_Donem_Tazminatı = sum(Bilinen_Donem_Tablosu$Donem_Tazminat)
  
  
  ## 4.2 Bilinmeyen Dönem Hesaplaması ----
  
  ### 4.2.1 Aktif Donem ----          
  
  bilinen_son_donem_gelir <- Bilinen_Donem_Tablosu$Gelir[nrow(Bilinen_Donem_Tablosu)]
  
  
  bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-sirket_odeme_tarihi_yas) * bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  bilinmeyen_aktif_donem_tazminat <- round(bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                            'Toplam Yıl' = (pasif_donem_yas-sirket_odeme_tarihi_yas),
                                                                            'Toplam Ay' = (pasif_donem_yas-sirket_odeme_tarihi_yas)*12,
                                                                            Gelir = bilinen_son_donem_gelir,
                                                                            'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                            'Kusur Oranı' = Kusur_Oranı,
                                                                            Toplam = bilinmeyen_aktif_donem_tazminat)
  
  
  
  
  ### 4.2.2 Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_sirket <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi_Sirket & Hesap_Tarihi_Sirket > D_B)
  pasif_donem_geliri <- Asgari_Tablo_pasif_donem_sirket$Bekar
  
  
  pasif_donem_beklenen_omur <- 
    
    if (sirket_odeme_tarihi_yas <= pasif_donem_yas) {
      
      (sirket_odeme_tarihi_yas + sirket_odeme_beklenen_omur - pasif_donem_yas) 
      
    } else {
      
      sirket_odeme_beklenen_omur
    }
  
  
  
  bilinmeyen_pasif_donem_tazminat <- pasif_donem_beklenen_omur * pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  
  
  sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                            'Toplam Yıl' = pasif_donem_beklenen_omur,
                                                                            'Toplam Ay' = pasif_donem_beklenen_omur*12,
                                                                            Gelir = pasif_donem_geliri,
                                                                            'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                            'Kusur Oranı' = Kusur_Oranı,
                                                                            Toplam = bilinmeyen_pasif_donem_tazminat)
  
  ### 4.2.3 Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                                     sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(sirket_odeme_tarihi_yas > pasif_donem_yas) {
    sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  
  bilinmeyen_donem_toplam_tazminat <- ifelse(sirket_odeme_tarihi_yas <= pasif_donem_yas, bilinmeyen_aktif_donem_tazminat + bilinmeyen_pasif_donem_tazminat, bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 4.2.4 Şirket Ödeme Tarihi Toplam Tazminat Tablosu ----
  
  sirket_odeme_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                            Tazminat = c(Bilinen_Donem_Tazminatı,bilinmeyen_donem_toplam_tazminat,
                                                                         (Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat))
                                                            
  )
  
  sirket_odeme_tarihi_tazminat <- sirket_odeme_tarihi_toplam_tazminat_tablosu[3,2]
  
  
  
  ### 4.2.5 Şirket Ödeme Tarihi Bakiye Tazminat Tablosu ----
  
  
  sirket_odeme_tarihi_bakiye <- (Kısmi_Odeme_Tutarı_1) - (Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat)
  
  
  sirket_odeme_tarihi_bakiye_hesap_tablosu <- data.frame(Sonuc = c("Şirket Ödeme Tarihi İtibari ile Tazminat Tutarı", "Şirket Ödemesi",  "Bakiye"),
                                                         Tutar = c((Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat),  
                                                                   (Kısmi_Odeme_Tutarı_1), 
                                                                   sirket_odeme_tarihi_bakiye)
  )
  
  
  
  
  # 5.0 HESAP TARİHİ İLE HESAPLAMA (AKTÜERYAL HESAPLAMA) ----
  
  ## 5.1 Hesap tarihi Bilinen Dönem Hesaplaması ----
  
  ### 5.1.1 Hesap tarihi Tam Maluliyet Donemi Tablosu ----
  
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  HT_Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
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
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  gecici_donem_tazminat <- sum(HT_Tam_Maluliyet_Donemi_Tablosu$Donem_Tazminat)
  
  
  ### 5.1.2 Hesap tarihi Tam Maluliyet Sonrası Dönem Tablosu ----
  
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi + 360)
  
  hesaplama_tarihi_chr <- as.character(Hesap_Tarihi)
  
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(HT_Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_chr
  
  
  HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- HT_Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    
    
    
    mutate(Kazanilan_Ay = round(as.numeric(difftime(as.Date(Donem_Sonu), as.Date(Donem_Baslangici), units = "days")) / 30, 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>% 
    
    
    mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  
  ### 5.1.3 Hesap Tarihi Bilinen Dönem Tablosu ve Tazminatı ----
  
  HT_Bilinen_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_Tablosu
  
  HT_Bilinen_Donem_Tazminatı = sum(HT_Bilinen_Donem_Tablosu$Donem_Tazminat)
  
  
  
  ## 5.2 Hesap Tarihi Bilinmeyen Dönem Hesaplaması ----
  
  ### 5.2.1 Hesap Tarihi Aktif Donem ----          
  
  ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu$Gelir[nrow(HT_Bilinen_Donem_Tablosu)]
  
  ht_bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-hesap_tarihi_yas) * ht_bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  ht_bilinmeyen_aktif_donem_tazminat <- round(ht_bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                     'Toplam Yıl' = (pasif_donem_yas-hesap_tarihi_yas),
                                                                     'Toplam Ay' = (pasif_donem_yas-hesap_tarihi_yas)*12,
                                                                     Gelir = ht_bilinen_son_donem_gelir,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_aktif_donem_tazminat)
  
  
  
  ### 5.2.2 Hesap Tarihi Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_ht <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi & Hesap_Tarihi > D_B)
  ht_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_ht$Bekar
  
  ht_pasif_donem_beklenen_omur <- 
    
    if (hesap_tarihi_yas <= pasif_donem_yas) {
      
      (hesap_tarihi_yas + hesaplama_tarihi_beklenen_omur - pasif_donem_yas) 
      
    } else {
      
      hesaplama_tarihi_beklenen_omur
    }
  
  
  ht_bilinmeyen_pasif_donem_tazminat <- ht_pasif_donem_beklenen_omur * ht_pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  
  
  hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                     'Toplam Yıl' = ht_pasif_donem_beklenen_omur,
                                                                     'Toplam Ay' = ht_pasif_donem_beklenen_omur*12,
                                                                     Gelir = ht_pasif_donem_geliri,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 5.2.3 Hesap Tarihi Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                              hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(hesap_tarihi_yas > pasif_donem_yas) {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  
  
  ht_bilinmeyen_donem_toplam_tazminat <- ifelse(hesap_tarihi_yas <= pasif_donem_yas, ht_bilinmeyen_aktif_donem_tazminat + ht_bilinmeyen_pasif_donem_tazminat, ht_bilinmeyen_pasif_donem_tazminat)
  
  HT_Bilinmeyen_Donem_Tazminatı <- round(ht_bilinmeyen_donem_toplam_tazminat,digits = 2)
  
  
  ## 5.3 Toplam Tazminat ----
  
  hesap_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                     Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, (HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı)))
  
  
  Toplam_Tazminat <- HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı
  Toplam_Tazminat <- round(Toplam_Tazminat,digits = 2)
  
  
  ## 5.4 Şirket Ödemesinin Hesap Tarihli Değeri ----
  
  ### ŞİRKET ÖDEMELERİ 
  
  # odeme1_gecen_gun <- as.numeric(Hesap_Tarihi - parametre_tablosu2$`Ödeme Tarihi-1`)
  # odeme2_gecen_gun <- as.numeric(Hesap_Tarihi - parametre_tablosu2$`Ödeme Tarihi-2`)
  # 
  # 
  # odeme_hesaplama_degeri_1 <- as.numeric(if (Kısmi_Odeme_Tutarı_1 == "none") {0} else {Kısmi_Odeme_Tutarı_1 * (1 + (faiz_oranı/100)*(odeme1_gecen_gun / 365)) })
  
  # odeme_hesaplama_degeri_2 <- as.numeric(if (Kısmi_Odeme_Tutarı_2 == "none") {0} else {Kısmi_Odeme_Tutarı_2 * (1 + (faiz_oranı/100)*(odeme2_gecen_gun / 365)) })
  
  
  odeme1_gecen_gun_9faiz <- as.numeric(as.Date(dmy("01.06.2024")) - parametre_tablosu2$`Ödeme Tarihi-1`)
  
  odeme1_gecen_gun_24faiz <- as.numeric(as.Date(Hesap_Tarihi) - as.Date(dmy("01.06.2024")))
  
  #
  
  
  donemde_elde_edilen_faiz9 <- (faiz_oranı/100) * (odeme1_gecen_gun_9faiz/365)
  donemde_elde_edilen_faiz24 <- (faiz_oranı2/100) * (odeme1_gecen_gun_24faiz/365)
  
  Sirket_Odeme_Tablosu_9faiz <- tibble(
    "Ödeme Tarihi" = as.character(format(ymd(Kısmi_Odeme_Tarihi_1),"%d/%m/%Y")), 
    "Hesaplama Tarihi" = as.character(format(dmy("30.05.2024"),"%d/%m/%Y")),
    "Geçen Gün Sayısı" = odeme1_gecen_gun_9faiz,
    "Faiz Oranı" = scales::percent(faiz_oranı, scale = 1),
    "Donemde Elde Edilen Faiz" = scales::percent(donemde_elde_edilen_faiz9, accuracy = 0.0001)
  )
  
  Sirket_Odeme_Tablosu_24faiz <- tibble(
    "Ödeme Tarihi" = as.character(format(dmy("01.06.2024"),"%d/%m/%Y")), 
    "Hesaplama Tarihi" = as.character(format(ymd(Hesap_Tarihi),"%d/%m/%Y")),
    "Geçen Gün Sayısı" = odeme1_gecen_gun_24faiz,
    "Faiz Oranı" = scales::percent(faiz_oranı2, scale = 1),
    "Donemde Elde Edilen Faiz" = scales::percent(donemde_elde_edilen_faiz24, accuracy = 0.0001)
  )
  
  
  Sirket_Odeme_Tablosu <- bind_rows(Sirket_Odeme_Tablosu_9faiz, Sirket_Odeme_Tablosu_24faiz)
  
  if (Kısmi_Odeme_Sayısı >= 1) {
    knitr::kable(Sirket_Odeme_Tablosu, caption = "Şirket Ödeme Tablosu") } else {str_glue("Dosya kapsamında şirket ödemesi görülmemiştir.")}
  
  
  
  toplam_faiz <- donemde_elde_edilen_faiz9 + donemde_elde_edilen_faiz24
  
  toplam_kismi_odeme <- (1+toplam_faiz)*Kısmi_Odeme_Tutarı_1
  
  
  # 6.0 BAKICI GİDERİ HESAPLAMASI ----
  
  Bakici_Gideri_Baslangic_Tarihi <- Kaza_Tarihi
  Bakici_Gideri_Bitis_Tarihi <- Kaza_Tarihi + Bakıcı_gideri_suresi
  
  Bakici_Gideri_Donemi_filtered <- Gelir_tablo %>% 
    filter(D_S >= Bakici_Gideri_Baslangic_Tarihi & Bakici_Gideri_Bitis_Tarihi > D_B)
  
  
  bakici_gideri_baslangic_tarihi_chr <- as.character(Bakici_Gideri_Baslangic_Tarihi)
  bakici_gideri_bitis_tarihi_chr <- as.character(Bakici_Gideri_Bitis_Tarihi)
  
  
  Bakici_Gideri_Donemi_filtered$Donem_Baslangic[1] <- bakici_gideri_baslangic_tarihi_chr
  Bakici_Gideri_Donemi_filtered$Donem_Son[nrow(Bakici_Gideri_Donemi_filtered)] <- bakici_gideri_bitis_tarihi_chr
  
  
  Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu <- Bakici_Gideri_Donemi_filtered %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    mutate(Kazanilan_Gun = signif(as.numeric(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Baslangici), "day")) , digits = 2)) %>%
    
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Donem_Baslangici, Donem_Son, Kazanilan_Gun, Brut, Kusur_Oranı) %>% 
    rename("Aylık_Bakıcı_Gideri" = Brut) %>% 
    mutate(Donem_Bakıcı_Gideri = round(Kazanilan_Gun * Aylık_Bakıcı_Gideri/30 * Kusur_Oranı/100, digits = 2))  
  
  
  
  Toplam_Bakici_gideri_hesaplama = sum(Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu$Donem_Bakıcı_Gideri) 
  
  
  Toplam_Bakici_Gideri <- if (Bakıcı_gideri == "Var") { Toplam_Bakici_gideri_hesaplama } else { 0 }
  
  
  # 7.0 BAKİYE TAZMİNAT ve SON TABLO ----
  
  kaza_tarihi_teminat_limiti2 <- as.numeric(kaza_tarihi_teminat_limiti)
  
  bakiye_tazminat <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti2) {
    kaza_tarihi_teminat_limiti2 - (Kısmi_Odeme_Tutarı_1)
  } else {
    Toplam_Tazminat - (toplam_kismi_odeme)
  }
  
  
  
  son_hesaplama_tablosu1 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat","Teminat Limiti", "Kısmi Ödeme", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat, kaza_tarihi_teminat_limiti,  (toplam_kismi_odeme), bakiye_tazminat)
  )
  
  
  son_hesaplama_tablosu2 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat",  "Teminat Limiti","Kısmi Ödeme", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat,  kaza_tarihi_teminat_limiti, (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2), bakiye_tazminat)
  )
  
  
  # 8.0 SONUÇ ----
  
  sonuc_tablosu <- tibble(
    "Dosya_No" = dosya_no,
    "Rapor_Turu" = rapor_turu,
    "Gecici" = gecici_donem_tazminat,
    "Surekli" = bakiye_tazminat,
    "Bakici" = Toplam_Bakici_Gideri,
    "Toplam" = sum(Gecici+Surekli+Bakici),
    "Odeme_HT_Degeri" = round(toplam_kismi_odeme,digits = 2)
    
  )
  
  sonuc_tablosu
  
  
}

tum_rapor_1odeme(dosya_bilgiler[5, ], "TRH-2010")


# F. TÜM RAPOR - 2 ÖDEME ----

tum_rapor_2odeme <- function(data, yasam_tablosu, teknik_faiz) {
  
  
  ## Dosya Bilgileri ----
  
  # dosya_bilgiler <- read_excel("../KURUMSAL_PROINSURE/data/dosya_bilgiler.xlsx", sheet = "Sayfa1")
  
  Asgari_Tablo <- read_excel("../KURUMSAL_PROINSURE/data/Asgari_Ucret_Tablosu_rvz.xlsx", sheet = "Program")
  
  Teminat_Limit_Tablosu <- read_excel("../KURUMSAL_PROINSURE/data/Teminat_Limit_Tablosu_rvz.xlsx", sheet = "Teminat")
  
  # PREPARE DATA ----
  
  # dosya_bilgileri <- dosya_bilgiler[4, ] # Use this to test the code
  
  dosya_bilgileri <- data 
  
  # Genel Bilgiler ----
  
  Teknik_Faiz <- teknik_faiz
  pasif_donem_yas <- 60
  faiz_oranı <- 9
  faiz_oranı2 <- 24
  
  
  ## Manuel Gelir Tablosu ----
  
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
  
  
  ## Kişisel Bilgiler ----
  
  dosya_no <- as.character(dosya_bilgileri$Dosya_No)
  rapor_turu <- dosya_bilgileri$Rapor_Turu
  Ad_Soyad <- dosya_bilgileri$Ad_Soyad
  Cinsiyet <- dosya_bilgileri$Cinsiyet
  Dogum_Tarihi <- as.Date(dosya_bilgileri$Dogum_Tarihi)
  Gelir_Durumu <- dosya_bilgileri$Gelir
  Kaza_Tarihi <- as.Date(dosya_bilgileri$Kaza_Tarihi)
  Maluliyet_Oranı <-dosya_bilgileri$Maluliyet_Orani
  Kusur_Oranı <- dosya_bilgileri$Kusur_Orani
  Gecici_Maluliyet_sure <- dosya_bilgileri$Gecici_Maluliyet_sure
  Kısmi_Odeme_Sayısı <- dosya_bilgileri$Kısmi_Odeme_Sayısı
  Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_1)
  Kısmi_Odeme_Tutarı_1 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_1
  Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgileri$Kısmi_Odeme_Tarihi_2)
  Kısmi_Odeme_Tutarı_2 <- dosya_bilgileri$Kısmi_Odeme_Tutarı_2
  Yasam_Tablosu <- yasam_tablosu
  
  ## Bakıcı Gideri ----
  
  # Bakıcı_gideri <- "Var"
  # Bakıcı_gideri_suresi <- 12
  
  #
  Bakıcı_gideri <- dosya_bilgileri$Bakıcı_Gideri
  Bakıcı_gideri_suresi <- dosya_bilgileri$`Bakıcı_Gideri_Süresi_(gun)`
  
  
  ## Genel Gelir Tablosu ----
  
  Gelir_tablo <-  Asgari_Tablo %>%
    left_join(gelir_tablosu) %>%
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  
  ## Kaza Tarihi Teminat Limiti ----
  
  Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
    separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
    mutate(D_B = as.Date(Donem_Baslangic),
           D_S = as.Date(Donem_Son))
  
  kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
    filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)
  
  kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE)) 
  
  
  
  # 1.0  YAS HESAPLAMALARI ----
  
  Hesap_Tarihi <- Sys.Date()
  Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1
  
  kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
  hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 
  
  sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 
  
  
  # 2.0 HESAPLAMADA KULLANILACAK TRH TABLOSU ----
  
  ## 2.1 Hesaplama Tarihine göre beklenen ömür ----
  
  
  PR_TRH_2010 <- if ( Yasam_Tablosu == "TRH-2010") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TRH-2010")
  } else if ( Yasam_Tablosu == "TUIK_20-22") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_20-22")
  } else if ( Yasam_Tablosu == "TUIK_19-21") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_19-21")
  } else if ( Yasam_Tablosu == "TUIK_18-20") {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "TUIK_18-20")
  } else {
    read_excel("../KURUMSAL_PROINSURE/data/All_Tables.xlsx", sheet = "PMF-1931")
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
  
  
  ## 2.2 Şirket Ödeme Tarihine göre beklenen ömür ----
  
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
  
  
  ## 2.3 Pasif Dönem Yas- TRH Tablosu ----
  
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
  
  
  # 3- PARAMATRE TABLOSU ----
  
  
  parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Ad-Soyad", "Kaza Tarihi Yas","Hesap Tarihi Yas", "Cinsiyet", "Kaza Tarihi", 
                                                "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)", "Bakıcı Süresi (gün)",
                                                "Şirket Ödeme Sayısı", "Ödeme Tarihi-1", "Ödeme Tutarı-1", "Ödeme Tarihi-2", "Ödeme Tutarı-2"),
                                  DEĞER = c(dosya_no, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet,as.character(Kaza_Tarihi), 
                                            Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure,Bakıcı_gideri_suresi, 
                                            Kısmi_Odeme_Sayısı, as.character(Kısmi_Odeme_Tarihi_1), Kısmi_Odeme_Tutarı_1, as.character(Kısmi_Odeme_Tarihi_2), Kısmi_Odeme_Tutarı_2)
  )
  
  parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")
  
  parametre_tablosu2 <- tibble(
    "Dosya_No" = dosya_no,
    "Ad-Soyad" = Ad_Soyad,
    "Yas" = kaza_tarihi_yas,
    "Hesap Tarihi Yas" = hesap_tarihi_yas,
    "Cinsiyet" = Cinsiyet, 
    "Kaza Tarihi" = as.character(Kaza_Tarihi),
    "Maluliyet Oranı" = Maluliyet_Oranı,
    "Teknik Faiz" = Teknik_Faiz,
    "Yaşam Tablosu" = Yasam_Tablosu,
    "Kusur Oranı" = Kusur_Oranı,
    "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure,
    "Bakıcı Süresi (gün)"= Bakıcı_gideri_suresi,
    "Kısmi Ödeme Sayısı" = Kısmi_Odeme_Sayısı,
    "Ödeme Tarihi-1" = Kısmi_Odeme_Tarihi_1,
    "Ödeme Tutarı-1" = Kısmi_Odeme_Tutarı_1,
    "Ödeme Tarihi-2" = Kısmi_Odeme_Tarihi_2,
    "Ödeme Tutarı-2" = Kısmi_Odeme_Tutarı_2
  )
  
  
  
  
  # 4.0 ŞİRKET ÖDEME TARİHİ İLE HESAPLAMA ----
  
  ## 4.1 Bilinen Dönem Hesaplaması ----
  
  ### 4.1.1 Tam Maluliyet Donemi Tablosu ----
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_tarihi_baslangıc & maluliyet_tarihi_bitis > D_B)
  
  Maluliyet_Donemi_tablosu$Donem_Baslangic[1] <- as.character(maluliyet_tarihi_baslangıc)
  Maluliyet_Donemi_tablosu$Donem_Son[nrow(Maluliyet_Donemi_tablosu)] <- as.character(maluliyet_tarihi_bitis)
  
  Maluliyet_Donemi_tazminat_hesaplama <- Maluliyet_Donemi_tablosu %>% 
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>% 
    mutate(Kazanilan_Ay = signif(as.numeric(time_length(Donem_Sonu - Donem_Baslangici, "day")/30) , digits = 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = 100) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  Tam_Maluliyet_Donemi_Tablosu <- Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  ### 4.1.2 Tam Maluliyet Sonrası Dönem Tablosu ----
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi_Sirket + 180)
  
  hesaplama_tarihi_sirket_chr <- as.character(Hesap_Tarihi_Sirket)
  
  
  Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_sirket_chr
  
  
  Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    mutate(Kazanilan_Ay = round(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Baslangici), "month") , digits = 1)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>%
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  Maluliyet_Sonrasi_Donem_Tablosu <- Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  ### 4.1.3 Bilinen Dönem Tablosu ve Tazminatı ----
  
  Bilinen_Donem_Tablosu <- Maluliyet_Sonrasi_Donem_Tablosu
  
  Bilinen_Donem_Tazminatı = sum(Bilinen_Donem_Tablosu$Donem_Tazminat)
  
  
  
  ## 4.2 Bilinmeyen Dönem Hesaplaması ----
  
  ### 4.2.1 Aktif Donem ----          
  
  bilinen_son_donem_gelir <- Bilinen_Donem_Tablosu$Gelir[nrow(Bilinen_Donem_Tablosu)]
  
  
  bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-sirket_odeme_tarihi_yas) * bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  bilinmeyen_aktif_donem_tazminat <- round(bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                            'Toplam Yıl' = (pasif_donem_yas-sirket_odeme_tarihi_yas),
                                                                            'Toplam Ay' = (pasif_donem_yas-sirket_odeme_tarihi_yas)*12,
                                                                            Gelir = bilinen_son_donem_gelir,
                                                                            'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                            'Kusur Oranı' = Kusur_Oranı,
                                                                            Toplam = bilinmeyen_aktif_donem_tazminat)
  
  
  ### 4.2.2 Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_sirket <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi_Sirket & Hesap_Tarihi_Sirket > D_B)
  pasif_donem_geliri <- Asgari_Tablo_pasif_donem_sirket$Bekar
  
  
  pasif_donem_beklenen_omur <- 
    
    if (sirket_odeme_tarihi_yas <= pasif_donem_yas) {
      
      (sirket_odeme_tarihi_yas + sirket_odeme_beklenen_omur - pasif_donem_yas) 
      
    } else {
      
      sirket_odeme_beklenen_omur
    }
  
  
  
  bilinmeyen_pasif_donem_tazminat <- pasif_donem_beklenen_omur * pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  
  
  sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                            'Toplam Yıl' = pasif_donem_beklenen_omur,
                                                                            'Toplam Ay' = pasif_donem_beklenen_omur*12,
                                                                            Gelir = pasif_donem_geliri,
                                                                            'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                            'Kusur Oranı' = Kusur_Oranı,
                                                                            Toplam = bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 4.2.3 Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                                     sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(sirket_odeme_tarihi_yas > pasif_donem_yas) {
    sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  
  bilinmeyen_donem_toplam_tazminat <- ifelse(sirket_odeme_tarihi_yas <= pasif_donem_yas, bilinmeyen_aktif_donem_tazminat + bilinmeyen_pasif_donem_tazminat, bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 4.2.4 Şirket Ödeme Tarihi Toplam Tazminat Tablosu ----
  
  sirket_odeme_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                            Tazminat = c(Bilinen_Donem_Tazminatı,bilinmeyen_donem_toplam_tazminat,
                                                                         (Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat))
                                                            
  )
  
  
  sirket_odeme_tarihi_tazminat <- sirket_odeme_tarihi_toplam_tazminat_tablosu[3,2]
  
  
  
  ### 4.2.5 Şirket Ödeme Tarihi Bakiye Tazminat Tablosu ----
  
  
  sirket_odeme_tarihi_bakiye <- (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2) - (Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat)
  
  
  sirket_odeme_tarihi_bakiye_hesap_tablosu <- data.frame(Sonuc = c("Şirket Ödeme Tarihi İtibari ile Tazminat Tutarı", "Şirket Ödemesi",  "Bakiye"),
                                                         Tutar = c((Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat),  
                                                                   (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2), 
                                                                   sirket_odeme_tarihi_bakiye)
  )
  
  
  # 5.0 HESAP TARİHİ İLE HESAPLAMA (AKTÜERYAL HESAPLAMA) ----
  
  ## 5.1 Hesap tarihi Bilinen Dönem Hesaplaması ----
  
  ### 5.1.1 Hesap tarihi Tam Maluliyet Donemi Tablosu ----
  
  
  maluliyet_tarihi_baslangıc <- Kaza_Tarihi
  maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)
  
  HT_Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_tarihi_baslangıc & maluliyet_tarihi_bitis > D_B)
  
  HT_Maluliyet_Donemi_tablosu$Donem_Baslangic[1] <- as.character(maluliyet_tarihi_baslangıc)
  HT_Maluliyet_Donemi_tablosu$Donem_Son[nrow(HT_Maluliyet_Donemi_tablosu)] <- as.character(maluliyet_tarihi_bitis)
  
  HT_Maluliyet_Donemi_tazminat_hesaplama <- HT_Maluliyet_Donemi_tablosu %>% 
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>% 
    mutate(Kazanilan_Ay = signif(as.numeric(time_length(Donem_Sonu - Donem_Baslangici, "day")/30) , digits = 2)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = 100) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
           Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  gecici_donem_tazminat <- sum(HT_Tam_Maluliyet_Donemi_Tablosu$Donem_Tazminat)
  
  
  ### 5.1.2 Hesap tarihi Tam Maluliyet Sonrası Dönem Tablosu ----
  
  
  maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
  
  maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
    filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi + 360)
  
  hesaplama_tarihi_chr <- as.character(Hesap_Tarihi)
  
  
  
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
  HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(HT_Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_chr
  
  
  HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- HT_Maluliyet_Sonrasi_Donem_tablosu %>% 
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    mutate(Kazanilan_Ay = round(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Baslangici), "month") , digits = 1)) %>%
    
    mutate(Kazanilan_Ay = case_when(
      Kazanilan_Ay >= 11.96 ~ plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay > 6.1 ~  plyr::round_any(Kazanilan_Ay, 0.1),
      Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1),
      Kazanilan_Ay < 5.9 ~ plyr::round_any(Kazanilan_Ay, 0.1),
      TRUE ~ Kazanilan_Ay)) %>%
    
    mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
    rename("Gelir" = Gelir_Durumu) %>% 
    mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 
  
  
  HT_Maluliyet_Sonrasi_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
    select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)
  
  
  
  ### 5.1.3 Hesap Tarihi Bilinen Dönem Tablosu ve Tazminatı ----
  
  HT_Bilinen_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_Tablosu
  
  HT_Bilinen_Donem_Tazminatı = sum(HT_Bilinen_Donem_Tablosu$Donem_Tazminat)
  
  
  
  ## 5.2 Hesap Tarihi Bilinmeyen Dönem Hesaplaması ----
  
  ### 5.2.1 Hesap Tarihi Aktif Donem ----          
  
  ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu$Gelir[nrow(HT_Bilinen_Donem_Tablosu)]
  
  ht_bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-hesap_tarihi_yas) * ht_bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  ht_bilinmeyen_aktif_donem_tazminat <- round(ht_bilinmeyen_aktif_donem_tazminat,digits = 2)
  
  hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                     'Toplam Yıl' = (pasif_donem_yas-hesap_tarihi_yas),
                                                                     'Toplam Ay' = (pasif_donem_yas-hesap_tarihi_yas)*12,
                                                                     Gelir = ht_bilinen_son_donem_gelir,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_aktif_donem_tazminat)
  
  
  
  ### 5.2.2 Hesap Tarihi Pasif Dönem ----
  
  Asgari_Tablo_pasif_donem_ht <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi & Hesap_Tarihi > D_B)
  ht_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_ht$Bekar
  
  ht_pasif_donem_beklenen_omur <- 
    
    if (hesap_tarihi_yas <= pasif_donem_yas) {
      
      (hesap_tarihi_yas + hesaplama_tarihi_beklenen_omur - pasif_donem_yas) 
      
    } else {
      
      hesaplama_tarihi_beklenen_omur
    }
  
  
  
  
  ht_bilinmeyen_pasif_donem_tazminat <- ht_pasif_donem_beklenen_omur * ht_pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
  
  
  hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                     'Toplam Yıl' = ht_pasif_donem_beklenen_omur,
                                                                     'Toplam Ay' = ht_pasif_donem_beklenen_omur*12,
                                                                     Gelir = ht_pasif_donem_geliri,
                                                                     'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                     'Kusur Oranı' = Kusur_Oranı,
                                                                     Toplam = ht_bilinmeyen_pasif_donem_tazminat)
  
  
  
  ### 5.2.3 Hesap Tarihi Bilinmeyen Dönem Toplam Tazminat Tablosu ----
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                              hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)
  
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(hesap_tarihi_yas > pasif_donem_yas) {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
  } else {
    hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
  }
  
  
  
  ht_bilinmeyen_donem_toplam_tazminat <- ifelse(hesap_tarihi_yas <= pasif_donem_yas, ht_bilinmeyen_aktif_donem_tazminat + ht_bilinmeyen_pasif_donem_tazminat, ht_bilinmeyen_pasif_donem_tazminat)
  
  HT_Bilinmeyen_Donem_Tazminatı <- round(ht_bilinmeyen_donem_toplam_tazminat,digits = 2)
  
  
  
  ## 5.3 Toplam Tazminat ----
  
  hesap_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                     Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, (HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı)))
  
  
  Toplam_Tazminat <- HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı
  Toplam_Tazminat <- round(Toplam_Tazminat,digits = 2)
  
  
  
  
  ## 5.4 Şirket Ödemesinin Hesap Tarihli Değeri ----
  
  ### ŞİRKET ÖDEMELERİ 
  
  odeme1_gecen_gun <- as.numeric(Hesap_Tarihi - parametre_tablosu2$`Ödeme Tarihi-1`)
  odeme2_gecen_gun <- as.numeric(Hesap_Tarihi - parametre_tablosu2$`Ödeme Tarihi-2`)
  
  
  odeme_hesaplama_degeri_1 <- as.numeric(if (Kısmi_Odeme_Tutarı_1 == "none") {0} else {Kısmi_Odeme_Tutarı_1 * (1 + (faiz_oranı/100)*(odeme1_gecen_gun / 365)) })
  
  odeme_hesaplama_degeri_2 <- as.numeric(if (Kısmi_Odeme_Tutarı_2 == "none") {0} else {Kısmi_Odeme_Tutarı_2 * (1 + (faiz_oranı/100)*(odeme2_gecen_gun / 365)) })
  
  odeme_hesaplama_degeri_1 + odeme_hesaplama_degeri_2
  
  Sirket_Odeme_Tablosu_1 <- tibble(
    "Ödeme Tarihi" = Kısmi_Odeme_Tarihi_1,
    "Hesaplama Tarihi" = as.character(format(ymd(Hesap_Tarihi),"%d/%m/%Y")),
    "Geçen Gün Sayısı" = odeme1_gecen_gun,
    "Ödenen Tutar" = if (Kısmi_Odeme_Tutarı_1 == "none") {0} else {Kısmi_Odeme_Tutarı_1},
    "Faiz Oranı" = scales::percent(faiz_oranı, scale = 1),
    "Hesap Tarihi Tutarı" = odeme_hesaplama_degeri_1
  )
  
  Sirket_Odeme_Tablosu_2 <- tibble(
    "Ödeme Tarihi" = Kısmi_Odeme_Tarihi_2,
    "Hesaplama Tarihi" = as.character(format(ymd(Hesap_Tarihi),"%d/%m/%Y")),
    "Geçen Gün Sayısı" = odeme2_gecen_gun,
    "Ödenen Tutar" = if (Kısmi_Odeme_Tutarı_2 == 0) {0} else {Kısmi_Odeme_Tutarı_2},
    "Faiz Oranı" = scales::percent(faiz_oranı, scale = 1),
    "Hesap Tarihi Tutarı" = odeme_hesaplama_degeri_2
  )
  
  
  Sirket_Odeme_Tablosu <- bind_rows(Sirket_Odeme_Tablosu_1, Sirket_Odeme_Tablosu_2) %>% 
    filter(`Ödenen Tutar` != 0)
  
  toplam_kismi_odeme <- sum(Sirket_Odeme_Tablosu$`Hesap Tarihi Tutarı`)
  
  
  # 6- BAKICI GİDERİ HESAPLAMASI ----
  
  
  Bakici_Gideri_Baslangic_Tarihi <- Kaza_Tarihi
  Bakici_Gideri_Bitis_Tarihi <- Kaza_Tarihi + Bakıcı_gideri_suresi
  
  Bakici_Gideri_Donemi_filtered <- Gelir_tablo %>% 
    filter(D_S >= Bakici_Gideri_Baslangic_Tarihi & Bakici_Gideri_Bitis_Tarihi > D_B)
  
  bakici_gideri_baslangic_tarihi_chr <- as.character(Bakici_Gideri_Baslangic_Tarihi)
  bakici_gideri_bitis_tarihi_chr <- as.character(Bakici_Gideri_Bitis_Tarihi)
  
  
  Bakici_Gideri_Donemi_filtered$Donem_Baslangic[1] <- bakici_gideri_baslangic_tarihi_chr
  Bakici_Gideri_Donemi_filtered$Donem_Son[nrow(Bakici_Gideri_Donemi_filtered)] <- bakici_gideri_bitis_tarihi_chr
  
  
  Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu <- Bakici_Gideri_Donemi_filtered %>% 
    
    
    mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
           Donem_Sonu = as.Date(Donem_Son)) %>%
    mutate(Kazanilan_Gun = signif(as.numeric(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Baslangici), "day")) , digits = 2)) %>%
    
    mutate(Kusur_Oranı = Kusur_Oranı) %>% 
    select(Donem_Baslangici, Donem_Son, Kazanilan_Gun, Brut, Kusur_Oranı) %>% 
    rename("Aylık_Bakıcı_Gideri" = Brut) %>% 
    mutate(Donem_Bakıcı_Gideri = round(Kazanilan_Gun * Aylık_Bakıcı_Gideri/30 * Kusur_Oranı/100, digits = 2))  
  
  
  Toplam_Bakici_gideri_hesaplama = sum(Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu$Donem_Bakıcı_Gideri) 
  
  
  Toplam_Bakici_Gideri <- if (Bakıcı_gideri == "Var") { Toplam_Bakici_gideri_hesaplama } else { 0 }
  
  
  
  # 7.0 BAKİYE TAZMİNAT ve SON TABLO ----
  
  kaza_tarihi_teminat_limiti2 <- as.numeric(kaza_tarihi_teminat_limiti)
  
  
  bakiye_tazminat <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti2) {
    kaza_tarihi_teminat_limiti2 - (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2)
  } else {
    Toplam_Tazminat - (toplam_kismi_odeme)
  }
  
  
  
  
  son_hesaplama_tablosu1 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat","Teminat Limiti", "Kısmi Ödeme", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat, kaza_tarihi_teminat_limiti,  (toplam_kismi_odeme), bakiye_tazminat)
  )
  
  
  son_hesaplama_tablosu2 <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat",  "Teminat Limiti","Kısmi Ödeme", "Bakiye Tazminat Tutarı"),
                                       Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat,  kaza_tarihi_teminat_limiti, (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2), bakiye_tazminat)
  )
  
  
  
  
  # 8.0 SONUÇ ----
  
  sonuc_tablosu <- tibble(
    "Dosya_No" = dosya_no,
    "Rapor_Turu" = rapor_turu,
    "Gecici" = gecici_donem_tazminat,
    "Surekli" = bakiye_tazminat,
    "Bakici" = Toplam_Bakici_Gideri,
    "Toplam" = sum(Gecici+Surekli+Bakici),
    "Odeme_HT_Degeri" = round(toplam_kismi_odeme,digits = 2)
    
  )
  
  sonuc_tablosu
  
  
}

tum_rapor_2odeme(dosya_bilgiler[7, ], "TRH-2010", 2)

