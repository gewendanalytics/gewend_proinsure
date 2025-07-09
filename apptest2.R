

# app.R - Tam D??zeltilmi?? Versiyon

# Dosya ve veri y??netimi fonksiyonlar??
data_file <- "data/proinsure_data.csv"

if (!dir.exists("data")) {
  dir.create("data")
}

load_data <- function(file) {
  if (file.exists(file)) {
    data <- read.csv(file, stringsAsFactors = FALSE)
    if (!"Duration" %in% names(data)) {
      data$Duration <- 1
    }
    if (!"ID" %in% names(data)) {
      data$ID <- sapply(1:nrow(data), function(x) {
        letters <- sample(LETTERS, 2, replace = TRUE)
        numbers <- sample(0:9, 3, replace = TRUE)
        pattern <- paste0(paste(letters, collapse = ""), paste(numbers, collapse = ""))
        return(pattern)
      })
    }
    data
  } else {
    data.frame(
      ID = character(),
      user_name = character(),
      DosyaNo = character(),
      Cinsiyet = character(),
      DogumTarihi = character(),
      Gelir = character(),
      KazaTarihi = character(),
      MaluliyetOran = numeric(),
      KusurOran = numeric(),
      GeciciMaluliyetSure = numeric(),
      KismiOdemeSay = numeric(),
      RaporTur = character(),
      Duration = numeric(),
      EntryTime = character(),
      stringsAsFactors = FALSE
    )
  }
}

save_data <- function(data, file) {
  write.csv(data, file, row.names = FALSE)
}

generate_id <- function() {
  letters <- sample(LETTERS, 2, replace = TRUE)
  numbers <- sample(0:9, 3, replace = TRUE)
  pattern <- paste0(paste(letters, collapse = ""), paste(numbers, collapse = ""))
  return(pattern)
}

# install.packages("officedown")
# Gerekli k??t??phaneleri y??kleyelim
library(shiny)
library(shinydashboard)  
library(shinyWidgets)
library(shinythemes)
library(shinyauthr)
library(plotly)
library(DT)
library(fresh)
library(tibble)
library(scales)
library(lubridate)
library(ggplot2)
library(shinyBS)
library(reactable)
library(formattable)
library(shinyalert)
library(kableExtra)
library(flextable)
library(tidyquant)
library(markdown)
library(shinyjs)
library(readxl)
library(waiter)
library(writexl)
library(dplyr)
library(tidyverse)

Sys.setlocale(locale = "Turkish")

# 2.0 LOAD SOURCES & DATA ----

# source("../PROINSURE_GEWEND/modules/module_login.R")


my_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
  
  
  :root {
    /* Futuristik Digital Color Palette */
    --cyber-blue: #00d4ff;
    --electric-purple: #8b5cf6;
    --neon-pink: #ff0080;
    --plasma-orange: #ff6b35;
    --quantum-green: #00ff88;
    --digital-teal: #06ffa5;
    --aurora-blue: #0099ff;
    --matrix-green: #00ff41;
    
    /* Backgrounds */
    --bg-primary: #0a0e1a;
    --bg-secondary: #1a1f2e;
    --bg-tertiary: #2a2f3e;
    --surface: rgba(255, 255, 255, 0.05);
    --surface-glass: rgba(255, 255, 255, 0.1);
    --surface-hover: rgba(255, 255, 255, 0.15);
    
    /* Text Colors */
    --text-primary: #ffffff;
    --text-secondary: #b0b7c3;
    --text-accent: #00d4ff;
    --text-muted: #6b7280;
    
    /* Effects */
    --glow-cyan: 0 0 20px rgba(0, 212, 255, 0.3);
    --glow-purple: 0 0 20px rgba(139, 92, 246, 0.3);
    --glow-pink: 0 0 20px rgba(255, 0, 128, 0.3);
    --glow-green: 0 0 20px rgba(0, 255, 136, 0.3);
    --shadow-digital: 0 8px 32px rgba(0, 212, 255, 0.15);
    --shadow-cyber: 0 16px 48px rgba(139, 92, 246, 0.2);
    
    --radius-sm: 8px;
    --radius-md: 12px;
    --radius-lg: 16px;
    --radius-xl: 20px;
  }
  
  /* Base Styles - Futuristik */
  body { 
    background: 
      radial-gradient(circle at 20% 80%, rgba(139, 92, 246, 0.15) 0%, transparent 50%),
      radial-gradient(circle at 80% 20%, rgba(0, 212, 255, 0.15) 0%, transparent 50%),
      linear-gradient(135deg, var(--bg-primary) 0%, var(--bg-secondary) 50%, var(--bg-tertiary) 100%);
    color: var(--text-primary);
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif; 
    margin: 0; 
    padding: 0;
    min-height: 100vh;
    font-size: 14px;
    line-height: 1.5;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    overflow-x: hidden;
  }

  /* Animated Background */
  #canvas-container {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
    opacity: 0.4;
  }

  /* Futuristik Header */
  .header-container {
    background: rgba(26, 31, 46, 0.95);
    backdrop-filter: blur(20px);
    -webkit-backdrop-filter: blur(20px);
    border-bottom: 2px solid;
    border-image: linear-gradient(90deg, var(--cyber-blue), var(--neon-pink), var(--electric-purple)) 1;
    box-shadow: var(--shadow-digital);
    padding: 20px 24px;
    margin-bottom: 32px;
    position: sticky;
    top: 0;
    z-index: 100;
  }
  
  .header-content {
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  
  .logo-section h1 {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--neon-pink) 50%, var(--electric-purple) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    font-size: 2.5rem;
    font-weight: 700;
    margin: 0;
    letter-spacing: -0.025em;
    text-shadow: var(--glow-cyan);
    animation: pulse-glow 3s ease-in-out infinite alternate;
  }
  
  @keyframes pulse-glow {
    from { filter: drop-shadow(0 0 5px rgba(0, 212, 255, 0.3)); }
    to { filter: drop-shadow(0 0 20px rgba(0, 212, 255, 0.6)); }
  }
  
  .logo-section p {
    color: var(--text-secondary);
    margin: 4px 0 0 0;
    font-size: 1rem;
    font-weight: 500;
    text-shadow: var(--glow-cyan);
  }

  /* Main Layout - Full Width Cyber */
.main-layout {
  background: rgba(26, 31, 46, 0.6);
  backdrop-filter: blur(20px);
  border: 1px solid rgba(0, 212, 255, 0.2);
  border-radius: var(--radius-xl);
  margin: 0 auto 32px; /* 0 200px yerine 0 auto yap */
  padding: 32px;
  box-shadow: var(--shadow-cyber), inset 0 1px 0 rgba(255, 255, 255, 0.1);
  min-height: calc(100vh - 180px);
  position: relative;
  overflow: hidden;
  
  /* Bu sat??rlar?? ekle */
  display: flex !important;
  gap: 32px;
  align-items: flex-start !important;
  max-width: none !important; /* 1200px'i kald??r */
  width: calc(100% - 300px) !important; /* 100px'den 400px'e de??i??tir (200+200=400) */
}
  
  .main-layout::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 3px;
    background: linear-gradient(90deg, var(--cyber-blue) 0%, var(--neon-pink) 50%, var(--electric-purple) 100%);
    border-radius: var(--radius-xl) var(--radius-xl) 0 0;
  }

  /* Cyber Sidebar */
  .sidebar-container {
    width: 340px;
    background: rgba(26, 31, 46, 0.8);
    backdrop-filter: blur(25px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-lg);
    box-shadow: var(--glow-cyan), inset 0 1px 0 rgba(255, 255, 255, 0.1);
    margin-right: 32px;
    overflow: hidden;
    height: fit-content;
    position: sticky;
    top: 32px;
  }
  
  .sidebar-header {
    padding: 24px;
    text-align: center;
    background: linear-gradient(135deg, rgba(0, 212, 255, 0.2) 0%, rgba(139, 92, 246, 0.2) 100%);
    border-bottom: 1px solid rgba(0, 212, 255, 0.3);
    color: var(--text-primary);
  }
  
  .sidebar-header h4 {
    color: var(--text-primary);
    font-weight: 600;
    margin: 0 0 8px 0;
    font-size: 1.125rem;
    text-shadow: var(--glow-cyan);
  }
  
  .status-badge {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 6px 12px;
    background: linear-gradient(135deg, var(--quantum-green) 0%, var(--digital-teal) 100%);
    border-radius: 20px;
    font-size: 12px;
    font-weight: 500;
    color: var(--bg-primary);
    box-shadow: var(--glow-green);
    animation: pulse-status 2s ease-in-out infinite;
  }
  
  @keyframes pulse-status {
    0%, 100% { transform: scale(1); box-shadow: var(--glow-green); }
    50% { transform: scale(1.05); box-shadow: 0 0 25px rgba(0, 255, 136, 0.5); }
  }
  
  .sidebar-content {
    padding: 24px;
  }
  
  .control-group {
    margin-bottom: 24px;
    padding: 20px;
    background: rgba(0, 212, 255, 0.05);
    border: 1px solid rgba(0, 212, 255, 0.2);
    border-radius: var(--radius-lg);
    backdrop-filter: blur(10px);
    transition: all 0.3s ease;
    position: relative;
    overflow: hidden;
  }
  
  .control-group::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 1px;
    background: linear-gradient(90deg, transparent, var(--cyber-blue), transparent);
  }
  
  .control-group:hover {
    background: rgba(0, 212, 255, 0.1);
    border-color: rgba(0, 212, 255, 0.4);
    transform: translateY(-2px);
    box-shadow: var(--glow-cyan);
  }
  
  .control-label {
    color: var(--text-accent);
    font-size: 13px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    margin-bottom: 12px;
    display: block;
    text-shadow: var(--glow-cyan);
  }

  /* Tablo Se??im Butonlar?? */
  .table-selection-group {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
    gap: 8px;
    margin-bottom: 12px;
  }
  
  .table-btn {
    padding: 10px 12px !important;
    font-size: 11px !important;
    background: rgba(0, 212, 255, 0.1) !important;
    color: var(--text-secondary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3) !important;
    border-radius: var(--radius-md) !important;
    transition: all 0.3s ease !important;
    backdrop-filter: blur(10px) !important;
    text-align: center !important;
    cursor: pointer !important;
    font-weight: 500 !important;
    min-height: 40px !important;
    display: flex !important;
    align-items: center !important;
    justify-content: center !important;
  }
  
  .table-btn:hover {
    background: rgba(0, 212, 255, 0.2) !important;
    color: var(--cyber-blue) !important;
    border-color: rgba(0, 212, 255, 0.5) !important;
    transform: translateY(-1px) !important;
    box-shadow: var(--glow-cyan) !important;
  }
  
  .table-btn.active {
    background: linear-gradient(135deg, var(--quantum-green) 0%, var(--digital-teal) 100%) !important;
    color: var(--bg-primary) !important;
    border-color: transparent !important;
    box-shadow: var(--glow-green) !important;
    transform: translateY(-1px) !important;
    font-weight: 600 !important;
  }

  /* Rapor T??r?? Grid */
  .rapor-grid {
    display: grid;
    grid-template-columns: 1fr;
    gap: 6px;
    margin-bottom: 12px;
  }
  
  .rapor-btn {
    padding: 8px 12px !important;
    font-size: 10px !important;
    background: rgba(0, 212, 255, 0.1) !important;
    color: var(--text-secondary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3) !important;
    border-radius: var(--radius-md) !important;
    transition: all 0.3s ease !important;
    backdrop-filter: blur(10px) !important;
    text-align: center !important;
    cursor: pointer !important;
    font-weight: 500 !important;
    min-height: 35px !important;
    display: flex !important;
    align-items: center !important;
    justify-content: center !important;
    line-height: 1.2 !important;
  }
  
  .rapor-btn:hover {
    background: rgba(0, 212, 255, 0.2) !important;
    color: var(--cyber-blue) !important;
    border-color: rgba(0, 212, 255, 0.5) !important;
    transform: translateY(-1px) !important;
    box-shadow: var(--glow-cyan) !important;
  }
  
  .rapor-btn.active {
    background: linear-gradient(135deg, var(--neon-pink) 0%, var(--plasma-orange) 100%) !important;
    color: var(--text-primary) !important;
    border-color: transparent !important;
    box-shadow: var(--glow-pink) !important;
    transform: translateY(-1px) !important;
    font-weight: 600 !important;
  }

  /* Futuristik Cards */
  .modern-card {
    background: rgba(255, 255, 255, 0.08);
    backdrop-filter: blur(20px);
    border: 1px solid rgba(0, 212, 255, 0.2);
    border-radius: var(--radius-xl);
    box-shadow: var(--shadow-digital), inset 0 1px 0 rgba(255, 255, 255, 0.1);
    margin-bottom: 24px;
    overflow: hidden;
    transition: all 0.3s ease;
    position: relative;
  }
  
  .modern-card::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 2px;
    background: linear-gradient(90deg, var(--cyber-blue) 0%, var(--neon-pink) 50%, var(--electric-purple) 100%);
  }
  
  .modern-card:hover {
    transform: translateY(-4px);
    box-shadow: var(--glow-cyan), var(--shadow-cyber);
    border-color: rgba(0, 212, 255, 0.5);
    background: rgba(255, 255, 255, 0.12);
  }
  
  .card-header {
    padding: 24px 32px;
    background: linear-gradient(135deg, rgba(0, 212, 255, 0.1) 0%, rgba(139, 92, 246, 0.1) 100%);
    border-bottom: 1px solid rgba(0, 212, 255, 0.2);
    backdrop-filter: blur(10px);
  }
  
  .card-header h4 {
    margin: 0;
    color: var(--text-primary);
    font-size: 1.25rem;
    font-weight: 600;
    display: flex;
    align-items: center;
    gap: 12px;
    text-shadow: var(--glow-cyan);
  }
  
  .card-header h4 i {
    color: var(--cyber-blue);
    font-size: 1.125rem;
    filter: drop-shadow(0 0 5px rgba(0, 212, 255, 0.5));
  }
  
  .card-body {
    padding: 32px;
    color: var(--text-primary);
  }

  /* Cyber Form Elements */
  .form-group {
    margin-bottom: 20px;
  }
  
  .form-label {
    display: block;
    margin-bottom: 8px;
    color: var(--text-accent) !important;
    font-size: 14px;
    font-weight: 500;
    letter-spacing: 0.01em;
    text-shadow: var(--glow-cyan);
  }
  
  .form-control {
    width: 100%;
    padding: 12px 16px;
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-md);
    font-size: 14px;
    background: rgba(255, 255, 255, 0.05) !important;
    color: var(--text-primary) !important;
    transition: all 0.3s ease;
    font-weight: 400;
    line-height: 1.5;
    backdrop-filter: blur(10px);
  }
  
  .form-control:focus {
    outline: none;
    border-color: var(--cyber-blue) !important;
    box-shadow: var(--glow-cyan);
    background: rgba(255, 255, 255, 0.1) !important;
    color: var(--text-primary) !important;
    transform: translateY(-1px);
  }
  
  .form-control::placeholder {
    color: var(--text-muted) !important;
    font-weight: 400;
  }

  /* Futuristik Buttons */
  .btn {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    padding: 12px 24px;
    border: none;
    border-radius: var(--radius-md);
    font-size: 14px;
    font-weight: 500;
    text-decoration: none;
    cursor: pointer;
    transition: all 0.3s ease;
    line-height: 1.5;
    letter-spacing: 0.01em;
    position: relative;
    overflow: hidden;
  }
  
  .btn::before {
    content: '';
    position: absolute;
    top: 0;
    left: -100%;
    width: 100%;
    height: 100%;
    background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.2), transparent);
    transition: left 0.5s;
  }
  
  .btn:hover::before {
    left: 100%;
  }
  
  .btn-primary {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--electric-purple) 100%);
    color: var(--text-primary) !important;
    box-shadow: var(--glow-cyan);
    border: 1px solid rgba(0, 212, 255, 0.3);
  }
  
  .btn-primary:hover {
    background: linear-gradient(135deg, var(--neon-pink) 0%, var(--plasma-orange) 100%);
    transform: translateY(-2px);
    box-shadow: var(--glow-pink);
    color: var(--text-primary) !important;
  }
  
  .btn-success {
    background: linear-gradient(135deg, var(--quantum-green) 0%, var(--digital-teal) 100%);
    color: var(--bg-primary) !important;
    box-shadow: var(--glow-green);
  }
  
  .btn-success:hover {
    transform: translateY(-2px);
    box-shadow: 0 0 25px rgba(0, 255, 136, 0.5);
    color: var(--bg-primary) !important;
  }
  
  .btn-warning {
    background: linear-gradient(135deg, var(--plasma-orange) 0%, #fbbf24 100%);
    color: var(--text-primary) !important;
    box-shadow: 0 0 20px rgba(255, 107, 53, 0.3);
  }
  
  .btn-warning:hover {
    transform: translateY(-2px);
    box-shadow: 0 0 25px rgba(255, 107, 53, 0.5);
    color: var(--text-primary) !important;
  }
  
  .btn-danger {
    background: linear-gradient(135deg, #ef4444 0%, #f87171 100%);
    color: var(--text-primary) !important;
    box-shadow: 0 0 20px rgba(239, 68, 68, 0.3);
  }
  
  .btn-danger:hover {
    transform: translateY(-2px);
    box-shadow: 0 0 25px rgba(239, 68, 68, 0.5);
    color: var(--text-primary) !important;
  }
  
  .btn-block {
    width: 100%;
    justify-content: center;
  }

  /* Cyber Radio Buttons */
  .btn-group-toggle .btn {
    background: rgba(0, 212, 255, 0.1) !important;
    color: var(--text-secondary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3) !important;
    padding: 10px 18px;
    font-size: 13px;
    border-radius: var(--radius-md);
    margin: 0 2px;
    font-weight: 500;
    transition: all 0.3s ease;
    backdrop-filter: blur(10px);
  }
  
  .btn-group-toggle .btn.active {
    background: linear-gradient(135deg, var(--quantum-green) 0%, var(--digital-teal) 100%) !important;
    color: var(--bg-primary) !important;
    border-color: transparent !important;
    box-shadow: var(--glow-green);
    transform: translateY(-1px);
    font-weight: 600;
  }
  
  .btn-group-toggle .btn:hover {
    background: rgba(0, 212, 255, 0.2) !important;
    color: var(--cyber-blue) !important;
    border-color: rgba(0, 212, 255, 0.5) !important;
    transform: translateY(-1px);
    box-shadow: var(--glow-cyan);
  }

  /* Futuristik Tabs */
  .nav-tabs {
    border-bottom: 1px solid rgba(0, 212, 255, 0.3);
    margin-bottom: 0;
    background: rgba(26, 31, 46, 0.8);
    backdrop-filter: blur(15px);
    border-radius: var(--radius-lg) var(--radius-lg) 0 0;
    padding: 8px 8px 0 8px;
  }
  
  .nav-tabs > li > a {
    color: var(--text-secondary) !important;
    border: none;
    border-radius: var(--radius-md) var(--radius-md) 0 0;
    padding: 16px 24px;
    font-size: 15px;
    font-weight: 500;
    background: transparent;
    transition: all 0.3s ease;
    margin-right: 4px;
    letter-spacing: 0.01em;
  }
  
  .nav-tabs > li > a:hover {
    background: rgba(0, 212, 255, 0.1);
    color: var(--cyber-blue) !important;
    transform: translateY(-2px);
    box-shadow: var(--glow-cyan);
  }
  
  .nav-tabs > li.active > a {
    color: var(--text-primary) !important;
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--electric-purple) 100%);
    border: none;
    box-shadow: var(--glow-cyan);
  }
  
  .tab-content {
    background: rgba(255, 255, 255, 0.05);
    backdrop-filter: blur(20px);
    padding: 32px;
    border-radius: 0 var(--radius-lg) var(--radius-lg) var(--radius-lg);
    border: 1px solid rgba(0, 212, 255, 0.2);
    box-shadow: var(--shadow-digital);
    color: var(--text-primary);
  }

  /* Cyber Data Tables */
  .dataTables_wrapper {
    background: rgba(26, 31, 46, 0.8);
    backdrop-filter: blur(20px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-lg);
    overflow: hidden;
    box-shadow: var(--shadow-digital);
    color: var(--text-primary);
  }
  
  .dataTables_wrapper .dataTables_length,
  .dataTables_wrapper .dataTables_filter,
  .dataTables_wrapper .dataTables_info,
  .dataTables_wrapper .dataTables_paginate {
    padding: 16px 20px;
    background: rgba(0, 212, 255, 0.05);
    color: var(--text-primary) !important;
  }
  
  .dataTables_wrapper .dataTables_filter input {
    background: rgba(255, 255, 255, 0.05) !important;
    color: var(--text-primary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-md);
    padding: 8px 12px;
    transition: all 0.3s ease;
  }
  
  .dataTables_wrapper .dataTables_filter input:focus {
    border-color: var(--cyber-blue);
    box-shadow: var(--glow-cyan);
  }
  
  .dataTables_wrapper .dataTables_length select {
    background: rgba(255, 255, 255, 0.05) !important;
    color: var(--text-primary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-md);
    padding: 6px 10px;
  }
  
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    background: rgba(0, 212, 255, 0.1);
    color: var(--text-primary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-md);
    margin: 0 2px;
    padding: 8px 12px;
    transition: all 0.3s ease;
  }
  
  .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--electric-purple) 100%);
    color: var(--text-primary) !important;
    transform: translateY(-1px);
    box-shadow: var(--glow-cyan);
  }
  
  .dataTables_wrapper .dataTables_paginate .paginate_button.current {
    background: linear-gradient(135deg, var(--neon-pink) 0%, var(--plasma-orange) 100%);
    color: var(--text-primary) !important;
    box-shadow: var(--glow-pink);
  }
  
  table.dataTable thead th {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--electric-purple) 100%);
    color: var(--text-primary) !important;
    font-weight: 600;
    font-size: 13px;
    padding: 16px 12px;
    border: none;
  }
  
  table.dataTable tbody td {
    padding: 14px 12px;
    font-size: 14px;
    color: var(--text-primary) !important;
    border-bottom: 1px solid rgba(0, 212, 255, 0.1);
  }
  
  table.dataTable tbody tr {
    background: rgba(0, 212, 255, 0.02);
    transition: all 0.3s ease;
  }
  
  table.dataTable tbody tr:hover {
    background: rgba(0, 212, 255, 0.1);
    transform: translateY(-1px);
  }
  
  table.dataTable tbody tr:nth-child(even) {
    background: rgba(0, 212, 255, 0.05);
  }

  /* Cyber Wells */
  .well {
    background: rgba(26, 31, 46, 0.8) !important;
    backdrop-filter: blur(20px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-lg);
    padding: 24px;
    margin-bottom: 20px;
    box-shadow: var(--shadow-digital), inset 0 1px 0 rgba(255, 255, 255, 0.1);
    transition: all 0.3s ease;
    color: var(--text-primary) !important;
  }
  
  .well:hover {
    transform: translateY(-2px);
    box-shadow: var(--glow-cyan), var(--shadow-cyber);
    border-color: rgba(0, 212, 255, 0.5);
    background: rgba(26, 31, 46, 0.9) !important;
  }
  
  .well label,
  .well p,
  .well span,
  .well div,
  .well strong {
    color: var(--text-primary) !important;
    font-weight: 500;
    font-size: 14px;
    margin-bottom: 8px;
    letter-spacing: 0.01em;
  }
  
  .well h6 {
    color: var(--cyber-blue) !important;
    font-weight: 600;
    font-size: 15px;
    margin-bottom: 16px;
    letter-spacing: 0.01em;
    text-shadow: var(--glow-cyan);
  }

  /* Cyber Collapsible Panels */
  .collapsible-btn {
    width: 100%;
    text-align: left;
    background: rgba(26, 31, 46, 0.8);
    backdrop-filter: blur(15px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-lg);
    padding: 16px 20px;
    color: var(--text-primary) !important;
    font-size: 14px;
    font-weight: 500;
    cursor: pointer;
    margin-bottom: 12px;
    transition: all 0.3s ease;
    box-shadow: var(--shadow-digital);
    letter-spacing: 0.01em;
    position: relative;
    overflow: hidden;
  }
  
  .collapsible-btn::before {
    content: '';
    position: absolute;
    top: 0;
    left: -100%;
    width: 100%;
    height: 100%;
    background: linear-gradient(90deg, transparent, rgba(0, 212, 255, 0.2), transparent);
    transition: left 0.6s ease;
  }
  
  .collapsible-btn:hover::before {
    left: 100%;
  }
  
  .collapsible-btn:hover {
    background: rgba(26, 31, 46, 0.9);
    transform: translateY(-2px);
    box-shadow: var(--glow-cyan);
    color: var(--cyber-blue) !important;
    border-color: var(--cyber-blue);
  }
  
  .collapsible-content {
    background: rgba(26, 31, 46, 0.8) !important;
    backdrop-filter: blur(20px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-lg);
    margin-bottom: 20px;
    overflow: hidden;
    box-shadow: var(--shadow-digital);
    color: var(--text-primary) !important;
  }
  
  .collapsible-content *,
  .collapsible-content label,
  .collapsible-content p,
  .collapsible-content div,
  .collapsible-content strong,
  .collapsible-content span {
    color: var(--text-primary) !important;
  }
  
  .collapsible-content h6 {
    color: var(--cyber-blue) !important;
    font-weight: 600;
    font-size: 15px;
    margin-bottom: 16px;
    letter-spacing: 0.01em;
    text-shadow: var(--glow-cyan);
  }

  /* Cyber Selectize */
  .selectize-input {
    border: 1px solid rgba(0, 212, 255, 0.3) !important;
    border-radius: var(--radius-md);
    padding: 12px 16px;
    background: rgba(255, 255, 255, 0.05) !important;
    color: var(--text-primary) !important;
    font-size: 14px;
    min-height: auto;
    transition: all 0.3s ease;
    font-weight: 400;
    backdrop-filter: blur(10px);
  }
  
  .selectize-input.focus {
    border-color: var(--cyber-blue) !important;
    box-shadow: var(--glow-cyan);
    background: rgba(255, 255, 255, 0.1) !important;
  }
  
  .selectize-input input {
    color: var(--text-primary) !important;
  }
  
  .selectize-input .item {
    color: var(--text-primary) !important;
  }
  
  .selectize-dropdown {
    background: rgba(26, 31, 46, 0.95) !important;
    backdrop-filter: blur(20px);
    border: 1px solid rgba(0, 212, 255, 0.4);
    border-radius: var(--radius-md);
    box-shadow: var(--shadow-cyber);
    overflow: hidden;
    z-index: 9999;
  }
  
  .selectize-dropdown-content .option {
    color: var(--text-primary) !important;
    padding: 12px 16px;
    font-weight: 400;
    transition: all 0.3s ease;
    background: transparent !important;
  }
  
  .selectize-dropdown-content .option:hover {
    background: rgba(0, 212, 255, 0.15) !important;
    color: var(--cyber-blue) !important;
    font-weight: 500;
  }
  
  .selectize-dropdown-content .option.active {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--electric-purple) 100%) !important;
    color: var(--text-primary) !important;
    font-weight: 500;
  }

  /* Futuristik Notifications */
  .shiny-notification {
    background: rgba(26, 31, 46, 0.95);
    backdrop-filter: blur(20px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-lg);
    color: var(--text-primary) !important;
    box-shadow: var(--glow-cyan);
    font-size: 14px;
  }
  
  .shiny-notification-message {
    background: linear-gradient(135deg, rgba(0, 212, 255, 0.2) 0%, rgba(139, 92, 246, 0.2) 100%);
    border-color: var(--cyber-blue);
  }

  /* Cyber Login Container */
  .login-container {
    background: rgba(26, 31, 46, 0.95);
    backdrop-filter: blur(25px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-xl);
    box-shadow: var(--glow-cyan), var(--shadow-cyber);
    padding: 48px;
    margin: 50px auto;
    max-width: 450px;
    position: relative;
    overflow: hidden;
  }
  
  .login-container::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 3px;
    background: linear-gradient(90deg, var(--cyber-blue) 0%, var(--neon-pink) 50%, var(--electric-purple) 100%);
  }
  
  .login-title {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--neon-pink) 50%, var(--electric-purple) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    font-weight: 700;
    font-size: 2.5rem;
    text-align: center;
    margin-bottom: 8px;
    letter-spacing: -0.025em;
    text-shadow: var(--glow-cyan);
  }
  
  .login-subtitle {
    color: var(--text-secondary);
    font-size: 1rem;
    font-weight: 400;
    text-align: center;
    margin-bottom: 32px;
  }

  /* Grid System */
  .row {
    display: flex;
    flex-wrap: wrap;
    margin: 0 -12px;
  }
  
  .col-6 {
    flex: 0 0 50%;
    max-width: 50%;
    padding: 0 12px;
  }
  
  .col-12 {
    flex: 0 0 100%;
    max-width: 100%;
    padding: 0 12px;
  }

  /* Cyber Verbatim Output */
  .shiny-text-output,
  pre {
    background: rgba(26, 31, 46, 0.8) !important;
    color: var(--text-primary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-md);
    padding: 16px;
    font-family: 'JetBrains Mono', 'Fira Code', monospace;
    font-size: 13px;
    box-shadow: var(--shadow-digital);
    backdrop-filter: blur(10px);
  }

  /* Cyber Input Overrides */
  input[type='date'],
  input[type='number'],
  input[type='text'] {
    background: rgba(255, 255, 255, 0.05) !important;
    color: var(--text-primary) !important;
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-md);
    padding: 12px 16px;
    font-size: 14px;
    font-weight: 400;
    transition: all 0.3s ease;
    backdrop-filter: blur(10px);
  }
  
  input[type='date']:focus,
  input[type='number']:focus,
  input[type='text']:focus {
    border-color: var(--cyber-blue) !important;
    box-shadow: var(--glow-cyan);
  }

  /* Cyber Sliders */
  .irs-bar {
    background: linear-gradient(90deg, var(--cyber-blue) 0%, var(--electric-purple) 100%);
    box-shadow: var(--glow-cyan);
  }
  
  .irs-handle {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--electric-purple) 100%);
    border: 2px solid var(--text-primary);
    box-shadow: var(--glow-cyan);
  }

  /* Cyber Scrollbar */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }
  
  ::-webkit-scrollbar-track {
    background: rgba(26, 31, 46, 0.5);
    border-radius: 4px;
  }
  
  ::-webkit-scrollbar-thumb {
    background: linear-gradient(135deg, var(--cyber-blue) 0%, var(--electric-purple) 100%);
    border-radius: 4px;
    box-shadow: var(--glow-cyan);
  }
  
  ::-webkit-scrollbar-thumb:hover {
    background: linear-gradient(135deg, var(--neon-pink) 0%, var(--plasma-orange) 100%);
  }

  /* Responsive Design */
@media (max-width: 1200px) {
  .sidebar-container {
    width: 300px;
  }
  
  .main-layout {
    margin: 0 30px 24px;
    padding: 24px;
    max-width: none !important;
    width: calc(100% - 60px) !important;
  }
}

@media (max-width: 992px) {
  .sidebar-container {
    width: 100%;
    margin-right: 0;
    margin-bottom: 20px;
    position: static;
  }
  
  .main-layout {
    flex-direction: column;
    margin: 0 20px 16px;
    padding: 20px;
    gap: 20px;
    max-width: none !important;
    width: calc(100% - 40px) !important;
  }
  
  .header-container {
    padding: 16px 20px;
    margin-bottom: 20px;
  }
  
  .card-body {
    padding: 20px;
  }
}

@media (max-width: 768px) {
  .col-6 {
    flex: 0 0 100%;
    max-width: 100%;
    margin-bottom: 15px;
  }
  
  .header-content {
    flex-direction: column;
    gap: 12px;
  }
  
  .logo-section h1 {
    font-size: 1.8rem;
  }
  
  .logo-section p {
    font-size: 0.9rem;
  }
  
  .main-layout {
    margin: 0 10px 12px;
    padding: 15px;
    gap: 15px;
    max-width: none !important;
    width: calc(100% - 20px) !important;
  }
  
  .sidebar-container {
    width: 100%;
  }
  
  .sidebar-content {
    padding: 16px;
  }
  
  .control-group {
    padding: 15px;
    margin-bottom: 15px;
  }
  
  .card-header {
    padding: 16px 20px;
  }
  
  .card-header h4 {
    font-size: 1.1rem;
  }
  
  .card-body {
    padding: 16px;
  }
  
  .table-selection-group {
    grid-template-columns: 1fr;
    gap: 6px;
  }
  
  .form-control {
    padding: 10px 12px;
    font-size: 16px; /* iOS zoom'u ??nlemek i??in */
  }
  
  .btn {
    padding: 10px 16px;
    font-size: 13px;
  }
  
  .modern-card {
    margin-bottom: 15px;
  }
}

@media (max-width: 480px) {
  .main-layout {
    margin: 0 5px 8px;
    padding: 10px;
    gap: 10px;
    max-width: none !important;
    width: calc(100% - 10px) !important;
  }
  
  .header-container {
    padding: 12px 15px;
    margin-bottom: 15px;
  }
  
  .logo-section h1 {
    font-size: 1.5rem;
  }
  
  .sidebar-content {
    padding: 12px;
  }
  
  .control-group {
    padding: 12px;
    margin-bottom: 12px;
  }
  
  .card-header {
    padding: 12px 15px;
  }
  
  .card-body {
    padding: 12px;
  }
  
  .form-control {
    padding: 8px 10px;
  }
  
  .btn {
    padding: 8px 12px;
    font-size: 12px;
  }
  
  .table-btn,
  .rapor-btn {
    padding: 8px 10px !important;
    font-size: 10px !important;
    min-height: 35px !important;
  }
  
  
  /* Modal Styles */
  .modal-dialog {
    max-width: 90%;
    width: 90%;
    margin: 30px auto;
  }
  
  .modal-content {
    background: rgba(26, 31, 46, 0.95);
    backdrop-filter: blur(25px);
    border: 1px solid rgba(0, 212, 255, 0.3);
    border-radius: var(--radius-xl);
    box-shadow: var(--glow-cyan), var(--shadow-cyber);
    color: var(--text-primary);
  }
  
  .modal-header {
    border-bottom: 1px solid rgba(0, 212, 255, 0.3);
    background: linear-gradient(135deg, rgba(0, 212, 255, 0.1) 0%, rgba(139, 92, 246, 0.1) 100%);
    padding: 20px 24px;
  }
  
  .modal-title {
    color: var(--text-primary);
    font-weight: 600;
    text-shadow: var(--glow-cyan);
  }
  
  .modal-body {
    padding: 0;
    max-height: 80vh;
    overflow-y: auto;
  }
  
  .modal-footer {
    border-top: 1px solid rgba(0, 212, 255, 0.3);
    background: linear-gradient(135deg, rgba(0, 212, 255, 0.05) 0%, rgba(139, 92, 246, 0.05) 100%);
    padding: 16px 24px;
  }
  
  .close {
    color: var(--text-primary);
    opacity: 0.8;
    font-size: 24px;
    font-weight: 600;
    text-shadow: var(--glow-cyan);
  }
  
  .close:hover {
    color: var(--cyber-blue);
    opacity: 1;
  }
  
  



  }
"


# UI ----
ui <- tagList(
  
  ### CSS & JS 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("flatly")),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML(my_css))
    # includeCSS("../project/styles.css")
  ),
  
  ### JS 
  shinyjs::useShinyjs(),
  
  # Y??kleme ekran??
  use_waiter(),
  waiter_show_on_load(
    html = tagList(
      spin_cube_grid(),
      h3("PROINSURE y??kleniyor...", style = "color: #00ccff; margin-top: 20px; font-weight: 600;")
    ),
    color = "#0a0e1a"
  ),
  
  # Canvas container (background animation)
  div(id = "canvas-container"),
  
  # Login Screen
  shinyauthr::loginUI(
    id = "login_3",
    title = div(
      class = "login-container",
      h1(class = "login-title", "PROINSURE"),
      p(class = "login-subtitle", "Futuristic Actuarial Analytics Platform"),
      div(style = "height: 2px; background: linear-gradient(90deg, #00ccff 0%, #ff2975 50%, #7b3fe4 100%); 
                    border-radius: 1px; margin: 24px 0;")
    ),
    error_message = "Hatal?? kullan??c?? ad?? veya ??ifre", 
    user_title = "Kullan??c?? Ad??", 
    pass_title = "??ifre", 
    login_title = "Sisteme Giri??"
  ),
  
  # Enhanced Canvas Animation Script
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      setTimeout(function() {
        const container = document.getElementById('canvas-container');
        if (!container) return;
        
        const canvas = document.createElement('canvas');
        container.appendChild(canvas);
        const ctx = canvas.getContext('2d');

        let width = window.innerWidth;
        let height = window.innerHeight;
        canvas.width = width;
        canvas.height = height;

        let mouse = { x: width / 2, y: height / 2, radius: 150 };
        let lastMouseX = width / 2;
        let lastMouseY = height / 2;
        let mouseVelocityX = 0;
        let mouseVelocityY = 0;

        window.addEventListener('mousemove', function(event) {
          mouseVelocityX = event.clientX - lastMouseX;
          mouseVelocityY = event.clientY - lastMouseY;
          lastMouseX = mouse.x = event.clientX;
          lastMouseY = mouse.y = event.clientY;
        });

        window.addEventListener('resize', function() {
          width = window.innerWidth;
          height = window.innerHeight;
          canvas.width = width;
          canvas.height = height;
          init();
        });

        const symbols = ['???', '???', '??', '???', '???', '??', '??', '??', '???', '???', '??', '??', '%', '???', '??', '#', '??', '??', '??', '??'];
        const colors = ['#00ccff', '#ff2975', '#7b3fe4', '#ff8d3f', '#10b981', '#f59e0b'];

        let symbolsArray = [];

        class Symbol {
          constructor(x, y, size) {
            this.x = x;
            this.y = y;
            this.z = Math.random() * 200 - 100;
            this.size = size;
            this.baseSize = (Math.random() * 30) + 12;
            this.velocity = { 
              x: (Math.random() - 0.5) * 0.8, 
              y: (Math.random() - 0.5) * 0.8, 
              z: (Math.random() - 0.5) * 0.5 
            };
            this.opacity = 0.15 + Math.random() * 0.25;
            this.symbol = symbols[Math.floor(Math.random() * symbols.length)];
            this.color = colors[Math.floor(Math.random() * colors.length)];
            this.glowIntensity = Math.random() * 0.5 + 0.3;
          }

          update() {
            const predictedMouseX = mouse.x + mouseVelocityX * 0.5;
            const predictedMouseY = mouse.y + mouseVelocityY * 0.5;

            let dx = this.x - predictedMouseX;
            let dy = this.y - predictedMouseY;
            let distance = Math.sqrt(dx * dx + dy * dy);

            if (distance < mouse.radius) {
              const force = Math.pow((mouse.radius - distance) / mouse.radius, 1.5) * 1.2;
              const directionX = dx / distance || 0;
              const directionY = dy / distance || 0;
              this.x += directionX * force * 12;
              this.y += directionY * force * 12;
              this.velocity.x += directionX * force * 3;
              this.velocity.y += directionY * force * 3;
              this.opacity = Math.min(0.8, 0.4 + force * 0.4);
              this.glowIntensity = Math.min(1, 0.5 + force * 0.5);
            } else {
              this.opacity = Math.max(0.15, this.opacity - 0.01);
              this.glowIntensity = Math.max(0.3, this.glowIntensity - 0.005);
            }

            this.x += this.velocity.x;
            this.y += this.velocity.y;
            this.z += this.velocity.z;

            if (this.x - this.size > width) this.x = 0 - this.size;
            if (this.x + this.size < 0) this.x = width + this.size;
            if (this.y - this.size > height) this.y = 0 - this.size;
            if (this.y + this.size < 0) this.y = height + this.size;

            if (this.z > 150) { this.z = 150; this.velocity.z *= -0.8; }
            if (this.z < -150) { this.z = -150; this.velocity.z *= -0.8; }

            this.velocity.x *= 0.985;
            this.velocity.y *= 0.985;
            this.velocity.z *= 0.985;

            const perspective = 700;
            const scale = perspective / (perspective + this.z);
            this.size = this.baseSize * scale;

            this.draw(scale);
          }

          draw(scale) {
            ctx.save();
            ctx.translate(this.x, this.y);
            ctx.scale(scale, scale);
            
            // Add glow effect
            ctx.shadowColor = this.color;
            ctx.shadowBlur = 15 * this.glowIntensity;
            ctx.globalAlpha = this.opacity;
            ctx.fillStyle = this.color;
            ctx.font = `${this.baseSize * 0.7}px Inter`;
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText(this.symbol, 0, 0);
            
            ctx.restore();
          }

          calculateDistance(symbol) {
            const dx = this.x - symbol.x;
            const dy = this.y - symbol.y;
            const dz = this.z - symbol.z;
            return Math.sqrt(dx * dx + dy * dy + dz * dz);
          }
        }

        function init() {
          symbolsArray = [];
          const numberOfSymbols = Math.min(150, Math.floor((width * height) / 8000));
          for (let i = 0; i < numberOfSymbols; i++) {
            const size = Math.random() * 20 + 10;
            const x = Math.random() * (width - size * 2) + size;
            const y = Math.random() * (height - size * 2) + size;
            symbolsArray.push(new Symbol(x, y, size));
          }
        }

        function drawConnections() {
          const connectionDistance = 200;
          const maxConnections = 200;
          let drawnConnections = 0;

          symbolsArray.sort((a, b) => b.z - a.z);

          for (let i = 0; i < symbolsArray.length && drawnConnections < maxConnections; i++) {
            for (let j = i + 1; j < symbolsArray.length && drawnConnections < maxConnections; j++) {
              const distance = symbolsArray[i].calculateDistance(symbolsArray[j]);
              if (distance < connectionDistance) {
                const opacity = Math.max(0.1, 0.3 * (1 - distance / connectionDistance));
                const gradient = ctx.createLinearGradient(
                  symbolsArray[i].x, symbolsArray[i].y,
                  symbolsArray[j].x, symbolsArray[j].y
                );
                gradient.addColorStop(0, symbolsArray[i].color + Math.floor(opacity * 255).toString(16).padStart(2, '0'));
                gradient.addColorStop(1, symbolsArray[j].color + Math.floor(opacity * 255).toString(16).padStart(2, '0'));
                
                ctx.strokeStyle = gradient;
                ctx.lineWidth = 1;
                ctx.beginPath();
                ctx.moveTo(symbolsArray[i].x, symbolsArray[i].y);
                ctx.lineTo(symbolsArray[j].x, symbolsArray[j].y);
                ctx.stroke();
                drawnConnections++;
              }
            }
          }
        }

        function animate() {
          requestAnimationFrame(animate);
          ctx.clearRect(0, 0, width, height);

          // Create animated background gradient
          const gradient = ctx.createRadialGradient(
            mouse.x, mouse.y, 0,
            mouse.x, mouse.y, mouse.radius * 2
          );
          gradient.addColorStop(0, 'rgba(0, 204, 255, 0.02)');
          gradient.addColorStop(0.5, 'rgba(123, 63, 228, 0.01)');
          gradient.addColorStop(1, 'rgba(0, 8, 20, 0.05)');
          
          ctx.fillStyle = gradient;
          ctx.fillRect(0, 0, width, height);

          drawConnections();
          
          symbolsArray.forEach(symbol => {
            symbol.update();
          });
        }

        init();
        animate();
      }, 300);
    });
    
    // Panel toggle function
    function togglePanel(panelId) {
      var panel = document.getElementById(panelId);
      if (panel.style.display === 'none' || panel.style.display === '') {
        panel.style.display = 'block';
      } else {
        panel.style.display = 'none';
      }
    }
    
    // Tablo ve rapor se??im fonksiyonlar??
    function selectTable(table, type) {
      // Mevcut aktif butonlar?? temizle
      document.querySelectorAll('.table-btn').forEach(btn => {
        btn.classList.remove('active');
      });
      
      // Se??ilen butonu aktif yap
      event.target.classList.add('active');
      
      // Shiny'a de??eri g??nder
      if (type === 'aktueryal') {
        Shiny.setInputValue('tablo1', table);
      } else {
        Shiny.setInputValue('tablo2', table);
      }
    }
    
    function selectRapor(rapor) {
      // Mevcut aktif butonlar?? temizle
      document.querySelectorAll('.rapor-btn').forEach(btn => {
        btn.classList.remove('active');
      });
      
      // Se??ilen butonu aktif yap
      event.target.classList.add('active');
      
      // Shiny'a de??eri g??nder
      Shiny.setInputValue('rapor', rapor);
    }
  ")),
  
  uiOutput(outputId = "web_page")
)

# SERVER ----
server <- function(input, output, session) {
  
  # Y??kleme ekran??n?? kald??r
  waiter_hide()
  
  ## USER CREDENTIALS ----
  user_base_tbl <- tibble(user_name = c("Erus1*", "Erus2*", "Erus3*"),
                          password = c("passErus1*", "passErus2", "passErus3")
  )
  
  ## SHINYAUTHR ----
  credentials <- shinyauthr::loginServer(
    id       = "login_3",
    data     = user_base_tbl,
    user_col = user_name,
    pwd_col  = password,
    log_out  = reactive(logout_init()))
  
  user_auth <- reactive({
    credentials()$user_auth
  })
  
  user_data <- reactive({
    credentials()$info
  })
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(user_auth())
  )
  
  ##RENDER UI ----
  output$web_page <- renderUI({
    
    req(user_auth()) 
    
    fluidPage(
      style = "padding: 0; margin: 0; width: 100vw; min-height: 100vh; overflow-x: hidden;",
      
      # Modern Header
      div(
        class = "header-container",
        div(
          class = "header-content",
          div(
            class = "logo-section",
            h1("PROINSURE"),
            p("Maluliyet ve Destek Tazminat?? Hesaplama Sistemi")
          ),
          actionButton("logout", "????k???? Yap", 
                       class = "btn btn-danger",
                       icon = icon("sign-out-alt"))
        )
      ),
      
      # Main Layout - Tam geni??lik
      div(
        class = "main-layout",
        
        # Futuristik Sidebar - Biraz daha dar
        div(
          class = "sidebar-container",
          style = "width: 280px; margin-right: 20px;",
          
          div(
            class = "sidebar-header",
            h4("Kontrol Merkezi"),
            div(class = "status-badge", 
                icon("circle"), "Sistem Aktif")
          ),
          
          div(
            class = "sidebar-content",
            
            # Hesaplama Y??ntemi
            div(
              class = "control-group",
              tags$span(class = "control-label", "Hesaplama Y??ntemi"),
              radioGroupButtons(
                inputId = "yontem",
                label = NULL,
                choices = c("Akt??eryal", "Progresif Rant"),
                selected = "Progresif Rant",
                justified = TRUE,
                size = "sm"
              )
            ),
            
            # Ya??am Tablosu Se??imi - Progresif
            conditionalPanel(
              condition = "input.yontem == 'Progresif Rant'",
              div(
                class = "control-group",
                tags$span(class = "control-label", "Ya??am Tablosu"),
                
                # Hidden input for tablo2
                div(style = "display: none;",
                    textInput("tablo2", NULL, value = "TRH-2010")),
                
                div(
                  class = "table-selection-group",
                  tags$button(class = "table-btn active", onclick = "selectTable('TRH-2010', 'progresif')", "TRH-2010"),
                  tags$button(class = "table-btn", onclick = "selectTable('PMF-1931', 'progresif')", "PMF-1931"),
                  tags$button(class = "table-btn", onclick = "selectTable('TUIK_20-22', 'progresif')", "TUIK 20-22"),
                  tags$button(class = "table-btn", onclick = "selectTable('TUIK_19-21', 'progresif')", "TUIK 19-21"),
                  tags$button(class = "table-btn", onclick = "selectTable('TUIK_18-20', 'progresif')", "TUIK 18-20"),
                  tags$button(class = "table-btn", onclick = "selectTable('CSO-1980', 'progresif')", "CSO-1980")
                ),
                div(style = "background: rgba(16, 185, 129, 0.1); padding: 8px; border-radius: 6px; margin-top: 8px;",
                    tags$small("Teknik Faiz: %0", style = "color: #10b981; font-weight: 500;"))
              )
            ),
            
            # Ya??am Tablosu Se??imi - Akt??eryal
            conditionalPanel(
              condition = "input.yontem == 'Akt??eryal'",
              div(
                class = "control-group",
                tags$span(class = "control-label", "Ya??am Tablosu"),
                
                # Hidden input for tablo1
                div(style = "display: none;",
                    textInput("tablo1", NULL, value = "TRH-2010")),
                
                div(
                  class = "table-selection-group",
                  tags$button(class = "table-btn active", onclick = "selectTable('TRH-2010', 'aktueryal')", "TRH-2010"),
                  tags$button(class = "table-btn", onclick = "selectTable('PMF-1931', 'aktueryal')", "PMF-1931"),
                  tags$button(class = "table-btn", onclick = "selectTable('TUIK/20-22', 'aktueryal')", "TUIK 20-22"),
                  tags$button(class = "table-btn", onclick = "selectTable('TUIK/19-21', 'aktueryal')", "TUIK 19-21"),
                  tags$button(class = "table-btn", onclick = "selectTable('TUIK/18-20', 'aktueryal')", "TUIK 18-20"),
                  tags$button(class = "table-btn", onclick = "selectTable('CSO-1980', 'aktueryal')", "CSO-1980")
                ),
                numericInput("teknik_faiz", "Teknik Faiz (%)", 
                             value = 1.8, step = 0.1, min = 0, max = 20)
              )
              
            ),
            
            # Ana ????lemler
            div(
              class = "control-group",
              tags$span(class = "control-label", "????lemler"),
              actionButton("action", "Hesaplama Ba??lat", 
                           class = "btn btn-primary btn-block",
                           icon = icon("calculator")),
              br(), br(),
              
              # Rapor T??r?? Se??imi
              div(
                tags$span(class = "control-label", "Rapor T??r??"),
                
                # Hidden input for rapor
                div(style = "display: none;",
                    textInput("rapor", NULL, value = "T??m Rapor-1 ??deme")),
                
                div(
                  class = "rapor-grid",
                  tags$button(class = "rapor-btn active", onclick = "selectRapor('T??m Rapor-1 ??deme')", "T??m Rapor-1 ??deme"),
                  tags$button(class = "rapor-btn", onclick = "selectRapor('T??m Rapor-2 ??deme')", "T??m Rapor-2 ??deme"),
                  tags$button(class = "rapor-btn", onclick = "selectRapor('T??m Rapor (??irket ??demesiz)')", "T??m Rapor (??irket ??demesiz)"),
                  tags$button(class = "rapor-btn", onclick = "selectRapor('S??rekli+Ge??ici (??irket ??demesiz)')", "S??rekli+Ge??ici (??irket ??demesiz)"),
                  tags$button(class = "rapor-btn", onclick = "selectRapor('Ge??ici (??irket ??demesiz)')", "Ge??ici (??irket ??demesiz)"),
                  tags$button(class = "rapor-btn", onclick = "selectRapor('S??rekli (??irket ??demeli)')", "S??rekli (??irket ??demeli)"),
                  tags$button(class = "rapor-btn", onclick = "selectRapor('S??rekli (??irket ??demesiz)')", "S??rekli (??irket ??demesiz)"),
                  tags$button(class = "rapor-btn", onclick = "selectRapor('Destek')", "Destek")
                )
              ),
              br(),
              actionButton("submit", "Verileri Kaydet", 
                           class = "btn btn-success btn-block",
                           icon = icon("save")),
              br(), br(),
              conditionalPanel(
                condition = "input.submit > 0",
                downloadButton("generate_report", "Rapor ??ndir",
                               class = "btn btn-warning btn-block",
                               icon = icon("file-download")),
                br(), br(),
                actionButton("view_html_report", "Raporu G??r??nt??le", 
                             class = "btn btn-info btn-block",
                             icon = icon("eye"))
              )
            )
          )
        ),
        
        # Main Content - Geni??letildi
        div(
          style = "flex: 1; min-width: 0;",
          
          tabsetPanel(
            id = "main_tabs",
            
            # Veri Giri??i
            tabPanel(
              title = tagList(icon("edit"), " Veri Giri??i"),
              value = "veri_girisi",
              div(
                class = "row",
                style = "margin: 0;",
                
                # Genel Bilgiler
                div(
                  class = "col-6", 
                  style = "padding-left: 5px; padding-right: 15px;",
                  div(
                    class = "modern-card",
                    div(class = "card-header",
                        h4(icon("info-circle"), " Genel Bilgiler")),
                    div(
                      class = "card-body",
                      div(class = "form-group",
                          tags$label(class = "form-label", "Dosya No"),
                          textInput("dosya", NULL, placeholder = "Dosya numaras??")),
                      div(class = "form-group",
                          tags$label(class = "form-label", "Ad Soyad"),
                          textInput("isim", NULL, placeholder = "Ad ve soyad")),
                      div(class = "row",
                          div(class = "col-6",
                              div(class = "form-group",
                                  tags$label(class = "form-label", "Kaza Tarihi"),
                                  dateInput("kazatarihi", NULL, value = Sys.Date()))),
                          div(class = "col-6",
                              div(class = "form-group",
                                  tags$label(class = "form-label", "Kusur Oran?? (%)"),
                                  sliderInput("kusur", NULL, min = 0, max = 100, value = 50, step = 5))))
                    )
                  )
                ),
                
                # ??demeler B??l??m?? - Tabs
                div(
                  class = "col-6",
                  style = "padding-left: 15px; padding-right: 5px;",
                  div(
                    class = "modern-card",
                    div(class = "card-header",
                        h4(icon("credit-card"), " ??demeler")),
                    div(
                      class = "card-body",
                      # Tab Navigation
                      tags$ul(
                        class = "nav nav-tabs",
                        role = "tablist",
                        tags$li(
                          role = "presentation",
                          class = "active",
                          tags$a(
                            href = "#sirket-odemeler",
                            `aria-controls` = "sirket-odemeler",
                            role = "tab",
                            `data-toggle` = "tab",
                            "??irket ??demeleri"
                          )
                        ),
                        tags$li(
                          role = "presentation",
                          tags$a(
                            href = "#sgk-odemeler",
                            `aria-controls` = "sgk-odemeler",
                            role = "tab",
                            `data-toggle` = "tab",
                            "SGK ??demesi"
                          )
                        )
                      ),
                      
                      # Tab Content
                      div(
                        class = "tab-content",
                        # ??irket ??demeleri Tab
                        div(
                          role = "tabpanel",
                          class = "tab-pane active",
                          id = "sirket-odemeler",
                          br(),
                          div(class = "form-group",
                              tags$label(class = "form-label", "K??smi ??deme Say??s??"),
                              numericInput("k??smiodeme", NULL, value = 0, min = 0, max = 3)),
                          
                          conditionalPanel(
                            condition = "input.k??smiodeme == '1'",
                            div(class = "well",
                                h6("??deme Bilgileri", style = "color: var(--text-dark); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                                div(class = "row",
                                    div(class = "col-6",
                                        dateInput("k??smiodemetarihi1", "1. ??deme Tarihi", value = Sys.Date())),
                                    div(class = "col-6",
                                        numericInput("ko1", "1. ??deme Tutar??", value = 0))))
                          ),
                          
                          conditionalPanel(
                            condition = "input.k??smiodeme == '2'",
                            div(class = "well",
                                h6("1. ??deme Bilgileri", style = "color: var(--text-dark); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                                div(class = "row",
                                    div(class = "col-6",
                                        dateInput("k??smiodemetarihi2", "1. ??deme Tarihi", value = Sys.Date())),
                                    div(class = "col-6",
                                        numericInput("ko2", "1. ??deme Tutar??", value = 0))),
                                h6("2. ??deme Bilgileri", style = "color: var(--text-dark); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                                div(class = "row",
                                    div(class = "col-6",
                                        dateInput("k??smiodemetarihi3", "2. ??deme Tarihi", value = Sys.Date())),
                                    div(class = "col-6",
                                        numericInput("ko3", "2. ??deme Tutar??", value = 0))))
                          ),
                          
                          conditionalPanel(
                            condition = "input.k??smiodeme == '3'",
                            div(class = "well",
                                h6("1. ??deme Bilgileri", style = "color: var(--text-dark); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                                div(class = "row",
                                    div(class = "col-6",
                                        dateInput("k??smiodemetarihi4", "1. ??deme Tarihi", value = Sys.Date())),
                                    div(class = "col-6",
                                        numericInput("ko4", "1. ??deme Tutar??", value = 0))),
                                h6("2. ??deme Bilgileri", style = "color: var(--text-dark); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                                div(class = "row",
                                    div(class = "col-6",
                                        dateInput("k??smiodemetarihi5", "2. ??deme Tarihi", value = Sys.Date())),
                                    div(class = "col-6",
                                        numericInput("ko5", "2. ??deme Tutar??", value = 0))),
                                h6("3. ??deme Bilgileri", style = "color: var(--text-dark); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                                div(class = "row",
                                    div(class = "col-6",
                                        dateInput("k??smiodemetarihi6", "3. ??deme Tarihi", value = Sys.Date())),
                                    div(class = "col-6",
                                        numericInput("ko6", "3. ??deme Tutar??", value = 0))))
                          )
                        ),
                        
                        # SGK ??demesi Tab
                        div(
                          role = "tabpanel",
                          class = "tab-pane",
                          id = "sgk-odemeler",
                          br(),
                          div(class = "form-group",
                              tags$label(class = "form-label", "SGK ??demesi Var m???"),
                              div(class = "btn-group-toggle", `data-toggle` = "buttons",
                                  radioGroupButtons("sgk_odeme_var", NULL,
                                                    choices = list("Var" = "var", "Yok" = "yok"),
                                                    selected = "yok"))),
                          
                          conditionalPanel(
                            condition = "input.sgk_odeme_var == 'var'",
                            div(class = "well",
                                h6("SGK ??deme Bilgileri", style = "color: var(--cyber-blue); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                                div(class = "row",
                                    div(class = "col-6",
                                        dateInput("sgk_odeme_tarihi", "??deme Tarihi", value = Sys.Date())),
                                    div(class = "col-6",
                                        numericInput("sgk_odeme_tutari", "??deme Tutar?? (TL)", value = 0, min = 0))))
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              div(
                class = "row",
                style = "margin: 0;",
                
                # Ki??isel Bilgiler
                div(
                  class = "col-6",
                  style = "padding-left: 5px; padding-right: 15px;",
                  div(
                    class = "modern-card",
                    div(class = "card-header",
                        h4(icon("user"), " Ki??isel Bilgiler")),
                    div(
                      class = "card-body",
                      div(class = "row",
                          div(class = "col-6",
                              div(class = "form-group",
                                  tags$label(class = "form-label", "Cinsiyet"),
                                  radioGroupButtons("cinsiyet", NULL,
                                                    choices = c("Erkek", "Kad??n"),
                                                    selected = "Erkek", justified = TRUE, size = "sm"))),
                          div(class = "col-6",
                              div(class = "form-group",
                                  tags$label(class = "form-label", "Do??um Tarihi"),
                                  dateInput("dogumtarihi", NULL, value = Sys.Date() - years(30))))),
                      div(class = "row",
                          div(class = "col-6",
                              div(class = "form-group",
                                  tags$label(class = "form-label", "Maluliyet Oran?? (%)"),
                                  numericInput("maluliyet", NULL, value = 0, step = 0.1, min = 0, max = 100))),
                          div(class = "col-6",
                              div(class = "form-group",
                                  tags$label(class = "form-label", "Ge??ici Maluliyet"),
                                  radioGroupButtons("gecici_maluliyet", NULL,
                                                    choices = c("Var", "Yok"), selected = "Yok",
                                                    justified = TRUE, size = "sm"),
                                  conditionalPanel(
                                    condition = "input.gecici_maluliyet == 'Var'",
                                    numericInput("maluliyet_sure", "S??re (Ay)", value = 0.1, step = 0.1, min = 0, max = 120))
                                  
                              )))
                      
                    )
                  )
                ),
                
                # Ek Bilgiler
                div(
                  class = "col-6",
                  style = "padding-left: 15px; padding-right: 5px;",
                  div(
                    class = "modern-card",
                    div(class = "card-header",
                        h4(icon("cogs"), " Ek Bilgiler")),
                    div(
                      class = "card-body",
                      
                      # Gelir Bilgisi
                      tags$button(
                        class = "collapsible-btn",
                        onclick = "togglePanel('gelir_panel')",
                        icon("chart-line"), " Gelir Bilgileri"
                      ),
                      div(
                        id = "gelir_panel",
                        class = "collapsible-content",
                        style = "display: none;",
                        div(style = "padding: 16px;",
                            radioGroupButtons("gelir", "Gelir Durumu",
                                              choices = c("Asgari ??cret", "Di??er"), selected = "Asgari ??cret",
                                              justified = TRUE, size = "sm"),
                            conditionalPanel(
                              condition = "input.gelir == 'Asgari ??cret'",
                              selectInput("asgari_durum", "Aile Durumu",
                                          choices = c("Bekar", "evli_cocuksuz", "1cocuk","2cocuk","3cocuk","4cocuk"),
                                          selected = "Bekar")),
                            conditionalPanel(
                              condition = "input.gelir == 'Di??er'",
                              h6("Manuel Gelir Giri??i", style = "color: var(--text-dark); margin-bottom: 12px; font-weight: 600; letter-spacing: 0.3px;"),
                              div(class = "row",
                                  div(class = "col-6",
                                      textInput("gelir_2021", "2021 Gelir", placeholder = "TL"),
                                      textInput("gelir_2022", "2022 Gelir", placeholder = "TL"),
                                      textInput("gelir_2023", "2023 Gelir", placeholder = "TL")),
                                  div(class = "col-6",
                                      textInput("gelir_2024", "2024 Gelir", placeholder = "TL"),
                                      textInput("gelir_2025", "2025 Gelir", placeholder = "TL"))),
                              verbatimTextOutput("ort_gelir")))
                      ),
                      
                      # Aile Bilgileri
                      tags$button(
                        class = "collapsible-btn",
                        onclick = "togglePanel('aile_panel')",
                        icon("users"), " Aile Bilgileri"
                      ),
                      div(
                        id = "aile_panel",
                        class = "collapsible-content",
                        style = "display: none;",
                        div(style = "padding: 16px; max-height: 300px; overflow-y: auto;",
                            # E?? bilgileri
                            radioGroupButtons("es", "E??",
                                              choices = c("Var", "Yok"), selected = "Yok",
                                              justified = TRUE, size = "sm"),
                            conditionalPanel(
                              condition = "input.es == 'Var'",
                              textInput("es_isim", "E?? Ad-Soyad"),
                              dateInput("esdogumtarihi", "E?? Do??um Tarihi")),
                            
                            # Anne-Baba
                            div(class = "row",
                                div(class = "col-6",
                                    radioGroupButtons("anne", "Anne",
                                                      choices = c("Var", "Yok"), selected = "Yok",
                                                      justified = TRUE, size = "sm"),
                                    conditionalPanel(
                                      condition = "input.anne == 'Var'",
                                      textInput("anne_isim", "Anne Ad-Soyad"),
                                      dateInput("annedogumtarihi", "Do??um Tarihi"))),
                                div(class = "col-6",
                                    radioGroupButtons("baba", "Baba", 
                                                      choices = c("Var", "Yok"), selected = "Yok",
                                                      justified = TRUE, size = "sm"),
                                    conditionalPanel(
                                      condition = "input.baba == 'Var'",
                                      textInput("baba_isim", "Baba Ad-Soyad"),
                                      dateInput("babadogumtarihi", "Do??um Tarihi")))),
                            
                            # ??ocuk Bilgileri - Birinci appteki gibi 5 ??ocu??a kadar
                            div(
                              p(tags$b("1. ??ocuk")),
                              radioGroupButtons("cocuk1", NULL,
                                                choices = c("Var", "Yok"),
                                                selected = "Yok", justified = TRUE, size = "sm"),
                              conditionalPanel(
                                condition = "input.cocuk1 == 'Var'",
                                textInput("cocuk1_isim", "1.??ocuk Ad-Soyad", value = ""),
                                dateInput("cocukdogumtarihi11", "1. ??ocuk Do??um Tarihi", value = Sys.Date())
                              )
                            ),
                            
                            div(
                              p(tags$b("2. ??ocuk")),
                              radioGroupButtons("cocuk2", NULL,
                                                choices = c("Var", "Yok"),
                                                selected = "Yok", justified = TRUE, size = "sm"),
                              conditionalPanel(
                                condition = "input.cocuk2 == 'Var'",
                                textInput("cocuk2_isim", "2.??ocuk Ad-Soyad", value = ""),
                                dateInput("cocukdogumtarihi22", "2. ??ocuk Do??um Tarihi", value = Sys.Date())
                              )
                            ),
                            
                            div(
                              p(tags$b("3. ??ocuk")),
                              radioGroupButtons("cocuk3", NULL,
                                                choices = c("Var", "Yok"),
                                                selected = "Yok", justified = TRUE, size = "sm"),
                              conditionalPanel(
                                condition = "input.cocuk3 == 'Var'",
                                textInput("cocuk3_isim", "3.??ocuk Ad-Soyad", value = ""),
                                dateInput("cocukdogumtarihi33", "3. ??ocuk Do??um Tarihi", value = Sys.Date())
                              )
                            ),
                            
                            div(
                              p(tags$b("4. ??ocuk")),
                              radioGroupButtons("cocuk4", NULL,
                                                choices = c("Var", "Yok"),
                                                selected = "Yok", justified = TRUE, size = "sm"),
                              conditionalPanel(
                                condition = "input.cocuk4 == 'Var'",
                                textInput("cocuk4_isim", "4.??ocuk Ad-Soyad", value = ""),
                                dateInput("cocukdogumtarihi44", "4. ??ocuk Do??um Tarihi", value = Sys.Date())
                              )
                            ),
                            
                            div(
                              p(tags$b("5. ??ocuk")),
                              radioGroupButtons("cocuk5", NULL,
                                                choices = c("Var", "Yok"),
                                                selected = "Yok", justified = TRUE, size = "sm"),
                              conditionalPanel(
                                condition = "input.cocuk5 == 'Var'",
                                textInput("cocuk5_isim", "5.??ocuk Ad-Soyad", value = ""),
                                dateInput("cocukdogumtarihi55", "5. ??ocuk Do??um Tarihi", value = Sys.Date())
                              )
                            )
                        )
                      ),
                      
                      # Bak??c?? Bilgisi
                      tags$button(
                        class = "collapsible-btn",
                        onclick = "togglePanel('bakici_panel')",
                        icon("user-nurse"), " Bak??c?? Bilgileri"
                      ),
                      div(
                        id = "bakici_panel",
                        class = "collapsible-content",
                        style = "display: none;",
                        div(style = "padding: 16px;",
                            radioGroupButtons("bakici_gider", "Bak??c?? Gideri",
                                              choices = c("Var", "Yok"), selected = "Yok",
                                              justified = TRUE, size = "sm"),
                            radioGroupButtons("bakici_tut", "Bak??c?? Tutuldu mu?",
                                              choices = c("Evet", "Hay??r"), selected = "Hay??r",
                                              justified = TRUE, size = "sm"),
                            conditionalPanel(
                              condition = "input.bakici_gider == 'Var'",
                              numericInput("bakici_sure", "Bak??c?? S??resi (G??n)", value = 0, step = 1, min = 0, max = 365)))
                      )
                    )
                  )
                )
              ),
              
              # Girilen Veriler Tablosu
              div(
                class = "modern-card",
                style = "margin-top: 30px; margin-left: 5px; margin-right: 5px;",
                div(class = "card-header",
                    h4(icon("table"), " Girilen Veriler")),
                div(class = "card-body",
                    DTOutput('table3'))
              )
            ),
            
            # Grafikler
            tabPanel(
              title = tagList(icon("chart-line"), " Grafikler"),
              value = "grafikler",
              div(
                class = "row",
                style = "margin: 0;",
                div(class = "col-6",
                    style = "padding-left: 5px; padding-right: 15px;",
                    div(class = "modern-card",
                        div(class = "card-header", h4(icon("chart-line"), " Veri Analizi")),
                        div(class = "card-body", plotlyOutput("veri_grafik", height = "300px")))),
                div(class = "col-6",
                    style = "padding-left: 15px; padding-right: 5px;",
                    div(class = "modern-card",
                        div(class = "card-header", h4(icon("chart-pie"), " Da????l??m")),
                        div(class = "card-body", plotlyOutput("dagilim_grafik", height = "300px"))))
              ),
              div(
                class = "modern-card",
                style = "margin-top: 30px; margin-left: 5px; margin-right: 5px;",
                div(class = "card-header", h4(icon("chart-bar"), " Kar????la??t??rma")),
                div(class = "card-body", plotlyOutput("karsilastirma_grafik", height = "300px"))
              )
            ),
            
            # ??statistikler
            tabPanel(
              title = tagList(icon("chart-bar"), " ??statistikler"),
              value = "istatistikler",
              div(
                class = "modern-card",
                style = "margin-left: 5px; margin-right: 5px;",
                div(class = "card-header", h4(icon("database"), " T??m Kay??tlar")),
                div(class = "card-body", DTOutput("tum_kayitlar"))
              ),
              div(
                class = "row",
                style = "margin-top: 30px; margin-left: 0; margin-right: 0;",
                div(class = "col-6",
                    style = "padding-left: 5px; padding-right: 15px;",
                    div(class = "modern-card",
                        div(class = "card-header", h4(icon("chart-area"), " ??zet ??statistikler")),
                        div(class = "card-body", verbatimTextOutput("ozet_istatistik")))),
                div(class = "col-6",
                    style = "padding-left: 15px; padding-right: 5px;",
                    div(class = "modern-card",
                        div(class = "card-header", h4(icon("download"), " Veri ??ndirme")),
                        div(class = "card-body",
                            downloadButton("dl", "Excel Raporu ??ndir", class = "btn btn-primary btn-block"),
                            br(), br(),
                            downloadButton("downloadData", "T??m Veritaban??", class = "btn btn-success btn-block"))))
              )
            ),
            
            # Yard??m
            tabPanel(
              title = tagList(icon("question-circle"), " Yard??m"),
              value = "yardim",
              div(
                class = "modern-card",
                style = "margin-left: 5px; margin-right: 5px;",
                div(class = "card-header", h4(icon("question-circle"), " PROINSURE Hakk??nda")),
                div(
                  class = "card-body",
                  p("PROINSURE, maluliyet ve destekten yoksun kalma tazminat?? hesaplamalar??n?? kolayla??t??rmak i??in geli??tirilmi?? futuristik bir ERP platformudur.", 
                    style = "color: #e2e8f0; line-height: 1.6;"),
                  
                  h5("Nas??l Kullan??l??r?", style = "color: var(--text-light); margin-top: 24px; font-weight: 600; text-shadow: 0 0 15px rgba(248, 250, 252, 0.7); letter-spacing: 0.5px;"),
                  tags$ol(
                    style = "color: #e2e8f0; line-height: 1.8;",
                    tags$li("Hesaplama y??ntemini se??in (Akt??eryal veya Progresif Rant)."),
                    tags$li("Ya??am tablosunu se??in ve ilgili parametreleri ayarlay??n."),
                    tags$li("T??m gerekli bilgileri formlar?? doldurarak girin."),
                    tags$li("'Hesaplama Ba??lat' butonuna t??klay??n."),
                    tags$li("Rapor t??r??n?? se??ip 'Verileri Kaydet' butonuna t??klay??n."),
                    tags$li("'Rapor ??ndir' butonuyla sonu??lar?? indirin.")
                  ),
                  
                  h5("Destek", style = "color: var(--text-light); margin-top: 24px; font-weight: 600; text-shadow: 0 0 15px rgba(248, 250, 252, 0.7); letter-spacing: 0.5px;"),
                  div(class = "well",
                      style = "background: rgba(0, 204, 255, 0.1); border-color: rgba(0, 204, 255, 0.3);",
                      p(tags$strong("E-posta:"), " destek@proinsure.com", style = "color: #e2e8f0;"),
                      p(tags$strong("Telefon:"), " +90 212 555 6789", style = "color: #e2e8f0;"),
                      p(tags$strong("Web:"), " www.proinsure.com", style = "color: #e2e8f0;"))
                )
              )
            )
          )
        )
      ),
      
      
    )
  })
  
  ## SERVER FUNCs ----
  
  # ReactiveValues object to store the accumulated data
  values <- reactiveValues(data = load_data(data_file))
  
  # Reactive value to control the visibility of the data table and download button
  show_table <- reactiveVal(FALSE)
  show_download_button <- reactiveVal(FALSE)
  
  # Tablo ve rapor se??imleri i??in reactive values
  observe({
    if (is.null(input$tablo1)) {
      updateTextInput(session, "tablo1", value = "TRH-2010")
    }
    if (is.null(input$tablo2)) {
      updateTextInput(session, "tablo2", value = "TRH-2010")
    }
    if (is.null(input$rapor)) {
      updateTextInput(session, "rapor", value = "T??m Rapor-1 ??deme")
    }
  })
  
  # ????lem ba??lat butonu
  observeEvent(input$action, {
    showNotification("Hesaplama i??lemi ba??lat??ld??...", type = "message")
    Sys.sleep(1)
    showNotification("Hesaplama i??lemi tamamland??!", type = "message")
  })
  
  # Observe event for the submit button
  observeEvent(input$submit, {
    # Calculate duration in minutes
    duration <- as.numeric(round(difftime(Sys.time(), values$start_time, units = "mins"), digits = 2))
    
    # Rapor t??r??n?? belirle
    rapor_turu <- if(!is.null(input$rapor)) input$rapor else "T??m Rapor-1 ??deme"
    
    # Create a new record
    new_record <- tibble(
      ID = generate_id(),
      user_name = isolate(user_data()$user_name),
      DosyaNo = ifelse(is.null(input$dosya) || input$dosya == "", "-", input$dosya),
      Cinsiyet = input$cinsiyet,
      DogumTarihi = as.character(input$dogumtarihi),
      Gelir = input$gelir,
      KazaTarihi = as.character(input$kazatarihi),
      MaluliyetOran = input$maluliyet, 
      KusurOran = input$kusur,
      GeciciMaluliyetSure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
      KismiOdemeSay = input$k??smiodeme,
      RaporTur = rapor_turu,
      Duration = duration,
      EntryTime = as.character(Sys.time())
    )
    
    # Append the new record to the existing data
    values$data <- bind_rows(values$data, new_record)
    
    # Save the updated data to the CSV file
    save_data(values$data, data_file)
    
    # Show the data table and download button
    show_table(TRUE)
    
    # Show a notification
    showNotification("Veriler ba??ar??yla kaydedildi!", type = "message")
  })
  
  # Initialize start_time reactive value
  observe({
    values$start_time <- Sys.time()
  })
  
  # Ortalama gelir hesaplamas??
  output$ort_gelir <- renderText({
    if(input$gelir == "Di??er") {
      gelir_values <- c(
        as.numeric(input$gelir_2021),
        as.numeric(input$gelir_2022),
        as.numeric(input$gelir_2023),
        as.numeric(input$gelir_2024),
        as.numeric(input$gelir_2025)
      )
      
      valid_values <- gelir_values[!is.na(gelir_values) & gelir_values > 0]
      
      if(length(valid_values) > 0) {
        ortalama <- round(mean(valid_values), 0)
        paste0("Ortalama Gelir: ", format(ortalama, big.mark = ".", decimal.mark = ","), " TL")
      } else {
        "L??tfen en az bir y??l i??in gelir giriniz."
      }
    } else {
      paste0("Asgari ??cret Bazl??: ", format(17002, big.mark = ".", decimal.mark = ","), " TL")
    }
  })
  
  # Reactive expression to create data frame of all input values
  sliderValues <- reactive({
    safe_date_check <- function(date_input) {
      if(is.null(date_input)) return("-")
      if(as.character(date_input) == as.character(Sys.Date())) return("-")
      return(as.character(date_input))
    }
    
    safe_value <- function(input_val, default = "") {
      if(is.null(input_val)) return(default)
      return(as.character(input_val))
    }
    
    kot1 <- ifelse(as.character(input$k??smiodemetarihi1) == Sys.Date(),"-",as.character(input$k??smiodemetarihi1))
    kot2 <- ifelse(as.character(input$k??smiodemetarihi2) == Sys.Date(),"-",as.character(input$k??smiodemetarihi2))
    kot3 <- ifelse(as.character(input$k??smiodemetarihi3) == Sys.Date(),"-",as.character(input$k??smiodemetarihi3))
    kot4 <- ifelse(as.character(input$k??smiodemetarihi4) == Sys.Date(),"-",as.character(input$k??smiodemetarihi4))
    kot5 <- ifelse(as.character(input$k??smiodemetarihi5) == Sys.Date(),"-",as.character(input$k??smiodemetarihi5))
    kot6 <- ifelse(as.character(input$k??smiodemetarihi6) == Sys.Date(),"-",as.character(input$k??smiodemetarihi6))
    
    excel_data <- if(input$k??smiodeme == '1' & input$gelir == "Asgari ??cret") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "Bak??c?? S??resi (ay)",
                 "K??smi ??deme Say??s??",
                 "K??smi ??deme Tarihi-1",
                 "K??smi ??deme Tutar??-1"
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$asgari_durum),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$k??smiodeme, "0"),
                  kot1,
                  safe_value(input$ko1, "0")
        )
      )
    } 
    
    else if(input$k??smiodeme == '2' & input$gelir == "Asgari ??cret") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Bak??c?? S??resi (ay)",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "K??smi ??deme Say??s??",
                 "K??smi ??deme Tarihi-1",
                 "K??smi ??deme Tutar??-1",
                 "K??smi ??deme Tarihi-2",
                 "K??smi ??deme Tutar??-2"
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$asgari_durum),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$k??smiodeme, "0"),
                  kot2,
                  safe_value(input$ko2, "0"),
                  kot3,
                  safe_value(input$ko3, "0")
        )
      )
    } 
    
    else if(input$k??smiodeme == '3' & input$gelir == "Asgari ??cret") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "Bak??c?? S??resi (ay)",
                 "K??smi ??deme Say??s??",
                 "K??smi ??deme Tarihi-1",
                 "K??smi ??deme Tutar??-1",
                 "K??smi ??deme Tarihi-2",
                 "K??smi ??deme Tutar??-2",
                 "K??smi ??deme Tarihi-3",
                 "K??smi ??deme Tutar??-3"
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$asgari_durum),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$k??smiodeme, "0"),
                  kot4,
                  safe_value(input$ko4, "0"),
                  kot5,
                  safe_value(input$ko5, "0"),
                  kot6,
                  safe_value(input$ko6, "0")
        )
      )
    }
    
    else if(input$k??smiodeme == '1' & input$gelir == "Di??er") {
      
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Bak??c?? S??resi (ay)",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "K??smi ??deme Say??s??",
                 "K??smi ??deme Tarihi-1",
                 "K??smi ??deme Tutar??-1"
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$k??smiodeme, "0"),
                  kot1,
                  safe_value(input$ko1, "0")
        )
      )
    }
    
    
    
    else if(input$k??smiodeme == '2' & input$gelir == "Di??er") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "Bak??c?? S??resi (ay)",
                 "K??smi ??deme Say??s??",
                 "K??smi ??deme Tarihi-1",
                 "K??smi ??deme Tutar??-1",
                 "K??smi ??deme Tarihi-2",
                 "K??smi ??deme Tutar??-2"
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$k??smiodeme, "0"),
                  kot2,
                  safe_value(input$ko2, "0"),
                  kot3,
                  safe_value(input$ko3, "0")
        )
      )
    } 
    
    else if(input$k??smiodeme == '3' & input$gelir == "Di??er") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "Bak??c?? S??resi (ay)",
                 "K??smi ??deme Say??s??",
                 "K??smi ??deme Tarihi-1",
                 "K??smi ??deme Tutar??-1",
                 "K??smi ??deme Tarihi-2",
                 "K??smi ??deme Tutar??-2",
                 "K??smi ??deme Tarihi-3",
                 "K??smi ??deme Tutar??-3"
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$k??smiodeme, "0"),
                  kot4,
                  safe_value(input$ko4, "0"),
                  kot5,
                  safe_value(input$ko5, "0"),
                  kot6,
                  safe_value(input$ko6, "0")
        )
      )
    }
    
    
    else if(input$k??smiodeme == '0' & input$gelir == "Asgari ??cret") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Bak??c?? S??resi (ay)",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "K??smi ??deme Say??s??"
                 
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$k??smiodeme, "0")
                  
        )
      )
    }
    
    
    else {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Do??um Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oran",
                 "Bak??c?? S??resi (ay)",
                 "Kusur Oran??",
                 "Ge??ici Maluliyet (ay)",
                 "K??smi ??deme Say??s??"
                 
        ),
        
        Value = c(safe_value(input$dosya),
                  safe_value(input$isim),
                  safe_value(input$cinsiyet),
                  safe_value(input$dogumtarihi),
                  safe_value(input$gelir),
                  safe_value(input$asgari_durum),
                  safe_value(input$kazatarihi),
                  safe_value(input$maluliyet, "0"),
                  safe_value(input$bakici_sure, "0"),
                  safe_value(input$kusur, "0"),
                  safe_value(input$maluliyet_sure, "0"),
                  safe_value(input$k??smiodeme, "0")
                  
        )
      )
    }
    
    
  })
  
  # Show the values in an HTML table
  output$table3 <- renderDT({
    datatable(
      sliderValues(),
      options = list(
        pageLength = 10,
        dom = 'rtip',
        scrollY = "300px",
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        ),
        language = list(
          emptyTable = "Hen??z veri girilmedi",
          info = "G??sterilen: _START_ - _END_ / Toplam: _TOTAL_",
          search = "Ara:",
          paginate = list(
            first = "??lk",
            last = "Son", 
            `next` = "Sonraki",
            previous = "??nceki"
          )
        )
      ),
      rownames = FALSE
    )
  })
  
  # Grafik ????kt??lar??
  output$veri_grafik <- renderPlotly({
    plot_data <- values$data %>%
      mutate(EntryDate = as.Date(EntryTime)) %>%
      group_by(EntryDate) %>%
      summarise(Count = n()) %>%
      arrange(EntryDate)
    
    plot_ly(data = plot_data, x = ~EntryDate, y = ~Count, type = "scatter", mode = "lines+markers",
            line = list(color = "#00ccff", width = 3),
            marker = list(color = "#ff2975", size = 8)) %>%
      layout(title = "G??nl??k Veri Giri??i",
             xaxis = list(title = "Tarih", gridcolor = "rgba(255,255,255,0.1)", 
                          tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0")),
             yaxis = list(title = "Kay??t Say??s??", gridcolor = "rgba(255,255,255,0.1)", 
                          tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0")),
             paper_bgcolor = "rgba(26, 31, 46, 0.8)",
             plot_bgcolor = "rgba(26, 31, 46, 0.8)",
             font = list(color = "#e2e8f0"))
  })
  
  output$dagilim_grafik <- renderPlotly({
    gender_data <- values$data %>%
      group_by(Cinsiyet) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    plot_ly(data = gender_data, labels = ~Cinsiyet, values = ~Count, type = "pie",
            textinfo = "label+percent",
            insidetextfont = list(color = "white"),
            marker = list(colors = c("#00ccff", "#ff2975"))) %>%
      layout(title = "Cinsiyet Da????l??m??",
             paper_bgcolor = "rgba(26, 31, 46, 0.8)",
             plot_bgcolor = "rgba(26, 31, 46, 0.8)",
             font = list(color = "#e2e8f0"),
             showlegend = FALSE)
  })
  
  output$karsilastirma_grafik <- renderPlotly({
    comp_data <- values$data %>%
      select(ID, MaluliyetOran, KusurOran) %>%
      arrange(ID) %>%
      head(10)
    
    plot_ly() %>%
      add_trace(data = comp_data, x = ~ID, y = ~MaluliyetOran, type = "bar", name = "Maluliyet Oran??",
                marker = list(color = "#00ccff")) %>%
      add_trace(data = comp_data, x = ~ID, y = ~KusurOran, type = "bar", name = "Kusur Oran??",
                marker = list(color = "#ff2975")) %>%
      layout(title = "Maluliyet ve Kusur Oranlar??",
             xaxis = list(title = "Kay??t ID", gridcolor = "rgba(255,255,255,0.1)", 
                          tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0")),
             yaxis = list(title = "Oran (%)", gridcolor = "rgba(255,255,255,0.1)", 
                          tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0")),
             barmode = "group",
             paper_bgcolor = "rgba(26, 31, 46, 0.8)",
             plot_bgcolor = "rgba(26, 31, 46, 0.8)",
             font = list(color = "#e2e8f0"),
             legend = list(font = list(color = "#e2e8f0")))
  })
  
  # T??m kay??tlar tablosu
  output$tum_kayitlar <- renderDT({
    datatable(values$data, 
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                dom = 'Bfrtip',
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                ),
                language = list(
                  emptyTable = "Hen??z kay??t bulunmuyor",
                  info = "_TOTAL_ kay??ttan _START_ - _END_ aras?? g??steriliyor",
                  search = "Ara:",
                  paginate = list(
                    previous = "??nceki",
                    `next` = "Sonraki"
                  )
                )
              ),
              rownames = FALSE)
  })
  
  # ??zet istatistikler
  output$ozet_istatistik <- renderPrint({
    if(nrow(values$data) > 0) {
      cat("??ZET ??STAT??ST??KLER\n\n")
      cat("Toplam Kay??t Say??s??:", nrow(values$data), "\n\n")
      
      cat("Cinsiyet Da????l??m??:\n")
      print(table(values$data$Cinsiyet))
      cat("\n")
      
      cat("Maluliyet Oran?? ??statistikleri:\n")
      print(summary(values$data$MaluliyetOran))
      cat("\n")
      
      cat("Kusur Oran?? ??statistikleri:\n")
      print(summary(values$data$KusurOran))
    } else {
      cat("Hen??z veri bulunmuyor.")
    }
  })
  
  # Downloads
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("proinsure_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$data, file, row.names = FALSE)
    }
  )
  
  output$dl <- downloadHandler(
    filename = function() {"proinsure_genel_bilgiler.xlsx"},
    content = function(file) {
      if(require(writexl)) {
        writexl::write_xlsx(sliderValues(), path = file)
      } else {
        write.csv(sliderValues(), file, row.names = FALSE)
      }
    }
  )
  
  ## GENERATE REPORT DOWNLOAD HANDLER - B??R??NC?? APPDEN KOPYALANDI ----
  output$generate_report <- downloadHandler(
    filename = "ProInsure_Report.docx",
    content = function(file) {
      
      if(input$rapor == "T??m Rapor-1 ??deme") {
        res <- rmarkdown::render(
          file.path(getwd(), "tum_report_1odeme.Rmd"),
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$k??smiodeme,
            PKismi_Odeme_Tarihi_1 = input$k??smiodemetarihi1,
            PKismi_Odeme_Tutari_1 = input$ko1,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PBakici = ifelse(is.null(input$bakici_gider), "Yok", input$bakici_gider),
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2)
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "T??m Rapor-2 ??deme") {
        res <- rmarkdown::render(
          file.path(getwd(), "tum_report_2odeme.Rmd"),
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$k??smiodeme,
            PKismi_Odeme_Tarihi_1 = input$k??smiodemetarihi2,
            PKismi_Odeme_Tutari_1 = input$ko2,
            PKismi_Odeme_Tarihi_2 = input$k??smiodemetarihi3,
            PKismi_Odeme_Tutar??_2 = input$ko3,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PBakici = ifelse(is.null(input$bakici_gider), "Yok", input$bakici_gider),
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2)
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "T??m Rapor (??irket ??demesiz)") {
        
        res <- rmarkdown::render(
          file.path(getwd(),  "tum_report_sirket_odemesiz1.Rmd"),
          
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PBakici = ifelse(is.null(input$bakici_gider), "Yok", input$bakici_gider),
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2)
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "S??rekli+Ge??ici (??irket ??demesiz)") {
        
        res <- rmarkdown::render(
          file.path(getwd(),  "surekli_gecici_sirket_odemesiz.Rmd"),
          
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2)
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Ge??ici (??irket ??demesiz)") {
        
        res <- rmarkdown::render(
          file.path(getwd(),  "gecici_sirket_odemesiz.Rmd"),
          
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$k??smiodeme,
            PKismi_Odeme_Tarihi_1 = input$k??smiodemetarihi2,
            PKismi_Odeme_Tutari_1 = input$ko2,
            PKismi_Odeme_Tarihi_2 = input$k??smiodemetarihi3,
            PKismi_Odeme_Tutari_2 = input$ko3,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PBakici = ifelse(is.null(input$bakici_gider), "Yok", input$bakici_gider),
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2),
            Psgk_odeme_tutar = input$sgk_odeme_tutari
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "S??rekli (??irket ??demesiz)") {
        
        res <- rmarkdown::render(
          file.path(getwd(),  "surekli_sirket_odemesiz.Rmd"),
          
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2)
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "S??rekli (??irket ??demeli)") {
        
        res <- rmarkdown::render(
          file.path(getwd(),  "surekli_sirket_odemeli.Rmd"),
          
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$k??smiodeme,
            PKismi_Odeme_Tarihi_1 = input$k??smiodemetarihi1,
            PKismi_Odeme_Tutari_1 = input$ko1,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PBakici = ifelse(is.null(input$bakici_gider), "Yok", input$bakici_gider),
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2)
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Destek") {
        
        res <- rmarkdown::render(
          file.path(getwd(),  "destek_hesap.Rmd"),
          
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$k??smiodeme,
            PKismi_Odeme_Tarihi_1 = input$k??smiodemetarihi1,
            PKismi_Odeme_Tutari_1 = input$ko1,
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2),
            PEs = ifelse(is.null(input$es), "Yok", input$es),
            PEsAd = ifelse(is.null(input$es_isim), "", input$es_isim),
            PEsDT = ifelse(is.null(input$esdogumtarihi), Sys.Date(), input$esdogumtarihi),
            PAnne = ifelse(is.null(input$anne), "Yok", input$anne),
            PAnneAd = ifelse(is.null(input$anne_isim), "", input$anne_isim),
            PAnneDT = ifelse(is.null(input$annedogumtarihi), Sys.Date(), input$annedogumtarihi),
            PBaba = ifelse(is.null(input$baba), "Yok", input$baba),
            PBabaAd = ifelse(is.null(input$baba_isim), "", input$baba_isim),
            PBabaDT = ifelse(is.null(input$babadogumtarihi), Sys.Date(), input$babadogumtarihi),
            PCocuksay = ifelse(is.null(input$cocuk1) || input$cocuk1 == "Yok", 0,
                               ifelse(is.null(input$cocuk2) || input$cocuk2 == "Yok", 1,
                                      ifelse(is.null(input$cocuk3) || input$cocuk3 == "Yok", 2,
                                             ifelse(is.null(input$cocuk4) || input$cocuk4 == "Yok", 3,
                                                    ifelse(is.null(input$cocuk5) || input$cocuk5 == "Yok", 4, 5))))),
            PCocuk1_DT = ifelse(is.null(input$cocukdogumtarihi11), Sys.Date(), input$cocukdogumtarihi11),
            PCocuk1_Ad = ifelse(is.null(input$cocuk1_isim), "", input$cocuk1_isim),
            PCocuk2_DT = ifelse(is.null(input$cocukdogumtarihi22), Sys.Date(), input$cocukdogumtarihi22),
            PCocuk2_Ad = ifelse(is.null(input$cocuk2_isim), "", input$cocuk2_isim),
            PCocuk3_DT = ifelse(is.null(input$cocukdogumtarihi33), Sys.Date(), input$cocukdogumtarihi33),
            PCocuk3_Ad = ifelse(is.null(input$cocuk3_isim), "", input$cocuk3_isim),
            PCocuk4_DT = ifelse(is.null(input$cocukdogumtarihi44), Sys.Date(), input$cocukdogumtarihi44),
            PCocuk4_Ad = ifelse(is.null(input$cocuk4_isim), "", input$cocuk4_isim),
            PCocuk5_DT = ifelse(is.null(input$cocukdogumtarihi55), Sys.Date(), input$cocukdogumtarihi55),
            PCocuk5_Ad = ifelse(is.null(input$cocuk5_isim), "", input$cocuk5_isim)
          )
        )
        file.rename(res, file)
        
      } else  {
        
        res <- rmarkdown::render(
          file.path(getwd(),  "destek_hesap.Rmd"),
          params = list(
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2)
          )
        )
        file.rename(res, file)
      }
      
    }
  )
  
  
  ##############################################################################
  
  
  # HTML Rapor ??retme ve g??r??nt??leme
  observeEvent(input$view_html_report, {
    if(input$rapor == "Ge??ici (??irket ??demesiz)") {
      tryCatch({
        # HTML rapor ??ret
        html_file <- tempfile(fileext = ".html")
        rmarkdown::render(
          file.path(getwd(), "gecici_sirket_odemesiz_html.Rmd"),
          output_file = html_file,
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$k??smiodeme,
            PKismi_Odeme_Tarihi_1 = input$k??smiodemetarihi2,
            PKismi_Odeme_Tutari_1 = input$ko2,
            PKismi_Odeme_Tarihi_2 = input$k??smiodemetarihi3,
            PKismi_Odeme_Tutari_2 = input$ko3,
            PGecici_Maluliyet_Sure = ifelse(is.null(input$maluliyet_sure), 0, input$maluliyet_sure),
            PBakici = ifelse(is.null(input$bakici_gider), "Yok", input$bakici_gider),
            PBakici_Sure = ifelse(is.null(input$bakici_sure), 0, input$bakici_sure),
            PGelir = ifelse(is.null(input$asgari_durum), "Bekar", input$asgari_durum),
            PYasam_Tablosu = ifelse(is.null(input$tablo2), "TRH-2010", input$tablo2),
            Psgk_odeme_tutar = input$sgk_odeme_tutari
          )
        )
        
        # HTML i??eri??ini oku
        html_content <- readLines(html_file, encoding = "UTF-8")
        html_content <- paste(html_content, collapse = "\n")
        
        # Modal g??ster
        showModal(modalDialog(
          title = "Akt??eryal Bilirki??i Raporu",
          HTML(html_content),
          size = "l",
          easyClose = TRUE,
          footer = tagList(
            # downloadButton("download_html_report", "HTML ??ndir", class = "btn btn-success"),
            modalButton("Kapat")

          )
        ))
        
        # Download handler'?? g??ncelle
        output$download_html_report <- downloadHandler(
          filename = function() {
            paste0("Bilirkisi_Raporu_", Sys.Date(), ".html")
          },
          content = function(file) {
            file.copy(html_file, file)
          }
        )
        
      }, error = function(e) {
        showNotification(paste("Rapor olu??turulurken hata:", e$message), type = "error")
      })
    } else {
      showNotification("Bu ??zellik ??u anda sadece 'Ge??ici (??irket ??demesiz)' raporu i??in mevcuttur.", type = "warning")
    }
  })
  
  
  ##############################################################################
  
  
  # Sample data
  observe({
    if(nrow(values$data) == 0) {
      sample_data <- data.frame(
        ID = sapply(1:5, function(x) generate_id()),
        user_name = c("Erus1*", "Erus2*", "Erus1*", "Erus3*", "Erus2*"),
        DosyaNo = paste0("PRDOC", 1001:1005),
        Cinsiyet = sample(c("Erkek", "Kad??n"), 5, replace = TRUE),
        DogumTarihi = as.character(as.Date("1980-01-01") + sample(1:10000, 5)),
        Gelir = sample(c("Asgari ??cret", "Di??er"), 5, replace = TRUE),
        KazaTarihi = as.character(as.Date("2022-01-01") + sample(1:365, 5)),
        MaluliyetOran = round(runif(5, 5, 85), 1),
        KusurOran = round(runif(5, 0, 100), 1),
        GeciciMaluliyetSure = sample(0:24, 5, replace = TRUE),
        KismiOdemeSay = sample(0:3, 5, replace = TRUE),
        RaporTur = sample(c("T??m Rapor-1 ??deme", "S??rekli (??irket ??demesiz)", "Destek"), 5, replace = TRUE),
        Duration = sample(5:120, 5, replace = TRUE),
        EntryTime = as.character(Sys.time() - sample(1:100, 5) * 60 * 60 * 24),
        stringsAsFactors = FALSE
      )
      values$data <- sample_data
      save_data(values$data, data_file)
    }
  })
  
  # Logout functionality
  observeEvent(input$logout, {
    showModal(modalDialog(
      title = "????k???? Onay??",
      "Sistemden ????kmak istedi??inizden emin misiniz?",
      footer = tagList(
        actionButton("confirmLogout", "Evet, ????k???? Yap", class = "btn btn-danger"),
        modalButton("??ptal")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirmLogout, {
    session$reload()
  })
}


# SGK ??demesi i??in yeni reactive'ler
observe({
  # SGK ??demesi "Yok" se??ildi??inde tutar?? s??f??rla
  if(!is.null(input$sgk_odeme_var) && input$sgk_odeme_var == "yok") {
    updateNumericInput(session, "sgk_odeme_tutari", value = 0)
  }
})

# SGK ??deme durumu kontrol??
sgk_odeme_durumu <- reactive({
  if(is.null(input$sgk_odeme_var)) {
    return(FALSE)
  }
  return(input$sgk_odeme_var == "var")
})

# SGK ??deme tutar??
sgk_odeme_tutari <- reactive({
  if(sgk_odeme_durumu() && !is.null(input$sgk_odeme_tutari)) {
    return(input$sgk_odeme_tutari)
  }
  return(0)
})

# SGK ??deme tarihi
sgk_odeme_tarihi <- reactive({
  if(sgk_odeme_durumu() && !is.null(input$sgk_odeme_tarihi)) {
    return(input$sgk_odeme_tarihi)
  }
  return(NA)
})




# Uygulamay?? ??al????t??r
shinyApp(ui = ui, server = server)

