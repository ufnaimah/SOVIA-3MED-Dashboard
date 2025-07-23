<img width="1366" height="768" alt="image" src="https://github.com/user-attachments/assets/a35245be-6f1f-4c4b-a9aa-d64e9fc7e0cf" />

# 🌍 SOVIA-3MED Dashboard

## Deskripsi
**SOVIA-3MED Dashboard** (Social Vulnerability Analysis – Mitigasi, Evakuasi, dan Dampak) adalah aplikasi interaktif berbasis **R Shiny** yang dirancang untuk menganalisis kerentanan sosial terhadap bencana alam di Indonesia. Menggunakan data **SUSENAS 2017** dan data spasial Indonesia, dashboard ini membantu pembuat kebijakan dan peneliti:

- Mengidentifikasi wilayah rentan
- Memprioritaskan intervensi
- Mendukung strategi mitigasi bencana berbasis data

📌 Proyek ini merupakan tugas Ujian Akhir Semester (UAS) mata kuliah **Komputasi Statistik** di **Politeknik Statistika STIS** oleh *Nur Na'imah Ma'ruf*.

---

Link dapat diakses melalui https://kmfotw-nur-na0imah0ma0ruf.shinyapps.io/SOVIA3MED/

## 🎯 Fitur Utama

### 📌 Beranda
- Gambaran umum kerentanan sosial di Indonesia
- Galeri foto dan peta nasional
- Fokus utama: Dampak, Mitigasi, dan Evakuasi
- Metadata struktural dan referensi
- Ekspor tampilan halaman sebagai PNG

### 📊 Eksplorasi Data
- Visualisasi: boxplot, histogram, peta Leaflet
- Statistik deskriptif: tabel interaktif (DT)
- Ekspor: grafik (PNG, DOCX), tabel (DOCX)

### 🛠️ Manajemen Data
- Transformasi variabel & kategorisasi
- Pemetaan kerentanan (Leaflet)
- Klasifikasi Jenks Natural Breaks
- Penentuan distrik prioritas
- Ekspor hasil: PNG & Excel

### 🧪 Uji Asumsi
- Uji Normalitas (QQ Plot & Histogram)
- Uji Homogenitas varians
- Interpretasi otomatis dan ekspor hasil (PNG, DOCX)

### 📐 Inferensia
- Uji t, proporsi, variansi, dan ANOVA
- Panel kontrol interaktif untuk memilih analisis
- Ekspor interpretasi hasil (DOCX)

### 📈 Regresi
- Regresi linear berganda & stepwise
- Uji asumsi klasik (normalitas, multikolinearitas, heteroskedastisitas, autokorelasi)
- Ringkasan model dan hasil interpretasi dapat diunduh (DOCX)

### 📥 Unduh Hasil
- Pusat unduhan untuk seluruh analisis:
  - Per halaman: PNG, CSV, Excel, PDF, Word
  - Paket lengkap: ZIP
  - Ringkasan eksekutif: PDF/Word
- Indikator progres pengunduhan
- Catatan: jalankan analisis terlebih dahulu agar file terisi

### 🧾 Footer
- Hak Cipta © 2025 Nur Na'imah Ma'ruf – Politeknik Statistika STIS

---

## 💻 Teknologi yang Digunakan

- **R Shiny**: framework aplikasi web interaktif
- **Plotly**: visualisasi interaktif (boxplot, histogram)
- **Leaflet**: pemetaan spasial interaktif
- **DT**: tabel interaktif (DataTables)
- **html2canvas**: ekspor tampilan halaman ke PNG
- **CSS kustom**: styling visual & efek spinner loading

---

## 📦 Prasyarat

- **R** versi terbaru (disarankan ≥ 4.0)
- Instalasi paket:
```r
install.packages(c("shiny", "plotly", "leaflet", "DT"))
```

## 📁 Struktur Repositori
```r
sovia-3med-dashboard/
├── app.R               # Kode utama aplikasi
├── www/                # File statis (gambar, css)
│   ├── foto1.jpg       # Gambar galeri
│   ├── custom.css      # Gaya visual aplikasi
├── data/               # Folder data (tidak diunggah)
└── README.md           # Dokumentasi proyek
```

## 🙋‍♀️ Kontribusi & Pengembang
Dashboard ini dikembangkan sebagai bagian dari tugas akhir semester untuk mata kuliah Komputasi Statistik oleh:
- Nur Na'imah Ma'ruf
📍 Politeknik Statistika STIS
