# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("knitr")
# install.packages("ggplot2")
# install.packages("plotly")

# Load library
library(dplyr)
library(lubridate)
library(knitr)
library(ggplot2)
library(plotly)

transaksi <- data.frame(
  tanggal = as.Date(c("2023-01-01", "2023-01-05", "2023-01-10", "2023-01-15", "2023-01-20")),
  keterangan = c("Modal Awal", "Pembelian Barang", "Penjualan Barang", "Biaya Operasional", "Pembayaran Hutang"),
  debit = c(10000000, 0, 8000000, 0, 0),
  kredit = c(0, 5000000, 0, 2000000, 3000000)
)

cat("Data Transaksi:\n")
kable(transaksi)

transaksi <- transaksi %>%
  mutate(saldo = cumsum(debit - kredit))

cat("\nTransaksi dengan Saldo:\n")
kable(transaksi)

kelompokkan_transaksi <- function(df) {
  df %>%
    mutate(
      kas = ifelse(grepl("Modal|Penjualan|Pembayaran", keterangan), debit, kredit),
      pendapatan = ifelse(grepl("Penjualan", keterangan), debit, 0),
      beban = ifelse(grepl("Biaya", keterangan), kredit, 0),
      hutang = ifelse(grepl("Hutang", keterangan), kredit, 0)
    ) %>%
    select(tanggal, keterangan, kas, pendapatan, beban, hutang)
}

transaksi_kelompok <- kelompokkan_transaksi(transaksi)

cat("\nTransaksi yang Dikelompokkan:\n")
kable(transaksi_kelompok)

buat_laporan_laba_rugi <- function(df) {
  total_pendapatan <- sum(df$pendapatan, na.rm = TRUE)
  total_beban <- sum(df$beban, na.rm = TRUE)
  laba_bersih <- total_pendapatan - total_beban
  
  laporan_lr <- data.frame(
    item = c("Total Pendapatan", "Total Beban", "Laba Bersih"),
    jumlah = c(total_pendapatan, total_beban, laba_bersih)
  )
  
  return(laporan_lr)
}

laporan_lr <- buat_laporan_laba_rugi(transaksi_kelompok)
cat("\nLaporan Laba Rugi:\n")
kable(laporan_lr)

buat_neraca <- function(df) {
  total_kas <- sum(df$kas, na.rm = TRUE)
  total_hutang <- sum(df$hutang, na.rm = TRUE)
  ekuitas <- total_kas - total_hutang
  
  neraca <- data.frame(
    item = c("Kas", "Hutang", "Ekuitas"),
    jumlah = c(total_kas, total_hutang, ekuitas)
  )
  
  return(neraca)
}
neraca <- buat_neraca(transaksi_kelompok)
cat("\nNeraca:\n")
kable(neraca)

cat("\nAnalisis Keuangan Sederhana:\n")
cat("- Rasio Laba terhadap Pendapatan:", 
    (laporan_lr$jumlah[3] / laporan_lr$jumlah[1]) * 100, "%\n")
cat("- Rasio Kas terhadap Hutang:", 
    (neraca$jumlah[1] / neraca$jumlah[2]) * 100, "%\n")

plot_saldo <- ggplot(transaksi, aes(x = tanggal, y = saldo)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Tren Saldo Kas",
       x = "Tanggal",
       y = "Saldo (Rp)") +
  theme_minimal()

print(plot_saldo)

plot_lr <- ggplot(laporan_lr, aes(x = item, y = jumlah, fill = item)) +
  geom_bar(stat = "identity") +
  labs(title = "Laporan Laba Rugi",
       x = "Item",
       y = "Jumlah (Rp)") +
  theme_minimal() +
  scale_fill_manual(values = c("Total Pendapatan" = "green", 
                               "Total Beban" = "red", 
                               "Laba Bersih" = "blue"))

print(plot_lr)

plot_neraca <- ggplot(neraca, aes(x = item, y = jumlah, fill = item)) +
  geom_bar(stat = "identity") +
  labs(title = "Neraca",
       x = "Akun",
       y = "Jumlah (Rp)") +
  theme_minimal() +
  scale_fill_manual(values = c("Kas" = "green", 
                               "Hutang" = "red", 
                               "Ekuitas" = "blue"))

print(plot_neraca)

plot_saldokas <- plot_ly(transaksi, x = ~tanggal, y = ~saldo, type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Tren Saldo Kas",
         xaxis = list(title = "Tanggal"),
         yaxis = list(title = "Saldo (Rp)"))

print(plot_saldokas)
