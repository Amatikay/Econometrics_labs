
#Poisson distribution
poisson_sample <- rpois(200, 5)  # Генерируем выборку с распределением Пуассона

#Sort sample
sorted_poisson_sample <- sort(poisson_sample)

#Create frequency table 
freq_table <- table(sorted_poisson_sample)
relative_frequencies <- prop.table(freq_table, margin = NULL)



#Create variation series
variation_series <- data.frame(
  Значение = as.numeric(names(freq_table)),
  Абсолютная_частота = as.numeric(freq_table),
  Относительная_частота = relative_frequencies[1]
)

cat("Ряд абсолютн и оносительных частот")
print(variation_series)


#Frequency Polygon
x11()
sample_hist <- hist(poisson_sample)
lines(sample_hist$counts ~ sample_hist$mids)

end <- readline(prompt = "Enter for end program")
