site_dictionary <- function(){
  site_dictionary <- read_delim(delim = " ", file = 
                          "old v2 v3 old2 new
Arh ARH arh Arhelleren Arhelleren
Ovs OVS ovs Ovstedal Ovstedalen
Ves VES ves Veskre Veskre
Skj SKJ skj Skjellingahaugen Skjelingahaugen
Lav LAV lav Lavisdalen Lavisdalen
Gud GUD gud Gudmedalen Gudmedalen
Ulv ULV ulv Ulvhaugen Ulvehaugen
Vik VIK vik Vikesland Vikesland
Hog HOG hog Hogsete Hogsete
Alr ALR alr Alrust Alrust
Fau FAU fau Fauske Fauske
Ram RAM ram Rambera Rambera")
}

dict_TTC_turf <- function(){dict_TTC_turf <- read_delim(delim = ";", file =
                              "TTtreat;plotID
51 TTC;Fau1C
57 TTC;Fau2C
68 TTC;Fau4C
73 TTC;Fau5C
29 TTC;Alr1C
31 TTC;Alr2C
37 TTC;Alr3C
134 TTC;Vik2C
140 TTC;Vik3C
141 TTC;Vik4C
146 TTC;Vik5C
101 TTC;Hog1C
110 TTC;Hog2C
115 TTC;Hog3C
286 TTC;Ovs1C
291 TTC;Ovs2C
297 TTC;Ovs3C
211 TTC;Arh1C
222 TTC;Arh3C
226 TTC;Arh4C
263 TTC;Ves1C
281 TTC;Ves4C
194 TTC;Ram4C
198 TTC;Ram5C
6 TTC;Ulv2C
11 TTC;Ulv3C
236 TTC;Skj1C
243 TTC;Skj2C
246 TTC;Skj3C
251 TTC;Skj4C
511 TTC;Gud12C
") 
}
