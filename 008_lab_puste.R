
library(data.table)
library(car)
library(MASS)
load("KrukUWr2018.Rdata")
set.seed(1234)

### Zadanie 1 ####

#  Przygotuj rankę danych `cases_loanamount` bazując na tabeli `cases` tylko z przypadkami kredytów gotówkowych. 



# * Sprawdź miary pozycyjne wszystkich cech w utworzonej ramce.

summary(cases_loanamount)

# * Sprawdź liczność braków danych dla wszystkich cech.


#### Zadanie 2 ####

# Interesującą nas cechą będzie `LoanAmount`, której wartość będziemy modelować w celu zastpąpienia `NA's`.

# * Metodą losowania z rozkładu zastąp braki danych w cesze `Land`.



# * Metodą ekspercką zastąp braki danych w cechach: `Other`,`Gender`, `MeanSalary` i `GDPPerCapita`.



# * Dokonaj dyskretyzacji cechy DPD a wartościom `NA` przypisz poziom `brak danych`, cechę zapisz jako `D_DPD`



#### Zadanie 3 ####

# Na podstawie `cases_loanamount` przygotuj ramki danych:

#   * `cases_loanamount_nas`, która zawiera wszystkie przypadki brakujacych wartości zmiennej `LoanAmount`.



#### Zadanie 4 ####

# Zbadaj rozkład  `cases_loanamount_wonas$LoanAmount`. Jeżeli jest taka potrzeba zaproponuj transformację. 



#### Zadanie 5 ####

# Zbuduj model regresji  liniowej `m1` gdzie zmienną modelowaną jest `LoanAmount` a zmiennymi objaśniającymi :
# `TOA`, `Principal`, `Interest`, `Other`, `GDPPerCapita`, `MeanSalry`, `D_DPD`, `Age`, `Gender` 



#### Zadanie 6 #####




# p-value < 0.05 - odrzucamy hipotezę o normlaności rozkładu

#### Zadanie 7 #####

# Korzystając ze zbioru testowego dokonaj predykcji (wyniki zapisz w obiekcie `m1_pred`), 
# a następinie oblicz bez używania gotowych funkcji: RSS, RSE, TSS i R^2.



#### Zadanie 8 #####

# Dokonaj oceny jakości predykcji za pomocą znanych Ci miar


# Zmierzymy za pomoca RMSE (Root Mean Square Error) i MAPE (Mean Absolute Percentage Erros)





### Zadanie 9 ####

# Sprawdź jak obserwacje odstające wpływają na współczynniki modelu oraz na ocenę jakości
# za pomocą wybranych przez Ciebie miar w zadaniu 9).



# a jaki byl by blad gdybysmy zastapili srednia lub mediana?




#### Zadanie 10 #####

# Czy w modelu `m1` istnieją cechy, które charakteryzują się współliniowością?
# Jeżeli tak jak zmieni się model i ocena jakości po ich wyeliminowaniu.



#APE - "na sucho"
#APE_m2 - bez outlierów
#APE_mean
#APE_median
#APE_m3 - bez skorelowanych


### Zadanie 11 ####

# Zaproponuj własny model regresji liniowej korzystając z wiedzy zdobytej na wykładzie
# (transformacje zmiennych, interakcje między cechami, dyskretyzacja cech ciągłych, itp...)
#, który będzie cechować się lepszymi parametrami oceny jakości predyckji od wyestymowanego na ćwiczeniach.


## Liczę na Państwa zaangażowanie i kreatywność :) Do dzieła !!! #############



