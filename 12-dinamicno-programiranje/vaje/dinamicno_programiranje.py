#(* ========== Vaje 6: Dinamično programiranje  ========== *)


#----------------------------------------------------------------------------*]
# Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
# samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
# v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
# različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
# zanima, katero pot naj ubere.

# Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
# sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
# optimalni poti.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # max_cheese test_matrix;;
# - : int = 13
#[*----------------------------------------------------------------------------*)
test_matrix = [[ 1, 2 ,0], [2, 4, 5], [7, 0, 1]]

tabela = [[0 for i in range(len(test_matrix))] for j in range(len(test_matrix))]

def max_cheese(i, j, matrika):
    if (i + 1 == len(matrika)) and (j + 1 == len(matrika)): #ko pridem do konca
        return matrika[i][j]
    if (i + 1 > len(matrika)):
        return 0
    if (j+1 > len(matrika)):
        return 0
    if (i + 1 < len(matrika)) and (j + 1 == (len(matrika))): # ko sem v zadnji vrstici lahko grem samo še dol
        tabela[i][j] = matrika[i][j] + max_cheese(i+1, j, matrika)
    if (i+1 == len(matrika)) and (j+1 < len(matrika)):
        tabela[i][j] = matrika[i][j] + max_cheese(i, j+1, matrika)
    else:
        tabela[i][j] = matrika[i][j] + max(max_cheese(i+1, j, matrika), max_cheese(i, j+1, matrika))

    return tabela[i][j]

max_cheese(0,0,test_matrix)
print(tabela[0][0])
    

    
    
    



#(*----------------------------------------------------------------------------*]
#Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
#različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
#3, rdeči pa višin 1 in 2.

# Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
# dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
# v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
# poljubne barve.

# Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # alternating_towers 10;;
# - : int = 35
#[*----------------------------------------------------------------------------*)
