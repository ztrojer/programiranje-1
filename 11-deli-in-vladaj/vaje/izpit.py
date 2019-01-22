##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################
def pivot(a, start, end):
    # save pivot
    pivot = a[start]
    # save pointers
    front_i = start
    back_i = end
    # move pointers and change elements if needed
    while front_i != back_i:
        if a[front_i + 1] <= pivot:
            front_i += 1
        elif a[back_i] > pivot:
            back_i -= 1
        else:
            temp = a[front_i + 1]
            a[front_i + 1] = a[back_i]
            a[back_i] = temp
    # move pivot
    a[start] = a[front_i]
    a[front_i] = pivot
    # return the final index of pivot
return front_i


def quicksort_part(a, start, end):
    if start >= end:
        return
    else:
        pivot_i = pivot_list(a, start, end)
        quicksort_part(a, start, pivot_i - 1)
        quicksort_part(a, pivot_i + 1, end)
        return


def quicksort(a):
    quicksort_part(a, 0, len(a) - 1)
    return

def kth_el_part(a, k, start, end):
    if start > end:
        return None
    else:
        pivot_i = pivot_list(a, start, end)
        if pivot_i == k:
            return a[pivot_i]
        elif pivot_i > k:
            return kth_el_part(a, k, start, pivot_i - 1)
        else:
            return kth_el_part(a, k, pivot_i + 1, end)


def kth_element(a, k):
    if k > len(a):
        return None
    else:
        return kth_el_part(a, k, 0, len(a)-1)