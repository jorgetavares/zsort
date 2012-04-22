# Overview
  
zsort is a collection of portable sorting algorithms. Common lisp provides the sort and stable-sort functions but these can have different algorithms implemented according to each implementation. Also, the standard sorting function might not be the best for a certain situation. This library aims to provide developers more options. Although for most situations the standard functions are more than enough, zsort could be a useful complement. 


# Sorting Algorithms

The following comparison based algorithms are implemented:

+ insertion-sort
+ quicksort 
+ randomized-quicksort
+ merge-sort
+ heapsort

At the moment, only one non-comparison algorithm is implemented:

+ counting-sort


# Install

zsort is available via [Quicklisp](http://www.quicklisp.org) and that is the preferable method of installation. To use it, first load Quicklisp in your Common Lisp implementation and then evaluate `(ql:quickload "zsort")`


# How to Use

All comparison sorts have the same syntax `(<sort> sequence predicate &key key)` and return a sorted sequence. All the functions sort destructively, i.e., keep a copy of the unsorted sequence if you wish to keep it. 

Counting sort only accepts sequences without key data and can be sorted in ascending or descending order, according to the value of the `:ascend` key (t for ascending and nil for descending). 

Although zsort accepts list sequences, the algorithms are expected to work on vectors/arrays. A specialized sort for lists will be included in the future.


# Examples

    CL-USER> (zsort:quicksort #(4 5 7 3 9 2 4 2 0 8 2 4 1 5 9) #'<)
     #(0 1 2 2 2 3 4 4 4 5 5 7 8 9 9)

    CL-USER> (zsort:merge-sort #((4 5) (7 3) (9 2) (4 2) (0 8) (2 4) (1 5) (9 1)) #'> :key #'first)
    #((9 2) (9 1) (7 3) (4 5) (4 2) (2 4) (1 5) (0 8))

    CL-USER> (zsort:counting-sort #(4 5 7 3 9 2 4 2 0 8 2 4 1 5 9) :ascend t)
    #(0 1 2 2 2 3 4 4 4 5 5 7 8 9 9)


# Todo

The following is planned for zsort:

+ More sorting algorithms, e.g., timsort, radix sort and bucket sort
+ External sorting to handle large amounts of data
+ Parallel implementations of some sorting algorithms


# License

zsort is available under an MIT-style license. See the LICENSE file.