This is a simple function that processes a checkbox-style list field into several columns. This is most useful when processsing data from Google Forms, but it can be used in other cases.

# example

    > test_checkbox_parsing()
      x check        val
    1 1 a,b,c  0.6692541
    2 2 a,c,d  0.6334059
    3 3        0.4261405
    4 4     e  0.9529560
    5 5   a,b -0.8133552

      x        val check_a check_b check_c check_d check_e
    1 1  0.6692541       1       1       1       0       0
    2 2  0.6334059       1       0       1       1       0
    3 3  0.4261405       0       0       0       0       0
    4 4  0.9529560       0       0       0       0       1
    5 5 -0.8133552       1       1       0       0       0

