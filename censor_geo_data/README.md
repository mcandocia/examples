# Instructions

All text `LIKE THIS` refers to adjustable parameters within the Python file. They can be altered to fit your needs, and they have comments explaining what they do.

This program will detect all GPX and CSV files within `SEARCH_DIRECTORY` directories located within `ROOT_DIRECTORY`. By providing longitude, latitude, and radius (in meters) coordinates to either a CSV with the filename `CENSORFILE` as located in the `ROOT_DIRECTORY` and/or by loading dictionaries manually into `CENSOR_COORDINATES`. You can configure various parameters to determine what variables will be censored if coordinates fall within the radius of one of the points you specified, or if the point will be removed entirely.

This program works with CSVs with 'longitude' and 'latitude' column names. You may specify your own, as well. Additional ones that are created by my [convert_fit_to_csv2.py](https://github.com/mcandocia/examples/blob/master/convert_fit_to_csv/convert_fit_to_csv2.py) are also specified, but these values do not need to be changed. You will have to change `ROOT_DIRECTORY`, `SEARCH_DIRECTORIES` in order for the program to work as provided (unless you have the same directory names as I do).

If there is a bug, please leave an issue here on GitHub.

______________

Author: Max Candocia
