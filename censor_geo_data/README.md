# Instructions

All text `LIKE THIS` refers to adjustable parameters within the Python file. They can be altered to fit your needs, and they have comments explaining what they do.

This program will detect all GPX and CSV files within `SEARCH_DIRECTORY` directories located within `ROOT_DIRECTORY`. By providing longitude, latitude, and radius (in meters) coordinates to either a CSV with the filename `CENSORFILE` as located in the `ROOT_DIRECTORY` and/or by loading dictionaries manually into `CENSOR_COORDINATES`. You can configure various parameters to determine what variables will be censored if coordinates fall within the radius of one of the points you specified, or if the point will be removed entirely.

This program works with CSVs with 'longitude' and 'latitude' column names. You may specify your own, as well. Additional ones that are created by my [convert_fit_to_csv2.py](https://github.com/mcandocia/examples/blob/master/convert_fit_to_csv/convert_fit_to_csv2.py) are also specified, but these values do not need to be changed. You will have to change `ROOT_DIRECTORY`, `SEARCH_DIRECTORIES` in order for the program to work as provided (unless you have the same directory names as I do). 

Also, you should either create a file 'censor.csv' or change `CENSORFILE`'s value to reflect whether or not you are using a CSV to provide coordinates or are manually doing so. Without specifying any coordinates, you are simply running a filter that does some minor formatting tweaking of GPX files and zips the result up.

You can also specify `'location':False` in `CENSOR_PARAMS`. If you do, then instead of censoring, n columns will be added to CSVs and various `<rflag>` (with an index attribute) elements will be added to the GPX files, indicating the index of which coordinate pair+radius was detected for a given point. This is useful for visualizing the effects of running the program in CENSOR mode, or if there are other features you want to extract based on distance (although I would probably just use a different program in the latter case).

If there is a bug, please leave an issue here on GitHub.

______________

Author: Max Candocia
