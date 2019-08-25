#!/usr/bin/env python3
ENCODING : str = 'iso-8859-1'

import sys
import pandas   # pip install pandas


# SETUP
print('**** CSV data to TXT ****', file=sys.stderr)

fin : str = sys.argv[1]             # filepath: input data as csv file
print('Input file: %s' % fin, file=sys.stderr)

column_field : str = '#'            # string: column field identifier
print('Column identifier: \'%s\'' % column_field, file=sys.stderr)

fmod : str = sys.argv[2]            # filepath: file model to follow
print('Model file: %s' % fmod, file=sys.stderr)

if len(sys.argv) > 3:
    fout : str = sys.argv[3]        # filepath: generated output file
    print('Output file: %s' % fout, file=sys.stderr)
    output = open(fout, mode='w+', encoding=ENCODING)
else:                               # otherwise send to stdout
    output = sys.stdout
    output.reconfigure(encoding=ENCODING)

data = pandas.read_csv(fin, sep=',', encoding=ENCODING)
model = open(fmod, mode='r', encoding=ENCODING)


# PROCESSING
model.seek(0, 2)
model_end = model.tell()
print('Model file has %d characters.' % model_end, file=sys.stderr)

rows : int = len(data.index)
print('Input csv file has %d rows.' % rows, file=sys.stderr)

print('Processing...', file=sys.stderr, end='')
for i in range(0, rows):
    print('.', file=sys.stderr, end='')
    cursor : int = 0

    # rewrites the model for each line in the csv file
    while (cursor < model_end):
        model.seek(cursor, 0)
        ch : str = model.read(1)    # one char at a time

        if ch == column_field:
            j = int(model.read(1))  # read the column number next to the column_field @fixme single digit
            cursor += 1
            output.write( str(data.iloc[i, j]) )    # write the specified cell

        else:
            output.write(ch)

        cursor += 1 # moves the cursor


# END
output.close()
model.close()
print('\nDone!', file=sys.stderr)
