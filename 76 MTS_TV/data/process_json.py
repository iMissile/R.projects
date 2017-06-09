#!/usr/bin/python
# -*- coding: cp1251 -*-
import os

def main():

    print(os.path.basename(os.getcwd()))
    fout = open("stat_fix.json", 'w')

    num_lines = sum(1 for line in open('stat.json')) # посчитали общее число строк в файле
    print(num_lines)
    
    try:
        with open("stat.json", 'r') as fin:
            i = 0
            for line in fin:
                # print(value='', sep=' ', end='\n', file=sys.stdout, flush=False)
                # print(line)
                i = i + 1
                print(i)
                if i % 10 == 0: 
                    print(i)
    finally:
        fin.close()
        fout.close()
        


if __name__ == '__main__':
    main()
    


        
        
        